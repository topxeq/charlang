// Copyright (c) 2020 Ozan Hacıbekiroğlu.
// Use of this source code is governed by a MIT License
// that can be found in the LICENSE file.

package charlang

import (
	"bytes"
	"encoding"
	"encoding/binary"
	"encoding/gob"
	"errors"
	"fmt"
	"io"
	"math"

	"github.com/topxeq/charlang/parser"
)

// Bytecode signature and version are written to the header of encoded Bytecode.
// Bytecode is encoded with current BytecodeVersion and its format,
// and versions up to BytecodeMaxVersion will be supported.
// Note: Always support 3 versions for Bytecode and Object encoding.
const (
	BytecodeSignature  uint32 = 0x75474F
	BytecodeVersion    uint16 = 1
	BytecodeMaxVersion uint16 = 3
)

const (
	binUndefinedV1 byte = iota
	binTrueV1
	binFalseV1
	binIntV1
	binUintV1
	binByteV1
	binCharV1
	binFloatV1
	binStringV1
	binBytesV1
	binArrayV1
	binMapV1
	binSyncMapV1
	binCompiledFunctionV1
	binFunctionV1
	binBuiltinFunctionV1

	binUnkownType byte = 255
)

type (
	sourceFileSet parser.SourceFileSet
	sourceFile    parser.SourceFile
)

var (
	errVarintTooSmall = errors.New("read varint error: buf too small")
	errVarintOverflow = errors.New("read varint error: value larger than 64 bits (overflow)")
)

// MarshalBinary implements encoding.BinaryMarshaler
func (bc *Bytecode) MarshalBinary() (data []byte, err error) {
	switch BytecodeVersion {
	case 1:
		var buf bytes.Buffer
		if err = bc.bytecodeV1Encoder(&buf); err != nil {
			return nil, err
		}
		return buf.Bytes(), nil
	default:
		panic(fmt.Errorf("invalid Bytecode version:%d", BytecodeVersion))
	}
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
// Do not use this method if builtin modules are used, instead use Decode method.
func (bc *Bytecode) UnmarshalBinary(data []byte) error {
	if len(data) < 6 {
		return errors.New("invalid Bytecode data")
	}

	sig := binary.BigEndian.Uint32(data[0:4])
	if sig != BytecodeSignature {
		return errors.New("Bytecode encoding signature mismatch")
	}

	version := binary.BigEndian.Uint16(data[4:6])
	switch {
	case version >= 1 && version <= BytecodeMaxVersion:
		buf := bytes.NewBuffer(data[6:])
		err := bc.bytecodeV1Decoder(buf)
		if err != nil {
			return err
		}
		return nil
	default:
		return fmt.Errorf("Bytecode unsupported version:%d", version)
	}
}

func putBytecodeHeader(w io.Writer) (err error) {
	sig := make([]byte, 4)
	binary.BigEndian.PutUint32(sig, BytecodeSignature)
	if _, err = io.Copy(w, bytes.NewReader(sig)); err != nil {
		return
	}

	bcVersion := make([]byte, 2)
	binary.BigEndian.PutUint16(bcVersion, BytecodeVersion)

	if _, err = io.Copy(w, bytes.NewReader(bcVersion)); err != nil {
		return
	}
	return nil
}

func (bc *Bytecode) bytecodeV1Encoder(w io.Writer) (err error) {
	if err = putBytecodeHeader(w); err != nil {
		return
	}

	// FileSet, field #0
	if bc.FileSet != nil {
		writeByteTo(w, 0)
		var data []byte
		fs := (*sourceFileSet)(bc.FileSet)
		if data, err = fs.MarshalBinary(); err != nil {
			return
		}
		var sz []byte
		if sz, err = Int(len(data)).MarshalBinary(); err != nil {
			return
		}
		w.Write(sz)
		w.Write(data)
	}

	// Main, field #1
	if bc.Main != nil {
		writeByteTo(w, 1)
		var data []byte
		if data, err = bc.Main.MarshalBinary(); err != nil {
			return
		}
		if _, err = w.Write(data); err != nil {
			return
		}
	}

	// Constants, field #2
	if bc.Constants != nil {
		writeByteTo(w, 2)
		var data []byte
		if data, err = Array(bc.Constants).MarshalBinary(); err != nil {
			return
		}
		if _, err = w.Write(data); err != nil {
			return
		}
	}

	// NumModules, field #3
	if bc.NumModules > 0 {
		writeByteTo(w, 3)
		var data []byte
		data, err = Int(bc.NumModules).MarshalBinary()
		if err != nil {
			return
		}
		if _, err = w.Write(data); err != nil {
			return
		}
	}
	return nil
}

func (bc *Bytecode) bytecodeV1Decoder(r *bytes.Buffer) error {
	for {
		field, err := r.ReadByte()
		if err != nil {
			if err == io.EOF {
				return nil
			}
			return err
		}

		switch field {
		case 0:
			obj, err := DecodeObject(r)
			if err != nil {
				return err
			}

			sz := obj.(Int)
			if sz <= 0 {
				continue
			}

			data := make([]byte, sz)
			if _, err = io.ReadFull(r, data); err != nil {
				return err
			}

			var fs sourceFileSet
			if err = fs.UnmarshalBinary(data); err != nil {
				return err
			}
			bc.FileSet = (*parser.SourceFileSet)(&fs)
		case 1:
			f, err := DecodeObject(r)
			if err != nil {
				return err
			}

			bc.Main = f.(*CompiledFunction)
		case 2:
			obj, err := DecodeObject(r)
			if err != nil {
				return err
			}

			bc.Constants = obj.(Array)
		case 3:
			num, err := DecodeObject(r)
			if err != nil {
				return err
			}

			bc.NumModules = int(num.(Int))
		default:
			return fmt.Errorf("unknown field:%d", field)
		}
	}
}

// DecodeObject decodes and returns Object from a io.Reader which is encoded with MarshalBinary.
func DecodeObject(r io.Reader) (Object, error) {
	btype, err := readByteFrom(r)
	if err != nil {
		return nil, err
	}

	switch btype {
	case binUndefinedV1:
		return Undefined, nil
	case binTrueV1:
		return True, nil
	case binFalseV1:
		return False, nil
	case binIntV1,
		binUintV1,
		binByteV1,
		binFloatV1,
		binCharV1:

		size, err := readByteFrom(r)
		if err != nil {
			return nil, err
		}

		buf := make([]byte, 2+size)
		buf[0] = btype
		buf[1] = size
		if size > 0 {
			if _, err = io.ReadFull(r, buf[2:]); err != nil {
				return nil, err
			}
		}

		switch btype {
		case binIntV1:
			var v Int
			if err = v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		case binUintV1:
			var v Uint
			if err = v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		case binByteV1:
			var v Byte
			if err = v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		case binFloatV1:
			var v Float
			if err = v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		case binCharV1:
			var v Char
			if err = v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		}
	case binCompiledFunctionV1,
		binArrayV1,
		binBytesV1,
		binStringV1,
		binMapV1,
		binSyncMapV1,
		binFunctionV1,
		binBuiltinFunctionV1:

		var vi varintConv
		value, readBytes, err := vi.readBytes(r)
		if err != nil {
			return nil, err
		}

		if value < 0 {
			return nil, errors.New("negative value")
		}

		n := 1 + len(readBytes)
		buf := make([]byte, n+int(value))
		buf[0] = btype
		copy(buf[1:], readBytes)

		if value > 0 {
			if _, err = io.ReadFull(r, buf[n:]); err != nil {
				return nil, err
			}
		}

		switch btype {
		case binCompiledFunctionV1:
			var v CompiledFunction
			if err := v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return &v, nil
		case binArrayV1:
			var v = Array{}
			if err := v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		case binBytesV1:
			var v = Bytes{}
			if err := v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		case binStringV1:
			var v String
			if err := v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		case binMapV1:
			var v = Map{}
			if err := v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return v, nil
		case binSyncMapV1:
			var v SyncMap
			if err := v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return &v, nil
		case binFunctionV1:
			var v Function
			if err := v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return &v, nil
		case binBuiltinFunctionV1:
			var v BuiltinFunction
			if err := v.UnmarshalBinary(buf); err != nil {
				return nil, err
			}
			return &v, nil
		}
	case binUnkownType:
		var x interface{}
		if err := gob.NewDecoder(r).Decode(&x); err != nil {
			return nil, err
		}

		if v, ok := x.(Object); ok {
			return v, nil
		}
		return nil, fmt.Errorf("decode error: unknown type '%T'", x)
	}
	return nil, fmt.Errorf("decode error: unknown encoding type:%d", btype)
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o undefined) MarshalBinary() ([]byte, error) {
	return []byte{binUndefinedV1}, nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o undefined) UnmarshalBinary(data []byte) error {
	if len(data) < 1 || data[0] != binUndefinedV1 {
		return errors.New("invalid undefined data")
	}
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Bool) MarshalBinary() ([]byte, error) {
	if o {
		return []byte{binTrueV1}, nil
	}
	return []byte{binFalseV1}, nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Bool) UnmarshalBinary(data []byte) error {
	if len(data) < 1 {
		return errors.New("invalid Bool data")
	}

	if data[0] == binTrueV1 {
		*o = True
		return nil
	}

	if data[0] == binFalseV1 {
		*o = False
		return nil
	}
	return errors.New("invalid Bool data")
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Int) MarshalBinary() ([]byte, error) {
	buf := make([]byte, 2+binary.MaxVarintLen64)
	buf[0] = binIntV1

	if o == 0 {
		buf[1] = 0
		return buf[:2], nil
	}

	n := binary.PutVarint(buf[2:], int64(o))
	buf[1] = byte(n)
	return buf[:2+n], nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Int) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binIntV1 {
		return errors.New("invalid Int data")
	}

	size := int(data[1])
	if size <= 0 {
		return nil
	}

	if len(data) < 2+size {
		return errors.New("invalid Int data size")
	}

	v, n := binary.Varint(data[2:])
	if n < 1 {
		if n == 0 {
			return errors.New("Int data buffer too small")
		}
		return errors.New("Int value larger than 64 bits")
	}

	*o = Int(v)
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Uint) MarshalBinary() ([]byte, error) {
	buf := make([]byte, 2+binary.MaxVarintLen64)
	buf[0] = binUintV1
	if o == 0 {
		buf[1] = 0
		return buf[:2], nil
	}

	n := binary.PutUvarint(buf[2:], uint64(o))
	buf[1] = byte(n)
	return buf[:2+n], nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Uint) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binUintV1 {
		return errors.New("invalid Uint data")
	}

	size := int(data[1])
	if size <= 0 {
		return nil
	}

	if len(data) < 2+size {
		return errors.New("invalid Uint data size")
	}

	v, n := binary.Uvarint(data[2:])
	if n < 1 {
		if n == 0 {
			return errors.New("Uint data buffer too small")
		}
		return errors.New("Uint value larger than 64 bits")
	}

	*o = Uint(v)
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Byte) MarshalBinary() ([]byte, error) {
	return []byte{binByteV1, 1, byte(o)}, nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Byte) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binByteV1 {
		return errors.New("invalid Byte data")
	}

	if data[1] != 1 {
		return errors.New("invalid Byte data size")
	}

	*o = Byte(data[2])
	return nil

}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Char) MarshalBinary() ([]byte, error) {
	buf := make([]byte, 2+binary.MaxVarintLen32)
	buf[0] = binCharV1
	if o == 0 {
		buf[1] = 0
		return buf[:2], nil
	}

	n := binary.PutVarint(buf[2:], int64(o))
	buf[1] = byte(n)
	return buf[:2+n], nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Char) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binCharV1 {
		return errors.New("invalid Char data")
	}

	size := int(data[1])
	if size <= 0 {
		return nil
	}

	if len(data) < 2+size {
		return errors.New("invalid Char data size")
	}

	v, n := binary.Varint(data[2:])
	if n < 1 {
		if n == 0 {
			return errors.New("Char data buffer too small")
		}
		return errors.New("value larger than 64 bits")
	}

	if int64(rune(v)) != v {
		return errors.New("Char value larger than 32 bits")
	}

	*o = Char(v)
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Float) MarshalBinary() ([]byte, error) {
	buf := make([]byte, 2+binary.MaxVarintLen64)
	buf[0] = binFloatV1
	if o == 0 {
		buf[1] = 0
		return buf[:2], nil
	}

	n := binary.PutUvarint(buf[2:], math.Float64bits(float64(o)))
	buf[1] = byte(n)
	return buf[:2+n], nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Float) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binFloatV1 {
		return errors.New("invalid Float data")
	}

	size := int(data[1])
	if size <= 0 {
		return nil
	}

	if len(data) < 2+size {
		return errors.New("invalid Float data size")
	}

	v, n := binary.Uvarint(data[2:])
	if n < 1 {
		if n == 0 {
			return errors.New("Float data buffer too small")
		}
		return errors.New("Float value larger than 64 bits")
	}

	*o = Float(math.Float64frombits(v))
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o String) MarshalBinary() ([]byte, error) {
	var buf bytes.Buffer
	buf.WriteByte(binStringV1)
	size := int64(len(o.Value))

	if size == 0 {
		buf.WriteByte(0)
		return buf.Bytes(), nil
	}

	var vi varintConv
	b := vi.toBytes(size)
	buf.Write(b)
	buf.WriteString(FromString(o))
	return buf.Bytes(), nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *String) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binStringV1 {
		return errors.New("invalid String data")
	}

	size, offset, err := toVarint(data[1:])
	if err != nil {
		return err
	}

	if size <= 0 {
		return nil
	}

	ub := 1 + offset + int(size)
	if len(data) < ub {
		return errors.New("invalid String data size")
	}

	*o = ToString(data[1+offset : ub])
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Bytes) MarshalBinary() ([]byte, error) {
	var buf bytes.Buffer
	buf.WriteByte(binBytesV1)
	size := int64(len(o))

	if size == 0 {
		buf.WriteByte(0)
		return buf.Bytes(), nil
	}

	var vi varintConv
	b := vi.toBytes(size)
	buf.Write(b)
	buf.Write(o)
	return buf.Bytes(), nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Bytes) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binBytesV1 {
		return errors.New("invalid Bytes data")
	}

	size, offset, err := toVarint(data[1:])
	if err != nil {
		return err
	}

	if size <= 0 {
		return nil
	}

	ub := 1 + offset + int(size)
	if len(data) < ub {
		return errors.New("invalid Bytes data size")
	}

	*o = []byte(string(data[1+offset : ub]))
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Array) MarshalBinary() ([]byte, error) {
	var buf bytes.Buffer
	buf.WriteByte(binArrayV1)
	if len(o) == 0 {
		buf.WriteByte(0)
		return buf.Bytes(), nil
	}

	var tmpBuf bytes.Buffer
	var vi varintConv
	b := vi.toBytes(int64(len(o)))
	tmpBuf.Write(b)

	for _, v := range o {
		if !isEncSupported(v) {
			tmpBuf.WriteByte(binUnkownType)
			if err := gob.NewEncoder(&tmpBuf).Encode(v); err != nil {
				return nil, err
			}
		} else {
			d, err := v.(encoding.BinaryMarshaler).MarshalBinary()
			if err != nil {
				return nil, err
			}
			tmpBuf.Write(d)
		}
	}

	b = vi.toBytes(int64(tmpBuf.Len()))
	buf.Write(b)
	buf.Write(tmpBuf.Bytes())
	return buf.Bytes(), nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Array) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binArrayV1 {
		return errors.New("invalid Array data")
	}

	size, offset, err := toVarint(data[1:])
	if err != nil {
		return err
	}

	if size <= 0 {
		return nil
	}
	ub := 1 + offset + int(size)
	if len(data) < ub {
		return errors.New("invalid Array data size")
	}

	rd := bytes.NewReader(data[1+offset : ub])
	var vi varintConv
	vi.reader = rd

	length, err := vi.read()
	if err != nil {
		return err
	}

	arr := make([]Object, 0, int(length))
	for rd.Len() > 0 {
		o, err := DecodeObject(rd)
		if err != nil {
			return err
		}
		arr = append(arr, o)
	}

	*o = arr
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o Map) MarshalBinary() ([]byte, error) {
	var buf bytes.Buffer
	buf.WriteByte(binMapV1)

	var tmpBuf bytes.Buffer
	var vi varintConv

	for k, v := range o {
		b := vi.toBytes(int64(len(k)))
		tmpBuf.Write(b)
		tmpBuf.WriteString(k)
		if !isEncSupported(v) {
			tmpBuf.WriteByte(binUnkownType)
			if err := gob.NewEncoder(&tmpBuf).Encode(v); err != nil {
				return nil, err
			}
		} else {
			d, err := v.(encoding.BinaryMarshaler).MarshalBinary()
			if err != nil {
				return nil, err
			}
			tmpBuf.Write(d)
		}
	}

	b := vi.toBytes(int64(tmpBuf.Len()))
	buf.Write(b)
	buf.Write(tmpBuf.Bytes())
	return buf.Bytes(), nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Map) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binMapV1 {
		return errors.New("invalid Map data")
	}

	size, offset, err := toVarint(data[1:])
	if err != nil {
		return err
	}

	if size <= 0 {
		return nil
	}

	if len(data) < 1+offset+int(size) {
		return errors.New("invalid Map data size")
	}

	rd := bytes.NewReader(data[1+offset : 1+offset+int(size)])
	strBuf := bytes.NewBuffer(nil)
	var vi varintConv
	vi.reader = rd
	m := *o

	for rd.Len() > 0 {
		value, err := vi.read()
		if err != nil {
			return err
		}

		var k string
		if value > 0 {
			strBuf.Reset()
			if _, err = io.CopyN(strBuf, rd, value); err != nil {
				return err
			}
			k = strBuf.String()
		}

		o, err := DecodeObject(rd)
		if err != nil {
			return err
		}
		m[k] = o
	}
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o *SyncMap) MarshalBinary() ([]byte, error) {
	var buf bytes.Buffer
	if o.Map == nil {
		buf.WriteByte(binSyncMapV1)
		buf.WriteByte(0)
		return buf.Bytes(), nil
	}

	b, err := o.Map.MarshalBinary()
	if err != nil {
		return nil, err
	}

	if len(b) > 0 {
		b[0] = binSyncMapV1
	}
	return b, nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *SyncMap) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binSyncMapV1 {
		return errors.New("invalid SyncMap data")
	}

	if data[1] == 0 {
		return nil
	}

	data[0] = binMapV1
	m := Map{}
	if err := m.UnmarshalBinary(data); err != nil {
		data[0] = binSyncMapV1
		return err
	}

	data[0] = binSyncMapV1
	o.Map = m
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o *CompiledFunction) MarshalBinary() ([]byte, error) {
	var tmpBuf bytes.Buffer
	var vi varintConv
	if o.NumParams > 0 {
		// NumParams field #0
		tmpBuf.WriteByte(0)
		b := vi.toBytes(int64(o.NumParams))
		tmpBuf.Write(b)
	}

	if o.NumLocals > 0 {
		// NumLocals field #1
		tmpBuf.WriteByte(1)
		b := vi.toBytes(int64(o.NumLocals))
		tmpBuf.Write(b)
	}

	if o.Instructions != nil {
		// Instructions field #2
		tmpBuf.WriteByte(2)
		data, err := Bytes(o.Instructions).MarshalBinary()
		if err != nil {
			return nil, err
		}
		tmpBuf.Write(data)
	}

	// Variadic field #3
	if o.Variadic {
		tmpBuf.WriteByte(3)
	}

	// Free field #4, ignore Free variables, doesn't make sense
	if o.SourceMap != nil {
		// SourceMap field #5
		tmpBuf.WriteByte(5)
		b := vi.toBytes(int64(len(o.SourceMap) * 2))
		tmpBuf.Write(b)
		for key, value := range o.SourceMap {
			b = vi.toBytes(int64(key))
			tmpBuf.Write(b)
			b = vi.toBytes(int64(value))
			tmpBuf.Write(b)
		}
	}

	var buf bytes.Buffer
	size := vi.toBytes(int64(tmpBuf.Len()))
	buf.WriteByte(binCompiledFunctionV1)
	buf.Write(size)
	buf.Write(tmpBuf.Bytes())
	return buf.Bytes(), nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *CompiledFunction) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binCompiledFunctionV1 {
		return errors.New("invalid CompiledFunction data")
	}

	size, offset, err := toVarint(data[1:])
	if err != nil {
		return err
	}

	if size <= 0 {
		return nil
	}

	rd := bytes.NewReader(data[1+offset : 1+offset+int(size)])
	var vi varintConv
	vi.reader = rd

	for rd.Len() > 0 {
		field, err := rd.ReadByte()
		if err != nil {
			return err
		}
		switch field {
		case 0:
			v, err := vi.read()
			if err != nil {
				return err
			}
			o.NumParams = int(v)
		case 1:
			v, err := vi.read()
			if err != nil {
				return err
			}
			o.NumLocals = int(v)
		case 2:
			obj, err := DecodeObject(rd)
			if err != nil {
				return err
			}
			o.Instructions = obj.(Bytes)
		case 3:
			o.Variadic = true
		case 4:
			return errors.New("unexpected field #4")
		case 5:
			length, err := vi.read()
			if err != nil {
				return err
			}

			sz := int(length / 2)
			// always put size to the map to decode faster
			o.SourceMap = make(map[int]int, sz)
			for i := 0; i < sz; i++ {
				key, err := vi.read()
				if err != nil {
					return err
				}
				value, err := vi.read()
				if err != nil {
					return err
				}
				o.SourceMap[int(key)] = int(value)
			}
		default:
			return fmt.Errorf("unknown field:%d", field)
		}
	}
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o *BuiltinFunction) MarshalBinary() ([]byte, error) {
	// Note: use string name instead of index of builtin
	s, err := ToString(o.Name).MarshalBinary()
	if err != nil {
		return nil, err
	}

	var vi varintConv
	b := vi.toBytes(int64(len(s)))
	data := make([]byte, 0, 1+len(b)+len(s))
	data = append(data, binBuiltinFunctionV1)
	data = append(data, b...)
	data = append(data, s...)
	return data, nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *BuiltinFunction) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binBuiltinFunctionV1 {
		return errors.New("invalid BuiltinFunction data")
	}

	size, offset, err := toVarint(data[1:])
	if err != nil {
		return err
	}

	if size <= 0 {
		return errors.New("invalid BuiltinFunction data size")
	}

	var s String
	if err := s.UnmarshalBinary(data[1+offset:]); err != nil {
		return err
	}

	index, ok := BuiltinsMap[FromString(s)]
	if !ok {
		return fmt.Errorf("builtin '%s' not found", s)
	}

	obj := BuiltinObjects[index]
	f, ok := obj.(*BuiltinFunction)
	if ok {
		*o = *f
		return nil
	}
	return fmt.Errorf("builtin '%s' not a BuiltinFunction type", s)
}

// MarshalBinary implements encoding.BinaryMarshaler
func (o *Function) MarshalBinary() ([]byte, error) {
	s, err := ToString(o.Name).MarshalBinary()
	if err != nil {
		return nil, err
	}

	var vi varintConv
	b := vi.toBytes(int64(len(s)))
	data := make([]byte, 0, 1+len(b)+len(s))
	data = append(data, binFunctionV1)
	data = append(data, b...)
	data = append(data, s...)
	return data, nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (o *Function) UnmarshalBinary(data []byte) error {
	if len(data) < 2 || data[0] != binFunctionV1 {
		return errors.New("invalid Function data")
	}

	size, offset, err := toVarint(data[1:])
	if err != nil {
		return err
	}

	if size <= 0 {
		return errors.New("invalid Function data size")
	}

	var s String
	if err := s.UnmarshalBinary(data[1+offset:]); err != nil {
		return err
	}
	o.Name = FromString(s)
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (sf *sourceFile) MarshalBinary() ([]byte, error) {
	var buf bytes.Buffer
	d, err := ToString(sf.Name).MarshalBinary()
	if err != nil {
		return nil, err
	}

	buf.Write(d)
	var vi varintConv
	b := vi.toBytes(int64(sf.Base))
	buf.Write(b)

	b = vi.toBytes(int64(sf.Size))
	buf.Write(b)

	b = vi.toBytes(int64(len(sf.Lines)))
	buf.Write(b)

	for _, v := range sf.Lines {
		b = vi.toBytes(int64(v))
		buf.Write(b)
	}
	return buf.Bytes(), nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (sf *sourceFile) UnmarshalBinary(data []byte) error {
	rd := bytes.NewReader(data)

	obj, err := DecodeObject(rd)
	if err != nil {
		return err
	}

	sf.Name = obj.String()
	var vi varintConv
	vi.reader = rd
	v, err := vi.read()
	if err != nil {
		return err
	}

	sf.Base = int(v)

	v, err = vi.read()
	if err != nil {
		return err
	}

	sf.Size = int(v)

	v, err = vi.read()
	if err != nil {
		return err
	}

	length := int(v)

	lines := make([]int, length)
	for i := 0; i < length; i++ {
		v, err = vi.read()
		if err != nil {
			return err
		}
		lines[i] = int(v)
	}

	if rd.Len() > 0 {
		return errors.New("unread bytes")
	}

	sf.Lines = lines
	return nil
}

// MarshalBinary implements encoding.BinaryMarshaler
func (sfs *sourceFileSet) MarshalBinary() ([]byte, error) {
	var buf bytes.Buffer
	var vi varintConv
	b := vi.toBytes(int64(sfs.Base))
	buf.Write(b)

	b = vi.toBytes(int64(len(sfs.Files)))
	buf.Write(b)

	for _, v := range sfs.Files {
		if v == nil {
			continue
		}
		d, err := (*sourceFile)(v).MarshalBinary()
		if err != nil {
			return nil, err
		}
		b := vi.toBytes(int64(len(d)))
		buf.Write(b)
		buf.Write(d)
	}

	return buf.Bytes(), nil
}

// UnmarshalBinary implements encoding.BinaryUnmarshaler
func (sfs *sourceFileSet) UnmarshalBinary(data []byte) error {
	rd := bytes.NewReader(data)
	var vi varintConv
	vi.reader = rd
	v, err := vi.read()
	if err != nil {
		return err
	}

	sfs.Base = int(v)

	v, err = vi.read()
	if err != nil {
		return err
	}

	length := int(v)
	files := make([]*parser.SourceFile, length)

	for i := 0; i < length; i++ {
		v, err = vi.read()
		if err != nil {
			return err
		}
		data := make([]byte, v)
		if _, err = io.ReadFull(rd, data); err != nil {
			return err
		}
		var file sourceFile
		if err = file.UnmarshalBinary(data); err != nil {
			return err
		}
		files[i] = (*parser.SourceFile)(&file)
	}

	if rd.Len() > 0 {
		return errors.New("unread bytes")
	}

	sfs.Files = files
	return nil
}

func readByteFrom(r io.Reader) (byte, error) {
	if br, ok := r.(io.ByteReader); ok {
		return br.ReadByte()
	}

	var one = []byte{0}
	n, err := r.Read(one)
	if err != nil {
		if err == io.EOF {
			if n == 1 {
				return one[0], nil
			}
		}
		return 0, err
	}

	if n == 1 {
		return one[0], nil
	}
	return 0, errors.New("byte read error")
}

func writeByteTo(w io.Writer, b byte) error {
	if bw, ok := w.(io.ByteWriter); ok {
		return bw.WriteByte(b)
	}

	n, err := w.Write([]byte{b})
	if err != nil {
		return err
	}

	if n != 1 {
		return errors.New("byte write error")
	}
	return nil
}

type varintConv struct {
	buf    [1 + binary.MaxVarintLen64]byte
	reader *bytes.Reader
}

func (vi *varintConv) toBytes(v int64) []byte {
	n := binary.PutVarint(vi.buf[1:], v)
	vi.buf[0] = byte(n)
	return vi.buf[:n+1]
}

func (vi *varintConv) read() (value int64, err error) {
	var n byte
	n, err = vi.reader.ReadByte()
	if err != nil {
		return
	}

	if int(n) > len(vi.buf) {
		return 0, errVarintOverflow
	}

	data := vi.buf[:n]
	if n == 0 {
		return
	}

	if _, err = io.ReadFull(vi.reader, data); err != nil {
		return
	}

	var offset int
	value, offset = binary.Varint(data)
	if offset < 1 {
		if offset == 0 {
			err = errVarintTooSmall
			return
		}
		err = errVarintOverflow
		return
	}
	return
}

func (vi *varintConv) readBytes(r io.Reader) (value int64, readBytes []byte, err error) {
	var n byte
	n, err = readByteFrom(r)
	if err != nil {
		return
	}

	if 1+int(n) > len(vi.buf) {
		return 0, nil, errVarintOverflow
	}

	readBytes = vi.buf[:1+n]
	readBytes[0] = n
	if n == 0 {
		return
	}

	if _, err = io.ReadFull(r, readBytes[1:]); err != nil {
		return
	}

	var offset int
	value, offset = binary.Varint(readBytes[1:])
	if offset < 1 {
		if offset == 0 {
			err = errVarintTooSmall
			return
		}
		err = errVarintOverflow
		return
	}
	return
}

// toVarint converts a byte slice to int64. If length of slice is 0, it panics.
func toVarint(data []byte) (value int64, offset int, err error) {
	size := int(data[0])
	if size == 0 {
		offset = 1
		return
	}

	if len(data) < 1+size {
		err = errVarintTooSmall
		return
	}

	value, offset = binary.Varint(data[1:])
	if offset < 1 {
		if offset == 0 {
			err = errVarintTooSmall
			return
		}
		err = errVarintOverflow
		return
	}

	offset++
	return
}

func isEncSupported(o Object) bool {
	switch o.(type) {
	case Bool, Int, Uint, Byte, Char, Float, String, Bytes, Array, Map,
		*SyncMap, *CompiledFunction, *Function, *BuiltinFunction:
		return true
	default:
		return o == Undefined
	}
}
