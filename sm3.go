package charlang

// Pure-Go implementation of the SM3 cryptographic hash function,
// following the Chinese national standard GB/T 32905-2016.
// No external dependencies; exposes an hash.Hash compatible type plus
// convenience helpers used by the sm3 builtin function.

import (
	"encoding/binary"
	"encoding/hex"
	"hash"
	"math/bits"
)

const (
	// sm3Size is the digest length in bytes (256 bits).
	sm3Size = 32
	// sm3BlockSize is the processing block length in bytes (512 bits).
	sm3BlockSize = 64
)

// sm3IV is the initial vector defined by GB/T 32905-2016.
var sm3IV = [8]uint32{
	0x7380166f, 0x4914b2b9, 0x172442d7, 0xda8a0600,
	0xa96f30bc, 0x163138aa, 0xe38dee4d, 0xb0fb0e4e,
}

// sm3T holds the two round constants T_j, selected by round index.
var sm3T = [2]uint32{0x79cc4519, 0x7a879d8a}

// sm3P0 is the permutation P0(X) = X ^ (X <<< 9) ^ (X <<< 17).
func sm3P0(x uint32) uint32 {
	return x ^ bits.RotateLeft32(x, 9) ^ bits.RotateLeft32(x, 17)
}

// sm3P1 is the permutation P1(X) = X ^ (X <<< 15) ^ (X <<< 23).
func sm3P1(x uint32) uint32 {
	return x ^ bits.RotateLeft32(x, 15) ^ bits.RotateLeft32(x, 23)
}

// sm3FF is the boolean function FF_j(X, Y, Z).
func sm3FF(x, y, z uint32, j int) uint32 {
	if j < 16 {
		return x ^ y ^ z
	}
	return (x & y) | (x & z) | (y & z)
}

// sm3GG is the boolean function GG_j(X, Y, Z).
func sm3GG(x, y, z uint32, j int) uint32 {
	if j < 16 {
		return x ^ y ^ z
	}
	return (x & y) | (^x & z)
}

// sm3Digest is the streaming SM3 state. It implements hash.Hash.
type sm3Digest struct {
	h   [8]uint32
	x   [sm3BlockSize]byte // pending partial block
	nx  int                // bytes buffered in x
	len uint64             // total bytes written
}

// NewSM3 returns a new hash.Hash computing SM3 digests.
func NewSM3() hash.Hash {
	d := &sm3Digest{}
	d.Reset()
	return d
}

func (d *sm3Digest) Reset() {
	d.h = sm3IV
	d.nx = 0
	d.len = 0
}

func (d *sm3Digest) Size() int { return sm3Size }

func (d *sm3Digest) BlockSize() int { return sm3BlockSize }

func (d *sm3Digest) Write(p []byte) (int, error) {
	nn := len(p)
	d.len += uint64(nn)
	if d.nx > 0 {
		n := copy(d.x[d.nx:], p)
		d.nx += n
		if d.nx == sm3BlockSize {
			d.block(d.x[:])
			d.nx = 0
		}
		p = p[n:]
	}
	for len(p) >= sm3BlockSize {
		d.block(p[:sm3BlockSize])
		p = p[sm3BlockSize:]
	}
	if len(p) > 0 {
		d.nx = copy(d.x[:], p)
	}
	return nn, nil
}

// block processes one full 64-byte block, updating the chaining value.
func (d *sm3Digest) block(p []byte) {
	var w [68]uint32
	for i := 0; i < 16; i++ {
		w[i] = binary.BigEndian.Uint32(p[i*4:])
	}
	for i := 16; i < 68; i++ {
		w[i] = sm3P1(w[i-16]^w[i-9]^bits.RotateLeft32(w[i-3], 15)) ^
			bits.RotateLeft32(w[i-13], 7) ^ w[i-6]
	}
	var w1 [64]uint32
	for i := 0; i < 64; i++ {
		w1[i] = w[i] ^ w[i+4]
	}

	a, b, c, dd, e, f, g, h := d.h[0], d.h[1], d.h[2], d.h[3], d.h[4], d.h[5], d.h[6], d.h[7]

	for j := 0; j < 64; j++ {
		tj := sm3T[0]
		if j >= 16 {
			tj = sm3T[1]
		}
		ss1 := bits.RotateLeft32(bits.RotateLeft32(a, 12)+e+bits.RotateLeft32(tj, j%32), 7)
		ss2 := ss1 ^ bits.RotateLeft32(a, 12)
		tt1 := sm3FF(a, b, c, j) + dd + ss2 + w1[j]
		tt2 := sm3GG(e, f, g, j) + h + ss1 + w[j]
		dd = c
		c = bits.RotateLeft32(b, 9)
		b = a
		a = tt1
		h = g
		g = bits.RotateLeft32(f, 19)
		f = e
		e = sm3P0(tt2)
	}

	d.h[0] ^= a
	d.h[1] ^= b
	d.h[2] ^= c
	d.h[3] ^= dd
	d.h[4] ^= e
	d.h[5] ^= f
	d.h[6] ^= g
	d.h[7] ^= h
}

// Sum appends the current digest to b and returns the result, without
// modifying the underlying state.
func (d *sm3Digest) Sum(b []byte) []byte {
	// Work on a copy so the caller can keep writing.
	dc := sm3Digest{
		h:   d.h,
		nx:  d.nx,
		len: d.len,
	}
	dc.x = d.x

	length := dc.len
	var tmp [sm3BlockSize]byte
	tmp[0] = 0x80
	if length%sm3BlockSize < 56 {
		// padding fills up the current block
		_, _ = dc.Write(tmp[:56-length%sm3BlockSize])
	} else {
		// padding spills into an extra block
		_, _ = dc.Write(tmp[:120-length%sm3BlockSize])
	}

	// length in bits, big-endian
	binary.BigEndian.PutUint64(tmp[:8], length<<3)
	_, _ = dc.Write(tmp[:8])

	if dc.nx != 0 {
		panic("sm3: invalid state after padding")
	}

	var digest [sm3Size]byte
	for i, s := range dc.h {
		binary.BigEndian.PutUint32(digest[i*4:], s)
	}
	return append(b, digest[:]...)
}

// SM3Sum returns the 32-byte SM3 digest of data.
func SM3Sum(data []byte) []byte {
	h := NewSM3()
	_, _ = h.Write(data)
	return h.Sum(nil)
}

// SM3Encrypt returns the lowercase hex SM3 digest of strA, matching the
// style of tk.MD5Encrypt.
func SM3Encrypt(strA string) string {
	return hex.EncodeToString(SM3Sum([]byte(strA)))
}

// builtinSm3Func implements the sm3 builtin function. It hashes the raw
// bytes of the first parameter (String values contribute their UTF-8
// bytes; Bytes values their raw contents) and returns the digest as a
// lowercase hex string.
func builtinSm3Func(c Call) (Object, error) {
	args := c.GetArgs()

	if len(args) < 1 {
		return NewCommonErrorWithPos(c, "not enough parameters"), nil
	}

	return String(SM3Encrypt(args[0].String())), nil
}
