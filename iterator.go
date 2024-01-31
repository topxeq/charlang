package charlang

import (
	"sync"
	"unicode/utf8"
)

// Iterator wraps the methods required to iterate Objects in VM.
type Iterator interface {
	// Next returns true if there are more elements to iterate.
	Next() bool

	// Key returns the key or index value of the current element.
	Key() Object

	// Value returns the value of the current element.
	Value() Object
}

// iteratorObject is used in VM to make an iterable Object.
type iteratorObject struct {
	ObjectImpl
	Iterator
}

var _ Object = (*iteratorObject)(nil)

// ArrayIterator represents an iterator for the array.
type ArrayIterator struct {
	V Array
	i int
}

var _ Iterator = (*ArrayIterator)(nil)

// Next implements Iterator interface.
func (it *ArrayIterator) Next() bool {
	it.i++
	return it.i-1 < len(it.V)
}

// Key implements Iterator interface.
func (it *ArrayIterator) Key() Object {
	return Int(it.i - 1)
}

// Value implements Iterator interface.
func (it *ArrayIterator) Value() Object {
	i := it.i - 1
	if i > -1 && i < len(it.V) {
		return it.V[i]
	}
	return Undefined
}

// BytesIterator represents an iterator for the bytes.
type BytesIterator struct {
	V Bytes
	i int
}

var _ Iterator = (*BytesIterator)(nil)

// Next implements Iterator interface.
func (it *BytesIterator) Next() bool {
	it.i++
	return it.i-1 < len(it.V)
}

// Key implements Iterator interface.
func (it *BytesIterator) Key() Object {
	return Int(it.i - 1)
}

// Value implements Iterator interface.
func (it *BytesIterator) Value() Object {
	i := it.i - 1
	if i > -1 && i < len(it.V) {
		return Int(it.V[i])
	}
	return Undefined
}

// CharsIterator represents an iterator for the chars.
type CharsIterator struct {
	V Chars
	i int
}

var _ Iterator = (*CharsIterator)(nil)

// Next implements Iterator interface.
func (it *CharsIterator) Next() bool {
	it.i++
	return it.i-1 < len(it.V)
}

// Key implements Iterator interface.
func (it *CharsIterator) Key() Object {
	return Int(it.i - 1)
}

// Value implements Iterator interface.
func (it *CharsIterator) Value() Object {
	i := it.i - 1
	if i > -1 && i < len(it.V) {
		return Int(it.V[i])
	}
	return Undefined
}

// MapIterator represents an iterator for the map.
type MapIterator struct {
	V    Map
	keys []string
	i    int
}

var _ Iterator = (*MapIterator)(nil)

// Next implements Iterator interface.
func (it *MapIterator) Next() bool {
	it.i++
	return it.i-1 < len(it.keys)
}

// Key implements Iterator interface.
func (it *MapIterator) Key() Object {
	return ToStringObject(it.keys[it.i-1])
}

// Value implements Iterator interface.
func (it *MapIterator) Value() Object {
	v, ok := it.V[it.keys[it.i-1]]
	if !ok {
		return Undefined
	}
	return v
}

// OrderedMapIterator represents an iterator for the map.
type OrderedMapIterator struct {
	V    *OrderedMap
	keys []string
	i    int
}

var _ Iterator = (*OrderedMapIterator)(nil)

// Next implements Iterator interface.
func (it *OrderedMapIterator) Next() bool {
	it.i++
	return it.i-1 < len(it.keys)
}

// Key implements Iterator interface.
func (it *OrderedMapIterator) Key() Object {
	return ToStringObject(it.keys[it.i-1])
}

// Value implements Iterator interface.
func (it *OrderedMapIterator) Value() Object {
	v, ok := it.V.Value.Get(it.keys[it.i-1])
	if !ok {
		return Undefined
	}
	return v.(Object)
}

// SyncIterator represents an iterator for the SyncMap.
type SyncIterator struct {
	mu sync.Mutex
	Iterator
}

// Next implements Iterator interface.
func (it *SyncIterator) Next() bool {
	it.mu.Lock()
	defer it.mu.Unlock()
	return it.Iterator.Next()
}

// Key implements Iterator interface.
func (it *SyncIterator) Key() Object {
	it.mu.Lock()
	defer it.mu.Unlock()
	return it.Iterator.Key()
}

// Value implements Iterator interface.
func (it *SyncIterator) Value() Object {
	it.mu.Lock()
	defer it.mu.Unlock()
	return it.Iterator.Value()
}

// StringIterator represents an iterator for the string.
type StringIterator struct {
	V String
	i int
	k int
	r rune
}

var _ Iterator = (*StringIterator)(nil)

// Next implements Iterator interface.
func (it *StringIterator) Next() bool {
	if it.i > len(it.V.Value)-1 {
		return false
	}

	r, s := utf8.DecodeRuneInString(it.V.Value[it.i:])
	if r == utf8.RuneError || s == 0 {
		return false
	}

	it.k = it.i
	it.r = r
	it.i += s
	return true
}

// Key implements Iterator interface.
func (it *StringIterator) Key() Object {
	return Int(it.k)
}

// Value implements Iterator interface.
func (it *StringIterator) Value() Object {
	return Char(it.r)
}

// MutableStringIterator represents an iterator for the string.
type MutableStringIterator struct {
	V *MutableString
	i int
	k int
	r rune
}

var _ Iterator = (*MutableStringIterator)(nil)

// Next implements Iterator interface.
func (it *MutableStringIterator) Next() bool {
	if it.i > len(it.V.Value)-1 {
		return false
	}

	r, s := utf8.DecodeRuneInString(it.V.Value[it.i:])
	if r == utf8.RuneError || s == 0 {
		return false
	}

	it.k = it.i
	it.r = r
	it.i += s
	return true
}

// Key implements Iterator interface.
func (it *MutableStringIterator) Key() Object {
	return Int(it.k)
}

// Value implements Iterator interface.
func (it *MutableStringIterator) Value() Object {
	return Char(it.r)
}

// IntIterator represents an iterator for the Int.
type IntIterator struct {
	V Int
	i int
}

var _ Iterator = (*IntIterator)(nil)

// Next implements Iterator interface.
func (it *IntIterator) Next() bool {
	it.i++
	return it.i-1 < int(it.V)
}

// Key implements Iterator interface.
func (it *IntIterator) Key() Object {
	return Int(it.i - 1)
}

// Value implements Iterator interface.
func (it *IntIterator) Value() Object {
	i := it.i - 1
	if i > -1 && i < int(it.V) {
		return Int(i)
	}

	return Undefined
}

// StackIterator represents an iterator for the Stack.
type StackIterator struct {
	V *Stack
	i int
}

var _ Iterator = (*StackIterator)(nil)

// Next implements Iterator interface.
func (it *StackIterator) Next() bool {
	it.i++
	return it.i-1 < it.V.Value.Size()
}

// Key implements Iterator interface.
func (it *StackIterator) Key() Object {
	return Int(it.i - 1)
}

// Value implements Iterator interface.
func (it *StackIterator) Value() Object {
	i := it.i - 1
	if i > -1 && i < it.V.Value.Size() {
		return it.V.Value.PeekLayer(i).(Object)
	}

	return Undefined
}
