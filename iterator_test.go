package charlang

import (
	"testing"

	"github.com/stretchr/testify/require"
	tk "github.com/topxeq/tkc"
)

func TestArrayIterator(t *testing.T) {
	arr := Array{Int(1), Int(2), Int(3)}
	it := &ArrayIterator{V: arr}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, Int(1), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, Int(2), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(2), it.Key())
	require.Equal(t, Int(3), it.Value())

	require.False(t, it.Next())
	require.Equal(t, Undefined, it.Value())
}

func TestBytesIterator(t *testing.T) {
	b := Bytes{'a', 'b', 'c'}
	it := &BytesIterator{V: b}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, Byte('a'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, Byte('b'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(2), it.Key())
	require.Equal(t, Byte('c'), it.Value())

	require.False(t, it.Next())
	require.Equal(t, Undefined, it.Value())
}

func TestCharsIterator(t *testing.T) {
	c := Chars{'x', 'y', 'z'}
	it := &CharsIterator{V: c}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, Char('x'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, Char('y'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(2), it.Key())
	require.Equal(t, Char('z'), it.Value())

	require.False(t, it.Next())
	require.Equal(t, Undefined, it.Value())
}

func TestMapIterator(t *testing.T) {
	m := Map{"a": Int(1), "b": Int(2)}
	it := &MapIterator{V: m, keys: []string{"a", "b"}}

	require.True(t, it.Next())
	require.Equal(t, String("a"), it.Key())
	require.Equal(t, Int(1), it.Value())

	require.True(t, it.Next())
	require.Equal(t, String("b"), it.Key())
	require.Equal(t, Int(2), it.Value())

	require.False(t, it.Next())
}

func TestOrderedMapIterator(t *testing.T) {
	om := &OrderedMap{Value: tk.NewOrderedMap()}
	om.Value.Set("x", Int(10))
	om.Value.Set("y", Int(20))
	it := &OrderedMapIterator{V: om, keys: []string{"x", "y"}}

	require.True(t, it.Next())
	require.Equal(t, String("x"), it.Key())
	v, _ := om.Value.Get("x")
	require.Equal(t, v.(Object), it.Value())

	require.True(t, it.Next())
	require.Equal(t, String("y"), it.Key())
	v, _ = om.Value.Get("y")
	require.Equal(t, v.(Object), it.Value())

	require.False(t, it.Next())
}

func TestSyncIterator(t *testing.T) {
	baseIt := &ArrayIterator{V: Array{Int(1), Int(2)}}
	syncIt := &SyncIterator{Iterator: baseIt}

	require.True(t, syncIt.Next())
	require.Equal(t, Int(0), syncIt.Key())
	require.Equal(t, Int(1), syncIt.Value())

	require.True(t, syncIt.Next())
	require.Equal(t, Int(1), syncIt.Key())
	require.Equal(t, Int(2), syncIt.Value())

	require.False(t, syncIt.Next())
}

func TestStringIterator(t *testing.T) {
	s := String("hello")
	it := &StringIterator{V: s}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, Byte('h'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, Byte('e'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(2), it.Key())
	require.Equal(t, Byte('l'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(3), it.Key())
	require.Equal(t, Byte('l'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(4), it.Key())
	require.Equal(t, Byte('o'), it.Value())

	require.False(t, it.Next())
}

func TestMutableStringIterator(t *testing.T) {
	ms := &MutableString{Value: "test"}
	it := &MutableStringIterator{V: ms}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, Byte('t'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, Byte('e'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(2), it.Key())
	require.Equal(t, Byte('s'), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(3), it.Key())
	require.Equal(t, Byte('t'), it.Value())

	require.False(t, it.Next())
}

func TestIntIterator(t *testing.T) {
	it := &IntIterator{V: Int(3)}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, Int(0), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, Int(1), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(2), it.Key())
	require.Equal(t, Int(2), it.Value())

	require.False(t, it.Next())
	require.Equal(t, Undefined, it.Value())
}

func TestUintIterator(t *testing.T) {
	it := &UintIterator{V: Uint(2)}

	require.True(t, it.Next())
	require.Equal(t, Uint(0), it.Key())
	require.Equal(t, Uint(0), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Uint(1), it.Key())
	require.Equal(t, Uint(1), it.Value())

	require.False(t, it.Next())
	require.Equal(t, Undefined, it.Value())
}

func TestStackIterator(t *testing.T) {
	stack := &Stack{Value: tk.NewSimpleStack()}
	stack.Value.Push(Int(100))
	stack.Value.Push(Int(200))
	it := &StackIterator{V: stack}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, Int(100), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, Int(200), it.Value())

	require.False(t, it.Next())
	require.Equal(t, Undefined, it.Value())
}

func TestQueueIterator(t *testing.T) {
	queue := &Queue{Value: tk.NewAnyQueue()}
	queue.Value.Push(Int(1))
	queue.Value.Push(Int(2))
	it := &QueueIterator{V: queue}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, Int(1), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, Int(2), it.Value())

	require.False(t, it.Next())
	require.Equal(t, Undefined, it.Value())
}

func TestMapArrayIterator(t *testing.T) {
	ma := &MapArray{Value: tk.NewSimpleFlexObject()}
	ma.Value.Items = append(ma.Value.Items, "first")
	ma.Value.Items = append(ma.Value.Items, "second")
	it := &MapArrayIterator{V: ma}

	require.True(t, it.Next())
	require.Equal(t, Int(0), it.Key())
	require.Equal(t, String("first"), it.Value())

	require.True(t, it.Next())
	require.Equal(t, Int(1), it.Key())
	require.Equal(t, String("second"), it.Value())

	require.False(t, it.Next())
	require.Equal(t, Undefined, it.Value())
}

func TestEmptyIterators(t *testing.T) {
	// Test empty ArrayIterator
	arrIt := &ArrayIterator{V: Array{}}
	require.False(t, arrIt.Next())
	require.Equal(t, Undefined, arrIt.Value())

	// Test empty BytesIterator
	bytesIt := &BytesIterator{V: Bytes{}}
	require.False(t, bytesIt.Next())
	require.Equal(t, Undefined, bytesIt.Value())

	// Test empty CharsIterator
	charsIt := &CharsIterator{V: Chars{}}
	require.False(t, charsIt.Next())
	require.Equal(t, Undefined, charsIt.Value())

	// Test empty StringIterator
	strIt := &StringIterator{V: String("")}
	require.False(t, strIt.Next())
	require.Equal(t, Undefined, strIt.Value())

	// Test empty MutableStringIterator
	msIt := &MutableStringIterator{V: &MutableString{Value: ""}}
	require.False(t, msIt.Next())
	require.Equal(t, Undefined, msIt.Value())

	// Test empty IntIterator
	intIt := &IntIterator{V: Int(0)}
	require.False(t, intIt.Next())
	require.Equal(t, Undefined, intIt.Value())

	// Test empty UintIterator
	uintIt := &UintIterator{V: Uint(0)}
	require.False(t, uintIt.Next())
	require.Equal(t, Undefined, uintIt.Value())
}
