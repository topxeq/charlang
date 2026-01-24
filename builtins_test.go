package charlang_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"
	. "github.com/topxeq/charlang"
)

func TestBuiltinTypes(t *testing.T) {
	for k, v := range BuiltinsMap {
		if BuiltinObjects[v] == nil {
			t.Fatalf("builtin '%s' is missing", k)
		}
		if _, ok := BuiltinObjects[v].(*BuiltinFunction); !ok {
			if _, ok := BuiltinObjects[v].(*Error); !ok {
				t.Fatalf("builtin '%s' is not *BuiltinFunction or *Error type", k)
			}
		}
	}
	if _, ok := BuiltinObjects[BuiltinGlobals].(*BuiltinFunction); !ok {
		t.Fatal("builtin 'global' is not *BuiltinFunction type")
	}
}

func TestBuiltinFuncs(t *testing.T) {
	if false {
		fmt.Println()
	}

	expectRun(t, `return len("abc")`, nil, Int(3))
	
	require.Equal(t, NewAny(1),  &Any{
		Value:        1,
		OriginalType: "",
		OriginalCode: -1,
	})
}

func TestBuiltinObjects(t *testing.T) {
	require.True(t, Char(99) == Char('c'))

	expectRun(t, `return spr("%v", bytes(1, 2, 3))`, nil, String{Value: "[1 2 3]"})

	expectRun(t, `return spt(bytesBuffer(bytes(1, 2, 3)).bytes())`, nil, String{Value: "[1 2 3]"})

	expectRun(t, `o1 := image("-height=200"); return spt(o1.width(), o1.height())`, nil, String{Value: "100 200"})
	
	o1, errT := NewImage(Call{Args: []Object{String{Value: "-width=300"}}})
	
	if errT != nil {
		t.Fatalf("error occur: %v", errT)
	}
	
	o1c, ok := o1.(NameCallerObject)
	
	if !ok {
		t.Fatalf("failed to create object: %v", "image")
	}
	
	r1, errT := o1c.CallName("width", Call{})

	if errT != nil {
		t.Fatalf("error occur: %v", errT)
	}
	
	r2, errT := o1c.CallName("height", Call{})

	if errT != nil {
		t.Fatalf("error occur: %v", errT)
	}
	
	require.Equal(t, fmt.Sprintf("%v,%v", r1, r2), "300,100")

}

