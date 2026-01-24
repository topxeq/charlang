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
}

