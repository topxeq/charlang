package tests_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"

//	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/tests"
)

func TestBuiltinObjects(t *testing.T) {
	require.Equal(t, "(<nil>) <nil>\n", fmt.Sprintf("%v", tests.Sdump(nil)))
	require.Equal(t, "(int) 18\n", fmt.Sprintf("%v", tests.Sdump(18)))
	require.Equal(t, "(string) b\n", fmt.Sprintf("%v", tests.Sdump("abc"[1:2])))
	require.Equal(t, "([]int len=1 cap=2) {\n#0  | (int) 5\n= []int{5}\n}\n", fmt.Sprintf("%v", tests.Sdump([]int{2,5,8}[1:2])))
	require.Equal(t, "(map[string]string len=1) {\n\"f1\":(string) v1\n= map[string]string{\"f1\":\"v1\"}\n}\n", fmt.Sprintf("%v", tests.Sdump(map[string]string{"f1":"v1"})))
	
}

