package registry_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"

//	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/registry"
)

func TestBuiltinObjects(t *testing.T) {
	rs, _ := registry.ToObject(true)
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", rs))
	
	rs2, _ := registry.ToInterface(true)
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", rs2))
	
	rs3, _ := registry.ToObject(3)
	
	require.Equal(t, "<nil>", fmt.Sprintf("%v", rs3))
	
}

