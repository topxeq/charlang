package token_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"

//	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/token"
)

func TestBuiltinObjects(t *testing.T) {
	require.Equal(t, "true", fmt.Sprintf("%v", token.For.IsKeyword()))
	
	require.Equal(t, "for", fmt.Sprintf("%v", token.Lookup("for")))

	require.Equal(t, "IDENT", fmt.Sprintf("%v", token.Lookup("for1")))

	require.Equal(t, "true", fmt.Sprintf("%v", token.Add.IsOperator()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", token.Lookup("-").IsOperator()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", token.Lookup("<=").IsBinaryOperator()))
	
	require.Equal(t, "true", fmt.Sprintf("%v", token.Rem.IsBinaryOperator()))
	
	require.Equal(t, "-", fmt.Sprintf("%v", token.Sub.String()))
	
	require.Equal(t, "false", fmt.Sprintf("%v", token.Quo.IsLiteral()))
	
	require.Equal(t, "1 2 3 4 5 0", fmt.Sprintf("%v %v %v %v %v %v", token.LOr.Precedence(), token.LAnd.Precedence(), token.Equal.Precedence(), token.Xor.Precedence(), token.Mul.Precedence(), token.Uint.Precedence()))
}

