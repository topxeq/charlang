package compat_test

import (
	"fmt"
	"testing"

	"github.com/stretchr/testify/require"

//	"github.com/topxeq/charlang"
	"github.com/topxeq/charlang/internal/compat"
)

type Struct1 struct {
	
}

func (o *Struct1) Write(p []byte) (n int, err error) {
	return 0, nil
}

func (o *Struct1) Width() (wid int, ok bool) {
	return 0, true
}

func (o *Struct1) Precision() (prec int, ok bool) {
	return 0, true
}

func (o *Struct1) Flag(c int) bool {
	return true
}

func TestBuiltinObjects(t *testing.T) {
	require.Equal(t, "FmtFormatString: % +-#00.0\x03", fmt.Sprintf("FmtFormatString: %v", compat.FmtFormatString(&Struct1{}, 3)))
	
}

