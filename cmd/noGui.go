//go:build noGui || linux
// +build noGui linux

package main

import (
	"fmt"

	tk "github.com/topxeq/tkc"
)

func guiHandler(argsA ...interface{}) interface{} {
	// fmt.Printf("%v\n", "GUI engined disabled")
	return fmt.Errorf("GUI engined disabled")
}

func SelectScript() string {
	return tk.GetInputf("Please input the full script path:")
}
