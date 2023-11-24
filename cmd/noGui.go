//go:build noGui && linux
// +build noGui,linux

package main

import (
	"fmt"
)

func guiHandler(argsA ...interface{}) interface{} {
	// fmt.Printf("%v\n", "GUI engined disabled")
	return fmt.Errorf("GUI engined disabled")
}
