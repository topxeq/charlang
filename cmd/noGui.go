//go:build noGui || linux
// +build noGui linux

package main

import (
	"fmt"

	tk "github.com/topxeq/tkc"
)

func initGUI() error {

	return fmt.Errorf("GUI not supported.")
}

func guiHandler(argsA ...interface{}) interface{} {
	actionA := tk.ToStr(argsA[0])

	// paramsA := argsA[1:]

	switch actionA {
	case "init":
		rs := initGUI()
		return rs
	}
	// fmt.Printf("%v\n", "GUI engined disabled")
	return fmt.Errorf("GUI engined disabled")
}

func SelectScript() string {
	return tk.GetInputf("Please input the full script path:")
}
