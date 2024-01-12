package main

import (
	"time"

	"github.com/topxeq/charlang"
	"github.com/topxeq/tk"
)

func main() {
	codeT := `
param ...vargs

pln(vargs)

return vargs[2]
	`

	rs := charlang.RunAsFunc(codeT, "abc", 123, true, map[string]interface{}{"now": time.Now(), "next": tk.NewByteQueue()})

	tk.Pl("rs: %#v", rs)
}
