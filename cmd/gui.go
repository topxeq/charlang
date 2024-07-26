//go:build !noGui && !linux
// +build !noGui,!linux

package main

import (
	"fmt"
	"runtime"
	"strings"

	jsoniter "github.com/json-iterator/go"
	"github.com/ncruces/zenity"

	tk "github.com/topxeq/tkc"

	"github.com/kbinani/screenshot"

	"github.com/jchv/go-webview2"

	"github.com/topxeq/charlang"
)

var windowStyleG = webview2.HintNone

func newWindowWebView2(paramsA []interface{}) interface{} {
	var paraArgsT []string = []string{}

	for i := 0; i < len(paramsA); i++ {
		paraArgsT = append(paraArgsT, tk.ToStr(paramsA[i]))
	}

	titleT := tk.GetSwitch(paraArgsT, "-title=", "dialog")
	widthT := tk.GetSwitch(paraArgsT, "-width=", "800")
	heightT := tk.GetSwitch(paraArgsT, "-height=", "600")
	iconT := tk.GetSwitch(paraArgsT, "-icon=", "2")
	debugT := tk.IfSwitchExistsWhole(paraArgsT, "-debug")
	centerT := tk.IfSwitchExistsWhole(paraArgsT, "-center")
	fixT := tk.IfSwitchExistsWhole(paraArgsT, "-fix")
	maxT := tk.IfSwitchExistsWhole(paraArgsT, "-max")
	minT := tk.IfSwitchExistsWhole(paraArgsT, "-min")

	if maxT {
		// windowStyleT = webview2.HintMax

		rectT := screenshot.GetDisplayBounds(0)

		widthT = tk.ToStr(rectT.Max.X)
		heightT = tk.ToStr(rectT.Max.Y)
	}

	if minT {
		// windowStyleT = webview2.HintMin

		widthT = "0"
		heightT = "0"
	}

	w := webview2.NewWithOptions(webview2.WebViewOptions{
		Debug:     debugT,
		AutoFocus: true,
		WindowOptions: webview2.WindowOptions{
			Title:  titleT,
			Width:  uint(tk.ToInt(widthT, 800)),
			Height: uint(tk.ToInt(heightT, 600)),
			IconId: uint(tk.ToInt(iconT, 2)), // icon resource id
			Center: centerT,
		},
	})

	if w == nil {
		return fmt.Errorf("创建窗口失败：%v", "N/A")
	}

	windowStyleG = webview2.HintNone

	if fixT {
		windowStyleG = webview2.HintFixed
	}

	w.SetSize(tk.ToInt(widthT, 800), tk.ToInt(heightT, 600), windowStyleG)

	w.Bind("delegateCloseWindow", func(argsA ...interface{}) interface{} {
		w.Destroy()

		return ""
	})

	var handlerT tk.QuickVarDelegate

	handlerT = func(argsA ...interface{}) interface{} {
		actionA := tk.ToStr(argsA[0])

		paramsA := argsA[1:]

		switch actionA {
		case "show":
			w.Run()
			return nil
		case "setSize":
			len1T := len(paramsA)
			if len1T < 2 {
				return fmt.Errorf("not enough parameters")
			}
			if len1T > 2 {
				windowStyleG = webview2.Hint(tk.ToInt(paramsA[2]))
			}

			w.SetSize(tk.ToInt(paramsA[0], 800), tk.ToInt(paramsA[1], 600), windowStyleG)
			return nil
		case "navigate":
			len1T := len(paramsA)
			if len1T < 1 {
				return fmt.Errorf("not enough parameters")
			}

			if len1T > 0 {
				w.Navigate(tk.ToStr(paramsA[0]))
			}

			return nil
		case "setTitle":
			len1T := len(paramsA)
			if len1T < 1 {
				return fmt.Errorf("not enough parameters")
			}

			if len1T > 0 {
				w.SetTitle(tk.ToStr(paramsA[0]))
			}

			return nil
		case "setHtml":
			len1T := len(paramsA)
			if len1T < 1 {
				return fmt.Errorf("not enough parameters")
			}

			if len1T > 0 {
				w.SetHtml(tk.ToStr(paramsA[0]))
			}

			return nil
		case "call", "eval":
			len1T := len(paramsA)
			if len1T < 1 {
				return fmt.Errorf("not enough parameters")
			}

			if len1T > 0 {
				w.Dispatch(func() {
					w.Eval(tk.ToStr(paramsA[0]))
				})
			}

			return nil
		case "close":
			w.Destroy()
			return nil
		case "setDelegate":
			len1T := len(paramsA)
			if len1T < 1 {
				return fmt.Errorf("not enough parameters")
			}
			// tk.Pl("----")
			// tk.Plo(paramsA)
			// tk.Pl("----")
			// tk.Pl("paramsA: %#v", paramsA)

			var codeT = paramsA[0]

			var deleT tk.QuickVarDelegate
			var ok bool
			var s1 string

			deleT, ok = codeT.(tk.QuickVarDelegate)
			if ok {
				errT := w.Bind("delegateDo", deleT)
				return errT
			} else {
				if s1, ok = codeT.(string); ok {
					// s1 = strings.ReplaceAll(s1, "~~~", "`")
					deleObjT, _ := charlang.BuiltinDelegateFunc(charlang.Call{Args: []charlang.Object{charlang.String{Value: s1}}})

					deleObjT.CallMethod("compile")
				} else {
					return fmt.Errorf("invalid compiled object: %v", codeT)
				}

			}

			// tk.Pl("here: %#v", deleT)
			errT := w.Bind("delegateDo", deleT)

			return errT

		case "setFunc":
			len1T := len(paramsA)
			if len1T < 2 {
				return fmt.Errorf("not enough parameters")
			}
			// tk.Pl("----")
			// tk.Plo(paramsA)
			// tk.Pl("----")
			// tk.Pl("paramsA: %#v", paramsA)

			var funcNameT = tk.ToStr(paramsA[0])

			// tk.Plv(paramsA[1])

			var fnT = paramsA[1]

			var deleT tk.QuickVarDelegate
			var ok bool

			deleT, ok = fnT.(tk.QuickVarDelegate)
			if ok {
				errT := w.Bind("delegateDo", deleT)
				return errT
			}

			nv, ok := fnT.(*charlang.CompiledFunction)

			if !ok {
				return fmt.Errorf("invalid parameter 1")
			}

			vs := paramsA[2:]

			lenVsT := len(vs)

			deleT = func(argsA ...interface{}) interface{} {
				lenT := len(argsA)

				var argObjectsT []charlang.Object = make([]charlang.Object, 0, lenVsT+lenT)

				for i := 0; i < lenVsT; i++ {
					argObjectsT = append(argObjectsT, charlang.ConvertToObject(vs[i]))
				}

				for i := 0; i < lenT; i++ {
					argObjectsT = append(argObjectsT, charlang.ConvertToObject(argsA[i]))
				}

				retT, errT := charlang.NewInvoker(CurrentVM, nv).Invoke(argObjectsT...)

				if errT != nil {
					return fmt.Errorf("failed to run compiled function: %v", errT)
				}

				return charlang.ConvertFromObject(retT)

			}

			// tk.Pl("here: %#v", deleT)
			errT := w.Bind(funcNameT, deleT)

			return errT

		// case "setGoDelegate":
		// 	var codeT string = tk.ToStr(paramsA[0])

		// 	// p := objA.(*xie.XieVM)

		// 	w.Bind("goDelegateDo", func(args ...interface{}) interface{} {
		// 		// args是WebView2中调用谢语言函数时传入的参数
		// 		// 可以是多个，谢语言中按位置索引进行访问
		// 		// strT := args[0].String()

		// 		vmT := xie.NewVMQuick()

		// 		// xie.GlobalsG.Vars["verbose"]. = p.VerboseM
		// 		// vmT.VerbosePlusM = p.VerbosePlusM

		// 		vmT.SetVar(vmT.Running, "inputG", args)

		// 		// argCountT := p.Pop()

		// 		// if argCountT == Undefined {
		// 		// 	return tk.ErrStrf()
		// 		// }

		// 		// for i := 0; i < argCountA; i++ {
		// 		// 	vmT.Push(p.Pop())
		// 		// }

		// 		lrs := vmT.Load(vmT.Running, codeT)

		// 		if tk.IsErrX(lrs) {
		// 			return lrs
		// 		}

		// 		go vmT.Run()

		// 		// 最后一定要返回一个值，空字符串也可以
		// 		return ""
		// 	})

		// 	return nil
		default:
			return fmt.Errorf("未知操作：%v", actionA)
		}

		return nil
	}

	// w.Show()
	// w.Run()

	return handlerT

}

func guiHandler(argsA ...interface{}) interface{} {
	actionA := tk.ToStr(argsA[0])

	paramsA := argsA[1:]

	switch actionA {
	case "init":
		rs := initGUI()
		return rs
	case "lockOSThread":
		runtime.LockOSThread()
		return nil
	case "method", "mt":
		if len(paramsA) < 1 {
			return fmt.Errorf("not enough parameters")
		}

		objT := paramsA[0]

		methodNameT := tk.ToStr(paramsA[1])

		v1p := 2

		switch nv := objT.(type) {
		case zenity.ProgressDialog:
			switch methodNameT {
			case "close":
				rs := nv.Close()
				return rs
			case "complete":
				rs := nv.Complete()
				return rs
			case "text":
				if len(paramsA) < v1p+1 {
					return fmt.Errorf("not enough parameters")
				}

				v1 := tk.ToStr(paramsA[v1p])

				rs := nv.Text(v1)
				return rs
			case "value":
				if len(paramsA) < v1p+1 {
					return fmt.Errorf("not enough parameters")
				}

				v1 := tk.ToInt(paramsA[v1p])

				rs := nv.Value(v1)
				return rs
			case "maxValue":
				rs := nv.MaxValue()
				return rs
			case "done":
				return nv.Done()
			}
		}

		rvr := tk.ReflectCallMethod(objT, methodNameT, paramsA[2:]...)

		return rvr

	case "new":
		if len(paramsA) < 1 {
			return fmt.Errorf("not enough parameters")
		}

		vs1 := tk.ToStr(paramsA[0])

		switch vs1 {
		case "window", "webView2":
			return newWindowWebView2(paramsA[1:])
		}

		return fmt.Errorf("不支持的创建类型：%v", vs1)

	case "close":
		if len(paramsA) < 1 {
			return fmt.Errorf("not enough parameters")
		}

		switch nv := paramsA[0].(type) {
		case zenity.ProgressDialog:
			nv.Close()
		}

		return ""

	case "showInfo":
		if len(paramsA) < 1 {
			return fmt.Errorf("not enough parameters")
		}

		return showInfoGUI(tk.ToStr(paramsA[0]), tk.InterfaceToStringArray(paramsA[1:])...)

	case "showError":
		if len(paramsA) < 1 {
			return fmt.Errorf("not enough parameters")
		}
		return showErrorGUI(tk.ToStr(paramsA[0]), tk.InterfaceToStringArray(paramsA[1:])...)

	case "getConfirm":
		if len(paramsA) < 1 {
			return fmt.Errorf("not enough parameters")
		}
		return getConfirmGUI(tk.ToStr(paramsA[0]), tk.InterfaceToStringArray(paramsA[1:])...)
	case "getInput":
		return getInputGUI(tk.InterfaceToStringArray(paramsA)...)
	case "selectList":
		if len(paramsA) < 2 {
			return fmt.Errorf("not enough parameters")
		}

		return selectListGUI(tk.ToStr(paramsA[0]), tk.JSONToStringArray(tk.ToJSONX(paramsA[1])), tk.InterfaceToStringArray(paramsA[2:])...)
	case "selectListMulti":
		if len(paramsA) < 2 {
			return fmt.Errorf("not enough parameters")
		}

		return selectListMultiGUI(tk.ToStr(paramsA[0]), tk.JSONToStringArray(tk.ToJSONX(paramsA[1])), tk.InterfaceToStringArray(paramsA[2:])...)
	case "selectFile":
		return selectFileGUI(tk.InterfaceToStringArray(paramsA)...)
	case "selectFileMultiple":
		return selectFileMultipleGUI(tk.InterfaceToStringArray(paramsA)...)
	case "selectFileToSave":
		return selectFileToSaveGUI(tk.InterfaceToStringArray(paramsA)...)
	case "selectDir":
		paramsT := tk.InterfaceToStringArray(paramsA)
		paramsT = append(paramsT, "-dir")
		return selectFileGUI(paramsT...)
	case "selectDirMultiple":
		paramsT := tk.InterfaceToStringArray(paramsA)
		paramsT = append(paramsT, "-dir")
		return selectFileMultipleGUI(paramsT...)
	case "selectDirectory":
		paramsT := tk.InterfaceToStringArray(paramsA)
		paramsT = append(paramsT, "-dir")
		return selectFileGUI(paramsT...)
	case "selectDirectoryMultiple":
		paramsT := tk.InterfaceToStringArray(paramsA)
		paramsT = append(paramsT, "-dir")
		return selectFileMultipleGUI(paramsT...)
	case "getActiveDisplayCount":
		return screenshot.NumActiveDisplays()
	case "getScreenResolution":
		var paraArgsT []string = []string{}

		for i := 0; i < len(paramsA); i++ {
			paraArgsT = append(paraArgsT, tk.ToStr(paramsA[i]))
		}

		formatT := tk.GetSwitch(paraArgsT, "-format=", "")

		idxStrT := tk.GetSwitch(paraArgsT, "-index=", "0")

		idxT := tk.StrToInt(idxStrT, 0)

		rectT := screenshot.GetDisplayBounds(idxT)

		if formatT == "" {
			return []interface{}{rectT.Max.X, rectT.Max.Y}
		} else if formatT == "raw" || formatT == "rect" {
			return rectT
		} else if formatT == "json" {
			return tk.ToJSONX(rectT, "-sort")
		}

		return []interface{}{rectT.Max.X, rectT.Max.Y}
	case "showProcess":
		var paraArgsT []string = []string{}

		for i := 0; i < len(paramsA); i++ {
			paraArgsT = append(paraArgsT, tk.ToStr(paramsA[i]))
		}

		optionsT := []zenity.Option{}

		titleT := tk.GetSwitch(paraArgsT, "-title=", "")

		if titleT != "" {
			optionsT = append(optionsT, zenity.Title(titleT))
		}

		okButtonT := tk.GetSwitch(paraArgsT, "-ok=", "")

		if titleT != "" {
			optionsT = append(optionsT, zenity.OKLabel(okButtonT))
		}

		cancelButtonT := tk.GetSwitch(paraArgsT, "-cancel=", "")

		if titleT != "" {
			optionsT = append(optionsT, zenity.CancelLabel(cancelButtonT))
		}

		if tk.IfSwitchExistsWhole(paraArgsT, "-noCancel") {
			optionsT = append(optionsT, zenity.NoCancel())
		}

		if tk.IfSwitchExistsWhole(paraArgsT, "-modal") {
			optionsT = append(optionsT, zenity.Modal())
		}

		if tk.IfSwitchExistsWhole(paraArgsT, "-pulsate") {
			optionsT = append(optionsT, zenity.Pulsate())
		}

		maxT := tk.GetSwitch(paraArgsT, "-max=", "")

		if maxT != "" {
			optionsT = append(optionsT, zenity.MaxValue(tk.ToInt(maxT, 100)))
		}

		dlg, errT := zenity.Progress(optionsT...)
		if errT != nil {
			return fmt.Errorf("创建进度框失败（failed to create progress dialog）：%v", errT)
		}

		return dlg

	case "newWindow":
		return newWindowWebView2(paramsA)

	default:
		return fmt.Errorf("未知方法")
	}

	return ""
}

func initGUI() error {

	return nil
}

func showInfoGUI(msgA string, optsA ...string) interface{} {
	optionsT := []zenity.Option{}

	titleT := tk.GetSwitch(optsA, "-title=", " ")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	okLabelT := tk.GetSwitch(optsA, "-okLabel=", "")

	if okLabelT != "" {
		optionsT = append(optionsT, zenity.OKLabel(okLabelT))
	}

	optionsT = append(optionsT, zenity.InfoIcon)

	errT := zenity.Info(msgA, optionsT...)

	// rs, errT := dlgs.Info(titleA, fmt.Sprintf(formatA, messageA...))

	// if errT != nil {
	// 	return errT
	// }

	return errT
}

func getConfirmGUI(msgA string, optsA ...string) interface{} {
	optionsT := []zenity.Option{}

	titleT := tk.GetSwitch(optsA, "-title=", "")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	okLabelT := tk.GetSwitch(optsA, "-okLabel=", "")

	if okLabelT != "" {
		optionsT = append(optionsT, zenity.OKLabel(okLabelT))
	}

	cancelLabelT := tk.GetSwitch(optsA, "-cancelLabel=", "")

	if cancelLabelT != "" {
		optionsT = append(optionsT, zenity.CancelLabel(cancelLabelT))
	}

	extraLabelT := tk.GetSwitch(optsA, "-extraLabel=", "")

	if extraLabelT != "" {
		optionsT = append(optionsT, zenity.ExtraButton(extraLabelT))
	}

	optionsT = append(optionsT, zenity.QuestionIcon)

	errT := zenity.Question(msgA, optionsT...)

	// tk.Plo(errT)

	// tk.Plv(errT == zenity.ErrExtraButton)

	if errT == nil {
		return ""
	}

	if errT == zenity.ErrExtraButton {
		return "extra"
	}

	if errT == zenity.ErrCanceled {
		return "cancel"
	}

	// rs, errT := dlgs.Info(titleA, fmt.Sprintf(formatA, messageA...))

	// if errT != nil {
	// 	return errT
	// }

	return "N/A"

	// flagT, errT := dlgs.Question(titleA, fmt.Sprintf(formatA, messageA...), true)
	// if errT != nil {
	// 	return errT
	// }

	// return flagT
}

func showErrorGUI(msgA string, optsA ...string) interface{} {
	optionsT := []zenity.Option{}

	titleT := tk.GetSwitch(optsA, "-title=", "")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	okLabelT := tk.GetSwitch(optsA, "-okLabel=", "")

	if okLabelT != "" {
		optionsT = append(optionsT, zenity.OKLabel(okLabelT))
	}

	optionsT = append(optionsT, zenity.ErrorIcon)

	errT := zenity.Error(msgA, optionsT...)

	// rs, errT := dlgs.Info(titleA, fmt.Sprintf(formatA, messageA...))

	// if errT != nil {
	// 	return errT
	// }

	return errT
	// rs, errT := dlgs.Error(titleA, fmt.Sprintf(formatA, messageA...))
	// if errT != nil {
	// 	return errT
	// }

	// return rs
}

// mt $pln $guiG selectFileToSave -confirmOverwrite -title=保存文件…… -default=c:\test\test.txt `-filter=[{"Name":"Go and TextFiles", "Patterns":["*.go","*.txt"], "CaseFold":true}]`

func selectFileToSaveGUI(argsA ...string) interface{} {
	optionsT := []zenity.Option{}

	optionsT = append(optionsT, zenity.ShowHidden())

	titleT := tk.GetSwitch(argsA, "-title=", "")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	defaultT := tk.GetSwitch(argsA, "-default=", "")

	if defaultT != "" {
		optionsT = append(optionsT, zenity.Filename(defaultT))
	}

	if tk.IfSwitchExistsWhole(argsA, "-confirmOverwrite") {
		optionsT = append(optionsT, zenity.ConfirmOverwrite())
	}

	filterStrT := tk.GetSwitch(argsA, "-filter=", "")

	// tk.Plv(filterStrT)

	var filtersT zenity.FileFilters

	if filterStrT != "" {

		errT := jsoniter.Unmarshal([]byte(filterStrT), &filtersT)

		if errT != nil {
			return errT
		}

		optionsT = append(optionsT, filtersT)
	}

	rs, errT := zenity.SelectFileSave(optionsT...)

	if errT != nil {
		if errT == zenity.ErrCanceled {
			return nil
		}

		return errT
	}

	return rs
}

func selectListGUI(textA string, itemsA []string, argsA ...string) interface{} {
	optionsT := []zenity.Option{}

	titleT := tk.GetSwitch(argsA, "-title=", "")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	okLabelT := tk.GetSwitch(argsA, "-okLabel=", "")

	if okLabelT != "" {
		optionsT = append(optionsT, zenity.OKLabel(okLabelT))
	}

	cancelLabelT := tk.GetSwitch(argsA, "-cancelLabel=", "")

	if cancelLabelT != "" {
		optionsT = append(optionsT, zenity.CancelLabel(cancelLabelT))
	}

	extraButtonT := tk.GetSwitch(argsA, "-extraButton=", "")

	if extraButtonT != "" {
		optionsT = append(optionsT, zenity.ExtraButton(extraButtonT))
	}

	defaultT := tk.GetSwitch(argsA, "-default=", "")

	if defaultT != "" {
		defaultListT := strings.Split(defaultT, "|")
		optionsT = append(optionsT, zenity.DefaultItems(defaultListT...))
	}

	if tk.IfSwitchExistsWhole(argsA, "-disallowEmpty") {
		optionsT = append(optionsT, zenity.DisallowEmpty())
	}

	rs, errT := zenity.List(textA, itemsA, optionsT...)

	if errT != nil {
		if errT == zenity.ErrCanceled {
			return nil
		}

		if errT == zenity.ErrExtraButton {
			return fmt.Errorf("extraButton")
		}

		return errT
	}

	return rs
}

func selectListMultiGUI(textA string, itemsA []string, argsA ...string) interface{} {
	optionsT := []zenity.Option{}

	titleT := tk.GetSwitch(argsA, "-title=", "")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	okLabelT := tk.GetSwitch(argsA, "-okLabel=", "")

	if okLabelT != "" {
		optionsT = append(optionsT, zenity.OKLabel(okLabelT))
	}

	cancelLabelT := tk.GetSwitch(argsA, "-cancelLabel=", "")

	if cancelLabelT != "" {
		optionsT = append(optionsT, zenity.CancelLabel(cancelLabelT))
	}

	extraButtonT := tk.GetSwitch(argsA, "-extraButton=", "")

	if extraButtonT != "" {
		optionsT = append(optionsT, zenity.ExtraButton(extraButtonT))
	}

	defaultT := tk.GetSwitch(argsA, "-default=", "")

	if defaultT != "" {
		defaultListT := strings.Split(defaultT, "|")
		optionsT = append(optionsT, zenity.DefaultItems(defaultListT...))
	}

	if tk.IfSwitchExistsWhole(argsA, "-disallowEmpty") {
		optionsT = append(optionsT, zenity.DisallowEmpty())
	}

	rs, errT := zenity.ListMultiple(textA, itemsA, optionsT...)

	if errT != nil {
		if errT == zenity.ErrCanceled {
			return nil
		}

		if errT == zenity.ErrExtraButton {
			return fmt.Errorf("extraButton")
		}

		return errT
	}

	return rs
}

func selectFileGUI(argsA ...string) interface{} {
	optionsT := []zenity.Option{}

	optionsT = append(optionsT, zenity.ShowHidden())

	titleT := tk.GetSwitch(argsA, "-title=", "")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	ifDirT := tk.IfSwitchExists(argsA, "-dir")

	if ifDirT {
		optionsT = append(optionsT, zenity.Directory())
	}

	defaultT := tk.GetSwitch(argsA, "-default=", "")

	if defaultT != "" {
		optionsT = append(optionsT, zenity.Filename(defaultT))
	}

	// `-filter=[{"Name": "Charlang Script Files", "Patterns": ["*.char"], "CaseFold": false}, {"Name": "Plain Text Files", "Patterns": ["*.txt"], "CaseFold": false}, {"Name": "All Files", "Patterns": ["*"], "CaseFold": false}]`
	filterStrT := tk.GetSwitch(argsA, "-filter=", "")

	var filtersT zenity.FileFilters

	if filterStrT != "" {

		errT := jsoniter.Unmarshal([]byte(filterStrT), &filtersT)

		if errT != nil {
			return errT
		}

		optionsT = append(optionsT, filtersT)
	}

	// fmt.Printf("optionsT: %v\n", optionsT)

	rs, errT := zenity.SelectFile(optionsT...)

	if errT != nil {
		if errT == zenity.ErrCanceled {
			return nil
		}

		return errT
	}

	return rs
}

func selectFileMultipleGUI(argsA ...string) interface{} {
	optionsT := []zenity.Option{}

	optionsT = append(optionsT, zenity.ShowHidden())

	titleT := tk.GetSwitch(argsA, "-title=", "")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	ifDirT := tk.IfSwitchExists(argsA, "-dir")

	if ifDirT {
		optionsT = append(optionsT, zenity.Directory())
	}

	defaultT := tk.GetSwitch(argsA, "-default=", "")

	if defaultT != "" {
		optionsT = append(optionsT, zenity.Filename(defaultT))
	}

	// `-filter=[{"Name": "Charlang Script Files", "Patterns": ["*.char"], "CaseFold": false}, {"Name": "Plain Text Files", "Patterns": ["*.txt"], "CaseFold": false}, {"Name": "All Files", "Patterns": ["*"], "CaseFold": false}]`
	filterStrT := tk.GetSwitch(argsA, "-filter=", "")

	var filtersT zenity.FileFilters

	if filterStrT != "" {

		errT := jsoniter.Unmarshal([]byte(filterStrT), &filtersT)

		if errT != nil {
			return errT
		}

		optionsT = append(optionsT, filtersT)
	}

	// fmt.Printf("optionsT: %v\n", optionsT)

	rs, errT := zenity.SelectFileMultiple(optionsT...)

	if errT != nil {
		if errT == zenity.ErrCanceled {
			return nil
		}

		return errT
	}

	return rs
}

func getInputGUI(argsA ...string) interface{} {
	optionsT := []zenity.Option{}

	optionsT = append(optionsT, zenity.ShowHidden())

	titleT := tk.GetSwitch(argsA, "-title=", "")

	if titleT != "" {
		optionsT = append(optionsT, zenity.Title(titleT))
	}

	defaultT := tk.GetSwitch(argsA, "-default=", "")

	if defaultT != "" {
		optionsT = append(optionsT, zenity.EntryText(defaultT))
	}

	hideTextT := tk.IfSwitchExistsWhole(argsA, "-hideText")
	if hideTextT {
		optionsT = append(optionsT, zenity.HideText())
	}

	modalT := tk.IfSwitchExistsWhole(argsA, "-modal")
	if modalT {
		optionsT = append(optionsT, zenity.Modal())
	}

	textT := tk.GetSwitch(argsA, "-text=", "")

	okLabelT := tk.GetSwitch(argsA, "-okLabel=", "")

	if okLabelT != "" {
		optionsT = append(optionsT, zenity.OKLabel(okLabelT))
	}

	cancelLabelT := tk.GetSwitch(argsA, "-cancelLabel=", "")

	if cancelLabelT != "" {
		optionsT = append(optionsT, zenity.CancelLabel(cancelLabelT))
	}

	extraButtonT := tk.GetSwitch(argsA, "-extraButton=", "")

	if extraButtonT != "" {
		optionsT = append(optionsT, zenity.ExtraButton(extraButtonT))
	}

	rs, errT := zenity.Entry(textT, optionsT...)

	if errT != nil {
		if errT == zenity.ErrCanceled {
			return nil
		}

		if errT == zenity.ErrExtraButton {
			return fmt.Errorf("extraButton")
		}

		return errT
	}

	return rs
}

func SelectScript() string {
	rsT := selectFileGUI("-title=Please select the script...", `-filter=[{"Name": "Charlang Script Files", "Patterns": ["*.char"], "CaseFold": false}, {"Name": "Plain Text Files", "Patterns": ["*.txt"], "CaseFold": false}, {"Name": "All Files", "Patterns": ["*"], "CaseFold": false}]`)

	if rsT == nil {
		return "TXERROR:"
	}

	if tk.IsError(rsT) {
		return tk.ErrorToString(rsT.(error))
	}

	return tk.ToStr(rsT)
}
