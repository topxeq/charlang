module github.com/topxeq/charlang/cmd/char

go 1.17

replace github.com/topxeq/sqltk v0.0.0 => ../../../sqltk

replace github.com/topxeq/tk v0.0.0 => ../../../tk

// replace github.com/topxeq/gods v0.0.0 => ../../../gods

// replace github.com/topxeq/goph v0.0.0 => ../../../goph

replace github.com/topxeq/charlang v0.0.0 => ../../../charlang

require (
	github.com/c-bata/go-prompt v0.2.6
	github.com/denisenkom/go-mssqldb v0.12.0
	github.com/go-sql-driver/mysql v1.6.0
	github.com/godror/godror v0.30.2
	github.com/mattn/go-sqlite3 v1.14.11
	github.com/stretchr/testify v1.7.0
	github.com/topxeq/charlang v0.0.0
	github.com/topxeq/tk v0.0.0
)
