module github.com/topxeq/charlang

go 1.15

require (
	github.com/denisenkom/go-mssqldb v0.12.3
	github.com/go-sql-driver/mysql v1.7.1
	github.com/godror/godror v0.39.1
	github.com/mattn/go-sqlite3 v1.14.8
	github.com/sijms/go-ora/v2 v2.7.17
	github.com/stretchr/testify v1.8.1
	github.com/topxeq/sqltk v0.0.0
	github.com/topxeq/tk v1.0.6
)

replace github.com/topxeq/tk v1.0.6 => ../tk

replace github.com/topxeq/sqltk v0.0.0 => ../sqltk

// replace github.com/topxeq/gods v0.0.0 => ../gods

// replace github.com/topxeq/goph v0.0.0 => ../goph
