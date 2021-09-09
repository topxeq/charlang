module github.com/topxeq/charlang

go 1.15

require (
	github.com/c-bata/go-prompt v0.2.6
	github.com/denisenkom/go-mssqldb v0.10.0
	github.com/go-sql-driver/mysql v1.6.0
	github.com/godror/godror v0.25.3
	github.com/mattn/go-sqlite3 v1.14.8
	github.com/stretchr/testify v1.7.0
	github.com/topxeq/sqltk v0.0.0
	github.com/topxeq/tk v0.0.0
)

replace github.com/topxeq/tk v0.0.0 => ../tk
replace github.com/topxeq/sqltk v0.0.0 => ../sqltk
