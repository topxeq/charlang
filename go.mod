module github.com/topxeq/charlang

go 1.15

require (
	github.com/peterh/liner v1.2.2
	github.com/stretchr/testify v1.8.2
)

require (
	github.com/denisenkom/go-mssqldb v0.12.3
	github.com/go-sql-driver/mysql v1.7.1
	github.com/mattn/go-runewidth v0.0.14 // indirect
	github.com/mattn/go-sqlite3 v1.14.17
	github.com/sijms/go-ora/v2 v2.7.17
	github.com/topxeq/sqltk v0.0.0
	github.com/topxeq/tk v1.0.6
	golang.org/x/sys v0.7.0 // indirect
)

replace github.com/topxeq/tk v1.0.6 => ../tk

replace github.com/topxeq/sqltk v0.0.0 => ../sqltk
