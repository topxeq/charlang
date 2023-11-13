module github.com/topxeq/charlang

go 1.15

require github.com/stretchr/testify v1.8.2

require (
	github.com/mattn/go-runewidth v0.0.14 // indirect
	github.com/mattn/go-sqlite3 v1.14.17 // indirect
	github.com/mholt/archiver/v3 v3.5.1
	github.com/topxeq/awsapi v0.0.0-20191115074250-1192cb0fdb97
	github.com/topxeq/sqltk v0.0.0
	github.com/topxeq/tk v1.0.6
)

replace github.com/topxeq/tk v1.0.6 => ../tk

replace github.com/topxeq/sqltk v0.0.0 => ../sqltk
