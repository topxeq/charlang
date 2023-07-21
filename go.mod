module github.com/topxeq/charlang

go 1.15

require (
	github.com/mattn/go-sqlite3 v1.14.8 // indirect
	github.com/stretchr/testify v1.8.1
	github.com/topxeq/sqltk v0.0.0-20220228051745-2503a98a4e47
	github.com/topxeq/tk v1.0.6
)

replace github.com/topxeq/tk v1.0.6 => ../tk

// replace github.com/topxeq/sqltk v0.0.0 => ../sqltk

// replace github.com/topxeq/gods v0.0.0 => ../gods

// replace github.com/topxeq/goph v0.0.0 => ../goph
