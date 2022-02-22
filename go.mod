module github.com/topxeq/charlang

go 1.15

require (
	github.com/mattn/go-sqlite3 v1.14.8 // indirect
	github.com/stretchr/testify v1.7.0
	github.com/topxeq/sqltk v0.0.0
	github.com/topxeq/tk v0.0.0
)

replace github.com/topxeq/tk v0.0.0 => ../tk

replace github.com/topxeq/sqltk v0.0.0 => ../sqltk

// replace github.com/topxeq/gods v0.0.0 => ../gods

// replace github.com/topxeq/goph v0.0.0 => ../goph
