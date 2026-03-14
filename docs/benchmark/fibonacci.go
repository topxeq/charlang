// Go 斐波那契性能测试
package main

import (
	"fmt"
	"time"
)

// 递归方式
func fibRecursive(n int) int {
	if n <= 1 {
		return n
	}
	return fibRecursive(n-1) + fibRecursive(n-2)
}

// 迭代方式
func fibIter(n int) int {
	if n <= 1 {
		return n
	}
	a, b := 0, 1
	for i := 2; i <= n; i++ {
		a, b = b, a+b
	}
	return b
}

func main() {
	fmt.Println("=== Go 斐波那契性能测试 ===")
	fmt.Println()

	// 迭代方式
	fmt.Println("=== 迭代方式 ===")

	t1 := time.Now()
	r := fibIter(35)
	t2 := time.Now()
	fmt.Printf("fib(35)   迭代: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	t1 = time.Now()
	r = fibIter(40)
	t2 = time.Now()
	fmt.Printf("fib(40)   迭代: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	t1 = time.Now()
	r = fibIter(50)
	t2 = time.Now()
	fmt.Printf("fib(50)   迭代: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	t1 = time.Now()
	r = fibIter(100)
	t2 = time.Now()
	fmt.Printf("fib(100)  迭代: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	t1 = time.Now()
	r = fibIter(1000)
	t2 = time.Now()
	fmt.Printf("fib(1000) 迭代: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	t1 = time.Now()
	r = fibIter(10000)
	t2 = time.Now()
	fmt.Printf("fib(10000)迭代: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	t1 = time.Now()
	r = fibIter(100000)
	t2 = time.Now()
	fmt.Printf("fib(100000)迭代:%4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	fmt.Println()

	// 递归方式
	fmt.Println("=== 递归方式 ===")

	t1 = time.Now()
	r = fibRecursive(35)
	t2 = time.Now()
	fmt.Printf("fib(35)   递归: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	t1 = time.Now()
	r = fibRecursive(40)
	t2 = time.Now()
	fmt.Printf("fib(40)   递归: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	t1 = time.Now()
	r = fibRecursive(45)
	t2 = time.Now()
	fmt.Printf("fib(45)   递归: %4d ms, result: %d\n", t2.Sub(t1).Milliseconds(), r)

	fmt.Println()
	fmt.Println("测试完成")
}