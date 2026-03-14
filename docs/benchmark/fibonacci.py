#!/usr/bin/env python3
# Python 斐波那契性能测试

import sys
import time

# 允许大整数转换
sys.set_int_max_str_digits(100000)

# 递归方式
def fib_recursive(n):
    if n <= 1:
        return n
    return fib_recursive(n-1) + fib_recursive(n-2)

# 迭代方式
def fib_iter(n):
    if n <= 1:
        return n
    a, b = 0, 1
    for i in range(2, n+1):
        a, b = b, a + b
    return b

def main():
    print("=== Python 斐波那契性能测试 ===")
    print()

    # 迭代方式
    print("=== 迭代方式 ===")

    t1 = time.time()
    r = fib_iter(35)
    t2 = time.time()
    print(f"fib(35)   迭代: {int((t2-t1)*1000):4d} ms, result: {r}")

    t1 = time.time()
    r = fib_iter(40)
    t2 = time.time()
    print(f"fib(40)   迭代: {int((t2-t1)*1000):4d} ms, result: {r}")

    t1 = time.time()
    r = fib_iter(50)
    t2 = time.time()
    print(f"fib(50)   迭代: {int((t2-t1)*1000):4d} ms, result: {r}")

    t1 = time.time()
    r = fib_iter(100)
    t2 = time.time()
    print(f"fib(100)  迭代: {int((t2-t1)*1000):4d} ms, result: {r}")

    t1 = time.time()
    r = fib_iter(1000)
    t2 = time.time()
    print(f"fib(1000) 迭代: {int((t2-t1)*1000):4d} ms, result: {r}")

    t1 = time.time()
    r = fib_iter(10000)
    t2 = time.time()
    print(f"fib(10000)迭代: {int((t2-t1)*1000):4d} ms, result: {r}")

    t1 = time.time()
    r = fib_iter(100000)
    t2 = time.time()
    print(f"fib(100000)迭代:{int((t2-t1)*1000):4d} ms, digits: {len(str(r))}")

    print()

    # 递归方式
    print("=== 递归方式 ===")

    t1 = time.time()
    r = fib_recursive(35)
    t2 = time.time()
    print(f"fib(35)   递归: {int((t2-t1)*1000):4d} ms, result: {r}")

    t1 = time.time()
    r = fib_recursive(40)
    t2 = time.time()
    print(f"fib(40)   递归: {int((t2-t1)*1000):4d} ms, result: {r}")

    t1 = time.time()
    r = fib_recursive(45)
    t2 = time.time()
    print(f"fib(45)   递归: {int((t2-t1)*1000):4d} ms, result: {r}")

    print()
    print("测试完成")

if __name__ == "__main__":
    main()