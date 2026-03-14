# Fibonacci Performance Benchmark Report

## Test Results

### Iterative Method

| n | Charlang | Go | Python | Best |
|---|----------|-----|--------|------|
| fib(35) | 0 ms | 0 ms | 0 ms | Tie |
| fib(40) | 0 ms | 0 ms | 0 ms | Tie |
| fib(50) | 0 ms | 0 ms | 0 ms | Tie |
| fib(100) | 0 ms | 0 ms | 0 ms | Tie |
| fib(1000) | 0 ms | 0 ms | 0 ms | Tie |
| fib(10000) | 1 ms | 0 ms | 1 ms | Go |
| fib(100000) | 6 ms | 0 ms | 60 ms | **Go** |

### Recursive Method

| n | Charlang | Go | Python | Best |
|---|----------|-----|--------|------|
| fib(35) | 1299 ms | 32 ms | 802 ms | **Go** |
| fib(40) | 14420 ms | 360 ms | 8917 ms | **Go** |
| fib(45) | 159313 ms | 3979 ms | 98691 ms | **Go** |

## Analysis

### Iterative Performance Comparison

```
fib(100000) Iterative Performance:
┌─────────────┬──────────┐
│ Go          │   0 ms   │ ████████████ Best
│ Charlang    │   6 ms   │ ██████████
│ Python      │  60 ms   │ █
└─────────────┴──────────┘
```

**Key Findings:**

1. **Small scale (n≤10000)**: All three are fast, differences are negligible
2. **Large scale (n=100000)**:
   - Go: 0ms - compiled language advantage is clear
   - Charlang: 6ms - performance close to Go, excellent
   - Python: 60ms - 10x slower than Charlang

3. **Charlang vs Python (Iterative)**:
   - fib(100000): Charlang is **10x faster** than Python

### Recursive Performance Comparison

```
fib(40) Recursive Performance:
┌─────────────┬──────────┐
│ Go          │   360 ms │ ████████████ Best
│ Python      │  8917 ms │ █
│ Charlang    │ 14420 ms │
└─────────────┴──────────┘

fib(45) Recursive Performance:
┌─────────────┬────────────┐
│ Go          │   3979 ms  │ ████████████ Best
│ Python      │  98691 ms  │
│ Charlang    │ 159313 ms  │
└─────────────┴────────────┘
```

**Key Findings:**

1. **Go recursive is ~40x faster than Charlang**
2. **Go recursive is ~25x faster than Python**
3. **Python recursive is ~1.6x faster than Charlang**
4. **O(2^n) time complexity causes severe performance degradation**
5. **Charlang has higher recursive call overhead**

### Recursive Performance Ratios

| Comparison | fib(35) | fib(40) | fib(45) |
|------------|---------|---------|---------|
| Go vs Charlang | 40.6x | 40.1x | 40.0x |
| Go vs Python | 25.1x | 24.8x | 24.8x |
| Python vs Charlang | 0.62x | 0.62x | 0.62x |

## Conclusion

### Charlang Performance Highlights
- ✅ Iterative performance close to Go
- ✅ Iterative 10x faster than Python
- ✅ Efficient large number operations
- ⚠️ Recursive calls have additional overhead

### Recommendations
- **Prefer iterative over recursive** - applies to all languages
- Charlang is suitable for numerical computation scenarios
- Large-scale data processing (iterative) performs excellently
- Recursive scenarios should control depth or switch to iterative

### Recursion Support Note
Charlang supports recursive functions using special syntax:
```charlang
var fib
fib = func(n) {
    if n <= 1 { return n }
    return fib(n-1) + fib(n-2)
}
```
Not the `fib := func(n) {...}` short declaration form.