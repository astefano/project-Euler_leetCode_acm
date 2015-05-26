def fib(n, k, p, q):
    if (n == k): return p + q
    else: return fib(n, k + 1, q, p + q)

print fib(5, 0, 1, 1)
    
