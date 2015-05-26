# number of ways to decompose n in 25, 10, 5, 1
def nways(n, c):
    if (c == 1): return 1
    s = 0
    nc = 1
    if (c == 25): nc = 10
    if (c == 10): nc = 5
    if (c == 5): nc = 1

    for i in range(0, n/c):
        s += nways(n-c*i, nc)
    
    return s

print nways(25, 25)

def nwaysIter(n):
    ways = [1 for i in range(n+1)]
    for i in range(5, 10):
        ways[i] = 2
    for i in range(10, 15):
        ways[i] = 3
    for i in range(15, 25):
        ways[i] = ways[i-10] + ways[i-5] + ways[i-1]
    for i in range(25, n+1):
        ways[i] = ways[i-25] + ways[i-10] + ways[i-5] + ways[i-1]
    return ways[n]

print nwaysIter(25)

