def lcs0(a, b):
    n = len(a)
    m = len(b)
    best = [["" for j in range(m)] for i in range(n)]
    for i in range(n):
        if (a[i] == b[0]):
            best[i][0] = b[0] 

    for j in range(m):
        if (a[0] == b[j]):
            best[0][j] = a[0] 

    for i in range(1, n):
        for j in range(1, m):
            if (a[i] == b[j]):
                best[i][j] = best[i-1][j-1] + a[i]
            else:
                best[i][j] = max(best[i-1][j], best[i][j-1]) 

    print best


def lcsV(a, b):
    n = len(a)
    m = len(b)
    prev = [0 for j in range(m) ]
    current = [0 for j in range(m)]
    maxv = 0

    for i in range(1, n):
        for j in range(1, m):
            if (a[i] == b[j]):
                current[j] = prev[j-1] + 1
            else:
                current[j] = max(prev[j], current[j-1])
            if (maxv < current[j]): maxv = current[j]
        prev = current
    print maxv

lcsV("abcabc", "abcbcca")
