# http://www.algorithm.cs.sunysb.edu/algowiki/index.php/Divide-TADM2E

def sumNaive(a):
    n = len(a)
    m = [[0 for i in range(n)] for j in range(n)]
    for i in range(n):
        m[i][i] = a[i]
    maxv = 0
    p = (0, 0)
    for i in range(n):
        for j in range(i+1, n):
            m[i][j] = m[i][j-1] + a[j]
            if maxv < m[i][j]:
                maxv = m[i][j]
                p = (i, j)
    print maxv, p


def msum(a):
    n = len(a)
    maxv = 0
    pi = 0
    pj = 0
    temp_i = 0
    temp_j = 0
    stemp = 0
    for i in range(n):
        if stemp + a[i] > a[i]:
            stemp += a[i]
            temp_j += 1
        else:
            stemp = a[i]
            temp_i = i
            temp_j = i
        if stemp < 0:
            stemp = a[i]
            pi = i
        if maxv < stemp:
            maxv = stemp
            pj = temp_j
            pi = temp_i
    print maxv, pi, pj


a = [-1,0,-3, 5, 7, -2, 13]
sumNaive(a)
msum(a)
