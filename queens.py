def isConfigValid(queens, col, delta):
    if (len(queens) < 1): 
        return True
    else:
       # not (on the same col or on the same diag)
        return (queens[0] != col and abs(queens[0] - col) != delta and isConfigValid(queens[1:], col, delta+1))

def queens(n):
    return placeQueens(n, n)

def placeQueens(n, k):
    if (k == 0): return [[]]
    else: 
         return [[c] + queens for queens in placeQueens(n, k-1) for c in range(n) if isConfigValid(queens, c, 1)]

def printQ(m, n):
    s = ""
    for i in range(n):
        for j in range(n):
            if (j == m[i]): s += "Q" 
            else: s += "_"
        s += "\n"
    print(s)
        
    for n in range(5,14):
    print len(queens(n))

#print r

#for ri in r:
#    printQ(ri, n)

