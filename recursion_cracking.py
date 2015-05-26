#9.6
def insert10(input, p):
    m1 = ~((1 << p) - 1)
    highBits= (input & m1)
    #print bin(highBits)
    m2 = (1 << p) - 1
    #print bin(m2)
    lowBits = input & m2
    #print bin(lowBits)
    return (highBits << 2) | (1 << (p+1)) | lowBits

#print bin(42)
#print bin(insert10(42, 2))
#print bin(insert10(42, 3))
#print bin(insert10(42, 4))

def gen(n):
    s = [set() for i in range(n+1)]
    s[1].add(2)

    for i in range(2, n+1):
        for el in s[i-1]:
            for p in range(0, 2*(i-1)):
                nel = insert10(el, p)
                s[i].add(nel)

    #els = [el for i in range(2, n+1) for el in s[i]]
    #print sorted(els)
    lens = [len(s[i]) for i in range(1, n+1)]
    #print lens
    r = []
    for i in range(2, n+1):
        for el in s[i]:
            r.append(bin(el)[2:].replace('1','(').replace('0', ')'))
            #print("%s %d"%(r, el))
    return r

#gen(13)

#9.9
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
        
n = 8
r = queens(n)

#print r
#for ri in r:
#    printQ(ri, n)


# 9.10
from sets import Set

def longestTower(boxes):
    n = len(boxes)
    t = []
    for i in range(n):
        t.append([i])
    for i in range(1, n):
        for pt in t:
            top = boxes[pt[0]]
            for k in range(n):
                for j in range(3):
                    if (boxes[k][j] < top[j]):
                        t.append([k] + pt)
    return t

#print longestTower([[2,3,4],[1,2,3],[4,3,4]])

#9.11
ops = ['|','^','&']
prec = { ops[0]:1, ops[1]:2, ops[2]:3 }
operands = ['0', '1']

def mkParseTree0(s, t, k, res, prec):
    print s
    if (k == len(t)-1):
        if (len(s) == 1):
            t[k] = s[0]
        else:
            print("s=%s"%s)
        print t
        print parseTreeStr(t, 1)
        #r = evalParseTree(t, 1)
        #if (r == res):
        #    print parseTreeStr(t, 1)
    else:
        if (len(s) == 1): 
            t[k] = s[0]
        n = len(s)
        i = 0
        mp = 4
        pi = 0
        while (i < n):
            if (s[i] in ops):
                if (prec[s[i]] < mp):
                    pi = i
                    mp = prec[s[i]]
            i += 1
        print ("pi = %d"%pi)
        t[k] = s[pi]
        if (pi > 0):
            mkParseTree(s[:pi], t, 2*k, res, prec)
            mkParseTree(s[pi+1:], t, 2*k+1, res, prec)

def mkParseTree(s, t, k, prec):
    if (len(s) <= 1):
        if (len(s) == 1): 
            t[k] = s[0]
    else:
        n = len(s)
        i = 0
        mp = 4
        pi = 0
        while (i < n):
            if (s[i] in ops):
                if (prec[s[i]] < mp):
                    pi = i
                    mp = prec[s[i]]
            i += 1
        t[k] = s[pi]
        if (pi > 0):
            mkParseTree(s[:pi], t, 2*k, prec)
            mkParseTree(s[pi+1:], t, 2*k+1, prec)

#def findParenthesis(s, res):    

def evalParseTree(t, k):
    if (t[k] == '0'): return 0
    if (t[k] == '1'): return 1
    if (t[k] == '|'): return evalParseTree(t, 2*k) | evalParseTree(t, 2*k+1)
    if (t[k] == '&'): return evalParseTree(t, 2*k) & evalParseTree(t, 2*k+1)
    if (t[k] == '^'): return evalParseTree(t, 2*k) ^ evalParseTree(t, 2*k+1)

def parseTreeStr(t, k):
    if (t[k] in ['0', '1', ' ']): 
        return t[k]
    else:
        return "(" + parseTreeStr(t, 2*k) + t[k] + parseTreeStr(t, 2*k+1) + ")"

def genParenthesis(s, r):
    n = len(s)
    for opt in [[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]]:
    #for opt in [ [2,1,3]]:#, [2,3,1], [3,1,2], [3,2,1]]:
        prec = { ops[0]:opt[0], ops[1]:opt[1], ops[2]:opt[2] }        
        t = [' ' for i in range(2*n+2)]
        mkParseTree(s, t, 1, prec)
        print parseTreeStr(t, 1)
        if evalParseTree(t, 1) == r:
            print "is sol"
        
print genParenthesis("1^0|0|1", 0)

#s = "1^0|0|1"
#n = len(s)
#print mkParseTree(s, [' ' for i in range(n+1)], 1, 1, prec)

#print printParseTree(" &&111", 1)

def evalString(s):
    stackOps = []
    stackOperands = []
    i = 0
    n = len(s)
    while i < n:
        while(s[i] != ')'):
            if (s[i] in ops): 
                stackOps.append(s[i])
            if (s[i] in operands):
                stackOperands.append(int(s[i]))
            i += 1
        while(i < n and s[i] == ')'):
            i+= 1
        print stackOps
        print stackOperands
        if (len(stackOps) > 0):
            op = stackOps.pop()
            l = stackOperands.pop()
            r = stackOperands.pop()                    
            if (op == '|'):
                stackOperands.append(l | r)
            if (op == '^'):
                stackOperands.append(l ^ r)
            if (op == '&'):
                stackOperands.append(l & r)        
    print stackOperands
    return stackOperands[0]

evalString("((((1)&(0)))&(1))")
