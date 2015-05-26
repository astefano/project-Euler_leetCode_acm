# cracking the code 11.1
def shift(a, n, start, k):
    i = n
    while(i > start):
        a[i + k] = a[i]
        a[i] = 0
        i -= 1
    return a


n = 4
a = [i if i < 4 else 0 for i in range(0, 8)]

#print a
#print shift(a, n, 1, 3)


def insert(a, n, b):
    na = len(a)
    nb = len(b)
    i = nb - 1
    #the nb of nonzero elements in a
    j = n
    k = na - 1
    while (i >= 0 and j >= 0):
        print("compare a[%d] = %d and b[%d] = %d to set k = %d"%(j,a[j],i,b[i],k))
        if (a[j] < b[i]):
            a[k] = b[i]
            i -= 1
        else:
            a[k] = a[j]
            j -= 1
        k -= 1
    #print("k=%d"%k)
    while (i >= 0):
        a[k] = b[i]
        i -= 1
        k -= 1        
        
    return a

#a = [5, 6, 0, 0, 0]
#b = [1, 2, 7]
#print a
#print b
#print insert(a, 1, b)

# chracking the code 11.2
from collections import defaultdict
m = defaultdict(list)

def sortAna(a):
    for ai in a:
        m[''.join(sorted(ai))].append(ai)
    #for mi in m:
    #    mi.
    print m.values()

#sortAna(["ema","anca","ame"])

def binSearch(a, l, r, x):
    if (l > r): return -1
    else:
        mid = l + (r - l)/2
        if (a[mid] < x): binSearch(a, mid + 1, r, x)
        else: 
            if (a[mid] > x): binSearch(a, l, mid - 1, x)
            else: 
                return mid


def binSearchIter(a, x):
    l = 0
    r = len(a)-1
    while (l <= r):
        mid = l + (r - l)/2
        if (a[mid] < x): l = mid + 1
        else: 
            if (a[mid] > x): r = mid - 1
        if (a[mid] == x): 
            return mid
    return -1


# chracking the code 11.3
def findElInRot(a, x):
    #find drop
    n = len(a)
    for i in range(n-1):
        if (a[i] > a[i+1]):
            break
    if (x < a[i]): return i + binSearchIter(a[i:], x)
    else: return binSearchIter(a[:i], x)

#print findElInRot([15,16,19,20,25,1,3,4,5,7,10,14], 5)

# chracking the code 11.3
def findString(a, s):
    l = 0
    r = len(a) - 1
    while (l <= r):
        mid = l + (r - l)/2
        midl = mid
        midr = mid
        while (a[midl] == ""):
            midl -= 1
        while (a[midr] == ""):
            midr += 1
        if (a[midl] < s): l = midr + 1
        if (a[midl] > s): r = midl - 1
        if (a[midl] == s): return midl
    return -1

print findString(["at", "", "", "", "ball", "", "", "car", "", "", "dad", "", ""], "ball")

def selectionSort(a):
    sortedA = []
    while(len(a) > 1):
        m = min(a)
        sortedA.append(m)
        a.remove(m)
    sortedA.append(a[0])
    return sortedA

#print "selection sort"
#print selectionSort([5,4,3,2,1])

#heaps http://algs4.cs.princeton.edu/24pq/
# pq[0] unused; pq[1] is the root; pq[2k] (pq[2k+1]) is the left (right) child of pq[k] 
# pq[1]
# pq[2]                      pq[3] 
# pq[4]       pq[5]          pq[6]          pq[7]
# pq[8] pq[9] pq[10] pq[11]  pq[12] pq[13]  pq[14] pq[15]
# ...
pq = [0]

def less(i, j): return pq[i] < pq[j]

def exch(i, j): 
    aux = pq[i]
    pq[i] = pq[j]
    pq[j] = aux

#reheapify bottom-up: the prop is violated because a child becomes greater than the parent
def swim(k):
    while(k > 1 and less(k/2, k)):
        exch(k, k/2)
        k = k/2

#reheapify top-down: the prop is violated because a parent becomes smaller than children
def sink(k):
    n = len(pq)
    while(2*k < n):
        j = 2*k
        if (j < n-1 and less(j, j+1)):
            j += 1
        if (not less(k, j)): 
            break
        exch(k, j)
        k = j    

def insertHeap(v):
    pq.append(v)
    swim(len(pq)-1)

def removeMax():
    v = pq[1]
    pq[1] = pq[-1]
    pq.pop(-1)
    sink(1)
    return v

def heapSortSilly(a):
    n = len(a)
for ai in a:
        insertHeap(ai)
    i = n-1
    print pq
    while(len(pq) > 1):
        a[i] = removeMax()
        i -= 1
    return a


def less(pq, i, j): return pq[i] < pq[j]

def exch(pq, i, j): 
    aux = pq[i]
    pq[i] = pq[j]
    pq[j] = aux

#reheapify top-down: the prop is violated because a parent becomes smaller than children
def sink(pq, k, n):
    while(2*k < n):
        j = 2*k
        if (j < n-1 and less(pq, j, j+1)):
            j += 1
        if (not less(pq, k, j)): 
            break
        exch(pq, k, j)
        k = j    

def heapSort(a):
    n = len(a)
    for k in range(n/2, 1, -1):
        sink(a, k, n)
    while(n > 1):
        exch(a, 1, n-1)
        n -= 1 
        sink(a, 1, n)    
    return a

#a = [0,5,4,3,2,1]
#a=[1,2,3]
a = [0]
for c in "SORTEXAMPLE":
    a.append(c)
print "Heapsort"
print heapSort(a)

# skiena pg 116: determine if the kth greatest element is <= x
#def isgreatestKsmallerThanX(heap, k, x):
#    i = 1
#    count = k
#    while(count > 0):
#....        

def insertionSort(a):    
    for i in range(1, n):
        j = i
        while (j > 0):
            if (less(a, j, j-1)): 
                exch(a, j-1, j)
            j -= 1
    return a

a = [5,4,3,2]
print insertionSort(a)

 
def quicksort(a, left, right):
    if (right - left > 1):
        mid = left + (right - left)/2
        partition(a, mid)
        quicksort(a, left, mid)
        quicksort(a, mid, right)

def partition(a, p):
    x = a[p]
    n = len(a)
    i = 0
    j = n-1
    while (i < p and j > p):
        while(a[i] < x):
            i += 1
        while(a[j] > x):
            j -= 1
        aux = a[i]
        a[i] = a[j]
        a[j] = aux
        i += 1
        j -= 1

#a = [5,4,3,2,1]
#print a
#quicksort(a, 0, 5)
#print a

# de la http://www.infoarena.ro/blog?tag=potw
#Se da un sir A de n + m numere intregi. Numerele de la 1 la n sunt in ordine crescatoare si numerele de la n + 1 la n + m sunt si ele in ordine crescatoare. Se cere sa se sorteze sirul in ordine crescatoare. Algoritmul trebuie sa foloseasca memorie suplimentara constanta, ordinea numerelor sa fie stabila, adica oricare doua numere egale din sir sa apara in aceeasi ordine dupa ce sirul a fost sortat, iar complexitatea algoritmului trebuie sa fie mai buna de O((n+m)^2).

def reverse0(a):
    n = len(a)
    for i in range(0, n/2):
        aux = a[i]
        a[i] = a[n-1-i]
        a[n-1-i] = aux

def reverse(a, start, end):
    n = end - start
    for i in range(0, n/2):
        aux = a[start + i]
        a[start + i] = a[end-1-i]
        a[end-1-i] = aux

#a = [1, 2, 3, 4]
#print a
#reverse(a, 1, 3)
#print a

def sortMN(a, n, m):
    i = n - 1
    j = m - 1
    while(i >= 0):
        while(a[i] < a[n + j]):
            j -= 1
        reverse(a, i + 1, j + n + 1)
        reverse(a, i, j + n + 1)        
        i -= 1
        j -= 1


#a = [3,5,7,9,1,2,4,6,10]
#print a
#sortMN(a, 4, 5)
#print a


#sa se roteasca la dreapta cu k pozitii in timp O(n) si folosind memorie suplimentara O(1). De exemplu pentru "abcdef", n = 6, k = 2 trebuie sa obtinem "efabcd".
def rotate(s, k):
    n = len(s)
    reverse(s, 0, n)
    reverse(s, k, n)

#s = [1,2,3,4,5,6]
#rotate(s, 2)
#print s

def convexHull(p):
    p.sort(key=lambda el: el[1])
    p.sort(key=lambda el: el[0])
    n = len(p)
    s = []
    s.append(p[0])
    s.append(p[1])
    minY = p[0][1]
    maxY = p[0][1]
    print s
    for i in range(2,n):
        el = s[-1]
        if (el[1] < maxY and el[1] > minY):
            s.pop()
        el = s[-1]
        if (minY > el[1]): minY = el[1]
        if (maxY < el[1]): maxY = el[1]
        if (p[i][0] > el[0] or p[i][1] > maxY or p[i][1] < minY):
            s.append(p[i])
        print ("added x=%d y=%d"%(p[i][0], p[i][1]))
        print s
        print ("new minY=%d maxY=%d"%(minY, maxY))
    s.sort(key=lambda el: el[1]*el[1]-el[0]*el[0])
    return s

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines as lines

def plotPoint(p):
    fig1 = plt.figure()
    for el in p:
        plt.plot(el[0], el[1], marker='o')
    maxY = max([el[1] for el in p])
    ch = convexHull(p)
    n = len(ch)
    for i in range(0, len(ch)-1):
        lx = (ch[i][0], ch[i+1][0])
        ly = (ch[i][1], ch[i+1][1])
        plt.plot(lx, ly, 'k-')
    lx = (ch[n-1][0], ch[0][0])
    ly = (ch[n-1][1], ch[0][1])
    plt.plot(lx, ly, 'k-')

    plt.xlim(0, maxY+2)
    plt.ylim(0, maxY+2)
    plt.xlabel('x')
    plt.title('test')
    plt.show()


p = [(3,2), (1,3), (1,2), (2,5), (3, 1), (5,2), (4, 2) , (6,4), (3,4)]

#print convexHull(p)

#plotPoint(p)

# 4-3 skiena
# partition 2n elements in pairs s.t. the max sum of pairs is the minimal among all partitions
def partMinMaxSum(a):
    n = len(a)/2
    quicksort(a, 0, 2*n)
    res = []
    for i in range(n):
        res.append([a[i], a[2*n-1-i]])
    return res

#a = [1,3,5,9]
#print partMinMaxSum(a)

# 4-4 skiena
# red = 0, blue = 1, yellow = 2
def sortColors(ac):
    buckets = [[] for i in range(3)]    
    for (i,j) in ac:
        buckets[j].append((i,j))
        j = 0
        for i in range(3):
            for el in buckets[i]:
                ac[j] = el
                j += 1
    return ac

#ac = [(1,1), (3,0), (4,1), (6,2), (9, 0)]
#print sortColors(ac)

# 4-8 skiena
# find if exists a, b in S of n elems s.t. a + b = x
def findSum(a, x):
    n = len(a)
    m = [0 for i in range(x)]
    quicksort(a, 0, n)
    curr = a[0]
    i = 0
    while (curr < x) and i < n:
        if (m[x - curr] == 1): 
            print("Sol found: (%d, %d)"%(curr, x-curr))
            break
        m[curr] = 1
        i += 1
        curr = a[i]

#a = [1, 2, 3, 6]
#findSum(a, 5)

def merge(a, b):
    n = len(a)
    m = len(b)
    c = [0 for i in range(n+m)]
    i = 0
    j = 0
    k = 0
    last = 0
    while(i < n and j < m):
        if (a[i] < b[j] and last < a[i]):
            c[k] = a[i]
            last = c[k]
            i += 1
            k += 1
        else:
            if (a[i] >= b[j] and last < b[j]):
                c[k] = b[j]
                last = c[k]
                j += 1
                k += 1
            else:
                if (last == a[i]): i += 1
                else: 
                    if (last == b[j]): j += 1
        print c
    for l in range(i, n):
        if (c[k-1] < a[l]):
            c[k] = a[l]
            k += 1
    for l in range(j, m):
        if (c[k-1] < b[l]):
            c[k] = b[l]
            k += 1
    return c

# 4-9
def unionAB(a, b):
    quicksort(a, 0, len(a))
    quicksort(b, 0, len(b))
    print a
    print b
    return merge(a, b)

#a = [1,3,2]
#b=[3,3,4,4,1]
#print unionAB(a, b)

def getKSmallest(a, k):
    n = len(a)
    for i in range(k):
        minv = a[i]
        pmin = i
        for j in range(i+1, n):
            if (minv > a[j]):
                minv = a[j]
                pmin = j
        aux = a[i]
        a[i] = a[pmin]
        a[pmin] = aux
        print minv 
        print pmin
        print a
    return a[:k]

#print getKSmallest([8,9,5,2,7,3,4,3,1], 4)

def quickpart(a, l, r, p):
    v = a[p]
    exch(a, p, r)
    sp = l
    for i in range(l, r):
        if (a[i] < v):
            exch(a, sp, i)
            sp += 1
    exch(a, r, sp)
    return sp

def quickSelect(a, l, r, k):
    if (r == l): 
        return a[l]
    else:
        pivot = l + (r-l)/2
        np = quickpart(a, l, r, pivot)
        if (np == k): 
            return a[k]
        if (np > k):
           quickSelect(a, l, pivot - 1, k)
        else:
            quickSelect(a, pivot + 1, r, k)

def quickSelectIter(a, l, r, k):
    np = 0
    while(np != k+1 and r != l):
        pivot = l + (r-l)/2
        np = quickpart(a, l, r, pivot)
        if (np == k+1): 
            return a[k]
        if (np > k): r = pivot - 1
        else: l = pivot + 1
    if (r == l): 
        return a[l]
        
#a = [8,9,5,2,7,10,4,3,1]
#print quickSelectIter(a, 0, len(a)-1, 1)

# 4-11
def findN2(a):
    n = len(a)
    occ = [0 for i in range(max(a)+1)]
    visited = [0,0]
    l = 0
    for i in range(n):
        occ[a[i]] += 1
    for i in range(n/2+1):
        if (l == 2):
            break
        if (occ[a[i]] >= n/2 and visited[l] == 0 and visited[l-1] != a[i]): 
            visited[l] = a[i]
            l += 1
            print("Sol found: %d"%a[i])

def findN4(a):
    n = len(a)
    occ = [0 for i in range(max(a)+1)]
    visited = []
    for i in range(n):
        occ[a[i]] += 1
    for i in range(3*n/4):
        if (len(visited) == 4):
            break
        if (occ[a[i]] >= n/4 and not (a[i] in visited)): 
            visited.append(a[i])
            print("Sol found: %d"%a[i])

#a = [1,6,1,1,6,6,1,6,2,1,2,2,2]
#findN4(a)
    

# imagine a is sorted; then the value which can be repeated more than n/2 times
# is that at pos [n/2], so find the [n/2]th smallest el in a and count its occ.
def findN2WithoutAux(a):
    v = quickSelectIter(a, 0, len(a)-1, n/2)
    c = 0
    for ai in a:
        if (ai == v): c += 1
    if (c >= n/2):
        print("Sol found: %d"%v)


a = [1,6,1,1,6,6,1,6,6]
#findN2WithoutAux(a)

from collections import defaultdict


V = [2,3,5,7,8,9,10,11]
E = [(7,11), (7,8), (5,11), (3,8), (3,10), (11,2), (11,9), (8,9)]

def getAdjL(V,E):
    adjL = {}
    for v in V:
        adjL[v] = []
    for (u,v) in E:
        adjL[u].append(v)
    return adjL

def visit(v, E, unmarked, visitedTemp, visitedPerm, res):
    print("v = %d"%v)
    #print unmarked
    print visitedTemp
    print res
    if visitedTemp[v] == True:
        print "not a DAG"
        return
    if v in unmarked:
        visitedTemp[v] = True        
        unmarked.remove(v)        
        for e in E:
            if e[0] == v:
                u = e[1]
                print("u = %d"%u)
                visit(u, E, unmarked, visitedTemp, visitedPerm, res)
        visitedPerm[v] = True
        visitedTemp[v] = False
        res[:0] = [v]

def topoSort(V,E):
    unmarked = V
    visitedTemp = { v: False for v in V }
    visitedPerm = { v: False for v in V }
    res = []
    n = len(V)
    while(len(unmarked) != 0):
        el = unmarked[0]
        visit(el, E, unmarked, visitedTemp, visitedPerm, res)
    return res
        
#print topoSort(V,E)

def binSearchMatrix(x, m, lx, ly, rx, ry):
    if (lx > rx or ly < ry): return False
    (midx, midy) = ((rx - lx)/2, (ry - ly)/2)
    if (m[midx][midy] == x): return True
    if (m[midx][midy] < x):
        res = binSearchMatrix(x, m, lx + midx, ly, rx, ry + midy)
        if not res:
            return binSearchMatrix(x, m, lx, ly - midy, rx, ry)
    else:
        a = [m[i][midx] for i in range(midy, ly)]
        res = binSearch(x, m, lx + midx, ly, rx, ry + midy)
        if not res:
            return binSearchMatrix(x, m, lx, ly, rx - midx, ry)

m = [[15,20,70,85],[20,35,80,95],[30,55,95,105],[40,80,100,120]]

#print binSearchMatrix(55, m, len(m), 0, 0, len(m))

def less(pq, i, j): return pq[i] < pq[j]

def greater(pq, i, j): return pq[i] > pq[j]

# for the taxicab pr: pq contains pairs
def taxiSum((i,j)): return i*i*i + j*j*j
def lessTaxi(pq, i, j): return taxiSum(pq[i]) > taxiSum(pq[j])

def compare(pq, i, j, op): 
    if op == "min": return less(pq, i, j)
    if op == "max": return greater(pq, i, j)
    if op == "minTaxi": return lessTaxi(pq, i, j)

def exch(pq, i, j): 
    aux = pq[i]
    pq[i] = pq[j]
    pq[j] = aux

#reheapify bottom-up: the prop is violated because a child becomes greater than the parent
def swim(pq, k, op):
    while(k > 1 and compare(pq, k/2, k, op)):
        exch(pq, k, k/2)
        k = k/2

#reheapify top-down: the prop is violated because a parent becomes smaller than children
def sink(pq, k, op):
    n = len(pq)
    while(2*k < n):
        j = 2*k
        if (j < n-1 and compare(pq, j, j+1, op)):
            j += 1
        if (not compare(pq, k, j, op)): 
            break
        exch(pq, k, j)
        k = j    

def insertHeap(pq, v, op):
    pq.append(v)
    swim(pq, len(pq)-1, op)

def removeHeap(pq, op):
    v = pq[1]
    pq[1] = pq[-1]
    pq.pop(-1)
    sink(pq, 1, op)
    return v

def insertMinHeap(pq, v):
    insertHeap(pq, v, "max")
def insertMaxHeap(pq, v):
    insertHeap(pq, v, "min")
def removeMinHeap(pq):
    return removeHeap(pq, "max")
def removeMaxHeap(pq):
    return removeHeap(pq, "min")

def insertMinTaxiHeap(pq, v):
    insertHeap(pq, v, "minTaxi")
def removeMinTaxiHeap(pq):
    return removeHeap(pq, "minTaxi")

#http://algs4.cs.princeton.edu/24pq/Taxicab.java.html
#find sols a^3 + b^3 = c^3 + d^3 for a,b,c,d < N
def taxicab(N):
    pq = []
    for i in range(0, N+1):
        insertMinTaxiHeap(pq, (i, i))
    prev = (0,0)
    run = 1
    while len(pq) > 1:
        el = removeMinTaxiHeap(pq)
        #print el, pq
        sprev = taxiSum(prev)
        sel = taxiSum(el)
        #print sprev, sel
        if sprev == sel:
            run += 1
            if run == 2:
                print sprev, '=', prev,
            print '=', el,
        else:
            if run > 1:
                print ""
                run = 1
        prev = el
        if el[1] < N:
            insertMinTaxiHeap(pq, (el[0], el[1] + 1))
    if run > 1:
        print ""

def findMedian(a):
    m = [0, a[0]]
    M = [0, a[1]]
    n = len(a)
    i = 2
    print("i=0: median =%d"%a[0])
    print("i=1: median =%d"%a[0])
    median = a[0]    
    while(i < n):
        # insert in either m or M s.t. we preserve the property any el from m is smaller than any el from M: \forall k,j. m[k] < M[j] or max(m) < min(M) and |len(m) - len(M)| < 2
        max_m = m[1]
        min_M = M[1]
        if (a[i] < min_M):
            insertMaxHeap(m, a[i])
        else:
            insertMinHeap(M, a[i])
        if (len(m) > len(M) + 1):
            el = removeMaxHeap(m)
            insertMinHeap(M, el)
        if (len(M) > len(m) + 1):
            el = removeMinHeap(M)
            insertMaxHeap(m, el)
        if (len(m) >= len(M)):
            median = m[1]
        else:
            median = M[1]
        print("i=%d: sorted(a)[i/2]=%d and median =%d"%(i, sorted(a)[i/2], median))
        print m
        print M
        print sorted(a[:i+1])
        i += 1

#findMedian([2,7,1,3,9,5,4,8,11,20,39,12,78,25])

import collections

#lines = open('data6_1.txt').read().splitlines()
#a = map(lambda x: int(x), lines)
#targets = range(-10000,10001)
#m = {}
#for x in a:
#    m[x] = True
#keys = [k for k in m.keys() if k < targets[-1]/2]
#okeys = sorted(keys)
#print len(okeys)

def findSumEmilie():
    n = len(m)
    nt = len(targets)
    i = 0
    total = 0
    #only need to check till 5000 
    for x in okeys:
        for t in targets:
            y = t - x
            if (y in m and y != x):
                total += 1
                targets.remove(t)
            if x > y:
                break
    return total

import time
start_time = time.time()
#print findSumEmilie()
taxicab(100)
print "seconds:"
print(time.time() - start_time)
