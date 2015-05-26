def n2R(x, res):
    if x < 2: return res
    c2 = 0
    for c in str(x):
        if c == '2':
            c2 += 1
    return n2R(x-1, c2 + res)

def n2(x):
    if x < 2: return 0
    n = 0
    n2i = 0
    for i in range(1,x+1):        
        n += n2i        
        if i % 10 == 2:
            n += 1
            vi = i
        #print("i = %d n = %d n2i = %d"%(i, n, n2i))
        while i > 0 and i % 10 == 0:
            i = i / 10 
            if i % 10 == 2:            
                n2i += 1
                n += 1
                #print("mod2: vi = %d i = %d n2i=%d n=%d"%(vi,i,n2i,n))
            #must be in a form ...30 so before it was ...29 so remove 1 for the 2 that dissapeared
            if i % 10 == 3:
                n2i -= 1
                n -= 1
                #print("mod3: vi = %d i = %d n2i=%d n=%d"%(vi,i,n2i,n))
    return n

#print n2(40)
                
#for i in range(350):
#    v1 = n2R(i, 0)
#    v2 = n2(i)
#    if v1 != v2:
#        print("i=%d v1=%d v2=%d"%(i, v1, v2))

#print n2(34235342)

#18.7
def isComposed(w, dict):
    res = False
    if w == "":
        return True
    for pref in dict:
        if w.startswith(pref):
            res = res or isComposed(w[len(pref):], dict)
    return res

def longestW(dict):
    dict.sort(key = len, reverse = True)
    for w in dict:
        if (isComposed(w, dict)):
            return w
         
dict = ["cat", "banana", "dog", "nana", "walk", "walker", "dogwalkeronthemoon","mo", "on", "the"]   
#print longestW(dict)

#18.9
def diff(i, o):
    n = len(i)
    ndiffs = 0
    for k in range(n):
        if i[k] != o[k]:
            ndiffs += 1
    return ndiffs

from collections import deque

def transf(i, o, dict):
    n = len(i)
    dictMin = [w for w in dict if len(w) == n]
    adj = {w:[] for w in dictMin}
    for w1 in dictMin:
        for w2 in dictMin:
            if diff(w1, w2) == 1: 
                adj[w1].append(w2)
    #not finished; do the bfs            
    tovisit = deque([i])
    visited = {w:False for w in dictMin}
    p = {}
    while (len(tovisit) > 0):
        x = tovisit.popleft()
        if x == o:
            el = o
            while (p[el] != i):
                print el
                el = p[el]
            print i
            break
        visited[x] = True
        for succ in adj[x]:
            if not visited[succ]:
                tovisit.append(succ)
                p[succ] = x

transf("1234", "4321", ["1234", "2234", "3234", "3224", "3221", "3321", "4321"])
