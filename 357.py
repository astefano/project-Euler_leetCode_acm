from collections import deque
from collections import defaultdict
import itertools

def genO357(k):
    #(a,b,c) encodes 7^a*5^b*3^c
    el = [(0,0,0) for i in range(k+1)]
    el[1] = (0,0,1)
    el[2] = (0,1,0)
    el[3] = (1,0,0)
    visited = defaultdict(int)
    visited[el[1]] = 3
    visited[el[2]] = 5
    visited[el[3]] = 7
    for i in range(4, k+1):
        minv = visited[el[i-1]]*3
        for (a,b,c) in el[1:i-1]:
            if (visited[(a,b,c+1)]==0):
                cand = (a, b, c+1)
                nv = visited[(a,b,c)]*3
                if (minv > nv):
                    minv = nv
                    el[i] = cand
            if (visited[(a,b+1,c)]==0):
                cand = (a, b+1, c)
                nv = visited[(a,b,c)]*5
                if (minv > nv):
                    minv = nv
                    el[i] = cand
            if (visited[(a+1,b,c)]==0):
                cand = (a+1, b, c)
                nv = visited[(a,b,c)]*7
                if (minv > nv):
                    minv = nv
                    el[i] = cand
        visited[el[i]] = minv
        print el[i]
        print visited[el[i]]

    p = el[k]
    print p
    #print visited

def gen357(k):
    if (k == 1): return 3
    if (k == 2): return 5
    if (k == 3): return 7
    q3 = deque([9,15,21])
    q5 = deque([25,35])
    q7 = deque([49])
    eli = 3
    for i in range(4, k+1):
        min3 = q3[0]
        min5 = q5[0]
        min7 = q7[0]
        minv = min(min3, min5, min7)
        if (minv == min3):
            q3.popleft()
        if (minv == min5):
            q5.popleft()
        if (minv == min7):
            q7.popleft()
        eli = minv
        q3.append(eli*3)
        q5.append(eli*5)
        q7.append(eli*7)
        print("i=%d v=%d"%(i,eli))
    return eli
            

print gen357(16)
                                                 
