def enumJ(s, n, k):
    start = s
    c = 0
    next = start
    visited = [False for i in range(n)]

    while(start != -1):
        visited[start] = True 
        next = start
        while (next < n):
            print next
            visited[next] = True
            i = next+1
            j = 0
            if (visited.index(False) >= 0):
                while (j < k):
                    if (i >= n): i = 0
                    if (visited[i] == False): j+=1
                    i += 1
                    next = i-1
                start = visited.index(False)

#enumJ(0, 12, 5)

from collections import deque

def enumJS(s, n, k):
    tovisit = deque(range(1,n+1))
    last = 0
    x = 1
    while(x < s):
        tovisit.append(x)
        x = tovisit.popleft()
    visited = tovisit.popleft()
    #print visited
    i = 1
    while(len(tovisit) > 1):
        while(i < k):
            x = tovisit.popleft()
            tovisit.append(x)
            i += 1
        visited = tovisit.popleft()
        last = visited
        i = 1
    print last

def enumJDP(n, k):
    f = [0 for i in range(n)]
    f[1] = 0
    for i in range(2,n):
        f[i] = (f[i-1]+k) % n 
    print f[n-1]

for n in range(2,12):
    enumJS(0, n, 3)
    enumJDP(n, 3)
