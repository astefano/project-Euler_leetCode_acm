import numpy

from collections import defaultdict

vertices = range(6)

#adjL = [[], [2], [1,3], [4], [5], []]
adjL = [[], [2], [3], [4], [5], []]
cost = [0, 0, 20, -60, -60, 0]
#cost = [0, 0, 21, -60, -60, 0]

startingEnergy = 100 #"The player begins in the start room with 100 energy points. "

n = len(vertices)
ln = range(n)

dist = [0 for i in ln]

p = []
for i in ln:
    p.append(-1)

# assume we have a cycle of length 4 v -> n1 -> n2 -> n3 -> v
# we need to check that the way to v doesn't cost more than the energy at v
# that is, cost[n1] + cost[n2] + cost[n3] is smaller than cost[v]
def costCycle(v, c):
    costC = 0
    n = c
    while (n != v):
        costC += cost[n]
        n = p[n]
    return costC + cost[v]

def searchPosCycle(end, visited):
    c = end
    after = [False for i in ln]
    k = 0
    while(c != -1 and k < 10):
        after[c] = True
        # if c points to a visited node, then we have a cycle
        adjv = [i for i in adjL[c] if visited[i] and not after[i]]
        for v in adjv:
            #if the total cost of the cycle is pos
            if (startingEnergy + costCycle(v, c) > 0):
                print("Found it with cycle from %d to %d"%(c,v))
                return True
        c = p[c]
        k += 1
    return False

from collections import deque

def findPosWay(start, end):
    visited = [False for i in ln]
    tovisit = deque([start])
    stop = False
    while (len(tovisit) > 0 and not stop):
        print(tovisit)
        x = tovisit.popleft()
        visited[x] = True
        print("popped %d with adjL"%x)
        print(adjL[x])
        if (x == end):
            # if we already have a positive path stop
            if (startingEnergy + dist[x] > 0):
                print("Found it.")
                stop = True
            else:
                stop = searchPosCycle(x, visited)
        for succ in adjL[x]:
            if (not visited[succ]):
                tovisit.append(succ)
                visited[succ] = True
                print("visiting %d"%x)
                dist[succ] = dist[x] + cost[succ]
                p[succ] = x

findPosWay(1,5)
