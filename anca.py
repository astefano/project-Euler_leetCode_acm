m = [[1,2,3,4,5],
     [1,2,3,4,5],
     [1,3,4,4,4],
     [3,4,4,5,5],
     [3,4,4,5,1]]

n = len(m)

rn = range(n)

from collections import defaultdict

def neigh(i,j):
   cand = [(i+1,j),(i-1,j),(i, j-1), (i, j+1), (i-1,j+1), (i+1,j-1)]
   r = filter(lambda(x,y): (0 <= x and x < n and 0 <= y and y < n), cand)
   #print("neigh(%s,%s)"%(i,j))
   #print(r)
   return r
   
adjL = defaultdict(list)
visited = defaultdict(list)

nodes = []
for i in rn:
    for j in rn:
         adjL[(i, j)] = [(k,l) for (k,l) in neigh(i, j) if m[i][j] == m[k][l]]
         nodes.append((i,j))
         visited[(i,j)] = False

#print adjL

nc = 0
tovisit = []
while(len(nodes) > 0):
    el = nodes[0]
    tovisit.append(el)
    nodes.remove(el)
    visited[el] = True
    compo = [el]
    while(len(tovisit) > 0):
        x = tovisit.pop()
        for y in adjL[x]:
            if (not visited[y]):
                tovisit.append(y)
                visited[y] = True
                compo.append(y)
            if (y in nodes):
                nodes.remove(y)
    nc += 1
    print("Component %d:"%nc)
    print(compo)

print(nc)            
