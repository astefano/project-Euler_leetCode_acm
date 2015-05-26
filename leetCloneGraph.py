# Definition for a undirected graph node
class UndirectedGraphNode:
    def __init__(self, x):
        self.label = x
        self.neighbors = []

class Solution:
    # @param node, a undirected graph node
    # @return a undirected graph node
    def cloneGraph0(self, node):
        if not node:
            return None
        cn = UndirectedGraphNode(node.label)
        tovisit = [(cn, node)]
        #visited = set([])
        cns = {}
        while len(tovisit) > 0:
            (cel, el) = tovisit.pop()
            #visited.add(el)
            for n in el.neighbors:
                #if n not in visited:
                if n.label not in cns:
                    print n.label
                    cnn = UndirectedGraphNode(n.label)                    
                    tovisit.append((cnn, n))
                    cns[cnn.label] = cnn
        print cns
        tovisit = [(cn, node)]
        visited = set([])
        visitedC = set([])
        while len(tovisit) > 0:
            (cel, el) = tovisit.pop()
            visited.add(el)
            print "visited = ", map(lambda x: x.label, visited)
            for n in el.neighbors:
                #if n not in visited:
                if n not in visited:
                    tovisit.append((cns[n.label], n))
                if cel not in visitedC:
                    cel.neighbors.append(n)
            visitedC.add(cel)
            print "neighs of ", cel.label, " = ", map(lambda x: x.label, cel.neighbors)
        return cn

    def cloneGraph(self, node):
        if not node:
            return None
        self.nodesMap = {}
        return self.createNode(node)

    def createNode(self, node):
        newNode = UndirectedGraphNode(node.label)
        self.nodesMap[newNode.label] = newNode
            
        for n in node.neighbors:
            if n.label not in self.nodesMap:
                self.createNode(n)
            newNode.neighbors.append(self.nodesMap[n.label])
        return newNode

    def serialize(self, node):
        tovisit = [node]
        visited = set([])
        nodesMap = {}
        while len(tovisit) > 0:
            el = tovisit.pop()
            visited.add(el)
            nodesMap[el.label] = filter(lambda x: x > el.label, map(lambda x: x.label, el.neighbors))
            for n in el.neighbors:
                if n not in visited:
                    tovisit.append(n)
        out = ""
        for el in sorted(nodesMap):
            l = ""
            if len(nodesMap[el]) > 0:
                l = reduce(lambda x, y: x+y, map(lambda x: ","+str(x), nodesMap[el]))
            out += str(el) + l +"#"
        return out

s = Solution()
g = UndirectedGraphNode(0)
g1 = UndirectedGraphNode(1)
g2 = UndirectedGraphNode(2)
g.neighbors = [g1, g2]
g1.neighbors = [g, g1]
g2.neighbors = [g]
cg = s.cloneGraph0(g)
print cg.label
print map(lambda x: x.label, cg.neighbors)
print "for ", cg.neighbors[0].label,  map(lambda x: x.label, cg.neighbors[0].neighbors)

n0 = UndirectedGraphNode(0)
n1 = UndirectedGraphNode(1)
n2 = UndirectedGraphNode(2)
n3 = UndirectedGraphNode(3)
n4 = UndirectedGraphNode(4)
n5 = UndirectedGraphNode(5)

n0.neighbors = [n1, n5]
n1.neighbors = [n0, n2, n5]
n2.neighbors = [n1, n3]
n3.neighbors = [n2, n4, n4]
n4.neighbors = [n3, n3, n5, n5]
n5.neighbors = [n0, n1, n4, n4]


cn = s.cloneGraph0(n0)
print s.serialize(n0)
print s.serialize(cn)
cn2 = s.cloneGraph(n0)
print s.serialize(cn2)
