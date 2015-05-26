#http://cslibrary.stanford.edu/110/BinaryTrees.html

# Definition for a  binary tree node
class TreeNode:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

class Solution:
    # @param root, a tree node
    # @return an integer
    def dist(self, p, el):
        d = 0
        while p[el] != None:
            el = p[el]
            d += 1
        return d

    def minDepth(self, root):
        if root == None:
            return 0
        from collections import deque
        import math
        queue = deque([])
        queue.append(root)
        p = {}
        p[root] = None
        while len(queue) > 0:
            #print [q.val for q in queue]
            el = queue.popleft()           
            if el.left == None and el.right == None:
                return 1 + self.dist(p, el)#int(math.log(d-1)/math.log(2)) + 1
            else:                    
                if el.left != None:
                    p[el.left] = el
                    queue.append(el.left)
                if el.right != None:
                    p[el.right] = el
                    queue.append(el.right)

    def infixTraversal(self, root, l):
        if root == None:
            return l
        self.printTree(root.left, l)
        l.append(root.val)
        self.printTree(root.right, l)
        return l

    def postTraversal(self, root, l):
        if root == None:
            return l
        self.postTraversal(root.left, l)
        self.postTraversal(root.right, l)
        l.append(root.val)
        return l

    def bottomup(self, root):
        from collections import deque
        level = deque([])
        l = []
        level.append(root)
        while len(level) > 0:
            gl = [x.val for x in level if x != None]
            if len(gl) > 0:
                l.append(gl)
            nlevel = deque([])
            while len(level) > 0:
                x = level.popleft()            
                if x != None:
                    nlevel.append(x.left)
                    nlevel.append(x.right)
            level = nlevel
        l.reverse()
        return l

    def path(self, p, el):
        if el == None:
            return []
        path = [el.val]
        while p[el] != None:
            el = p[el]
            path.append(el.val)
        return path

    def pathSumRec(self, root, sum, p, l, prev):
        if (root.left == None or root.right == None) and sum == root.val:        
            l.add(tuple(self.path(p, prev)))
        if root.left != None:
            p[root.left] = root
            self.pathSumRec(root.left, sum-root.val, p, l, root.left)
        if root.right != None:
            p[root.right] = root            
            self.pathSumRec(root.right, sum-root.val, p, l, root.right)
        print l
        return list(l)

    def pathSum0(self, root, sum):
        p = {}
        p[root] = None
        s = set()
        print s
        res = self.pathSumRec(root, sum, p, s, None)
        print res
        return [list(x) for x in res] 

    def pathSum(self, root, sum):
        if root == None:
            return []
        if root.left == None and root.right == None and root.val == sum:
            return [[root.val]]
        left = []
        if root.left != None:
            left = self.pathSum(root.left, sum - root.val)
        right = []
        if root.right != None:
            right = self.pathSum(root.right, sum - root.val)        
        return [[root.val] + x for x in left + right]

    def preorderTraversalRec(self, root, l):
        if root == None:
            return l        
        l.append(root.val)
        print root.val
        #if root.left != None:
        self.preorderTraversalRec(root.left, l)
        #if root.right != None:
        self.preorderTraversalRec(root.right, l)
        return l

    def preorderTraversal(self, root):
        return self.preorderTraversalRec(root, [])

s = Solution()
t = TreeNode(3)
t.left = TreeNode(9)
t.right = TreeNode(20)
t.right.left = TreeNode(15)
t.right.right = TreeNode(7)

t1 = TreeNode(4)
t1.left = TreeNode(2)
t1.left.left = TreeNode(1)
t1.left.right = TreeNode(3)
t1.right = TreeNode(5)
print s.minDepth(t)
print s.postTraversal(t, [])

print s.bottomup(t)

t2 = TreeNode(5)
t2.left = TreeNode(4)
t2.right = TreeNode(8)
t2.left.left = TreeNode(11)
t2.right.left = TreeNode(13)
t2.right.right = TreeNode(4)
t2.left.left.left = TreeNode(7)
t2.left.left.right = TreeNode(2)
t2.right.right.left = TreeNode(5)
t2.right.right.right = TreeNode(1)
print s.pathSum(t2, 22)

t3= TreeNode(1)
t3.left = TreeNode(2)
print s.pathSum(t3, -5)

t4 = TreeNode(1)
print s.preorderTraversal(t1)
