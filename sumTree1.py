# Definition for a  binary tree node
class TreeNode:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

class Solution:
    # @param root, a tree node
    # @return an integer
    def sumNumbers(self, root):
        if root == None: 
            return 0
        s = 0
        tovisit = []
        tovisit.append((root, root.val))
        while len(tovisit) > 0:
            print tovisit
            print s
            (node, v) = tovisit.pop()
            if (node.left != None): 
                tovisit.append((node.left, v*10 + node.left.val))
            if (node.right != None): 
                tovisit.append((node.right, v*10 + node.right.val))
            if (node.left == None and node.right == None):
                s += v
        return s
            

s = Solution()
root = TreeNode(1)
root.left = TreeNode(0)
root.right = TreeNode(2)
print s.sumNumbers(root)
