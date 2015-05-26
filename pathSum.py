# Definition for a  binary tree node
class TreeNode:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

class Solution:
    # @param root, a tree node
    # @param sum, an integer
    # @return a boolean
    def hasPathSum(self, root, sum):
        if root == None:
            return False
        tovisit = []
        tovisit.append((root, sum))
        while (len(tovisit) > 0):
            (n, s) = tovisit.pop()
            if n.val == s:
                return True
            if n.left != None:
                tovisit.append((n.left, s-n.val))
            if n.right != None:
                tovisit.append((n.right, s-n.val))
        return False

s = Solution()
n = TreeNode(2)
print s.hasPathSum(n, 1)
