# Definition for a  binary tree node
class TreeNode:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

import sys

class Solution:
    # @param root, a tree node
    # @return a boolean
    def isBalanced(self, root):
        if (root == None):
            return True
        tovisit = [(root,0)]
        prevh = 0
        while(len(tovisit) > 0):
            print tovisit
            (node, h) = tovisit.pop()
            print h
            print prevh
            if (abs(prevh - h) > 1):
                print "ha"
                return False
            prevh = h
            if (node.left != None):
                tovisit.append((node.left, h+1))
            if (node.right != None):
                tovisit.append((node.right, h+1))
        return True

root = TreeNode(1)
root.left = TreeNode(2)
root.right = TreeNode(3)
root.left.left = TreeNode(4)
root.left.right = TreeNode(5)
root.right.left = TreeNode(6)
root.right.right = TreeNode(7)
root.left.left.left = TreeNode(8)
root.left.left.right = TreeNode(9)
root.left.right.left = TreeNode(10)
root.left.right.right = TreeNode(11)
root.right.left.left = TreeNode(12)
root.right.left.right = TreeNode(13)
root.left.left.left.left = TreeNode(14)
root.left.left.left.right = TreeNode(15)

s = Solution()
print s.isBalanced(root)

