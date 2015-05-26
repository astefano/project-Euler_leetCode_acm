# Definition for a  binary tree node
class TreeNode:
     def __init__(self, x):
         self.val = x
         self.left = None
         self.right = None

class BSTIterator:

    # @param root, a binary search tree's root node
    def __init__(self, root):
      self._hasNext = False
      self._theNext = None
      self._iter = BSTIterator._walk(root)

    # @return a boolean, whether we have a next smallest number
    def hasNext(self):
      if not self._hasNext:
        try:
          self.next()
        except StopIteration: return False
      return True

    # @return an integer, the next smallest number
    def next(self):
      if not self._hasNext:
        self._theNext = next(self._iter)
      self._hasNext = not self._hasNext
      return self._theNext
    
    @staticmethod
    def _walk(node):
      if (node==None): return
      yield from BSTIterator._walk(node.left)
      yield node.val
      yield from BSTIterator._walk(node.right)

# Your BSTIterator will be called like this:
root = TreeNode(2)
left = TreeNode(1)
right = TreeNode(3)
root.left = left
root.right = right
i, v = BSTIterator(root), []
while i.hasNext(): 
  e = i.next()
  v.append(e)

print(v)
