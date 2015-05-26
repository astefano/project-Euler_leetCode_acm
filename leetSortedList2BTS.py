# Definition for singly-linked list.
class ListNode:
    def __init__(self, x):
        self.val = x
        self.next = None

# Definition for a binary tree node.
class TreeNode:
    def __init__(self, x):
        self.val = x
        self.left = None
        self.right = None

def toString(t):
    s = ""
    tovisit = [t]
    while len(tovisit) > 0:
        el = tovisit.pop()
        if el: 
            s += str(el.val) + " "
            tovisit.append(el.left)
            tovisit.append(el.right)
        else:
            s += " * "        
    print s

class Solution:
    # @param {ListNode} head
    # @return {TreeNode}
    # Definition for a  binary tree node
    def sortedArrayToBST(self, num):
        n = len(num)
        if n == 0:
            return None
        tree = TreeNode(num[n/2])
        tree.left = self.sortedArrayToBST(num[:n/2])
        tree.right = self.sortedArrayToBST(num[n/2+1:])
        return tree
        
    def sortedListToBST(self, head):
        if not head:
            return None
        l = [head.val]
        while head.next:
            head = head.next
            l.append(head.val)
        print l
        return self.sortedArrayToBST(l)
        
s = Solution()
l = ListNode(0)
t = s.sortedListToBST(l)
toString(t)
