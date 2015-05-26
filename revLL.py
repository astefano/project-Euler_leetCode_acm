# Definition for singly-linked list.
class ListNode:
    def __init__(self, x):
        self.val = x
        self.next = None

class Solution:
    def printl(self, head):
        c = head
        while c != None:
            print c.val, 
            c = c.next
        print

    def reverseRec(self, head, res):
        if head == None:
            return res
        n = ListNode(head.val)
        n.next = res
        res = n
        return self.reverseRec(head.next, res)

    # @param head, a ListNode
    def reverse(self, head):
        c = head
        prev = None
        while c != None:
            temp = c.next
            c.next = prev
            prev = c
            c = temp
        return prev

s = Solution()
l = ListNode(3)
l.next = ListNode(2)
l.next.next = ListNode(1)
l.next.next.next = ListNode(0)
s.printl(s.reverse(l))

        
