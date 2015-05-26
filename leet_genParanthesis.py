class Solution:
    # @param n, an integer
    # @return a string[]
    def generateParenthesis(self, n):
        if n == 0: 
            return set("")
        if n == 1:
            return set("()")
        else:
            res = self.generateParenthesis(n-1)
            s = set()
            for x in res:
                print x
                s.add("(" + x + ")")
                s.add(x + "()")
            return s

s = Solution()
print s.generateParenthesis(2)
