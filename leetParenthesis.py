class Solution:
    def neg(self, s):
        if s == '(':
            return ')'
        if s == '[':
            return ']'
        if s == '{':
            return '}'
            
    # @return a boolean
    def isValid(self, s):
        n1 = 0
        n2 = 0
        n3 = 0
        n = len(s)
        i = 0
        stack = []
        while i < n:
            while i < n and (s[i] == '(' or s[i] == '[' or s[i] == '{'):
                stack.append(s[i])
                i += 1
            if len(stack) == 0:
                return False
            print stack
            while i < n and (s[i] == ')' or s[i] == ']' or s[i] == '}'):
                last = stack.pop()
                print last
                if self.neg(last) != s[i]: 
                    return False
                i += 1
        return len(stack) == 0

s = Solution()
print s.isValid("()")
