class Solution:
    # @return an integer
    def atoi(self, str):
        l = len(str)
        i = 0
        sign = 1
        n = 0
        while i < l and str[i] == ' ':
            i += 1
        if i < l:
            if str[i] == '-':
                sign = -1
            if str[i] == '-' or str[i] == '+':
                i += 1
        while i < l and '0' <= str[i] and str[i] <= '9':
            n = n*10 + int(str[i])
            i += 1
            print n
        return sign*n
            
s = Solution()
print s.atoi("-+1")
