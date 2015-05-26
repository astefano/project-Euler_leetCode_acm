class Solution:
    # @return a string
    def convert(self, s, nRows):
        if len(s) < 2 or nRows == 1:
            return s                        
        out = ""
        n = len(s)
        c = nRows + 1
        j = 0
        for i in range(nRows/2):
            out += s[i+j*(nRows+1)]
            j += 1
        k = nRows/2
        while k < n:
            out += s[k]
            k += nRows/2
        for i in range(nRows/2, nRows):
            out += s[i+j*(nRows+1)]
            j += 1
        return out
        

s = Solution()
print s.convert("1234", 2)


