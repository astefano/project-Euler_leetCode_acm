class Solution:
    # @param {string} s
    # @return {string[]}
    def findRepeatedDnaSequences(self, s):
        n = len(s)
        print n
        if n <= 10:
            return []
        d = {s[i:i+10]:0 for i in range(n-9)}
        for i in range(n-9):
            d[s[i:i+10]] += 1
        return filter(lambda x: d[x] > 1, d)    
            
            
s = Solution()
x = "AAAAACCCCCAAAAACCCCCCAAAAAGGGTTT"
#x = "AAAAAAAAAAA"
#x = "AAAAAAAAAA"
#x = "CAAAAAAAAAC"
print s.findRepeatedDnaSequences(x)
