class Solution:
    # @param num, a list of integer
    # @return a list of lists of integer
    def subsetsWithDup(self, S):
        S.sort()
        res = reduce(lambda x,y: x+[e+[y] for e in x], l, [[]])
        res.sort()
        n = len(res)
        return [res[i] for i in range(n) if i == 0 or res[i] != res[i-1]]

            
s = Solution()
l = [1, 1, 3, 2]
#print s.subsetsWithDup(l)
print s.subsetsWithDup(l)
