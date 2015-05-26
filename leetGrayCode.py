class Solution:
    # @return a list of integers
    def grayCode(self, n):
        if n == 0:
            return [0]
        if n == 1:
            return [0, 1]
        r = self.grayCode(n-1)
        print r
        l = 1<<n
        nr = []
        for i in range(l/2):
            nr.append(r[i])
            print nr
        for i in range(l/2, l):
            print l-i
            nr.append(l/2 + r[l-1-i])
            print nr
        return nr

s = Solution()
print s.grayCode(3)
