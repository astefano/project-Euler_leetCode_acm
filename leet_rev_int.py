class Solution:
    # @return an integer
    def reverse(self, x):
        pos = 1
        if x < 0:
            pos = -1
        xa = abs(x)
        res = 0
        while(xa > 0):
            r = xa % 10
            xa = xa / 10
            res += r
            res *= 10
        res = res/10
        maxint32 = (1 << 31)
        print maxint32
        if pos == 1 and res > maxint32:
            return 0
        minint32 = ~(1 << 31)*-1
        if pos == -1 and res > minint32:
            return 0
        else:
            return res*pos
        

s = Solution()
#print s.reverse(-2147483648)
print s.reverse(-214)
