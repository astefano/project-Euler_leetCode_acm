class Solution:
    # @param n, an integer
    # @return an integer
    def reverseBits(self, n):
        if n == 0:
            return 0
        nr = 0
        bits = 0
        while n > 0:
            print n&1
            nr = (nr<<1) + (n&1)
            n = n >> 1
            bits += 1
            print nr, n, bits
        while bits < 32:
            nr = nr << 1
            bits += 1
        return nr

s = Solution()
print s.reverseBits(5)
