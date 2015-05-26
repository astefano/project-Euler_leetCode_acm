class Solution:
    # @param {integer} n
    # @return {boolean}
    def isHappy(self, n0):
        from collections import defaultdict
        a = defaultdict(lambda: "default", key=0)
        a[n0]=1
        stop = False
        found = False
        n = n0
        while not stop:
            ln = map(lambda x: int(x)*int(x), str(n))
            print ln
            if len(ln) == 1:
                nn = ln[0]*ln[0]
            else:
                nn = reduce(lambda x, y: x + y, ln)
            print "nn = ", nn
            if nn == 1:
                stop = True
                found = True
            if a[nn] == 1:
                stop = True
            else:
                a[nn] = 1
                n = nn

        return found

s = Solution()
print s.isHappy(99)
