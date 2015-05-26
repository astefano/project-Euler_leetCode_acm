class Solution:
    # @return a list of lists of length 3, [[val1,val2,val3]]
    def threeSum(self, num):
        num.sort()
        print num
        n = len(num)
        res = set([])
        i = 0 
        while i < n - 2 and num[i] <= 0:
            a = num[i]
            j = i + 1
            k = n - 1
            while j < k:
                b = num[j]
                c = num[k]
                if a + b + c == 0:                    
                    res.add((a, b, c))
                if a + b + c > 0:
                    k -= 1
                else:
                    j += 1
            i += 1
        return res


s = Solution()
v = [-1, 0, 1, 2, -1, -4]
print s.threeSum(v)
