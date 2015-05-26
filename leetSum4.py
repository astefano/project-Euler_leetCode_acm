class Solution:
    # @return a list of lists of length 4, [[val1,val2,val3, val4]]
    def fourSum0(self, num, target):
        num.sort()
        print num
        n = len(num)
        #import collections
        #res = collections.defaultdict(set)
        res = []
        i = 0 
        while i < n - 3:
            a = num[i]
            j = i + 1
            while j < n - 2:                
                b = num[j]
                k = j + 1
                l = n - 1
                while k < l:
                    c = num[k]
                    d = num[l]
                    s = a + b + c + d
                    if s == target:
                        res.append([a, b, c, d])
                    if s > target:
                        l -= 1
                    else:
                        k += 1
                    print a, b, c, d
                j += 1
            i += 1            
        return res
        #print res
        #resf = []
        #for d in num:
        #    for p in res[d]:
        #        resf.append([p[0], p[1], p[2], d])
        #        return resf

    # should divide in 2 & 2
    def fourSum(self, num, target):
        return "not done"

s = Solution()
#v, t = [-1, 0, 1, 2, -1, -4], 2
#v, t = [1, 0, -1, 0, -2, 2], 0
v, t = [0,4,-5,2,-2,4,2,-1,4], 12
print s.fourSum(v, t)
