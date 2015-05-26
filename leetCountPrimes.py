class Solution:
    import math
    # @param {integer} n
    # @return {integer}
    def countPrimes0(self, n):
        if n <= 1:
            return 0
        if n == 2:
            return 1
        a = {i: True for i in range(2, n+1)}
        np = n - 1
        l = int((n**(1/2.))+1)
        #print l
        for i in range(2,l):
            if a[i] == True:
                j = i*i
                while j <= n:
                    if a[j]:
                        np -= 1
                    a[j] = False
                    j += i
        print filter(lambda i: a[i] == True, range(2, n+1))
        return len(filter(lambda i: a[i], range(2, n+1)))
        #return np

    def countPrimes(self, n):
        if n <= 1:
            return 0
        if n == 2:
            return 1
        # from odd nbs remove 1, add 2.
        np = n/2 
        slicel = np
        print np
        if np % 7 == 0:
            slicel = np/7
        if np % 10 == 0:
            slicel = np/10
        if np % 11 == 0:
            slicel = np/11
        if np % 13 == 0:
            slicel = np/13
        if np % 17 == 0:
            slicel = np/17

        print "slicel = ", slicel

        nslices = n/slicel
        cslice = 0
        offset = 2
        p = set([3])
        starts = 0
        end = starts + 2*slicel + 1
        while starts < n-slicel:
            a = {starts + 2*i+1: True for i in range(1, slicel+1)}
            end = starts + 2*slicel + 1
            start = starts + 1 
            print "cslice = ", cslice, " start = ", start, " end = ", end, " a = ", sorted(a)
            for pi in p | set(a):
                #print "pi = ", pi
                if pi*pi <= end:
                    i = 0                    
                    o = (start - pi*pi)/pi 
                    if o > 0:
                        i = o+1
                    j = pi*pi + i*pi
                    #print "j =", j
                    while j <= end:
                        if j <= n and j > start and j % 2 == 1 and a[j]:
                            np -= 1
                            a[j] = False
                            print "elim ", j
                        j += pi
            if start*start < n:
                #p |= set(filter(lambda i: a[i]==True and i*i <= n, a))
                p |= set(filter(lambda i: a[i]==True, a))
            print " p= ", sorted(p)
            cslice += 1
            starts = end - 1
        #return len(filter(lambda i: a[i], range(2, n+1)))
        if end > n:
            return np
        else:
            return np + 1

s = Solution()
#print s.countPrimes(499979)
#print s.countPrimes(1500000)
import sys

n = int(sys.argv[1])

print s.countPrimes(n)
print s.countPrimes0(n)
#print s.countPrimes(49)
