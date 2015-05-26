class Solution:
    # @param A, a list of integers
    # @return an integer
    def maxProduct(self, A):
        n = len(A)
        if n == 1:
            return A[0]
        i = 0 
        tneg = 0
        leftneg = 0
        exists0 = False
        maxp = A[0]
        p = 1
        while i < n:
            if A[i] > 0:
                p *= A[i]
            else:
                #either a 0 or a neg, so reset p and upd maxp
                if maxp < p:
                    maxp = p
                    p = 1
                if A[i] < 0:
                    if tneg == 0:
                        leftneg = A[i]
                    tneg += 1
                else:
                    exists0 = True
                    
            i += 1
        if tneg == 1:
            res = max(maxp, p)
            if res < 0 and exists0:
                return 0
            else:
                return res
        
        print tneg
        i = 0
        nneg = 0
        p = 1
        while i < n:
            if A[i] == 0:
                i += 1
                continue
            if A[i] < 0:
                nneg += 1
            p *= A[i]
            i += 1
            oldi = i
            while i < n and A[i] > 0:
                p *= A[i]
                i += 1
            #if i + 1 < n and A[i] > 0 and A[i + 1] < 0:
            if tneg - nneg == 1:
                print "ha"
                maxp = p
                right = abs(A[i])
                i += 1
                while (i < n):
                    right *= A[i]
                    i += 1
                print p, right
                return max(p/abs(leftneg)*abs(right), p)                
            if i == n - 1:
                return p
            if oldi < i:
                nneg += 1
            print i, nneg, leftneg, p

        if exists0:
            return max(p, 0)
        else:
            return p

s = Solution()
a = [-4, -3]#, 5, -2, 8]
print s.maxProduct(a)
