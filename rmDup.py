class Solution:
    # @param a list of integers
    # @return an integer
    def removeDuplicates(self, A):
        if (len(A) < 2):
            return len(A)
        i = 0
        n = len(A)
        lasti = 1
        while(i < n):
            print ("before:i=%d"%i)
            while (i + 1 < n and A[i] == A[i+1]):
                i += 1
            print ("after:i=%d"%i)
            if (i+1 < n):
                A[lasti] = A[i+1]            
            print A
            i += 1
            lasti += 1
        return lasti-1 

s = Solution()
A = [1,1,2,2,2,2,3,3,4]
n = s.removeDuplicates(A)
print n
print A[:n]
