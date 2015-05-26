class Solution:
    # @param A  a list of integers
    # @param m  an integer, length of A
    # @param B  a list of integers
    # @param n  an integer, length of B
    # @return nothing(void)
    def merge(self, A, m, B, n):
        la = m - 1
        lb = n - 1
        lt = m + n - 1
        i = m
        while i <= lt:
            A.append(0)
            i += 1
        print A
        while lb > -1:
            print A
            if la > -1 and A[la] > B[lb]:
                A[lt] = A[la]
                la -= 1
            else:
                A[lt] = B[lb]
                lb -=1
            lt -= 1
                

s = Solution()
A = [4,5,6]
s.merge(A, 3, [1,2,3],3)
print A
