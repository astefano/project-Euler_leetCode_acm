class Solution:
    # @param triangle, a list of lists of integers
    # @return an integer
    def minimumTotal(self, triangle):
        n = len(triangle)
        m = triangle[n-1]
        print m
        for j in range(n-1,0,-1):
            for i in range(j):
                m[i] = min(m[i], m[i+1]) + triangle[j-1][i]
            print m
        return m[0]

    def minimumTotal0(self, triangle):
        tmp = triangle[len(triangle) - 1]
        for level in range(1, len(triangle))[::-1]:
            for i in range(0, len(triangle[level]) - 1):
                tmp [i] = triangle[level-1][i] + min(tmp[i], tmp[i+1])
            print tmp
        return tmp[0]

s = Solution()
triangle = [[-7],[-2,1],[-5,-5,9],[-4,-5,4,4],[-6,-6,2,-1,-5],[3,7,8,-3,7,-9],[-9,-1,-9,6,9,0,7],[-7,0,-6,-8,7,1,-4,9],[-3,2,-6,-9,-7,-6,-9,4,0],[-8,-6,-3,-9,-2,-6,7,-5,0,7],[-9,-1,-2,4,-2,4,4,-1,2,-5,5],[1,1,-6,1,-2,-4,4,-2,6,-6,0,6],[-3,-3,-6,-2,-6,-2,7,-9,-5,-7,-5,5,1]]
print s.minimumTotal(triangle)
#print s.minimumTotal([[-1],[2,3],[1,-1,-3],[-5,1,2,7]])
