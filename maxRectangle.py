class Solution:
    def compute(self, (u,v), (p,q)):
        return (max(u,p), max(q, v)) 
    
    def area(self, (u,v), (p,q)):
        return abs(u-p+1)*abs(v-q+1)
        
    # @param matrix, a list of lists of 1 length string
    # @return an integer
    def maximalRectangle(self, matrix):
        #lines
        m = len(matrix)
        if m == 0:
            return 0            
        #col    
        n = len(matrix[0])
        print n, m
        a = [[0 for i in range(n)] for j in range(m)]
        b = [[(-1,-1) for i in range(n)] for j in range(m)]
        
        print a, b

        if matrix[0][0] == "1":
            b[0][0] = (0, 0)
            a[0][0] = 1
            
        for j in range(1,n):
            if matrix[0][j] == "1":
                b[0][j] = b[0][j-1]
                (u, v) = b[0][j]
                a[0][j] = max(j - v + 1, a[0][j-1])
    
        for j in range(1,m):
            if matrix[j][0] == "1":
                b[j][0] = b[j-1][0]
                (u, v) = b[j][0]
                a[j][0] = max(j - u + 1, a[j-1][0])
                
        for i in range(1,m):
            for j in range(1,n):
                print i, j
                if matrix[i][j] == "1":
                    if matrix[i-1][j] == "1" and matrix[i][j-1] == "1":
                        x = self.compute(b[i-1][j], b[i][j-1])
                        b[i][j] = x
                        a[i][j] = max([self.area(x,(i,j)), a[i-1][j], a[i][j-1]])
                else:
                    a[i][j] = max([a[i-1][j], a[i][j-1]])
                    
        return a[m-1][n-1]            
                    
s = Solution()
a = ["111011","111001"]
print s.maximalRectangle(a)
