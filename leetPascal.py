class Solution:
    # @return a list of lists of integers
    def generate0(self, numRows):
        if numRows == 0:
            return []
        if numRows == 1:
            return [[1]]
        l = self.generate(numRows-1)
        print l
        ll = l[-1]
        nl = [1]
        for i in range(numRows-2):
            nl.append(ll[i] + ll[i+1])
        nl.append(1)
        return l.append(nl)

    def generate(self, numRows):
        if numRows == 0:
            return []
        i = 1
        l = [[1]]
        while i < numRows: 
            ll = l[-1]
            nl = [1]
            for j in range(i-1):
                nl.append(ll[j] + ll[j+1])
            nl.append(1)
            l.append(nl)
            i += 1
        return l

    def getRow(self, rowIndex):
        if rowIndex == 0:
            return []
        i = 0
        l = [1]
        while i < rowIndex: 
            for j in range(i, i/2, -1):
                l[j] = l[j-1]+l[j]
            for j in range(1, i/2+1):                
                l[j] = l[i+1-j]
            i += 1
            l.append(1)
        return l

s = Solution()
print s.generate(5)
print s.getRow(6)

# 3: 1 3 3 1
# 4: 1 4 6 4 1
# 5: 1 5 10 10 5 1
# 6: 1 6 15 20 ...
