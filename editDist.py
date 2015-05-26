class Solution:
    def longestCommonSubsequence(self, word1, word2):
        w1 = " " + word1
        w2 = " " + word2
        n = len(w1)
        m = len(w2)
        s = [["" for i in range(m)] for j in range(n)]
        l = [[0 for i in range(m)] for j in range(n)]
        for i in range(1, n):
            for j in range(1, m):
                if w1[i] == w2[j]:
                    l[i][j] = l[i-1][j-1] + 1
                    s[i][j] = s[i-1][j-1] + w1[i]
                else:
                    l[i][j] = max(l[i-1][j], l[i][j-1])
                    s[i][j] = max([s[i-1][j], s[i][j-1]], key=len)
        print s[n-1][m-1]
        return l[n-1][m-1]
        
    def lev(self, word1, word2):
        w1 = " " + word1
        w2 = " " + word2
        n = len(w1)
        m = len(w2)
        l = [[0 for i in range(m)] for j in range(n)]
        # we need i steps to transf a string of length i into ""
        for i in range(n):
            l[i][0] = i
        # we need i steps to transf "" into a string of length i 
        for i in range(m):
            l[0][i] = i
        for i in range(1, n):
            for j in range(1, m):
                diff = 1
                if w1[i] == w2[j]:                    
                    diff = 0
                l[i][j] = min(l[i-1][j] + 1, l[i][j-1] + 1, l[i-1][j-1] + diff)
        for i in range(n):
            print l[i]
        return l[n-1][m-1]

    #less memory: only 2 row matrices needed (from wiki: http://en.wikipedia.org/wiki/Levenshtein_distance#Iterative_with_two_matrix_rows); not yet working
    def levMemEff(self, word1, word2):
        w1 = " " + word1
        w2 = " " + word2
        if w1 == w2:
            return 0
        if len(w1) == 1 or len(w2) == 1:
            return max(len(w1), len(w2))
        m = len(w2)
        v0 = [0 for i in range(m)] 
        v1 = [0 for j in range(m)]
        # we need i steps to transf a string of length i into ""
        for i in range(m):
            v0[i] = i
        for i in range(1, m):
            # we need i steps to transf "" into a string of length i 
            v1[0] = i
            for j in range(1, m):
                diff = 1
                if w1[i] == w2[j]:                    
                    diff = 0
                v1[j] = min(v1[j-1] + 1, v0[j] + 1, v0[j-1] + diff)
        v0 = v1
        return v1[m-1]


    # @return an integer
    def minDistance(self, word1, word2):
        # this is taken from Ullman, big data, pg 96: edit distance = |w1| + |w2| - 2*|lcs|
        # in this case, the only ops allowed are insert, delete
        #return len(word1) + len(word2) - 2*self.longestCommonSubsequence(word1, word2)
        # edit distance where change is also allowed is called levenshtein; this dist
        # is <= edit dist.
        print self.lev(word1, word2)
        return self.levMemEff(word1, word2)

s = Solution()
#print s.minDistance("darna", "danar")
print s.minDistance("sea", "ate")
