class Solution:
    # @return an integer
    def lengthOfLongestSubstring(self, s):
        n = len(s)
        if n == 0:
            return 0
        if n == 1:
            return 1
        maxl = 0
        i = 0
        visited = {c:(-1,-1) for c in s[i:]}
        for j in range(n-maxl):
            print s[i-1], visited[s[i-1]]
            i = visited[s[i-1]][0] + 1
            print i
            ltemp = 1
            print "rems: ", s[i:]
            visited = {c:(-1,-1) for c in s[i:]}
            visited[s[i]] = (visited[s[i]][1], i)
            while i + 1 < n and visited[s[i+1]][1] == -1:
                visited[s[i+1]] = (visited[s[i+1]][1], i+1)
                i += 1
                ltemp += 1
            if maxl < ltemp:
                maxl = ltemp
            print "maxl", maxl
            print "rems2: ", s[i:]
            while i < n and visited[s[i]][1] >= 0:
                visited[s[i]] = (visited[s[i]][1],i)
                print "i", i
                i += 1
            print "endi", i
            print visited
            if i == n:
                return maxl
        return maxl

    def lengthOfLongestSubstring0(self, s):
        n = len(s)
        if n < 2:
            return s
        maxl = 0
        i = 0
        for j in range(n-maxl):
            i = j 
            while i < n-1:
                ltemp = 1
                visited = {c:False for c in s[i:]}
                visited[s[i]] = True
                while i + 1 < n and not visited[s[i+1]] and s[i] != s[i+1]:
                    visited[s[i+1]] = True
                    i += 1
                    ltemp += 1
                if maxl < ltemp:
                    maxl = ltemp
                while i + 1 < n and  s[i] == s[i+1]:
                    i += 1
        return maxl

s = Solution()
#print s.lengthOfLongestSubstring("anviaj")
print s.lengthOfLongestSubstring("bbbbabcdea")
