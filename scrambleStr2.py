class Solution:
    def countLetters(self, s):
        occ = {c:0 for c in s}
        for c in s:
            occ[c] += 1
        return occ

    def isScrambleR(self, s1, s2, m):
        n = len(s1)
        if n == 1:
            m[(s1, s2)] = s1 == s2
            return s1 == s2    
        if (s1, s2) in m.keys():
            return m[(s1, s2)]
        found = False
        for i in range(1, n):
            if self.isScrambleR(s1[:i], s2[:i],m) and self.isScrambleR(s1[i:], s2[i:],m):
                m[(s1, s2)] = True
                found = True
                break
            else:
                if self.isScrambleR(s1[i:], s2[:n-i],m) and self.isScrambleR(s1[:i], s2[n-i:],m):
                    m[(s1, s2)] = True                
                    found = True
                    break
        if not found:
            m[(s1, s2)] = False
        return m[(s1, s2)]    
                
    # @return a boolean
    def isScramble(self, s1, s2):
        occ1 = self.countLetters(s1) 
        occ2 = self.countLetters(s2) 
        for k in occ1.keys():
            if (not k in occ2.keys()) or occ1[k] != occ2[k]:
                return False
        n = len(s1)
        if len(s2) != n:
            return False
        m = {}
        return self.isScrambleR(s1, s2, m)

s = Solution()
#print s.isScramble("great","rgtae")
print s.isScramble("abcd", "bdac")
print s.isScramble("tdfiajsnfmvbanthzcrjaidnkjbljo", "aaablojjkndijrczhtnbvmfnsjifdt")
print s.isScramble("iydzdwbqbfixognqhbmimhwyhmdnrm", "nmbywdbnmmfybqqighdriizmxdhwho")
