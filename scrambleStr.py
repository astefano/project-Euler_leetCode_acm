#great
#rgtae

#abcd
#bdac

class Solution:
    def isScramble(self, s1, s2):
        n = len(s1)
        if len(s2) != n:
            return False
        m = {}
        self.isScrambleR(s1, s2, m, 0)
        return m[(s2, s1)]

    # @return a boolean
    def isScrambleR(self, s1, s2, m, k):
        tab = " "
        for i in range(k):
            tab += " "
        print tab, s1, s2
        if (s2, s1) in m.keys():
            print tab, s1, s2, m[(s2, s1)]
            return m[(s2, s1)]
        n = len(s1)
        if n == 1:
            m[(s2, s1)] = (s1 == s2)
            return s1 == s2
        found = False
        for i in range(1,n/2+1):
            l2 = s2[:i]
            r2 = s2[i:]
            l1 = s1[:i]
            r1 = s1[i:]
            if self.isScrambleR(l1, l2, m, k+1) and self.isScrambleR(r1, r2, m, k+1):
                m[(s2, s1)] = True
                found = True
                break
            else:
                l2b = s2[n-i:]
                r2b = s2[:n-i]
                if self.isScrambleR(l1, l2b, m, k+1) and self.isScrambleR(r1, r2b, m, k+1):
                    m[(s2, s1)] = True
                    found = True
                    break
        if not found:
            m[(s2, s1)] = False
        return m[(s2, s1)]
        
                                    
s = Solution()
print s.isScramble("great","rgtae")
print s.isScramble("abcd", "bdac")
print s.isScramble("tdfiajsnfmvbanthzcrjaidnkjbljo", "aaablojjkndijrczhtnbvmfnsjifdt")
print s.isScramble("iydzdwbqbfixognqhbmimhwyhmdnrm", "nmbywdbnmmfybqqighdriizmxdhwho")
