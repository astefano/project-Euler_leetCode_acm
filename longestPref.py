class Solution:
    # @return a string
    def longestCommonPrefix(self, strs):
        ns = len(strs)
        if ns == 0:
            return ""
        lens = [len(s) for s in strs]
        minl = min(lens)
        if minl == 0:
            return ""
        
        i = 0
        last = strs[0][0]
        pref = ""
        print minl
        while i < minl and strs[0][i] == last:
            print ("i = %i last = %s"%(i,last))
            for j in range(1, ns):
                if strs[j][i] != last:
                    return pref
            pref += last
            i += 1
            if (i < minl):
                last = strs[0][i]
            else:
                return pref
        return pref
                
s = Solution()
#strs = ["abab","aba","abc"]
strs = ["a"]
print s.longestCommonPrefix(strs)
