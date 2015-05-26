class Solution:
    def wordBreakR(self, s, dict, cache):
        res = False
        n = len(s)
        if (s in dict): 
            res = True
        if (n == 0):
            return False
        print s
        print res
        print cache
        for i in range (n):
            prefix = s[:i+1]
            if (prefix in cache):
                res = res or self.wordBreakR(s[i+1:], dict, cache)                
                if (res):
                    return True
            else:
                if (prefix in dict): 
                    cache[prefix] = True
                    res = res or self.wordBreakR(s[i+1:], dict, cache)
                    if (res):
                        return True
        return res

    # @param s, a string
    # @param dict, a set of string
    # @return a boolean
    def wordBreak(self, s, dict):
        n = len(s)
        if (s in dict): return True
        alphs = {}
        nd = len(dict)
        for c in s:
            for i in range(nd):
                if c in dict[i]:
                    break
            if i == nd-1:
                return False
                    
        #check if all letters in s are in dict
        #cache = {s[:i]:False for i in range(len(s))}
        cache = {}
        return self.wordBreakR(s, dict, cache)


o = Solution()        
#print o.wordBreak("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaab", ["a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa"])
print o.wordBreak("aaaaab", ["a","aa","aaa","aaaa","aaaaa","aaaaaa","aaaaaaa","aaaaaaaa","aaaaaaaaa","aaaaaaaaaa"])

