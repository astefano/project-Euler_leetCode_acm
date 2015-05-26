class Solution:
    # @param {string[]} strs
    # @return {string[]}
    def anagrams(self, strs):
        if len(strs) == 1:
            return strs
        out = []
        dict = {''.join(sorted(s)):[] for s in strs}
        for s in strs:
            dict[''.join(sorted(s))].append(s)
        for el in dict:
            out.append(dict[el])
        return out

s = Solution()
x = ["an", "na"]
print s.anagrams(x)
