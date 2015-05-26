class Solution:
    # @param haystack, a string
    # @param needle, a string
    # @return an integer
    def strStr(self, haystack, needle):
        lh = len(haystack)
        ln = len(needle)
        if lh < ln:
            return -1
        if ln == 0:
            return 0
        if ln == lh:
            if haystack == needle:
                return 0
            return -1
        i = 0
        j = 0
        while i <= lh - ln:
            print haystack[i:], needle[j:]
            while j < ln and i < lh and needle[j] == haystack[i]:
                j += 1
                i += 1
            if j == ln:
                return i-ln
            else:
                i = i - j + 1
                j = 0
        return -1            

s = Solution()
h = "mississippi"
n = "pi"
print s.strStr(h, n)
