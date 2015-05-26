class Solution:
    # @param digits, a list of integer digits
    # @return a list of integer digits
    def plusOne(self, digits):
        carry = 0
        n = len(digits)
        i = n-1        
        while(i > -1 and digits[i]+1 == 10):
            digits[i] = 0
            i -= 1
        print i
        if i < 0:
            return [1]+digits
        else:
            digits[i] += 1
            return digits

s = Solution()
print s.plusOne([9,9,9,9])
