class Solution:
    # @param {integer[]} nums
    # @return {void} Do not return anything, modify nums in-place instead.
    def nextPermutation(self, nums):
        n = len(nums)
        i = 0
        while i < n-1 and nums[i] >= nums[i+1]:
            i += 1
        #decreasing
        if i == n-1:
            print "eo"
            for k in range(n/2):
                aux = nums[k]
                nums[k] = nums[n-1-k]
                nums[n-1-k] = aux            
        else:
            i = n-1
            while i > 0 and nums[i-1] >= nums[i]:
                i -= 1
            j = n
            while j > 0 and nums[j-1] <= nums[i-1]:
                j -= 1
            print i, j
            aux = nums[i-1]
            nums[i-1] = nums[j-1]
            nums[j-1] = aux
        
            i += 1
            j = n
            while i < j:
                aux = nums[i-1]
                nums[i-1] = nums[j-1]
                nums[j-1] = aux
                i += 1
                j -= 1

s = Solution()
xl = [[1,2,3], [2,1,3], [3, 2, 1], [1, 3, 2], [5,1,1], [1,5,1]]
for x in xl:
    print x
    s.nextPermutation(x)
    print x
