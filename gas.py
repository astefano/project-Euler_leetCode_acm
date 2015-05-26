class Solution:
    # @param gas, a list of integers
    # @param cost, a list of integers
    # @return an integer
    def canCompleteCircuit(self, gas, cost):
        n = len(gas)
        load = 0
        pos = 0
        for i in range(n):
            tc = gas[i] - cost[i]
            load += tc
            if (load < 0):
                pos = i + 1
                load = 0
            print("i=%d: load=%d"%(i, load))
        print sum(gas)
        print sum(cost)
        if (sum(gas) >= sum(cost)):
            return pos
        else:
            return -1

s = Solution()
#print s.canCompleteCircuit([2, 3, 1], [3, 2, 1])
#print s.canCompleteCircuit([6, 1, 4, 3, 5], [3, 8, 2, 4, 2])

#3 -7 2 -1 3

print s.canCompleteCircuit([1,2,3,4,5], [3,4,5,1,2])
