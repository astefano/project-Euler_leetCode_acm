class Solution:
    # Dijkstra, A Discipline of Programming, pg 71
    # from http://www.cut-the-knot.org/do_you_know/AllPerm.shtml
    def getNext(self, p0):
        n = len(p0)
        p = [p0[i] for i in range(n)]
        i = n-1
        while p[i-1] >= p[i]:
            i -= 1
        j = n
        while p[j-1] <= p[i-1]:
            j -= 1
        #swap i-1, j-1
        aux = p[i-1]
        p[i-1] = p[j-1]
        p[j-1] = aux
        
        i += 1
        j = n
        while i < j:
            #swap i-1, j-1
            aux = p[i-1]
            p[i-1] = p[j-1]
            p[j-1] = aux
            i += 1
            j -= 1
        return p
            
    # @param num, a list of integer
    # @return a list of lists of integers
    def permute0(self, num):
        l = []
        idp = [x for x in range(1, num+1)]
        l.append(idp)
        stop = False
        pnew = []
        pold = idp
        stop = false
        i = 0
        while not stop:
            print l
            pnew = self.getNext(pold)
            print pnew
            l.append(pnew)
            #stop = idp == pnew
            pnew = pold
            i += 1
        return l        
        
    def permute(self, num):
        l = []
        perm = [x for x in range(1, num+1)] 
        count = 1
        numPerm = 20
        while (count < numPerm):
            N = len(perm)        
            i = N-1
            while (perm[i - 1] >= perm[i]):
                i = i - 1 
            j = N
            while (perm[j - 1] <= perm[i - 1]):
                j = j - 1
                #swap(i - 1, j - 1)        
                print "ha1", i-1, j-1
                aux = perm[i-1]
                perm[i-1] = perm[j-1]
                perm[j-1] = aux
            i+=1
            j = N
            while (i < j):
                #swap(i - 1, j - 1)
                print "ha2", i-1, j-1
                aux = perm[i-1]
                perm[i-1] = perm[j-1]
                perm[j-1] = aux
                i+=1
                j-=1

            count+=1
     
            print perm
            #permNum = ""
            #for k in range(N):
            #    permNum = permNum + str(perm[k])
            #print "*" + permNum

    g = []
    def permutationsG(self, head, tail=''):
        res = []
        if len(head) == 0: 
            #self.g.append(tail)
            return tail
        else:
            for i in range(len(head)):
                res += self.permutations(head[0:i] + head[i+1:], tail+head[i])
            #res.append(res0)
            return res
        
    def permutations0(self, head, res, tail=''):
        if len(head) == 0: 
            res.append(map(lambda x: int(x), tail))
        else:
            for i in range(len(head)):
                self.permutations(head[0:i] + head[i+1:], res, tail+head[i])

    def permuteF0(self, num):
        if len(num) < 2:
            return num
        else:
            x = []
            s = reduce(lambda x, y: str(x)+str(y), sorted(num))
            print s
            self.permutations(s, x)
            return x

    def permutations(self, head, res, tail=[]):
        if len(head) == 0: 
            res.append(map(lambda x: int(x), tail))
        else:
            for i in range(len(head)):
                self.permutations(head[0:i]+head[i+1:], res, tail+[head[i]])

    def permuteF(self, num):
        if len(num) < 2:
            return num
        else:
            x = []
            self.permutations(sorted(num), x)
            return x

s = Solution()
#print s.permute(3)
x = []
s.permutations([-1,2,3,4], x)
print x
print s.permuteF([2,-2])
