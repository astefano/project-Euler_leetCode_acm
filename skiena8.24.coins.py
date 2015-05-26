def solveR(c, s, sol):
    print c, s, sol
    if len(c) == 1:        
        solf = -1
        if s % c[0] == 0:
            solf = sol + [(c[0], s/c[0])]
        return (s % c[0] == 0, solf)
    else:
        for i in range(len(c)):            
            ci = c[i]
            times = s/ci
            while times > 0:
                (isSol, solf) = solveR(c[:i]+c[i+1:], s-times*ci, sol + [(ci, times)])
                if isSol:                
                    return (True, solf)
                else:
                    times -= 1
        if i == len(c)-1:
            return (False, [])

def solve(c, s):
    m = {}
    c.sort(reverse = True)
    return solveR(c, s, [])



print solve([2,3,4,9], 17)

#print solve([2,7,4,5], 27)
