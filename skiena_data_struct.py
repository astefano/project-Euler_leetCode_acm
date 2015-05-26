# 3-12 from http://www.algorithm.cs.sunysb.edu/algowiki/index.php/Data-structures-TADM2E
def bb(s, k):
    print s, k
    if k == 0:
        return True
    if len(s) == 0:
        return k == 0
    for si in s:
        if bb(s-set([si]), k-si):
            return True
    return False

def findSk0(s, k):
    res = []
    for si in s:
        if not bb(s-set([si]), k):
            res.append(si)
    return res

def findSk(s, k):
    for si in s:
        if bb(s-set([si]), k):
            s = s-set([si])
    return s

print findSk(set([1,2,4,3,5]), 9)
