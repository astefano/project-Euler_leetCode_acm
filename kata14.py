import collections

from random import randint

def kaba14(txt):
    trigrams = collections.defaultdict(list)
    txtl = txt.split(" ")
    n = len(txtl)
    for i in range(n-2):
        trigrams[(txtl[i],txtl[i+1])].append(txtl[i+2])
    print trigrams
    nt = len(trigrams)
    start_rand = randint(0, n-1)
    slo = []
    sl = [txtl[start_rand], txtl[start_rand+1]]
    i = 0
    #while slo != sl and i < 10:        
    while i < 10:        
        last = (sl[-2:][0], sl[-2:][1])
        print last
        cand = trigrams[last]
        print "cand", cand
        if len(cand) == 0:
            break
        nexti = randint(0, len(cand)-1)
        print nexti
        slo = sl
        sl.append(cand[nexti])
        print "sl", sl
        i+=1
    s = ""
    for w in sl:
        s += " " + w
    return s

print kaba14("I wish I may I wish I might")
