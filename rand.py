import random 

#cracking the code 17.11
def genRand7():
    occ = [0 for i in range(7)]
    for i1 in range(5):
        for i2 in range(5):
            for i3 in range(5):
                for i4 in range(5):
                    for i5 in range(5):
                        for i6 in range(5):
                            for i7 in range(5):
                                i = (i1 + i2 + i3 + i4 + i5 + i6 + i7) % 7
                                occ[i] += 1
    print occ

genRand7()

def genRand7():
    occ = [0 for i in range(7)]
    for i1 in range(5):
        for i2 in range(5):
            i = (i1 + i2) % 7
            occ[i] += 1
    print occ

genRand7()

def genRand7():
    occ = [0 for i in range(7)]
    for r1 in range(5):
        for r2 in range(5):
            r =  5 * r1 + r2
            #if r < 21:
            if r < 21:
                r = r % 7
                occ[r] += 1
    print occ


