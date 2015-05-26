def mand(l):
    s = l[0]
    for i in range(1, len(l)):
        s += " && " + l[i]
    return s

def mor(l):
    s = l[0]
    for i in range(1, len(l)):
        s += " || " + l[i]
    return s

#p0 == !pl && !pr
def p0(i):
    return mand(["!" + "p"+str(i)+"l", "!" + "p"+str(i)+"r"])

#p1 ==  pl && !pr
def p1(i):
    return mand([      "p"+str(i)+"l", "!" + "p"+str(i)+"r"])

#p2 == !pl &&  pr
def p2(i):
    return mand(["!" + "p"+str(i)+"l",       "p"+str(i)+"r"])

#p3 ==  pl &&  pr
def p3(i):
    return mand([      "p"+str(i)+"l",       "p"+str(i)+"r"])

#fi0 == !fi
def f0(i):
    return "!f"+str(i)

#fi1 == fi
def f1(i):
    return "f"+str(i)

def ct(s):
    return "[ " + s + " ]"

def appi(s, i):
    return s + str(i)

# at p11 /\ f10 //f10 is !f1
# en(take1) = ((p11 && !f1) -> ([t1 <= gamma1]))
def enTake(i):
    return mand([p1(i), f0(i), ct(appi("t", i) + " <= " + appi("gamma", i))])

# at p12 /\ f20 
# en(eat1) = ((p12 && !f2) -> ([t1 <= 3])) 
def enEat(i, n):
    i1 = i
    if i1 + 1 == n:
        i1 = 0
    return mand([p2(i), f0(i1+1), ct(appi("t", i) + " <= 3")])

# p13 /\ f11 /\ f21
# en(release1) = ((p13 && f1 && f2) -> ([2 - alpha1 <= t1 - ts1] && [t1 - ts1 <= 3] && [ts1 <= alpha1] && [t1 <= 3])) 
def enRelease(i, n):
    i1 = i
    if i1 + 1 == n:
        i1 = 0
    return mand([p3(i), f1(i), f1(i1+1), 
                 ct(appi("2 - alpha", i) + " <= " + appi("t", i) + " - " + appi("ts", i)), 
                 ct("3 >= " + appi("t", i) + " - " + appi("ts", i)), 
                 ct(appi("ts", i) + " <= " + appi("alpha", i)),
                 ct(appi("t", i) + " <= 3")
             ])

# en(reset) = ((p10 && p20 -> ([ts1 <= alpha1] && [ts2 <= alpha2] && [ts2 - ts1 <= alpha2 - beta1] && [ts1 - ts2 <= alpha1 - beta2]))
def enReset(n):
    l = [p0(i) for i in range(1,n+1)]
    for i in range(1,n+1):
        l.append(ct(appi("ts", i) + " <= " + appi("alpha", i)))
        for j in range(1,n+1):
            if (i != j):
                l.append(ct(appi("t", i) + " - " + appi("ts", j) + " <= " + appi("alpha", i) +  " - " + appi("beta", j)))
    return mand(l)


def guaranteePhilo(n):
    l = []
    for i in range(1, n+1):
        l.append(enTake(i))
        l.append(enEat(i, n+1))
        l.append(enRelease(i, n+1))
    l.append(enReset(n))
    return "Guarantee( " + mor(l) + " )"

import sys

print guaranteePhilo(int(sys.argv[1]))
