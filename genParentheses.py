def insert10(input, p):
    m1 = ~((1 << p) - 1)
    highBits= (input & m1)
    #print bin(highBits)
    m2 = (1 << p) - 1
    #print bin(m2)
    lowBits = input & m2
    #print bin(lowBits)
    return (highBits << 2) | (1 << (p+1)) | lowBits

#print bin(42)
#print bin(insert10(42, 2))
#print bin(insert10(42, 3))
#print bin(insert10(42, 4))

def gen(n):
    s = [set() for i in range(n+1)]
    s[1].add(2)

    for i in range(2, n+1):
        for el in s[i-1]:
            for p in range(0, 2*(i-1)):
                nel = insert10(el, p)
                s[i].add(nel)

    #els = [el for i in range(2, n+1) for el in s[i]]
    #print sorted(els)
    lens = [len(s[i]) for i in range(1, n+1)]
    print lens
    i = n
    #for i in range(2, n+1):
    for el in s[i]:
        r = bin(el)[2:].replace('1','(').replace('0', ')')
        print("%s %d"%(r, el))
        
gen(4)

def balanced(s):
    i = 0
    c = 0
    n = len(s)
    while (i < n):
        while(s[i] == '('):
            c += 1
            i += 1        
        while(i < n and s[i] == ')'):
            c -= 1
            i += 1
    if (c != 0):
        return False
    return True

print balanced("((())())()")
