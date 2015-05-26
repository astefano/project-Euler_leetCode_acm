def getBit(x, i): 
    if (x & (1 << i) != 0): 
        return 1
    else: 
        return 0

def setBit(x, i):
    return x | (1 << i)

def clearBit(x, i):
    return x & ~(1 << i)

def updBit(x, i, v):
    nx = clearBit(x, i)
    return nx | (v << i)
 
def p5_1(n, m, j, i):
    x = n
    for k in range(j, i+1): 
        x = updBit(x, k, getBit(m, k-j))
    return x

print bin(1 << 2)

print bin(p5_1(1 << 10, 0b10011, 2, 6))

def diffs(n, m):
    d = 0
    while(n > 0 and m > 0):
        if (getBit(n, 0) != getBit(m, 0)):
            d += 1
        n = n >> 1
        m = m >> 1
    while (n > 0):
        d += 1
        n = n >> 1
    while (m > 0):
        d += 1
        m = m >> 1
    return d

print diffs(31, 14)

def diffs2(n, m):
    d = 0
    c = n^m
    while(c > 0):
        d += (c & 1 == 1)
        c = c >> 1
    return c

print diffs2(31, 14)

def test(input):
    return (input & 1) | ((input & 2) << 1) | ((input & 4) << 2) | ((input & 8) << 3) | ((input & 16) << 4)

# b1...bp...bk => b1...bp v ... bk
def insert(input, p, v):
    highBits= input & ~((1 << p) - 1)
    #print "high"
    #print bin(highBits)
    lowBits = input & ((1 << p) - 1)
    #print "low"
    #print bin(lowBits)
    return (highBits << 1) | (v << p) | lowBits

print bin(42)
print bin(insert(42, 1, 1))
print bin(insert(42, 2, 1))
print bin(insert(42, 3, 1))
print bin(insert(42, 4, 1))
print bin(insert(42, 5, 1))
print bin(insert(42, 6, 1))

