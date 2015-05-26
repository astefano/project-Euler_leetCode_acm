from collections import defaultdict

# assume |a| > |b|
def longestSeq(a, b):
    na = len(a)
    nb = len(b)
    ls = ""
    for i in range(nb):
        k = 0
        for offset in range(na-i):
            while(k < min(nb - i, na-offset) and a[k+offset] == b[i+k]):
                print("i=%d k=%d offset=%d"%(i,k,offset))
                k += 1
        if (len(ls) < k): ls = b[i:i+k]
        print(ls)
    return ls

longestSeq("abcbcca", "abcabc")

#knuth morris pratt: w \subseteq s, i.e., s = s1ws2
def kmp(s, w):
    ns = len(s)
    nw = len(w)
    offset = 0
    for i in range(offset, ns - nw):
        j = 0
        while(w[j] == s[i + j]): 
            j += 1
            offset = 



