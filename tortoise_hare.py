def findDuplicate(a):
    n = len(a)
    tortoise = n-1
    hare = n-1
    while True:
        tortoise = a[tortoise]
        hare = a[a[hare]]
        if (tortoise == hare):
            break
    tortoise = n-1
    while(hare != tortoise):
        tortoise = a[tortoise]
        hare = a[hare]
    return hare

a = [0,3,2,1,6,4,5,3,7,8]
#a = [0,5,2,3,4,1]
print findDuplicate(a)


def isPerm(a):
    return findDuplicate(a) == (len(a)-1)

a = [0,3,2,1,3,3]
print a[1:len(a)] 
print isPerm(a)   
