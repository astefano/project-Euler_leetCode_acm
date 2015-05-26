def kmin(a, b, k):
    i = 0
    j = 0
    nm = 1
    na = len(a)
    nb = len(b)
    while i < na and j < nb and nm < k:
        if a[i] < b[j]:
            i += 1
        else:
            j += 1
        nm += 1    
    if i < na and j < nb:
        return min(a[i], b[j])
    if i < na:
        while nm < k:
            i += 1
            nm += 1
        return a[i]
    if i < nb:
        while nm < k:
            j += 1
            nm += 1
        return b[j]

a = [1, 2, 8]
b = [4, 5, 6, 7]
print kmin(a, b, 6)

