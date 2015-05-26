
u(n) = 1 - n + n^2 - n^3 + n^4 - n^5 + n^6 - n^7 + n^8 - n^9 + n^10

genM(k: Int) = {m = matid(k); for(i=1, k, for(j=1, k, m[i,j]=i^(k-j))); m}

genV(k: Int) = {v = vectorv(k); for(i=1, k, v[i]=u(i)); v}

genVX(x: Int, k: Int) = {v = vector(k+1); for(i=1, k, v[i]=x^(k+1-i)); v[k+1]=1; v}

fiB(k: Int) = genVX(k+1,k-1)*matsolve(genM(k), genV(k))

for(k=1, 10, print1(fiB(k)" "))

sum(k=1, 10,fiB(k))    

