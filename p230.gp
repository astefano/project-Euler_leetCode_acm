i=2;for(n=0,17, x=(127+19*n)*(7^n); print1(x": "); while(fibonacci(i)*100<x, i+=1); print1(i"\n"))
127: 3
1022: 7
8085: 11
63112: 16
487403: 20
3731154: 24
28353409: 28
214121180: 32
1608379479: 37
12025374886: 41
89544653933: 45
664381785648: 49
4913656956355: 53
36236489892218: 57
266541667629657: 62
1955995342096516: 66
14323393075498031: 70
104683731294243150: 74

a = 1415926535
b = 8979323846


dd(x,ndx,y,ndy,k)={ newy = x*10^ndy+y; s = ndx+ndy; if (k<=s, floor(newy / (10^(s-k)))%10, dd(y, ndy, newy, ndy+ndx, k)) }

? dd(a,10,b,10,35)
9

A = 1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679
B = 8214808651328230664709384460955058223172535940812848111745028410270193852110555964462294895493038196

//l is the length of w
getLetter(w,p,l)={r=floor(w/(10^(l-p))); print1(r); r%10}

getDigitInFibWAB(n) = { x=2+floor(n*phi)-floor((n+1)*phi);if(x==1,"A","B") }

fibW(n)={l=fibonacci(n);forstep(i=l,1,-1,print1(getDigitInFibWAB(i)))}

fibW(n)={l=fibonacci(n);v=vector(l);for(i=1,l,v[l-i+1]=getDigitInFibW(i));v}

i=2;posFW=vector(18);for(n=0,17, x=(127+19*n)*(7^n); print1(x": "); while(fibonacci(i)*100<x, i+=1); print1(i"\n"); posFW[n+1]=i)

getDigitInRevFibWAB(k,n)=getDigitInFibWAB(fibonacci(k)-n+1)

dab(n)={vn=(127+19*n)*(7^n); print1("vn= "vn"\n"); k=posFW[n+1]; print1("k= "k"\n"); p=floor(vn/100)+1; print1("p= "p"\n"); block=getDigitInRevFibWAB(k,p); print1("block= "block"\n"); evalBlock=A; if (block=="B",evalBlock=B); pb=(vn%100); print("vn= "vn" pb= "pb"\n"); getLetter(evalBlock,pb,100)}

s=0;for(n=0,17,s+=10^n*dab(n))
