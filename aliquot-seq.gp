n=562

visited = Set(n)

f = sigma(n) - n

while(f<10^6 && !setsearch(visited,f),f=sigma(f)-f;l=l+1; visited=setunion(visited,Set(f)); print1(f", "))

quit
