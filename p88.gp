f(n,x) = (n-x+1)*(x*(n-x+1))^(1./(n-x))

ff(n) = solve(x=n/2,n-1,(f'(n,x)))

for(n=2,12000,print1(floor(ff(n))" "))

quit
