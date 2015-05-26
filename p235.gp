u(k,r)=(900-3*k)*r^(k-1)
s(n,r)=sum(i=1,n,u(i,r))
fs(x)=s(5000,x)+600000000000
solve(x=1,2,fs(x))

