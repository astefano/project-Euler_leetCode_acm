sol(m) = {n=0;for(m2=1, m, for(m3=m2, m, d1 = ((m2+m3)^2 + m^2); d2= ((m+m2)^2+m3^2); d3 = ((m+m3)^2+m2^2); mind = min(d1, min(d2,d3)); if(issquare(mind) == 1, n+=1))); n}

minsol = 2060

minm = 100

for(m=101, 10000, minsol += sol(m); if (minsol >= 10^6, minm=m; break); print1(m", "minsol"\n"))

print1(minm)

quit

