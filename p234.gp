ff(l) = {s=0; pp=2; sql = nextprime(sqrt(l)); forprime(p=3, sql, m=min(l,p^2-1)/pp; for(i=pp+1,m, cand=i*pp; if (cand % p > 0, s+=cand)); m=min(l,p^2-1)/p; for(i=round(pp^2/p),m, cand=i*p; if (cand>pp^2 && cand % pp > 0, s+=cand)); pp = p); print1(s) }
