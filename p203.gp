f(limn)= {s=1; visited=Set(); for(n=2,limn,for(k=1,n/2, r=binomial(n,k); if (issquarefree(r), if(setsearch(visited, r)==0, print1(r" "); s+=r); visited=setunion(Set(r),visited)));print()) }

