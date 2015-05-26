lenChain(n)={cn = n; res=eulerphi(cn); len=1; while(res > 1, len+=1; cn=res; res=eulerphi(cn)); len+1}

s=0; forprime(p=10^4+1, lim, if (lenChain(p)==25, s+=p))
