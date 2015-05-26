trim(n,k)={while(n % 10 == 0, n = n / 10); n % 10^k}

np(n,p)={m=floor(log(n)/log(p)); s=0; for(i=1,m, s+=floor(n/(p^i))); s}

nf(k,d)= { x=10^k; res=trim(2^(np(x,2)-np(x,5)), d)* trim(3^np(x,3),d); lp=precprime(x); forprime(p=7, lp, cp=p^np(x,p); res*=trim(cp,d); res=trim(res,d)); res }

f5(k,p) = { pp=1;for(n=1,k*10^p-1,if(n%5==0,pp*=n));pp }

fn5(k,p) = { pp=1;for(n=1,k*10^p-1,if(n%5>0,pp*=n));pp }

fnx5(x) = { pp=1;for(n=1,x,if(n%5>0,pp*=n));pp }

default(primelimit,999999999989)

psup=999999999989

modpow2(base, expo, mod)={c=1; e = expo; b = base; while(e > 0, if (e % 2 == 1, c = (c*b) % mod) ; e = e >> 1; b = (b*b) % mod); c}

ini(k, d) = { x = 10^k; mod = 10^d; res = modpow2(2, np(x,2)-np(x,5), mod)* modpow2(3, np(x,3), mod); res % mod }

f(k,d)= { res = ini(k,d); print1(res"\n");x=10^k;lp=precprime(x);mod=10^d;forprime(p=7,lp,cp=modpow2(p,np(x,p),mod);res*=cp;res=res%mod); res }

ini(x, d) = { mod = 10^d; res = modpow2(2, np(x,2)-np(x,5), mod)* modpow2(3, np(x,3), mod); res % mod }

f(x,d)= { res = ini(x,d); print1(res"\n"); lp=precprime(x);mod=10^d;forprime(p=7,lp,cp=modpow2(p,np(x,p),mod);res*=cp;res=res%mod); res }


10^6: 12544
10^7: 94688
10^8: 54176
10^9: 38144
10^10: 46112
10^11: 18752*...
10^12: 8448*...

nlp
999999999989
break> p
306309789161
break> res
77248



