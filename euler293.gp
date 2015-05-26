// de la http://oeis.org/search?q=2%2C4%2C6%2C8%2C12%2C16%2C18%2C24%2C30%2C32%2C36%2C48&sort=&language=&go=Search

list(lim, p=2)=my(v=[1], q=nextprime(p+1), t=1); while((t*=p)<=lim, v=concat(v, t*list(lim\t, q))); vecsort(v)

myf(lim)={v=list(lim);s=Set();res=0;for(i=2,#v,nv=nextprime(v[i]+2)-v[i]; if(setsearch(s, Str(nv)) == 0, res+=nv; s=setunion(s, Set(nv)))); res}
? r = myf(10^9)

