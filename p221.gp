m=2634775110933660;na=1; p=1; visited=Set(6); while(na<150001, pp1 = p*p+1; r = divisors(pp1); for(i=1, #r, a=p*(p+r[i])*(p+pp1/r[i]); if (a<m && setsearch(visited,a)==0, na+=1; visited=setunion(visited,Set(a)))); p+=1)

