sol(m) = {n=0;for(m2=1, m, for(m3=m2, m, d1 = ((m2+m3)^2 + m^2); d2= ((m+m2)^2+m3^2); d3 = ((m+m3)^2+m2^2); mind = min(d1, min(d2,d3)); if(issquare(mind) == 1, n+=1; print1("("m2", "m3", "m"):  "sqrt(mind)"\n")))); n}

all(m) = {a=0; for(i=1,m,a+=sol(i)); print1(a)}

all(100)

quit

