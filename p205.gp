config6(nfaces) = { v=matrix(0,6); for(p1=1,nfaces, for(p2=p1,nfaces, for(p3=p2, nfaces, for(p4=p3, nfaces, for(p5=p4, nfaces, for(p6=p5, nfaces, v=concat(v, [p1,p2,p3,p4,p5,p6]))))))); v}

res6=config6(6)

config9(nfaces) = { v=matrix(0,9); for(p1=1,nfaces, for(p2=p1,nfaces, for(p3=p2, nfaces, for(p4=p3, nfaces, for(p5=p4, nfaces, for(p6=p5, nfaces, for(p7=p6,nfaces, for(p8=p7, nfaces, for(p9=p8, nfaces, v=concat(v, [p1,p2,p3,p4,p5,p6,p7,p8,p9])))))))))); v}

res9=config9(4)

totalCases = matsize(res6)[1]*matsize(res9)[1]

msum(v)={s = 0; for(i=1, #v, s+=v[i])}

winCases=0; for(icp9=1, ncp9, cs9=msum(res9[icp9,]); for(icp6=1, ncp6, cs6=msum(res6[icp6,]); if (cs6 < cs9, winCases+=1)))


config6N(nfaces) = { v=[]; for(p1=1,nfaces, for(p2=1,nfaces, for(p3=1, nfaces, for(p4=1, nfaces, for(p5=1, nfaces, for(p6=1, nfaces, v=concat(v, [p1+p2+p3+p4+p5+p6]))))))); v}

res6N=config6N(6)

config9N(nfaces) = { v=[]; for(p1=1,nfaces, for(p2=1,nfaces, for(p3=1, nfaces, for(p4=1, nfaces, for(p5=1, nfaces, for(p6=1, nfaces, for(p7=1,nfaces, for(p8=1, nfaces, for(p9=1, nfaces, v=concat(v, [p1+p2+p3+p4+p5+p6+p7+p8+p9])))))))))); v}

res9N=config9N(4)

tc6=46656
tc9=262144

gc=0; for(i9=1, tc9, for(i6=1, tc6, if (res6N[i6] < res9N[i9], gc+=1)))

