setPrimes_Pows(n,hlim)= { myp=vector(hlim); wp=[]; forprime(p=2,hlim,myp[p]=round(log(n)/log(p)); wp=concat(wp, [p])); [myp,wp] }

c=0; res=setPrimes_Pows(10^9, 100); pows=res[1]; myp=res[2];

for(i1=0, pows[myp[1]], paux=myp[1]^i1; for(i2=0, pows[myp[2]], for(i3=0, pows[myp[3]], for(i4=0, pows[myp[4]], for(i5=0, pows[myp[5]], for(i6=0, pows[myp[6]], for(i7=0, pows[myp[7]], for(i8=0, pows[myp[8]], for(i9=0, pows[myp[9]], for(i10=0, pows[myp[10]], for(i11=0, pows[myp[11]], for(i12=0, pows[myp[12]], for(i13=0, pows[myp[13]], for(i14=0, pows[myp[14]], for(i15=0, pows[myp[15]], for(i16=0, pows[myp[16]], for(i17=0, pows[myp[17]], for(i18=0, pows[myp[18]], for(i19=0, pows[myp[19]], for(i20=0, pows[myp[20]], for(i21=0, pows[myp[21]], for(i22=0, pows[myp[22]], for(i23=0, pows[myp[23]], for(i24=0, pows[myp[24]], for(i25=0, pows[myp[25]], if (*myp[2]^i2*myp[3]^i3*myp[4]^i4*myp[5]^i5*myp[6]^i6*myp[7]^i7*myp[8]^i8*myp[9]^i9*myp[10]^i10*myp[11]^i11*myp[12]^i12*myp[13]^i13*myp[14]^i14*myp[15]^i15*myp[16]^i16*myp[17]^i17*myp[18]^i18*myp[19]^i19*myp[20]^i20*myp[21]^i21*myp[22]^i22*myp[23]^i23*myp[24]^i24*myp[25]^i25 <= 10^9, c+=1))))))))))))))))))))))))))


