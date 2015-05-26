n=0

for(a1=1,9,for(a2=0,9-a1,for(a3=0,max(9-a2-a1,0), for(a4=0, max(9-a3-a2,0), for(a5=0, max(9-a4-a3, 0), for(a6=0, max(9-a5-a4, 0), for(a7=0, max(9-a6-a5, 0), for(a8=0, max(9-a7-a6, 0), for(a9=0, max(9-a8-a7, 0), for(a10=0, max(9-a9-a8, 0), n+=1))))))))))

print1(n)

quit
