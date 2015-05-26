# let nsols(n) be the number of strings of len n on O, A with at most 2 consec occ. of A
#n solutions = 
               the number of "valid" strings with no L; this is precisely nsols(n) + 
	       n times the number of "valid" strings with 1 L, so n*nsols(n-1) // for each valid string of len n-1 we can insert L in n places and get n new valid strings
	       the number of "invalid" strings which have exactly 1 appearance of 3 consec A; for each such string we can insert L in 2 places to get valid strings of len n; example, for n = 4, the invalid string AAA gives AALA and ALAA	

# number of bit arrangements with at least one appearance of 3 consec occurences of bit 0 (in our case A is 0, O is 1)
(http://math.stackexchange.com/questions/178605/number-of-bit-strings-with-3-consecutive-zeros-or-4-consecutive-1s)
also http://oeis.org/A050231


a(n)=if (n<=2, 0, if (n==3, 1, 2*a(n-1)+2^(n-4)-a(n-4)))

nsols(n) = 2*nsols(n-1) - a(n)	

or???
nsols(n) = 2^n - a(n)	


#aaao............. => nsols(n-4)
#.............oaaa => nsols(n-4)
#...oaaao......... => sum_i nsols(i)*nsols(n-5-i) for i = 0, n-5
narrWithExactly3consecOcc(n) = sum(i=0, n-5, nsols(i)*nsols(n-5-i)) + 2*nsols(n-4)

#aaaao............. => nsols(n-5)
#.............oaaaa => nsols(n-5)
#...oaaaao......... => sum_i nsols(i)*nsols(n-6-i) for i = 0, n-6
narrWithExactly4consecOcc(n) = sum(i=0, n-6, nsols(i)*nsols(n-6-i)) + 2*nsols(n-5)


nsolsF(n) = nsols(n) + n*nsols(n-1) + 2*narrWithExactly3ConsecOcc(n-1) //L can be inserted in 2 places in the seq AAA (AALA, ALAA)
                                    + narrWithExactly4ConsecOcc(n-1) //L can be inserted only in one place in AAAA (AALAA) 
nsols2(n) = if (n<=1, 0, if (n==2 || n==3, 1, 2*nsols2(n-1)-nsols2(n-4)))


 Neal Stephensons Anathem

"Manfredo Perdigao Do Carmo" riemann geom

