def p71( n ):
    """Python function to print the nth Farey sequence, ascending."""
    stop = False
    a, b, c, d = 0, 1,  1  , n     # (*)
    while (c <= n and not stop):
        k = int((n + b)/d)
        a, b, c, d = c, d, k*c - a, k*d - b
	#print "%d/%d " % (a,b)	
	if (c == 3 and d == 7):
	        print "%d/%d" % (a,b)	
		stop = True

p71(19)

p71(26)

p71(50)

p71(100)

p71(1243)
