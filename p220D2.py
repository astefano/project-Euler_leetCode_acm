from turtle import pos, heading, right, left, forward, speed, exitonclick, hideturtle
 
sqrt2 = 1.41421356237

LEVEL=3

def dragon(level=LEVEL, size=pow(sqrt2,LEVEL), direction=45):
    global mcount 
    if level:
        #right(direction)
        left(direction)
        dragon(level-1, size/sqrt2, 45)
        #left(direction * 2)
        right(direction * 2)
        dragon(level-1, size/sqrt2, -45)
        #right(direction)
        left(direction)
    else:
        p = pos()
        h = heading()
        if (mcount == 500): print p
        if (mcount < 30): print h, p, size
        forward(size)
        mcount += 1


mcount = 0

speed(-100)
hideturtle()
dragon()
print "at the end:", heading(), pos()
exitonclick() # click to exit
