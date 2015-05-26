import random

def genPoints(n):
    i = 0
    points = []
    while i < n:
        x = random.random() % 100
        y = random.random() % 100
        points.append((x,y))
        i += 1
    return points 

points = genPoints(100)

def convexHull(p):
    p.sort(key=lambda el: el[1])
    p.sort(key=lambda el: el[0])
    n = len(p)
    s = []
    s.append(p[0])
    s.append(p[1])
    minY = p[0][1]
    maxY = p[0][1]
    print s
    for i in range(2,n):
        el = s[-1]
        if (el[1] < maxY and el[1] > minY):
            s.pop()
        el = s[-1]
        if (minY > el[1]): minY = el[1]
        if (maxY < el[1]): maxY = el[1]
        if (p[i][0] > el[0] or p[i][1] > maxY or p[i][1] < minY):
            s.append(p[i])
        print ("added x=%d y=%d"%(p[i][0], p[i][1]))
        print s
        print ("new minY=%d maxY=%d"%(minY, maxY))
    s.sort(key=lambda el: el[1]*el[1]-el[0]*el[0])
    return s

import numpy as np
import matplotlib.pyplot as plt
import matplotlib.lines as lines

def plotPoint(p):
    fig1 = plt.figure()
    for el in p:
        plt.plot(el[0], el[1], marker='bo')
    maxY = max([el[1] for el in p])
    ch = convexHull(p)
    n = len(ch)
    for i in range(0, len(ch)-1):
        lx = (ch[i][0], ch[i+1][0])
        ly = (ch[i][1], ch[i+1][1])
        plt.plot(lx, ly, 'k-')
    lx = (ch[n-1][0], ch[0][0])
    ly = (ch[n-1][1], ch[0][1])
    plt.plot(lx, ly, 'k-')

    plt.xlim(0, maxY+2)
    plt.ylim(0, maxY+2)
    plt.xlabel('x')
    plt.title('test')
    plt.show()


#p = [(3,2), (1,3), (1,2), (2,5), (3, 1), (5,2), (4, 2) , (6,4), (3,4)]
#print convexHull(p)

#plotPoint(points)

def dist(p1, p2):
    (x1, y1) = p1
    (x2, y2) = p2
    return (x1-x2)**2 + (y1-y2)**2

def distManhattan(p1, p2):
    (x1, y1) = p1
    (x2, y2) = p2
    return abs(x1-x2) + abs(y1-y2)

def sumDistsFrom(p0, points, dists):
    smin = 0
    for p in points:
        d = dist(p0, p)
        if (dists[(p0, p)] == 0):
            dists[(p0, p)] = d
            smin += d
    return smin

# http://www.careercup.com/question?id=4807138387951616
def getClosestNaive(points):
    dists = {}
    for p1 in points:
        for p2 in points:
            dists[(p1, p2)] = 0
    zippedSums = zip([sumDistsFrom(p, points, dists) for p in points], range(len(points)))
    r = min(zippedSums)
    print zippedSums
    print r
    return r[1]

# http://stackoverflow.com/questions/12905663/given-list-of-2d-points-find-the-point-closest-to-all-other-points
def getClosest(points):
    n = len(points)
    # sort on x
    points.sort(key=lambda el: el[0])
    # calc horizontal cost 
    ch = {p:0 for p in points}
    ch1 = [0 for i in range(n)]
    ch2 = [0 for i in range(n)]
    for i in range(1,n):
        diff = points[i][0] - points[i-1][0]
        ch1[i] = i*diff + ch1[i-1]
    ch[points[n-1]] = ch1[n-1]
    for i in range(n-2,-1,-1):
        diff = points[i+1][0] - points[i][0]
        ch2[i] = (n-1-i)*diff + ch2[i+1]
        ch[points[i]] = ch1[i] + ch2[i]
    # sort on y
    print ch
    points.sort(key=lambda el: el[1])
    # calc vertical cost 
    cv = {p:0 for p in points}
    cv1 = [0 for i in range(n)]
    cv2 = [0 for i in range(n)]
    for i in range(1,n):
        diff = points[i][1] - points[i-1][1]
        cv1[i] = i*diff + cv1[i-1]
    cv[points[n-1]] = cv1[n-1]
    for i in range(n-2,-1,-1):
        diff = points[i+1][1] - points[i][1]
        cv2[i] = (n-1-i)*diff + cv2[i+1]
        cv[points[i]] = cv1[i] + cv2[i]
    print cv
    minv = ch[points[0]] + cv[points[0]]
    minp = points[0]
    for p in points:
        nv = ch[p] + cv[p]
        if minv > nv:
            minv = nv
            minp = p
    print ch[minp] + cv[minp]
    return minp

def plotPoint2(points):
    fig1 = plt.figure()
    for el in points:
        plt.plot(el[0], el[1], 'bo')
    pr = getClosest(points)
    i = getClosestNaive(points)
    plt.plot(pr[0], pr[1], 'ro')
    plt.plot(points[i][0], points[i][1], 'gs')
    maxY = max([el[1] for el in points])
    maxX = max([el[0] for el in points])
    plt.xlim(0, maxX)
    plt.ylim(0, maxY)
    plt.xlabel('x')
    plt.title('test')
    plt.show()

points = genPoints(100)
#points = [(3,9), (1,3), (1,2), (2,5), (3, 1), (5,2), (4, 2) , (6,4), (3,4)]
#points = [(y,x) for (x,y) in [(1,5), (3,7), (1,10), (2, 12), (4,15)]]

plotPoint2(points)

#def getNearestPoint(p, points):

