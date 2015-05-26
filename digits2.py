import sys
from itertools import combinations
from itertools import product
from itertools import chain
from sets import Set

digits = {1:[False,True,True,False,False,False,False], 2:[True,True,False,True,True,False,True], 3:[True,True,True,True,False,False,True], 4:[False,True,True,False,False,True,True], 5:[True,False,True,True,False,True,True], 6:[True,False,True,True,True,True,True], 7:[True,True,True,False,False,False,False], 8:[True,True,True,True,True,True,True], 9:[True,True,True,True,False,True,True], 0:[True,True,True,True,True,True,False]}

inv_digits = {str(v): k for k, v in digits.items()}

def main():
  inputfilename = sys.argv[1]

  outputfilename = inputfilename.replace(".in",".out")
  inputfile = open(inputfilename, 'r')
  outputfile = open(outputfilename, 'w')
  
  lines = inputfile.readlines()
  nb_cases = int(lines[0])
  for i in range(1,nb_cases+1):
    outputfile.write("Case #"+str(i)+": ")
    print("Case #"+str(i)+": ")
    data = [[int(c)==1 for c in s] for s in lines[i].split()[1:]]
    result = solution(data)
    print(result)
    outputfile.write(result)
    outputfile.write("\n")

  inputfile.close()
  outputfile.close()

def solution(data):

  candidate_segments = Set([i for i,v in enumerate(reduce(lambda x1,x2:_or(x1,x2),data)) if not v])

  candidate_sequences = []
  for d in data:
    l = [] 
    for k,v in masks(candidate_segments).items():
      dd = str(_or(d,v))
      if dd in inv_digits:
        l += [((inv_digits[dd],k))]
    candidate_sequences += [l]
  
  sol_nextdigit = None
  sol_broken_segments = []

  for v0,k0 in candidate_sequences[0]:
    s = [(v0,k0)]
    for i in range(1,len(data)):
      for vi,ki in candidate_sequences[i]:
        if ((v0-vi+10)%10 == i%10):
          s += [(vi,ki)]
   
    # test if we have found a decreasing sequence
    if (len(s)==len(data)):
      broken_segments = set(chain(*[k for v,k in s]))
  
      # calculate the next digit with broken segments
      vn,_ = s[len(s)-1]
      nextdigit = list(digits[(vn-1+10)%10])
      for i in broken_segments:
        nextdigit[i] = False

      if (sol_nextdigit is None):
        # first solution found
        sol_nextdigit = nextdigit
        sol_broken_segments += [broken_segments]
      else:
        # check ambiguities
        if (sol_nextdigit != nextdigit):
          return "ERROR!"
        else:
          sol_broken_segments += [broken_segments]

  if not(sol_nextdigit is None):

    # test if there is an ambiguity for some superset of potential broken segments
    for s in sol_broken_segments:
      if (s != candidate_segments): 
        ss = candidate_segments.symmetric_difference(s)
        for i in ss:
          if sol_nextdigit[i]:
            return "ERROR!"
    
    return display(sol_nextdigit)

  return "ERROR!"

def _or(l1,l2) :
  return [l1[i] or l2[i] for i in range(len(l1))]

def masks(segments):
  masks = dict()
  for i in range(len(segments)+1):
    for s in combinations(segments,i):
      masks[s] = [i in s for i in range(7)]
  return masks

def display(d):
  return ''.join(['1' if b else '0' for b in d])

if __name__ == "__main__":
  main()
