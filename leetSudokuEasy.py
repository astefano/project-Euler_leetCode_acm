class Solution:
    # @param mat, a 9x9 2D array
    # @return a boolean
    def isValidSudoku(self, mat0):
        mat = [[0 for j in range(10)] for i in range(10)]
        for i in range(9):
            for j in range(0, 9):
                if mat0[i][j] == '.':
                    mat[i][j] = 0
                else:
                    mat[i][j] = int(mat0[i][j])
        for i in range(9):
            print mat0[i]
        #check rows
        for i in range(9):
            digits = [0 for k in range(10)]
            for j in range(9):                
                if 0 < mat[i][j] and mat[i][j] < 10:
                    # check if unique
                    if digits[mat[i][j]] > 0:
                        return False
                    digits[mat[i][j]] += 1
        #check cols
        for j in range(9):
            digits = [0 for z in range(10)]
            for i in range(9):
                if 0 < mat[i][j] and mat[i][j] < 10:
                    # check if unique
                    if digits[mat[i][j]] > 0:
                        return False
                    digits[mat[i][j]] += 1        
                    
        #check subsquares
        for k in range(3):
            for d in range(3):
                digits = [0 for z in range(10)]
                for i in range(3):
                    i3 = 3*k+i
                    for j in range(3):
                        j3 = j+3*d
                        if 0 < mat[i3][j3] and mat[i3][j3] < 10:
                            # check if unique
                            if digits[mat[i3][j3]] > 0:
                                return False
                            digits[mat[i3][j3]] += 1
                        print mat[i3][j3], digits
                print "==="
        return True

s = Solution()
#m = [".87654321","2........","3........","4........","5........","6........","7........","8........","9........"]
#m = ["..4...63.",".........","5......9.","...56....","4.3.....1","...7.....","...5.....",".........","........."]
#m = ["7...4....","...865...",".1.2.....",".....9...","....5.5..",".........","......2..",".........","........."]
m = ["....5..1.",".4.3.....",".....3..1","8......2.","..2.7....",".15......",".....2...",".2.9.....","..4......"]
print s.isValidSudoku(m)
