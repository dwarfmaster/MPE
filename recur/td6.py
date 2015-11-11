#!/usr/bin/python3
# vim:set foldmethod=marker:

from random import *
from pylab import *

# {{{ Exercice 1
def cantor(x, y):
    if x == 0 and y == 0:
        return 0
    elif y == 0:
        return 1 + cantor(0,x-1)
    else:
        return 1 + cantor(x+1, y-1)

print(cantor(2,1))
print(cantor(10,10))

# }}}

# {{{ Exercice 2
# On considère une dont le n premiers termes sont déjà triés
# On insert le n+1-eme dedans

def suite(n):
    if n == 0:
        return []
    else:
        return [randint(0,99)] + suite(n-1)

def rec_dicho(l, x, i, j):
    if j - i == 0:
        return i
    elif j - i == 1:
        if l[j] <= x:
            return j
        else:
            return i

    m = int((i+j)/2)
    if l[m] < x:
        return rec_dicho(l, x, m+1, j)
    elif l[m] > x:
        return rec_dicho(l, x, i, m-1)
    else:
        return m

print(rec_dicho(list(range(100)), 50, 0, 99))

def _tri_insertion_dicho(l, n):
    if n <= 0:
        return
    elif n == 1:
        l[0],l [1] = min(l[0], l[1]), max(l[1], l[1])
        return

    _tri_insertion_dicho(l, n-1)
    t = l[n]
    i = rec_dicho(l, t, 0, n-1)
    for j in range(n-1, i-1, -1):
        l[j+1] = l[j]
    l[i] = t

def tri_insertion_dicho(l):
    _tri_insertion_dicho(l, len(l)-1)

l = suite(5)
print(l)
tri_insertion_dicho(l)
print(l)

# }}}

# {{{ Exercice 3
def prod_strassen(M,N):
    n = shape(M)[0]
    if n == 1:
        r = zeros(1,1)
        r[0,0] = M[0,0] * N[0,0]
        return r
    n2 = int(n/2)
    A = M[0:n2,0:n2]
    B = M[0:n2,n2:n]
    C = M[n2:n,0:n2]
    D = M[n2:n,n2:n]
    E = N[0:n2,0:n2]
    F = N[0:n2,n2:n]
    G = N[n2:n,0:n2]
    H = N[n2:n,n2:n]
    P1 = prod_strassen(A, F-H)
    P2 = prod_strassen(A+B, H)
    P3 = prod_strassen(C+D, E)
    P4 = prod_strassen(D, G-E)
    P5 = prod_strassen(A+D, E+H)
    P6 = prod_strassen(B-D, G+H)
    P7 = prod_strassen(A-C,E+F)
    C = zeros(n,n)
    I = P5+P4-P2+P6
    J = P1+P2
    K = P3+P4
    L = P1+P5-P3-P7
    for i in range(0,n2):
        for j in range(0,n2):
            C[i,j] = I[i,j]
            C[n2+i,j] = J[i,j]
            C[i,n2+j] = K[i,j]
            C[n2+i,n2+j] = L[i,j]
    return C

prod_strassen(matrix('1 2; 3 4'), matrix('5 6; 7 8'))

# }}}

