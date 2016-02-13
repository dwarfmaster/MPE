#!/usr/bin/python3
# vim:set foldmethod=marker:

from numpy import *
from random import *

# {{{ Exercice 1
def hamming(l):
    mat = [[1, 0, 0, 0],
           [0, 1, 0, 0],
           [0, 0, 1, 0],
           [0, 0, 0, 1],
           [1, 1, 1, 0],
           [1, 1, 0, 1],
           [1, 0, 1, 1]]
    l2 = dot(mat, l)
    ret = []
    for a in l2:
        ret.append(divmod(a,2)[1])
    return ret
print(hamming([0,0,1,1]))

def pertalea(l):
    i = randint(0,6)
    l[i] = randint(0,1)

def decode_hamming(l):
    b1 = divmod(l[0] + l[1] + l[2] - l[4], 2)[1] != 0
    b2 = divmod(l[0] + l[1] + l[3] - l[5], 2)[1] != 0
    b3 = divmod(l[0] + l[2] + l[3] - l[6], 2)[1] != 0
    if b1 and b2 and b3:
        l[0] = 1 - l[0]
    elif b1 and b2 and not b3:
        l[1] = 1 - l[1]
    elif b1 and not b2 and b3:
        l[2] = 1 - l[2]
    elif not b1 and b2 and b3:
        l[3] = 1 - l[3]
    elif b1 and not b2 and not b3:
        l[4] = 1 - l[4]
    elif not b1 and b2 and not b3:
        l[5] = 1 - l[5]
    elif not b1 and not b2 and b3:
        l[6] = 1 - l[6]

def test(l):
    msg = hamming(l)
    print(msg)
    pertalea(msg)
    print(msg)
    decode_hamming(msg)
    print(msg)
test([0,0,1,0])
test([1,1,0,0])
# }}}

# {{{ Exercice 2
def mult(a, P):
    R = []
    for i in range(len(P)):
        R.append(P[i] * a)
    return R

def add(P, Q):
    S = []
    for i in range(max(len(P),len(Q))):
        S.append(0)
        if i < len(P):
            S[i] = S[i] ^ P[i]
        if i < len(Q):
            S[i] = S[i] ^ Q[i]
    return S
print(add([5,6,7], [3,4,5,6]))

def dec(k, P):
    R = []
    for i in range(k+len(P)):
        if i < k:
            R.append(0)
        else:
            R.append(P[i-k])
    return R

def norm(P):
    R = []
    Q = []
    for i in range(len(P)):
        Q.append(P[i])
        if P[i] != 0:
            R += Q
            Q = []
    return R
print(norm([1, 4, 6, 0, 0]))

def div(A,G):
    P = norm(A)
    while len(P) >= len(G):
        P = norm(add(P, dec(len(P) - len(G), G)))
    return P
print(div([1,0,1,1,1], [1,0,1]))

def codepoly(l, G):
    r = 5
    R = div(dec(r, l), G)
    return add(dec(r,l), R)
# }}}

