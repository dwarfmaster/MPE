#!/usr/bin/python3
# vim:set foldmethod=marker:

from pylab import *

# {{{ Exercice 1
def fusion(l1, l2):
    if len(l1) == 0:
        return l2
    elif len(l2) == 0:
        return l1

    if l1[0] < l2[0]:
        return [l1[0]] + fusion(l1[1:],l2)
    else:
        return [l2[0]] + fusion(l1,l2[1:])

def tri_fusion(l):
    n = len(l)
    if n <= 1:
        return l
    m = int(n/2)
    return fusion(tri_fusion(l[:m]), tri_fusion(l[m:]))

print(tri_fusion([4,1, 7, 3, 9, 0, 2, 7, 4, 1]))
print(tri_fusion(list(randint(0,100,15))))

# }}}

# {{{ Exercice 2
def _francis(A,n):
    if n == 0:
        return A
    (q,r) = qr(A)
    return _francis(dot(r,q), n-1)

def francis(A,n):
    m = _francis(A,n)
    a = []
    for i in range(len(m)):
        a.append(m[i,i])
    return a

print(_francis([[1,2,3],[3,4,5],[4,5,6]], 5))
print(francis([[1,2,3],[3,4,5],[4,5,6]], 5))

def mat_alea_123():
    m = randint(5,size=(3,3))
    while det(m) == 0:
        m = randint(5,size=(3,3))
    m2 = diag([1,3,2])
    return dot(dot(m, m2), inv(m))

print(mat_alea_123())
print(francis(mat_alea_123(), 10))

# TODO gram_qr

# }}}

