#!/usr/bin/python3
# vim:set foldmethod=marker:

import matplotlib.pyplot as plt
import numpy as np

# {{{ Exercice 1
def partition(l):
    lft = []
    rgh = []
    for i in range(1, len(l)):
        if l[i] < l[0]:
            lft.append(l[i])
        else:
            rgh.append(l[i])
    return (lft, [l[0]], rgh)

def tri_rapide(l):
    if len(l) <= 1:
        return l
    (lft, m, rgh) = partition(l)
    lft = tri_rapide(lft)
    rgh = tri_rapide(rgh)
    return lft + m + rgh

print(tri_rapide([5, 7, 3, 8, 6, 1, 9, 0, 3]))

# }}}

# {{{ Exercice 2
def p_n(n, t):
    s = 1
    p = 1
    f = 1
    for i in range(1, n):
        s += p / f
        p *= t
        f *= i
    return s

def compute(n, x):
    y = []
    for e in x:
        y.append(p_n(n, e))
    return (x,y)

def graph(i, j, a, b):
    x = np.linspace(a, b, 100)
    for k in range(i, j+1):
        dx,dy = compute(k, x)
        plt.plot(dx, dy, linewidth = 1)
    plt.title("Exp sums")
    plt.show()

# graph(2, 7, 0, 1)

# TODO

# }}}

# {{{ Exercice 3
def swap(l, i, j):
    t = l[i]
    l[i] = l[j]
    l[j] = t

def partition2(l, a, b):
    m = a
    M = b
    p = l[0]
    i = a + 1
    while i < M:
        if l[i] < p:
            swap(l,m,i)
            m = m + 1
            i = i + 1
        elif l[i] > p:
            swap(l,i,M-1)
            M = M - 1
        else:
            i = i + 1
    return (m,M)

def tri_rapide2(l, i, j):
    if j - i <= 1:
        return
    m,M = partition2(l, i, j)
    print(l, m, M)
    tri_rapide2(l, i, m)
    tri_rapide2(l, M, j)

l = [5, 3, 5, 7, 6, 1, 4, 78, 3, 8, 0]
print(l)
tri_rapide2(l, 0, len(l))
print(l)
            
# }}}

