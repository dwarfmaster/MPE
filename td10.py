#!/usr/bin/python3
# vim:set foldmethod=marker:
from pylab import *

#{{{ Exercice 1
def rot_pi(x):
    return complex(cos(x*pi), sin(x*pi))

def VKS(A,B,a):
    l = [A, 0, 0, 0, B]
    u = (B-A) / 3
    l[1] = A + u
    l[3] = B - u
    l[2] = l[1] + u*rot_pi(a)
    return l

def VKG(L, a):
    ret = []
    for i in range(len(L) - 1):
        tmp = VKS(L[i],L[i+1],a)
        for j in range(4):
            ret.append(tmp[j])
    ret.append(L[-1])
    return ret

def koch(n, L, a):
    for i in range(n):
        L = VKG(L,a)
    x = []
    y = []
    for i in range(len(L)):
        x.append(L[i].real)
        y.append(L[i].imag)
    plot(x,y, linewidth = 1, color="blue")

def VK(n,a):
    koch(n, [0,1],a)

def flocon(n,a):
    koch(n, [0, complex(0.5, sqrt(3)/2), 1, 0],a)


#flocon(4, 1/3)
#axis("equal")
#title("Koch")
#show()

N = 40
for i in range(N):
    clf()
    axis("equal")
    title("Koch")
    flocon(4, 1/6 + 2*i/(6*N))
    pause(0.05)

# }}}

#{{{ Exercice 3
def permaleapond(L):
    pond = []
    n = len(L)
    for i in range(n):
        pond.append(randint(1, n*n))
    for i in range(n):
        j = 0
        while pond[i] > pond[j] and j < i:
            j = j+1
        for k in range(i-1, j-1, -1):
            pond[k], pond[k+1] = pond[k+1], pond[k]
            L[k], L[k+1] = L[k+1], L[k]

def permaleadir(L):
    n = len(L)
    for i in range(n):
        j = randint(i,n-1)
        L[i], L[j] = L[j], L[i]

def permtrirapide(L):
    n = len(L)
    if n <= 1:
        return L
    elif n == 2:
        return [min(L[0],L[1]),max(L[0],L[1])]
    i = randint(0, n-1)
    m = []
    M = []
    p = []
    for j in range(n):
        if L[j] == L[i]:
            p.append(L[j])
        elif L[j] < L[i]:
            m.append(L[j])
        else:
            M.append(L[j])
    return permtrirapide(m) + p + permtrirapide(M)
#}}}

