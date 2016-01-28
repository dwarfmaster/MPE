#!/usr/bin/python3
# vim:set foldmethod=marker:

import matplotlib.pyplot as plt
import numpy as np

# {{{ Exercice 1
epsilon = 1e-5
N = 1000

def p_n(n, t):
    s = 1
    p = 1
    f = 1
    for i in range(1, n):
        s += p / f
        p *= t
        f *= i
    return s

def dp_n(n, t):
    return p_n(n-1, t)

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

# graph(2, 7, -3, 3)

def newton(f, d, x):
    xi = x
    n = 0
    while abs(f(xi)) > epsilon and n <= N:
        xi = xi - f(xi)/d(xi)
        n = n + 1
    if n > N:
        return 0
    else:
        return xi

def g(n):
    return (lambda t:p_n(n,t))
def dg(n):
    return (lambda t:dp_n(n,t))

def is_in(z, zs):
    for zi in zs:
        if abs(zi-z) < epsilon:
            return True
    return False

def zeros(f, d, r):
    zrs = []
    xs = np.linspace(-r, r, 50)
    ys = np.linspace(-r, r, 50)
    for x in range(len(xs)):
        for y in range(len(ys)):
            zi = newton(f,d,complex(xs[x], ys[y]))
            if zi != 0 and not is_in(zi, zrs):
                zrs.append(zi)
    return zrs

def draw_cpx(zrs):
    for i in range(len(zrs)):
        xs = [z.real for z in zrs[i]]
        ys = [z.imag for z in zrs[i]]
        plt.plot(xs, ys, 'o')
    plt.title("Zeros")
    plt.axis("equal")
    plt.show()

def calc_zrs(n1, n2):
    zrs = []
    for n in range(n1,n2+1):
        zrs.append(zeros(g(n), dg(n), 10))
        print(n, "computed")
    return zrs

#draw_cpx(calc_zrs(3, 7))

# }}}


