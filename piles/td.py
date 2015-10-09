#!/usr/bin/python3
# vim:set foldmethod=marker:

import random as rd

# {{{ Piles
def pile_new():
    return []

def pile_empty(l):
    return len(l) == 0

def pile_push(l, e):
    l.append(e)

def pile_pop(l):
    if pile_empty(l):
        return None
    return l.pop()

def retourner(l):
    l2 = pile_new()
    while not pile_empty(l):
        pile_push(l2, pile_pop(l))
    return l2

def permcirc(l, n):
    hd = pile_new()
    tl = pile_new()
    c = 0
    while not pile_empty(l):
        if c < n:
            pile_push(hd, pile_pop(l))
        else:
            pile_push(tl, pile_pop(l))
        c += 1
    while not pile_empty(hd):
        pile_push(l, pile_pop(hd))
    while not pile_empty(tl):
        pile_push(l, pile_pop(tl))

# }}}

# {{{ TD4
# {{{ Exercice 2
def cre_id(n):
    m = []
    for i in range(n):
        m += [[]]
        for j in range(n):
            m[i] += [0]
        m[i][i] = 1
    return m

def cre_nil(n):
    m = []
    for i in range(n):
        m += [[]]
        for j in range(n):
            m[i] += [0]
    return m

def sum_mat(A1, lb, A2):
    I = cre_id(len(A1))
    for i in range(len(A1)):
        for j in range(len(A1)):
            I[i][j] = A1[i][j] + lb * A2[i][j]
    return I

def prod_mat(A1, A2):
    n = len(A1)
    P = cre_id(n)
    for i in range(n):
        for j in range(n):
            P[i][j] = 0
            for k in range(n):
                P[i][j] += A1[i][k] * A2[k][j]
    return P

def trace_mat(A):
    tr = 0
    for i in range(len(A)):
        tr += A[i][i]
    return tr

def make_vect(s, v):
    ret = []
    for i in range(s):
        ret += [v]
    return ret

def Faddeev(A):
    n = len(A)
    I = cre_id(n)
    Bs = make_vect(n, I)
    a = make_vect(n, 0)
    M = A
    a[n-1] = -1 * trace_mat(M)
    for i in range(n-2, -1, -1):
        Bs[i] = sum_mat(M, a[i+1], I)
        M = prod_mat(Bs[i], A)
        a[i] = trace_mat(M) / (i-n)
    if a[0] == 0:
        return ("matrice non inversible", a)
    else:
        return (sum_mat(cre_nil(n), -1/a[0], Bs[0]), a)

def stringify(f):
    s = " "
    if f > 0:
        s += "+ "
    else:
        s += "- "
    f = abs(f)
    if int(f) ==f:
        f = int(f)
    s += "{} ".format(f)
    return s

def poly(a):
    n = len(a)
    pl = "X^({})".format(n)
    for i in range(n-1, 1, -1):
        if a[i] == 0:
            continue
        pl += stringify(a[i])
        pl += "* X^({})".format(i)
    pl += stringify(a[1]) + "* X" + stringify(a[0])
    return pl

def test(A):
    (inv, a) = Faddeev(A)
    p = poly(a)
    return (inv, p)
print(test([[1, 2, 3, 4], [3, 4, 5, 6], [4, 5, 6, 7], [5, 6, 7, 7]]))
# }}}

# {{{ Exercice 3
def PostFixe(expr):
    p = pile_new()
    for i in range(len(expr)):
        if expr[i] == "x":
            pile_push(p, pile_pop(p)*pile_pop(p))
        elif expr[i] == "+":
            pile_push(p, pile_pop(p)+pile_pop(p))
        elif expr[i] == "/":
            v = pile_pop(p)
            pile_push(p, pile_pop(p)/v)
        elif expr[i] == "-":
            v = pile_pop(p)
            pile_push(p, pile_pop(p)-v)
        else:
            pile_push(p, expr[i])
    return pile_pop(p)
# }}}
# }}}

# {{{ TD5
# {{{ Exercice 1
def remplacer(pile, anc, nouv):
    p = pile_new()
    while not pile_empty(pile):
        e = pile_pop(pile)
        if e == anc:
            pile_push(p, nouv)
        else:
            pile_push(p, e)
    return retourner(p)

# }}}

# {{{ Exercice 2
def isop(c):
    return c == "+" or c == "-" or c == "*"
def priop(c):
    if c == "+":
        return 2
    elif c == "-":
        return 1
    else:
        return 0

def postfix(expr):
    p = pile_new()
    pile_push(p, "(")
    expr = expr.split(" ") + [")"]
    s = ""
    i = 0
    while not pile_empty(p):
        c = expr[i]
        if c == "(":
            pile_push(p, "(")
        elif isop(c):
            op = pile_pop(p)
            while isop(p) and priop(p) >= priop(c):
                s += " {}".format(op)
                op = pile_pop(p)
            pile_push(p, op)
            pile_push(p, c)
        elif c == ")":
            op = pile_pop(p)
            while op != "(":
                s += " {}".format(op)
                op = pile_pop(p)
        else:
            s += " {}".format(c)
        i += 1
    return s

print(postfix("( 2 - ( 3 * 4 ) ) * 5"))
print(postfix("( 1 + 2 ) * 3 + 4"))
print(postfix("32 - 2 * ( 12 - ( 7 - 2 ) * 3 )"))
# }}}

# {{{ Exercice 3
def evl(expr):
    pf = expr.split(" ")
    p = pile_new()
    for i in range(len(pf)):
        if pf[i] == "":
            continue
        if isop(pf[i]):
            n1 = pile_pop(p)
            n2 = pile_pop(p)
            if pf[i] == "+":
                pile_push(p, n1 + n2)
            elif pf[i] == "*":
                pile_push(p, n1 * n2)
            elif pf[i] == "-":
                pile_push(p, n2 - n1)
        else:
            pile_push(p, int(pf[i]))
    return pile_pop(p)
print(evl("30 5 2 3 + * -"))
print(evl(postfix("( 3 + 4 ) * ( 3 - 1 )")))
print(evl(postfix("32 - 2 * ( 12 - ( 7 - 2 ) * 3 )")))
# }}}

# {{{ Exercice 4
def _permalea(l, n):
    if n == 0:
        return l
    i = rd.randint(0, n-1)
    t = l[i]
    l[i] = l[n]
    l[n] = t
    return _permalea(l, n-1)
def permalea(l):
    return _permalea(l, len(l) - 1)
print(permalea(list(range(10))))

def sortinsert(l):
    for i in range(1, len(l)):
        t = l[i]
        j = i-1
        while j >= 0 and l[j] > t:
            l[j+1] = l[j]
            j = j-1
        j = max(0, j+1)
        l[j] = t

l = permalea(list(range(20)))
print(l)
sortinsert(l)
print(l)

# Complexité dans le pire des cas : O(n^2)
# Complexité meilleurs des cas :    O(n)
# }}}

# {{{ Exercice 5
def pgcd(a, b):
    if b == 0:
        return a
    else:
        d = divmod(a, b)
        return pgcd(b, d[1])

def facteur(n):
    d = 1
    while d == 1:
        a = rd.randint(0, n-1)
        b = rd.randint(0, n-1)
        d = pgcd(abs(a-b), n)
        if d != 1 and d != n:
            return d
    return n
# print(facteur(15770708441))

def f(x, n):
    return divmod(x*x+1, n)[1]

def rhoPollard(n):
    u = 1
    v = 2
    d = 1
    while d == 1:
        u = f(u, n)
        v = f(f(v, n), n)
        d = pgcd(abs(u-v), n)
    return d
print(rhoPollard(15770708441))
print(rhoPollard((2**(2**5)) + 1))
print(rhoPollard((2**(2**6)) + 1))
print(rhoPollard((2**(2**10)) + 1))

# }}}

# }}}


