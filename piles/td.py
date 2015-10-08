#!/usr/bin/python3

# {{{ Exercice 1
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

