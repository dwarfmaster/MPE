#!/usr/bin/python3
# vim:set foldmethod=marker:

# {{{ Utilities
def f(x):
    return divmod(2*x + 1, 10)[1]
f0 = []
for i in range(10):
    f0 += [f(i)]
print(f0)

def make_vect(l, v):
    t = []
    for i in range(l):
        t.append(v)
    return t
# }}}

# {{{ Partie 1
# {{{ 1/ admet_point_fixe
def admet_point_fixe(a):
    for i in range(len(a)):
        if a[i] == i:
            return True
    return False

print(admet_point_fixe(f0))
# }}}

# {{{ 2/ nb_points_fixes
def nb_points_fixes(a):
    nb = 0
    for i in range(len(a)):
        if a[i] == i:
            nb += 1
    return nb

print(nb_points_fixes(f0))
# }}}

# {{{ 3/ itere
def itere(t, x, k):
    y = x
    for i in range(k):
        y = t[y]
    return y

print(itere(f0, 9, 10))
print(itere([1,2,0], 1, 2))
# }}}

# {{{ 4/ nb_points_fixes_iteres
def nb_points_fixes_iteres(t, k):
    n = 0
    for i in range(len(t)):
        if i == itere(t, i, k):
            n += 1
    return n

print(nb_points_fixes_iteres(f0, 2))
print(nb_points_fixes_iteres(f0, 0))
# }}}

# {{{ 5/ admet_attracteur_principal
def admet_attracteur_principal(t):
    for i in range(len(t)):
        if t[i] != i:
            continue
        valid = True
        for j in range(len(t)):
            vs = []
            k = j
            while not k in vs:
                if k == i:
                    break
                vs.append(k)
                k = t[k]
            if k in vs:
                valid = False
                break
        if valid:
            return True
    return False

def admet_attracteur_principal2(t):
    n = len(t)
    z = itere(t, 0, n)
    if t[z] != z:
        return False
    i = 1
    while i < n and itere(t, i, n) == z:
        i += 1
    return i == n

print(admet_attracteur_principal(f0))
print(admet_attracteur_principal([5, 5, 2, 2, 0, 2, 2]))
print(admet_attracteur_principal2(f0))
print(admet_attracteur_principal2([5, 5, 2, 2, 0, 2, 2]))
# }}}

# {{{ 6/ temps_de_convergence
def temps_de_convergence(t, x):
    if t[x] == x:
        return 0
    return temps_de_convergence(t, t[x]) + 1

print(temps_de_convergence([5, 5, 2, 2, 0, 2, 2], 1))
# }}}

# {{{ 7/ temps_de_convergence_max
def temps_de_convergence_max(t):
    values = make_vect(len(t), -1)
    m = 0
    for i in range(len(t)):
        if values[i] != -1:
            continue
        j = i
        first = 0
        n = 0
        while t[j] != j:
            if values[j] != -1:
                first = values[j]
                break
            else:
                n += 1
            j = t[j]
        v = n + first
        if v > m:
            m = v
        j = i
        while values[j] == -1:
            values[j] = n + first
            n -= 1
    return m

print(temps_de_convergence_max([5, 5, 2, 2, 0, 2, 2, 0, 0, 0, 0, 0, 10, 0, 0]))
# }}}
# }}}

# {{{ Partie 2
# {{{ 8/ est_croissante
def est_croissante(t):
    for i in range(len(t)-1):
        if t[i] > t[i+1]:
            return False
    return True
# }}}

# {{{ 9/ point_fixe
def point_fixe(t):
    m = 0
    M = len(t)
    while M - m > 1:
        i = int((M+m)/2)
        if t[i] == i:
            return i
        elif t[i] > i:
            m = i + 1
        else:
            M = i

print(point_fixe([1, 3, 3, 5, 5, 5]))
# }}}

# {{{ 10/ Démonstration
# Preuve terminaison
# Considérons l'entier (M-m)
# On pose i = E((M+m)/2)
# Si t[i] == i:
#     ca termine.
# Si t[i] > i:
#     M'=M et m'=i+1, on a i >= m => m'>m
#     Donc M'-m' < M-m
#     De plus, i<M => m'<=M => M'-m' >= 0
# Si t[i] < i:
#     M'=i et m'=m, on a i<M => M'<M
#     Donc M'-m' < M-m
#     De plus, i>=m => M'>=m' => M'-m' >= 0
# Donc l'algorithme termine
# }}}

# {{{ 11/
# m et x deux pts fixes
# m <= x => f^k(m) <= f^k(x)
# }}}

fn = [0, 2, 4, 6, 4, 8, 0, 2, 0, 6]
# {{{ 12/ 
# Posons x = pgcd(xi)
# Vi, f^k(1) | xi => f^k(1) | x
# f^k(i) \in {xi} => x | f^k(1)
# Donc f^k(1) = x
# }}}

# {{{ 13/ pgcd_points_fixes
def pgcd_points_fixes(t):
    x = 1
    while t[x] != x:
        x = t[x]
    return x

print(pgcd_points_fixes(fn))
# }}}

# {{{ 14/ Preuve temps logarithmique
# f^k(1) != f^(k+1)(1) => [ f^k(1) | f^(k+1)(1) ] f^(k+1)(1) = 0 ou f^(k+1)(1) >= 2f^k(1)
# La suite 1,f(1),...,f^(k+1)(1) est une suite croissante de termes distincts non nuls. D'où n-1 >= f^k(1) >= 2^k => k <= log2(n-1)
# }}}

# }}}
