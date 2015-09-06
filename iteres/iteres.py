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

