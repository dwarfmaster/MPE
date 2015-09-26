#!/usr/bin/python3

# {{{ Exercice 1: inv(a,b)
def inv(a,b):
    u1 = 1
    v1 = 0
    u2 = 0
    v2 = 1
    while a*u2+b*v2 != 0:
        q = divmod(u1*a+v1*b, u2*a+v2*b)[0]
        u3 = u1 - q*u2
        v3 = v1 - q*v2
        u1 = u2
        v1 = v2
        u2 = u3
        v2 = v3
    if u1 < 0:
        return b+u1
    else:
        return u1
print("15*", inv(15,19), "=", divmod(15*14, 19)[1],"[19]")
# }}}

# {{{ Exercice 2
# {{{ 1/ clefRSA(p,q)
def pgcd(a, b):
    if b == 0:
        return a
    else:
        return pgcd(b, divmod(a,b)[1])

def clefRSA(p,q):
    n = p*q
    phi = (p-1)*(q-1)
    d = 2
    while pgcd(d, phi) != 1:
        d += 1
    e = inv(d, phi)
    return [[n,e], [n,d]]

cle = clefRSA(89, 97)
print(cle)
# }}}

# {{{ 2/ crypt
# TODO
# }}}
# }}}

