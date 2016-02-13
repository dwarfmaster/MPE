#!/usr/bin/python3

from matplotlib.pyplot import *
from matplotlib.image import *
from numpy import *

A = array([[148,129,43]*4]*5,dtype=uint8)
# imshow(A)
print(A.shape)

drap_france = array([[[0,0,255]] * 10 + [[255,255,255]] * 10 + [[255,0,0]] * 10] * 20, dtype=uint8)
# imshow(drap_france, interpolation='nearest')

drap_jap_arr = []
r = 3 / 5 * 60
for y in range(120):
    drap_jap_arr.append([])
    for x in range(180):
        if sqrt((x - 90)*(x - 90) + (y - 60)*(y - 60)) < r:
            drap_jap_arr[y].append([255,0,0])
        else:
            drap_jap_arr[y].append([255,255,255])
# imshow(array(drap_jap_arr, dtype=uint8))

def extract(image, x1, x2, y1, y2):
    pic = []
    for y in range(y1,y2):
        pic.append([])
        for x in range(x1,x2):
            pic[-1].append(image[y][x])
    return pic

def quadrant(image, i, j):
    (h,w,_) = image.shape
    if i == 1: x = 0
    else:      x = int(w/2)
    if j == 1: y = 0
    else:      y = int(h/2)
    return extract(image, x, x + int(w/2), y, y + int(h/2))

picture = imread("picture.png")
# imshow(quadrant(picture, 2, 1))

def symetrie(image):
    (h,w,_) = image.shape
    pic = []
    for y in range(h):
        pic.append([])
        for x in range(w):
            pic[y].append(image[y][w-1-x])
    return pic
# imshow(symetrie(picture))

def rotation(image):
    (h,w,_) = image.shape
    pic = []
    for y in range(w):
        pic.append([])
        for x in range(h):
            pic[y].append(image[x][w-1-y])
    return pic
# imshow(rotation(picture))

def select_comp(c, k):
    if k == 0:
        return [0, c[1], c[2]]
    elif k == 1:
        return [c[0], 0, c[2]]
    else:
        return [c[0], c[1], 0]

def composante(image, k):
    (h,w,_) = image.shape
    pic = []
    for y in range(h):
        pic.append([])
        for x in range(w):
            pic[y].append(select_comp(image[y][x], k))
    return pic
# imshow(composante(picture, 2))

def primaire(image, k):
    k1 = divmod(k + 1, 3)[1]
    k2 = divmod(k + 2, 3)[1]
    return composante(array(composante(image, k1)), k2)
imshow(primaire(picture, 0))

axis('off')
show()
