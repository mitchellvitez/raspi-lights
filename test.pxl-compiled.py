from raspilights import *

def sparse():
    return Array("-----*", black(), red())

def effect(t):
    arr = sparse()
    for _ in range(t):
        arr = shifthue(arr, 12)
        arr = rotate(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])

def a():
    return Array("*--*--", blue(), yellow())

def b():
    return Array("-*-**-", lime(), green())

def c():
    return Array("*--*", b(), black())

def everyThirdWhite():
    return Array("--*", black(), white())

def everySeventhWhite():
    return Array("------*", black(), white())

def everyThirdWhiteEveryFifthRed():
    return Array("--*--", everyThirdWhite(), red())

def candyCane():
    return Array("--***-*", red(), white())

def chunksOfFiveRedGreen():
    return Array("*******-------", red(), green())

def diminishingBlue():
    return Array("*****-****--***---**----*-----**----***---****--", blue(), white())

def showOffTheBlue(t):
    arr = diminishingBlue()
    for _ in range(t):
        arr = rotate(arr, 1)
        arr = darken(arr, 8)
        for i in all_pixels():
            set_pixel(i, arr[i])

def redGreenAlternating(t):
    arr = chunksOfFiveRedGreen()
    for _ in range(t):
        arr = rotate(arr, 7)
        for i in all_pixels():
            set_pixel(i, arr[i])

def movieThing(t):
    arr = everyThirdWhite()
    for _ in range(t):
        arr = rotate(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])

def movieThing2(t):
    arr = everySeventhWhite()
    for _ in range(t):
        arr = rotate(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])

def fancy7(t):
    arr = c()
    for _ in range(t):
        arr = darken(arr, 5)
        arr = shifthue(arr, 6)
        arr = rotate(arr, 7)
        for i in all_pixels():
            set_pixel(i, arr[i])

def fancy2(t):
    arr = c()
    for _ in range(t):
        arr = darken(arr, 5)
        arr = shifthue(arr, 6)
        arr = rotate(arr, 2)
        for i in all_pixels():
            set_pixel(i, arr[i])

def fancy(t):
    arr = c()
    for _ in range(t):
        arr = darken(arr, 5)
        arr = shifthue(arr, 6)
        arr = rotate(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])

def rainbowCycle(t):
    arr = colors()
    for _ in range(t):
        arr = rotate(arr, 5)
        for i in all_pixels():
            set_pixel(i, arr[i])

def inversion(t):
    arr = colors()
    for _ in range(t):
        arr = invert(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])

def rainbowColors(t):
    arr = red()
    for _ in range(t):
        arr = shifthue(arr, 12)
        for i in all_pixels():
            set_pixel(i, arr[i])

def randomDarken(t):
    arr = randomcolors()
    for _ in range(t):
        arr = darken(arr, 12)
        for i in all_pixels():
            set_pixel(i, arr[i])

def brightnessDecrease(t):
    arr = white()
    for _ in range(t):
        arr = darken(arr, 15)
        for i in all_pixels():
            set_pixel(i, arr[i])

def brightnessIncrease(t):
    arr = black()
    for _ in range(t):
        arr = brighten(arr, 15)
        for i in all_pixels():
            set_pixel(i, arr[i])

if __name__ == '__main__':
    set_mode('software')

    procedures = [effect,showOffTheBlue,redGreenAlternating,movieThing,movieThing2,fancy7,fancy2,fancy,rainbowCycle,inversion,rainbowColors,randomDarken,brightnessDecrease,brightnessIncrease]

    while True:
        proc = random.choice(procedures)
        sh = random.choice([show, reversed_show])
        for t in range(20):
            proc(t)
            sh(0.2)
