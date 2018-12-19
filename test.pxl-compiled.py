from raspilights import *

def sparse():
    return Array("-----*", black(), red())

def effect(sh):
    arr = sparse()
    for _ in range(20):
        arr = shifthue(arr, 12)
        arr = rotate(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

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

def showOffTheBlue(sh):
    arr = diminishingBlue()
    for _ in range(20):
        arr = rotate(arr, 1)
        arr = darken(arr, 8)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def redGreenAlternating(sh):
    arr = chunksOfFiveRedGreen()
    for _ in range(10):
        arr = rotate(arr, 7)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.5)

def movieThing(sh):
    arr = everyThirdWhite()
    for _ in range(20):
        arr = rotate(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def movieThing2(sh):
    arr = everySeventhWhite()
    for _ in range(20):
        arr = rotate(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def fancy7(sh):
    arr = c()
    for _ in range(20):
        arr = darken(arr, 5)
        arr = shifthue(arr, 6)
        arr = rotate(arr, 7)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def fancy2(sh):
    arr = c()
    for _ in range(20):
        arr = darken(arr, 5)
        arr = shifthue(arr, 6)
        arr = rotate(arr, 2)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def fancy(sh):
    arr = c()
    for _ in range(20):
        arr = darken(arr, 5)
        arr = shifthue(arr, 6)
        arr = rotate(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def rainbowCycle(sh):
    arr = colors()
    for _ in range(100):
        arr = rotate(arr, 5)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.1)

def inversion(sh):
    arr = colors()
    for _ in range(3):
        arr = invert(arr, 1)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(2.0)

def rainbowColors(sh):
    arr = red()
    for _ in range(20):
        arr = shifthue(arr, 12)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def randomDarken(sh):
    arr = randomcolors()
    for _ in range(20):
        arr = darken(arr, 12)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def brightnessDecrease(sh):
    arr = white()
    for _ in range(20):
        arr = darken(arr, 15)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def brightnessIncrease(sh):
    arr = black()
    for _ in range(20):
        arr = brighten(arr, 15)
        for i in all_pixels():
            set_pixel(i, arr[i])
        sh(0.2)

def increaseRed(gen, n):
    def _increaseRed(c):
        r, g, b = c
        r = r+n
        g = g
        b = b
        return (clamp(r), clamp(g), clamp(b))
    gen.transform(_increaseRed)
    return gen

def brighten(gen, n):
    def _brighten(c):
        r, g, b = c
        r = r+n
        g = g+n
        b = b+n
        return (clamp(r), clamp(g), clamp(b))
    gen.transform(_brighten)
    return gen

def darken(gen, n):
    def _darken(c):
        r, g, b = c
        r = r-n
        g = g-n
        b = b-n
        return (clamp(r), clamp(g), clamp(b))
    gen.transform(_darken)
    return gen

def invert(gen, n):
    def _invert(c):
        r, g, b = c
        r = 255-r
        g = 255-g
        b = 255-b
        return (clamp(r), clamp(g), clamp(b))
    gen.transform(_invert)
    return gen

if __name__ == '__main__':
    set_mode('software')

    procedures = [effect,showOffTheBlue,redGreenAlternating,movieThing,movieThing2,fancy7,fancy2,fancy,rainbowCycle,inversion,rainbowColors,randomDarken,brightnessDecrease,brightnessIncrease]

    while True:
        proc = random.choice(procedures)
        sh = random.choice([show, reversed_show])
        proc(sh)
