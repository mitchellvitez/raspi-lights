# Checklist for running on hardware:
#   uncomment neopixel import
#   set PIXEL_COUNT to number of physical LEDs
#   set MODE to HARDWARE

import time
from itertools import tee
import Adafruit_GPIO.SPI as SPI
from xtermcolor import colorize
import sys
import random
import colorsys
# from neopixel import *

# these LED_ flags are from https://github.com/jgarff/rpi_ws281x
LED_PIN        = 18      # GPIO pin connected to the pixels (18 uses PWM!).
LED_FREQ_HZ    = 800000  # LED signal frequency in hertz (usually 800khz)
LED_DMA        = 10      # DMA channel to use for generating signal (try 10)
LED_BRIGHTNESS = 255     # Set to 0 for darkest and 255 for brightest
LED_INVERT     = False   # True to invert the signal (when using NPN transistor level shift)
LED_CHANNEL    = 0       # set to '1' for GPIOs 13, 19, 41, 45 or 53

PIXEL_COUNT = 80

HARDWARE = 1
SOFTWARE = 2

MODE = SOFTWARE

def set_mode(mode):
    if mode == 'hardware':
        import RPi.GPIO as GPIO
        MODE = HARDWARE
    elif mode == 'software':
        MODE = SOFTWARE
    else:
        raise Exception('Invalid mode passed to set_mode')

SPI_PORT = 0
SPI_DEVICE = 0

class Pixels:
    def __init__(self):
        self.pixels = [(0,0,0)] * PIXEL_COUNT

    def show(self):
        for pixel in self.pixels:
            h = '%02x%02x%02x' % pixel
            print(colorize('\u25ae', rgb=int(h, 16)), end='')
        sys.stdout.write('\n')

    def reverse(self):
        self.pixels = self.pixels[::-1]

    def set_pixel(self, i, color):
        self.pixels[i] = color

    def clear(self):
        self.pixels = [(0, 0, 0)] * PIXEL_COUNT

    def get_color(self, i):
        return self.pixels[i]

pixels = Pixels()
if MODE == HARDWARE:
    # For WS2801 lights
    # pixels = ADA.WS2801Pixels(PIXEL_COUNT, spi=SPI.SpiDev(SPI_PORT, SPI_DEVICE), gpio=GPIO)

    # For WS2812 lights
    pixels = Adafruit_NeoPixel(PIXEL_COUNT, LED_PIN, LED_FREQ_HZ, LED_DMA, LED_INVERT, LED_BRIGHTNESS, LED_CHANNEL)
    pixels.begin()

def get_color(i):
    if MODE == HARDWARE:
        # return pixels.get_pixel_rgb(i)
        return pixels.getPixelColor(i)
    return pixels.get_color(i)

def all_pixels():
    return range(PIXEL_COUNT)

def color(r, g, b):
    return (r, g, b)

def clear():
    pixels.clear()

def all_colors():
    return [_wheel(x) for x in range(256)]

def random_color():
    return (random.randint(0, 255), random.randint(0, 255), random.randint(0, 255))

def set_pixel(i, color):
    if MODE == HARDWARE:
        pixels.setPixelColor(i, Color(*color))
    else:
        pixels.set_pixel(i, color)

def set_all_pixels(color):
    for i in range(PIXEL_COUNT):
        set_pixel(i, color)

def reversed_show(seconds=0.1):
    if MODE == HARDWARE:
        p = [get_color(i) for i in range(PIXEL_COUNT)]
        for i, x in enumerate(p[::-1]):
            pixels.setPixelColor(i, x)
    else:
        pixels.reverse()
    show(seconds)

def show(seconds=0.1):
    pixels.show()
    time.sleep(seconds)

def _wheel(pos):
    if pos < 85:
        return color(pos * 3, 255 - pos * 3, 0)
    elif pos < 170:
        pos -= 85
        return color(255 - pos * 3, 0, pos * 3)
    else:
        pos -= 170
        return color(0, pos * 3,  255 - pos * 3)

# Color constants
BLACK=color(0,0,0)
RED=color(255,0,0)
GREEN=color(0,255,0)
BLUE=color(0,0,255)
YELLOW=color(255,255,0)
CYAN=color(0,255,255)
PINK=MAGENTA=color(255,0,255)
WHITE=color(255,255,255)

DARK_RED=color(127,0,0)
DARK_GREEN=color(0,127,0)
DARK_BLUE=color(0,0,127)
DARK_YELLOW=color(127,127,0)
DARK_CYAN=color(0,127,127)
DARK_PINK=color(127,0,127)
GRAY=GREY=color(127,127,127)

LIGHT_RED=color(255,127,127)
LIGHT_GREEN=color(127,255,127)
LIGHT_BLUE=color(127,127,255)
LIGHT_YELLOW=color(255,255,127)
LIGHT_PINK=LIGHT_MAGENTA=color(255,127,255)
LIGHT_CYAN=color(127,255,255)

ORANGE=color(255,127,0)
PURPLE=color(64, 0, 255)
SALMON=color(255,0,127)
AQUAMARINE=color(0,255,127)
LIME=color(127,255,0)
AUBERGINE=color(127,0,255)
CERULEAN=color(0,127,255)

def clamp(x):
    if x > 255:
        return 255
    elif x < 0:
        return 0
    else:
        return x

# an Array is an infinite generator that keeps the first PIXEL_COUNT elements around for easy access
class Array:
    def __init__(self, pattern, arr1, arr2):
        self.next_gen = next_gen(pattern, arr1, arr2)
        self.elements = []
        for i in range(PIXEL_COUNT):
            self.elements.append(next(self.next_gen))

    def __getitem__(self, i):
        return self.elements[i]

    def __iter__(self):
        return self

    def __next__(self):
        first = self.elements[0]
        self.elements = self.elements[1:]
        self.elements.append(next(self.next_gen))
        return first

    def transform(self, f):
        self.elements = [f(elt) for elt in self.elements]

def next_gen(pattern, arr1, arr2):
    while True:
        for char in pattern:
            if char == '|' or char == '-':
                yield next(arr1)
            elif char == '*':
                yield next(arr2)

# Transforms for Arrays
def rotate(gen, n):
    for i in range(n):
        next(gen)
    return gen

def invert(gen, n):
    def _invert(c):
        r, g, b = c
        return color(255 - r, 255 - g, 255 - b)
    gen.transform(_invert)
    return gen

def darken(gen, n):
    def _darken(c):
        r, g, b = c
        r = max(0, r - n)
        g = max(0, g - n)
        b = max(0, b - n)
        return color(r, g, b)
    gen.transform(_darken)
    return gen

def brighten(gen, n):
    def _brighten(c):
        r, g, b = c
        r = min(255, r + n)
        g = min(255, g + n)
        b = min(255, b + n)
        return color(r, g, b)
    gen.transform(_brighten)
    return gen

def shifthue(gen, n):
    def _shifthue(c):
        h, s, v = colorsys.rgb_to_hsv(*c)
        h += n / 256
        r, g, b = colorsys.hsv_to_rgb(h, s, v)
        return color(int(r), int(g), int(b))
    gen.transform(_shifthue)
    return gen

def randhue(gen, n):
    def _randhue(c):
        return random_color()
    gen.transform(_randhue)
    return gen

# an Array containing the rainbow
def colors():
    return Array("*", _colors(), _colors())

def _colors():
    pos = 0
    while True:
        yield _wheel(pos)
        pos += 1
        pos = pos % 255

def randomcolors():
    return Array("*", _randomcolors(), _randomcolors())

def randomcolor():
    def _randomcolor():
        c = random.choice(COLORS)
        while True:
            yield c
    return Array("*", _randomcolor(), _randomcolor())


def _randomcolors():
    while True:
        yield random_color()

COLORS = [BLACK, RED, GREEN, BLUE, YELLOW, CYAN, PINK, WHITE, GRAY, PURPLE,
            DARK_RED, DARK_GREEN, DARK_BLUE, DARK_YELLOW, DARK_CYAN,
            LIGHT_RED, LIGHT_GREEN, LIGHT_BLUE, LIGHT_YELLOW, LIGHT_PINK, LIGHT_CYAN,
            ORANGE, SALMON, AQUAMARINE, LIME, AUBERGINE, CERULEAN]
#     print(f'def _{color.lower()}():\n    while True: yield {color}\ndef {color.lower()}():\n    return Array("*", _{color.lower()}(), _{color.lower()}())')

# Arrays for each default color
def _black():
    while True: yield BLACK
def black():
    return Array("*", _black(), _black())
def _red():
    while True: yield RED
def red():
    return Array("*", _red(), _red())
def _green():
    while True: yield GREEN
def green():
    return Array("*", _green(), _green())
def _blue():
    while True: yield BLUE
def blue():
    return Array("*", _blue(), _blue())
def _yellow():
    while True: yield YELLOW
def yellow():
    return Array("*", _yellow(), _yellow())
def _cyan():
    while True: yield CYAN
def cyan():
    return Array("*", _cyan(), _cyan())
def _pink():
    while True: yield PINK
def pink():
    return Array("*", _pink(), _pink())
def _white():
    while True: yield WHITE
def white():
    return Array("*", _white(), _white())
def _gray():
    while True: yield GRAY
def gray():
    return Array("*", _gray(), _gray())
def _purple():
    while True: yield PURPLE
def purple():
    return Array("*", _purple(), _purple())
def _dark_red():
    while True: yield DARK_RED
def dark_red():
    return Array("*", _dark_red(), _dark_red())
def _dark_green():
    while True: yield DARK_GREEN
def dark_green():
    return Array("*", _dark_green(), _dark_green())
def _dark_blue():
    while True: yield DARK_BLUE
def dark_blue():
    return Array("*", _dark_blue(), _dark_blue())
def _dark_yellow():
    while True: yield DARK_YELLOW
def dark_yellow():
    return Array("*", _dark_yellow(), _dark_yellow())
def _dark_cyan():
    while True: yield DARK_CYAN
def dark_cyan():
    return Array("*", _dark_cyan(), _dark_cyan())
def _light_red():
    while True: yield LIGHT_RED
def light_red():
    return Array("*", _light_red(), _light_red())
def _light_green():
    while True: yield LIGHT_GREEN
def light_green():
    return Array("*", _light_green(), _light_green())
def _light_blue():
    while True: yield LIGHT_BLUE
def light_blue():
    return Array("*", _light_blue(), _light_blue())
def _light_yellow():
    while True: yield LIGHT_YELLOW
def light_yellow():
    return Array("*", _light_yellow(), _light_yellow())
def _light_pink():
    while True: yield LIGHT_PINK
def light_pink():
    return Array("*", _light_pink(), _light_pink())
def _light_cyan():
    while True: yield LIGHT_CYAN
def light_cyan():
    return Array("*", _light_cyan(), _light_cyan())
def _orange():
    while True: yield ORANGE
def orange():
    return Array("*", _orange(), _orange())
def _salmon():
    while True: yield SALMON
def salmon():
    return Array("*", _salmon(), _salmon())
def _aquamarine():
    while True: yield AQUAMARINE
def aquamarine():
    return Array("*", _aquamarine(), _aquamarine())
def _lime():
    while True: yield LIME
def lime():
    return Array("*", _lime(), _lime())
def _aubergine():
    while True: yield AUBERGINE
def aubergine():
    return Array("*", _aubergine(), _aubergine())
def _cerulean():
    while True: yield CERULEAN
def cerulean():
    return Array("*", _cerulean(), _cerulean())
