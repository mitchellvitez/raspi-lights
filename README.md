# Raspberry Pi Lights

Mitchell Vitez, 2018

In which I create a custom programming language just to make lights turn pretty colors

## The Pixll Language

Pixll is a tiny infinite-array generation and transformation language used for creating one-dimensional lightshows. The name came from "raspberry PI X-mas Light Language".

Take a look at `test.pxl`. You can compile this file by running `./PixllCompiler test.pxl > test.pxl-compiled.py`

There are only two kinds of code blocks in Pixll (as well as comments which are just lines beginning with `#`). The first is an array declaration.

These begin with the keyword `array` followed by the name of the array. Then there are two array names separated by a "stars and bars" pattern. This pattern provides a way to combine the arrays on either side. Read the pattern from left to right. Every time you see a bar, fill in the next element of the left-hand array. Every time you see a star, fill in the next element of the right-hand array. This pattern cycles infinitely. In this way, very complicated arrays can be built up from interations betwen simpler ones.

```
# An array where every third pixel is white
array myArray
  black --* white
```

There are primitive arrays for all the colors in `raspilights.py` (e.g. white, black, red, orange, etc.). There are also two special primitive arrays: `colors`, which has the color wheel in rainbow order, and `randomcolors`, which generates an infinite array of random color values.

The second kind of code block in Pixll denotes a procedure. These have a name, followed by an initial array with syntax `> ` in front, then a list of transformations that can be done to the array

```
# A procedure which starts with the array above and at each step shifts the hue by 12 colors, and rotates the array by 1 pixel
myProcedure
> myArray
  shifthue 12
  rotate 1
```

Each transformation takes a single integer argument. The primitive transformations are:
- `rotate` - rotates array by that number of pixels
- `darken` - takes an array and darkens each member of it
- `brighten` - takes an array and brightens each member of it
- `shifthue` - cycles through colors (runs through hues on color wheel)
- `invert` - flips every color to its opposite

Pixll is intended to run continuously (you wouldn't want your lights suddenly shutting off!). At runtime, it takes all the user-defined procedures, and selects a random one. It also chooses random whether to display the LEDs in forward or reverse order (transformations will also be reversed). It then runs that selected procedure for a set time, then loops back and chooses another procedure to run.

You now know about all there is to know about Pixll! (It was lots of fun designing a tiny special-purpose language like this)

## The Pixll Compiler

The Pixll compiler is a small Haskell parser/printer that transpiles Pixll to Python. 

You can build the compiler with `ghc PixllCompiler.hs`. Compilation takes a filename as the first argument and prints the result to stdout.

This is a very simple compiler. You can see that it essentially parses a `.pxl` file to a syntax tree, converts each relevant element of that tree to Python code, and generates a little extra Python code that acts to select random functions from the `.pxl` file to keep the lights running.


## The raspilights Library

`raspilights.py` is a Python library designed to be used as either a standalone driver for driving LED strips in python (as in `light_patterns.py`), or as an included library in compiled Pixll files (e.g. `test.pxl-compiled.py`).

raspilights has both hardware and software modes, which can be set via the respective flags in `raspilights.py`. In hardware mode, the library sends GPIO output from a Raspberry Pi to drive a WS2812B LED strip. In software mode, the library prints arrays of colored rectangles to the terminal, simulating what would happen if your code were hooked up to a hardware LED strip.

The number of pixels you have to play with is settable via `PIXEL_COUNT`. I've found that around 60 works well in software mode, and in hardware mode set this to the number of LEDs on my physical LED strip (in my case, 300).

The library supports the following functions, where `i` is an integer from 0 up to `PIXEL_COUNT`, and colors are handled internally as (red, green, blue) tuples of integers between 0 and 255.

- `get_color(i)` - the color of a pixel at index idx
- `all_pixels()` - a range from [0, `PIXEL_COUNT`)
- `clear()` - resets all pixels 
- `all_colors()` - a list of 256 colors spanning the rainbow
- `random_color()` - a color with independently random r, g, and b elements
- `set_pixel(i, color)` - set the pixel at index i to the color
- `set_all_pixels(color)` - sets every pixel to that color
- `show(seconds)` - needs to be called to see pixels light up. waits for the given number of seconds before continuing
- `reversed_show(seconds)` - like show, puts out pixels in reverse order

The library also supports many named color constants (BLACK, WHITE, AUBERGINE, etc.) which can be found by reading the source or looking at the default colors demo in `light_patterns.py`. Each color has a corresponding infinite Array. (The arrays are lowercase, whereas the constants are uppercase)

In `raspilights.py`, an Array is an infinite generator that keeps the first `PIXEL_COUNT` elements around for easy access. This lets us generate infinite lists of pixels and transform them in various ways. These are used by the Pixll language.
