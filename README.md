# Raspberry Pi Lights

Mitchell Vitez, 2018

In which I create a custom programming language just to make lights turn pretty colors

## The Pixll Language

Pixll is a tiny infinite-array generation and transformation language used for creating one-dimensional lightshows. The name came from "raspberry PI X-mas Light Language".

Take a look at `test.pxl`. You can compile this file by running `./PixllCompiler test.pxl > test.pxl-compiled.py`

There are only three kinds of declarations in Pixll (as well as comments which are just lines beginning with `#`): arrays, procedures, and transformations.

### Arrays

These begin with the keyword `array` followed by the name of the array. Then there are two array names separated by a "stars and bars" pattern. This pattern provides a way to combine the arrays on either side. Read the pattern from left to right. Every time you see a bar, fill in the next element of the left-hand array. Every time you see a star, fill in the next element of the right-hand array. This pattern cycles infinitely. In this way, very complicated arrays can be built up from interactions betwen simpler ones.

```
# An array where every third pixel is white
array myArray
  black --* white

# An array with pattern B R B W R B B R W B R B W R...
array combinedArr
  myArray -*- red
```

There are primitive arrays for all the colors in `raspilights.py` (e.g. white, black, red, orange, etc.). There are also two special primitive arrays: `colors`, which has the color wheel in rainbow order, and `randomcolors`, which generates an infinite array of random color values.

### Procedures

Another kind of code block in Pixll denotes a procedure. These have a name, followed by an initial array with syntax `> ` in front, then a list of transformations that will be done to the array on each step. You can optionally denote how many steps will run and how long each step will be (in seconds) by adding `<steps>@<seconds_per_step>` after the procedure name. The default is 20 steps at 0.2 seconds per step.

```
# A procedure which starts with the array above and at each step shifts the hue by 12 colors, and rotates the array by 1 pixel
myProcedure
> myArray
  shifthue 12
  rotate 1

# A white array that gets darker each step. Happens in 10 steps at 1/10 second per step
fadeToBlack 10@0.1
> white
  darken 20
```

Each transformation takes a single integer argument. Some of the included library transformations are:
- `rotate` - rotates array by that number of pixels
- `darken` - takes an array and darkens each member of it
- `brighten` - takes an array and brightens each member of it
- `shifthue` - cycles through colors (runs through hues on color wheel)
- `invert` - flips every color to its opposite

### Transformations

Finally, you can write your own simple transformations directly in Pixll. These take a color (r, g, b) and an argument (n). Each one begins with the keyword `transform`, and the name of the transformation. Following this are three lines, one each for red, green, and blue. When your transformation runs, it will run the operations on each of these lines and create a new color from the passed-in r, g, b, and n values. Each value is then automatically clamped to the range [0, 255].

```
transform increaseRed
  r + n
  g
  b

transform swapRedBlue
  b
  g
  r
```

You can also write your own more complicated transformations in Python. You'll need to call `array.transform()` with a function that maps colors to colors. In Python, you can do this with a nested function that takes and returns an `(r, g, b)` tuple. Then the outer function can call `array.transform(innerFunction)`. Don't forget to import your custom transformations after you've written them.

```
def increaseRed(array, arg):
    def _increaseRed(c):
        r, g, b = c
        new_r = min(255, r + arg)
        return (new_r, g, b)
    array.transform(_increaseRed)
    return array
```

Pixll is intended to run continuously (you wouldn't want your lights suddenly shutting off!). At runtime, it takes all the user-defined procedures, and selects a random one. It also chooses randomly whether to display the LEDs in forward or reverse order (transformations will also be reversed). It then runs that selected procedure for a set time, then loops back and chooses another procedure to run.

You now know about all there is to know about Pixll! (It was lots of fun designing a tiny special-purpose language like this)

## The Pixll Compiler

The Pixll compiler is a small Haskell parser/printer that transpiles Pixll to Python. 

You can build the compiler with `ghc PixllCompiler.hs`. Compilation takes a filename as the first argument and prints the result to stdout.

The compiler essentially parses a `.pxl` file to a syntax tree, converts each relevant element of that tree to Python code, and generates a little extra Python code that acts to select random functions from the `.pxl` file to keep the lights running.

This is a very simple compiler. It doesn't even parse some of its expressions, just passes them along to Python in a more Pythonic form. As always, don't compile and run code you don't trust. In this case, it may contain arbitrary Python code!

## The raspilights Library

`raspilights.py` is a Python library designed to be used as either a standalone driver for driving LED strips in python (as in `light_patterns.py`), or as an included library in compiled Pixll files (as in `test.pxl-compiled.py`).

raspilights has both hardware and software modes, which can be set via the respective flags in `raspilights.py`. In hardware mode, the library sends GPIO output from a Raspberry Pi to drive a WS2812B LED strip. In software mode, the library prints arrays of colored rectangles to stdout, simulating what would happen if your code were hooked up to a hardware LED strip.

The number of pixels you have to play with is settable via `PIXEL_COUNT`. I've found that around 80 works well in software mode, and in hardware mode set this to the number of LEDs on the physical LED strip (in my case, 300).

The library supports the following functions, where `i` is an integer from 0 up to `PIXEL_COUNT`, and colors are handled internally as (red, green, blue) tuples of integers between 0 and 255.

- `get_color(i)` - the color of a pixel at index i
- `all_pixels()` - a range of indices from [0, `PIXEL_COUNT`)
- `clear()` - resets all pixels to (0, 0, 0)
- `all_colors()` - a list of 256 colors spanning the rainbow
- `random_color()` - a color with independently random r, g, and b elements
- `set_pixel(i, color)` - set the pixel at index i to the given color
- `set_all_pixels(color)` - sets every pixel to the given color
- `show(seconds)` - needs to be called to see pixels light up. waits for the given number of seconds before continuing
- `reversed_show(seconds)` - like show, but puts out pixels in reverse order

The library also supports many named color constants (`BLACK`, `WHITE`, `AUBERGINE`, etc.) which can be found by reading the source or looking at the default colors demo in `light_patterns.py`. Each color has a corresponding infinite Array. (The array names are lowercase, whereas the constants are uppercase)

In `raspilights.py`, an Array is an infinite generator that keeps the first `PIXEL_COUNT` elements around for easy access. This lets us generate infinite lists of pixels and transform them in various ways. These are then used by the Pixll language to easily create and manipulate infinite Arrays.
