
""" Flashes lights at a certain BPM
example usage:
  python bpm.py 128
"""

from raspilights import *
import sched
import time

colors = [RED, BLUE]
bpm = 100

def act():
    global colors
    s.enter(60/bpm, 0, act)
    set_all_pixels(colors[0])
    show(0)
    colors = colors[1:] + [colors[0]]

if __name__ == '__main__':
    if len(sys.argv) > 1 and int(sys.argv[1]):
        bpm = int(sys.argv[1])
    s = sched.scheduler(time.time, time.sleep)
    s.enter(60/bpm, 0, act)
    s.run()
