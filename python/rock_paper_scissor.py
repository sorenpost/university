#!/usr/bin/env python

keep_playing = input("Do you want to keep playing? [y]es, [n]o \n > ")

# elements:
# 1. who-won function
# 2. score keeping function
# 3. keep playing function
# 4. user input
# 5. DONE computer choosing

# computer choosing
# generate random integer values
from numpy.random import seed
from numpy.random import randint
# seed random number generator

# generate some integers
value = randint(0, 3, 1)

#


while keep_playing.lower() == "y" or keep_playing.lower() == "yes":
