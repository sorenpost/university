#!/usr/bin/env python

# This script makes a guessing game.


# generate random integer values
from numpy.random import seed
from numpy.random import randint
# seed random number generator
seed(1)

# generate some integers
value = randint(0, 20, 1)
guess_int = None

while guess_int != value:

    guess_str = input("Take a guess: \n > ")

    try:
        guess_int = int(guess_str)
        print("Okay, your lucky number is: ", guess_int)
    except ValueError:
        print("This is not a valid number. It isn't a number at all! This is a string, go and try again. Better luck next time!")

    if guess_int > value:
        print("I'm afraid you number is a bit too high.")
    elif guess_int < value:
        print("I'm afraid that number is a bit too low.")
    elif guess_int == value:
        print("You fucking got it! Well done.")
        break
