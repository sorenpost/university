#!/usr/bin/env python

from numpy.random import seed
from numpy.random import randint
import pandas as pd


# Define scores
ai_score = 0
user_score = 0

#
word_list = pd.read_csv("./english_words.csv")
word_val = randint(0, len(word_list), 1)
word_chosen = word_list[word_val]

# create word
print(len(word_chosen) * " _ ")

## Score keeper
def score_keeper(winner):
    if winner == "ai":
        ai_score += 1
    elif winner == "user":
        user_score += 1


print(ai_score)
print(user_score)
