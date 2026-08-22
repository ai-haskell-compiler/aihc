module Demo where

data Choice = First | Second

choose First = Second
choose Second = First

answer = choose First
