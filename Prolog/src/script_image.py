from random import randint

for i in range(784):
    if i < 500: print 0
    else: 
        print ((randint(0, 100000)%100000)/100000.0) 
