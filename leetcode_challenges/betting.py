N = 2147483647
K = 1000

ans = 6

myAns = 0
bets = []

while N > 1:
    if N % 2 == 0:
        if K > 0:
            N /= 2
            K -= 1
            myAns += 1
        else:
            myAns += N - 1
            N = 0
    else:
        N -= 1
        myAns += 1


print(myAns)