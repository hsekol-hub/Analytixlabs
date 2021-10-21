# ####
# Write a function in C++ that accepts two integers countX and countO and returns a string containing exactly countX
# times the letter 'X' and exactly countO times the letter 'O', but with no four consecutive letters being the same.
# That is, "XXXX" and "OOOO" may not occur in the result string.
#
# Examples:
# 1. Called with countX=2 and countO=3, the function may return "XOOOX".
# Other correct return strings are possible. The function may return any of those, for example "OXOXO".
# 2. Called with countX=6 and countO=1, the function must return "XXXOXXX", which is the only correct result in this case.  ####


def myFunction(countx, counto):
    # Algorithm
    str = ''
    # get maximum and minimum
    if countx >= counto:
        mx, mn = countx, counto
        f, s = 'X', 'O'
    else:
        mx, mn = counto, countx
        f, s = 'O', 'X'
    # check if values passed are legal
    if mn + 1 >= mx / 3:
        while mx > 0 or mn > 0:  # condition to exit
            # case 1
            if mn <= mx / 3:
                str += f * min(3, mx)
                mx -= min(3, mx)
                str += s * min(1, mn)
                mn -= min(1, mn)
            else:
                str += f * min(3, mx)
                mx -= min(3, mx)
                sub = int(mn - mx / 3)
                str += s * min(3, sub)
                mn -= min(3, sub)
    else:
        print('Not Possible')
    print(str)

myStr = myFunction(10, 9)
print(myStr)