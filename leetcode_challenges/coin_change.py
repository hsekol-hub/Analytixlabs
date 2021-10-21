string = '3[AB]2[5[B]]C'

import re


start, end = [x.start() for x in re.finditer('[\[]', string)], [x.start() for x in re.finditer('[\]]', string)]
assert len(start) == len(end)

newstr = []
ind = 0
for head, s in enumerate(start):

    prefix = string[ind:s]
    repeat = string[s+1:end[head]]

    if prefix.isalpha():
        newstr.append(prefix)
    if prefix.isdigit():

        tmp = [repeat for i in range(int(prefix))]

    ind = end[head] + 1

    print(prefix, repeat, tmp)
    print('#')




#
#
# newstr = []
# while len(s) > 0:
#     ind = 0
#     char = s[ind]
#     if char.isalpha():
#         newstr.append(char)
#         del s[ind]
#     if char.isdigit():
#         mult_start, mult_end = re.search('[\d]', ''.join(s)).start(), re.search('[\d]', ''.join(s)).end()
#         char_start, char_end = re.search('[\[]', ''.join(s)).start(), re.search('[\]]', ''.join(s)).start()
#         mult = s[mult_start:mult_end]
#         mult = int(''.join(mult))
#         subchar = s[char_start+1:char_end]
#         subchar = str(''.join(subchar))
#
#         # repeat subchar
#         tmp = ''.join([subchar for i in range(mult)])
#         newstr.append(tmp)
#         # update index
#         del s[ind:char_end+1]
#         # ind = char_end
#     if ']' in ''.join(s):
#         del s[ind]
#
# print(''.join(newstr))