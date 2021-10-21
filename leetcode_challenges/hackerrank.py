from collections import Counter

l = [1, 1, 2, 3, 4, 1, 0, 5, 5]
c = Counter(sorted(l))
d = dict(c)
s = sorted(d.items(), reverse=True)
d = dict(s)

# remove element from a dictionary
mag = 'two times three is not four'.split()
note = 'two times two is four'.split()
m, n = dict(Counter(mag)), Counter(note)

flag = True
for word in note:
    if word in m.keys():
        freq = m[word]
        m[word] -= 1
        if m[word] == 0:
            m.pop(word)
    else:
        flag = False
if flag:
    print('Yes')
else:
    print('No')


# substring match
from collections import Counter
s1, s2 = 'hello', 'world'
s1, s2 = Counter(list(s1)), list(s2)

count = 0
for char in s2:
    if char in s1.keys():
        count += 1
        s1[char] -= 1
        if s1[char] == 0:
            s1.pop(char)

print(count)

# Anagrams
s = list('kkkk')
from collections import Counter
x = [''.join(sorted(s[pick: pick + ind])) for ind in range(1, len(s)) for pick in range(0, len(s) - ind+1)]

counter = 0
for key, value in Counter(x).items():
    if value / 2 == 1:
        counter += 1
    if value > 2:
        counter  += value

for i in Counter(x).values():
    print(i, sum(range(i)))

# nth fibbonacii
n = 30
s = [0, 1]
for i in range(2, n+1):
    s.append(s[i-1] + s[i-2])
print(s[-1])






# balanced delimiter
s = '{{()()}}[]'
from collections import defaultdict
d = defaultdict(int)
for char in s:
    d[char] += 1

keys = d.keys()