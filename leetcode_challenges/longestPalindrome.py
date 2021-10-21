s = 'babadtenet'
# s = 'babad'
s = "cbbd"
s = "ac"

res = ''
resLen = 0
for i in range(len(s)):

    if len(s) % 2 != 0 or len(s) == 2:
        # odd length
        l, r = i, i
        while l >= 0 and r < len(s) and s[l] == s[r]:
            if r - l + 1 > resLen:
                res = s[l:r + 1]
                resLen = r - l + 1
            l -= 1
            r += 1

    elif len(s) % 2 == 0:
        # even length
        l, r = i, i + 1
        while l >= 0 and r < len(s) and s[l] == s[r]:
            if r - l + 1 > resLen:
                res = s[l:r + 1]
                resLen = r - l + 1
            l -= 1
            r += 1

print(res)