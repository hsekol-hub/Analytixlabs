s = 'z3[a2[bc]d]ac4[w]'
s = "3[a]2[bc]"
s = "3[a2[c]]"
s = "2[abc]3[cd]ef"
s = "abc3[cd]xyz"

stack = []
for char in s:
    print(char)
    if char.isalpha():
        if len(stack):
            elem = stack.pop()
            if elem.isalpha():
                stack.append(elem + char)  #
            else:
                stack.append(elem)
                stack.append(char)
        else:
            stack.append(char)

    elif char.isdigit():
        if len(stack):
            elem = stack.pop()
            if elem.isdigit():
                stack.append(int(elem)*10 + int(char))  #
            else:
                stack.append(elem)
                stack.append(char)
        else:
            stack.append(char)
    elif char == '[':
        stack.append(char)
    elif char == ']':
        subchar = stack.pop()
        stack.pop()  # removes the open bracket always
        repeat = stack.pop()
        if repeat.isdigit():
            subchar = ''.join([subchar for i in range(int(repeat))])
            if len(stack):
                prefix = stack.pop()
                if prefix.isalpha():
                    subchar = prefix + subchar
                    stack.append(subchar)  #
            else:
                stack.append(subchar)  #

print(stack.pop())