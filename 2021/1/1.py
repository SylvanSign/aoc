file = open('input', 'r')
inc_count = -1
prev = -1
for line in file:
  depth = int(line)
  if depth > prev:
    inc_count += 1
  prev = depth
print(inc_count)
