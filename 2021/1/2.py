file = open('input', 'r')
all_lines = [int(line) for line in file]
trips = list(zip(all_lines, all_lines[1:], all_lines[2:]))
inc_count = -1
prev = -1
for (a, b, c) in trips:
  depth = a + b + c
  if depth > prev:
    inc_count += 1
  prev = depth
print(inc_count)
