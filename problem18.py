# 18 - Maximum path sum I
data18 = [[75], [95, 64], [17, 47, 82], [18, 35, 87, 10], [20, 4, 82, 47, 65], [19, 01, 23, 75, 03, 34], [88, 02, 77, 73, 7, 63, 67], [99, 65, 4, 28, 06, 16, 70, 92], [41, 41, 26, 56, 83, 40, 80, 70, 33], [41, 48, 72, 33, 47, 32, 37, 16, 94, 29], [53, 71, 44, 65, 25, 43, 91, 52, 97, 51, 14], [70, 11, 33, 28, 77, 73, 17, 78, 39, 68, 17, 57], [91, 71, 52, 38, 17, 14, 91, 43, 58, 50, 27, 29, 48], [63, 66, 04, 68, 89, 53, 67, 30, 73, 16, 69, 87, 40, 31], [04, 62, 98, 27, 23, 9, 70, 98, 73, 93, 38, 53, 60, 04, 23]]

length = int('1'+''.join(['1' for x in range(len(data18))]), 2) + 1
routes = filter(lambda i: i[0] != 1, map(lambda l: l[1:], [map(lambda x: int(x), bin(n)[2:]) for n in range(2**len(data18), length)]))

calc = 0
for a in range(len(routes)):
    ant = 0
    val = 0
    for p in range(len(routes[a])):
        val = val + data18[p][routes[a][p] + ant]
        ant = routes[a][p] + ant
    if val > calc:
        calc = val

print calc
