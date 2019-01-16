x = 1:10
y = x[x<4]
x[-4]

vector1 = c(1,3,5,7,9,10)
vector2 = c(1,3)

match(vector1, vector2)

vector1[!is.na(match(vector1, vector2))]

