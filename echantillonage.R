
data=data[1:10000,] 

S=0.7 
perm = sample(1:nrow(data),ceiling(nrow(data)*S))
train = data[perm,] 
valid = data[-perm,]
