l1 = [x | x <- =[1..1000], x `mod` 2 == 1]

l2 = [(x,y) | x <- [1..10],y <-[1..10],x+y==10]