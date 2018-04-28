type ListZipper a = ([a],[a])

goForward :: ListZipper a -> ListZipper a
goForward (x:xs,bs) = (xs,x:bs)

goBack :: ListZipper a -> ListZipper a
goBack(xs,b:bs) = (b:xs,bs)

-- example
xs = [1,2,3,4]
newFocus :: ListZipper Int
newFocus =goForward(xs,[])
