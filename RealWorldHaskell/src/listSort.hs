--sublist length smaller than p
ss :: Int-> [[a]] -> [[a]]
ss p lists = [ list | list <- lists,(length list)<p]

--sublist length bigger than p
bs:: Int-> [[a]] -> [[a]]
bs p lists = [ list | list  <- lists,(length list)>p]

--quicksort sublists by length
qs :: [[a]]->[[a]]
qs []=[]
qs [[x]]=[[x]]
qs (plist:lists) = qs (ss p lists) ++ plist :(qs (bs p lists))
    where p = length plist

