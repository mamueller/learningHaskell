listSum :: [Double] -> [Double] 
listSum l =
  if l == []
    then []
  else
    let 
      x:xs = l
      nxs = listSum xs
    

    in
      if nxs == []
        then [x]
      else
        (x+head nxs): nxs
