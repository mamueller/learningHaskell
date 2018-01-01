bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
  | bmi <= skinny  = "You're underweight"
  | bmi <= normal = "You're normal"
  | bmi <= fat  = "You're fat"
  | otherwise = "you are a whale"
  where bmi = weight / height ^ 2
        (skinny , normal , fat) = ( 18.5, 25.0, 30 )

initials :: String -> String -> String
initials firstname lastname = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstname
          (l:_) = lastname

calcBmis :: (RealFloat a) => [(a,a)]-> [a]
calcBmis xs = [ bmi w h | (w,h) <- xs ]
    where bmi weight height = weight / height ^2
