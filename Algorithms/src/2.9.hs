--2.9)
import Data.Char
string2int cs= 
    let chars2ints:: [Char]->[Int]
        chars2ints []=[]
        chars2ints (c:cs)
            | isDigit c = (digitToInt c):(chars2ints cs)
            | otherwise = error "this works only for digits not arbitrary character"
    -----------
        intlist2int::[Int]->Int
        intlist2int xs = 
            let n=length xs
                rxs= reverse xs
            in sum [ (rxs !! i)*(10^i) | i<-[0..(n-1)]]
    in intlist2int (chars2ints cs)
