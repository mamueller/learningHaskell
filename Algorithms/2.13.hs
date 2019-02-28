import Data.Array
--a)
m1=array((1,1),(3,3))[
    ((1,1),2)
    ,((1,2),3)
    ,((1,3),4)
    ,((2,1),5)
    ,((2,2),6)
    ,((2,3),7)
    ,((3,1),8)
    ,((3,2),9)
    ,((3,3),10)
    ]
--b)
m2=array((1,1),(3,3))[((i,j),3*(i-1)+j+1)|i<-[1..3],j<-[1..3]]
t=m1==m2
--c)
transpose33 m = array ((1,1),(3,3))[((i,j),m ! (j,i)) |i<-[1..3],j<-[1..3]]
--d)
transpose mat = 
    let uB= snd (bounds mat)
        m= fst uB 
        n= snd uB
    in  array ((1,1),(n,m))[((i,j),mat ! (j,i)) |i<-[1..n],j<-[1..m]]
