module P31 where


import Data.Array

kinds = [1,2,5,10,20,50,100,200]

-- ways to construct value t by using k or less kinds.
ways t k tmax = r !! (t, k) where
  array ((0,0),(tmax,length kinds)) [((x,y),w x y) | x<-[0..tmax], y<-[0..length kinds]]
  w 0 _ = 0
  w _ 0 = 0
  w x y = 

