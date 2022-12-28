{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
module Bug2 where

import BugRepro

data T1

instance Sized T1 where
  type SizeOf T1 = 4

x :: A '[T1]
x = undefined

y :: B '[ '(1,T2), '(0,T2) ]
y = undefined

main :: IO ()
main = do
  let c = CC x y
  putStrLn "OK"

