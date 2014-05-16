test :: Int -> Either Int Int
test x = Right x >>= \y -> Left y >>= \z -> Right 7

testDo :: Int -> Either Int Int
testDo x = do
  y <- Right x 
  z <- Left y
  Right 7


Maybe

Just x >>= \y -> f y = f x
Nothing >>= \y -> f y = Nothing

