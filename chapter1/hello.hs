import Text.XHtml (action)
fac 0 = 1
fac n = n * fac (n-1)

printStringNTimes :: (Eq t, Num t) => t -> IO ()
printStringNTimes 0 = return ()
printStringNTimes n = do
    putStrLn "test"
    printStringNTimes (n-1)

cube :: Num a => a -> a
cube n = n*n*n

sum1 :: Num a => [a] -> a
sum1 [] = 0
sum1 (n:ns) = n + sum ns

qsort :: Ord a => [a] -> [a]
qsort []     = []
qsort (x:xs) = qsort smaller ++ [x] ++ qsort larger
               where 
                 smaller = [a | a <- xs, a <= x]
                 larger  = [b | b <- xs, b > x]

seqn :: [IO a] -> IO [a]
seqn []         = return []
seqn (act:acts) = do x <- act
                     xs <- seqn acts
                     return (x:xs)

double :: Num a => a -> a
double x = x+x

quad :: Num a => a -> a
quad x = double (double x)

main :: IO Integer
main = do

    putStrLn "hello"

    print (fac 4)
    
    printStringNTimes 2

    print (cube 3)

    print(sum1 [1,2,3]) -- comment test

    print(qsort [3,5,1,4,2])

    return 0