import Control.Monad

exec :: State s a -> s -> s
exec m s = snd (getState m s)

eval :: State s a -> s -> a
eval m s = fst (getState m s)

ask :: State a a 
ask =  State $ \s -> (s,s)

put :: Monoid s => s -> State s ()
put s = State $ \s0 -> ((), s <> s0)

newtype State s a = State { getState :: s -> (a, s)}

instance Functor (State s) where
    fmap f fa = State $ \s0 -> let (a,s1) = (getState fa) s0
                                in (f a, s1)
instance Applicative (State s) where
    pure a = State $ \s -> (a,s)
    (State af) <*> (State aa) = State $ \s0 -> let (f, s1) = af s0
                                               in let (a,s2) = aa s1
                                                   in (f a, s2)
instance Monad (State a) where
    return = pure
    ma >>= f = State $ \s0  -> let (a ,s1) = getState ma s0 
                                in getState (f a) s1

parseListInteger :: String -> [Int]
parseListInteger = map (\x -> read x :: Int) . lines 

groupByThree :: [Int] -> [[Int]]
groupByThree xs | length xs < 3 = []
                | otherwise = (take 3 xs) : groupByThree (drop 1 xs) 

counter :: Int -> State (Int, Integer) Integer
counter i = do
 State $ \(prev,count) -> let incremented = if i > prev then (i,count + 1) else (i, count)
                in (count, incremented) 

main :: IO Integer
main = do
    st <- readFile "input"
    return $ snd $ (exec (mapM counter (map sum $ groupByThree $ parseListInteger st)) (0,(-1))) --(-1) because no previous count and 0 will most likely be smaller and therfore falsly counts.
