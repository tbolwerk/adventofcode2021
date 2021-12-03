import Control.Monad

data Direction = Forward Int | Down Int | Up Int
 deriving Show
mapToDirection :: String -> [Direction]
mapToDirection st = map (mapDirection . words) $ lines st
 where mapDirection (x:xs) = case x of
                               "forward" -> Forward (read $ last xs)
                               "up"      -> Up (read $ last xs)
                               "down"    -> Down (read $ last xs)

data Position = Position {
                  depth :: Int,
                  horizontal :: Int
                }
  deriving Show

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


calculatePosition dir = do
   State $ \(Position d h) -> case dir of
                    Forward x -> let newPos = Position d (h + x) in (newPos, newPos)
                    Up x -> let newPos = Position (d - x) h in (newPos, newPos)
                    Down x -> let newPos = Position (d + x) h in (newPos, newPos)

main :: IO Int
main = do
   st <- readFile "input"
   let pos = exec (mapM calculatePosition (mapToDirection st)) ( (Position 0 0))
   return (depth pos * horizontal pos)
