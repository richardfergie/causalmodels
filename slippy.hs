import Control.Monad.Random
import Data.List

data Season = Winter|Spring|Summer|Autumn deriving (Show,Eq,Bounded,Enum)
data Sprinkler = On|Off deriving (Show,Eq,Bounded,Enum)
data Weather = Rain|Sun deriving (Show,Eq,Bounded,Enum)
data Path = Wet|Dry deriving (Show,Eq,Bounded,Enum)
data Slippy = Slippy|Grippy deriving (Show,Eq,Bounded,Enum)

data WorldState = WorldState !Season !Sprinkler !Weather !Path !Slippy deriving Show

season :: (RandomGen g) => Rand g Season
season = do
  i <- getRandomR (1::Int,4)
  case i of
    1 -> return Winter
    2 -> return Spring
    3 -> return Summer
    4 -> return Autumn
    
sprinkler :: (RandomGen g) => Season -> Rand g Sprinkler
sprinkler Summer = noise 0 On 
sprinkler _ = noise 0 Off

weather :: (RandomGen g) => Season -> Rand g Weather
weather Summer = noise 0 Sun
weather Spring = noise 0 Sun
weather Autumn = noise 0 Rain
weather Winter = noise 0 Rain

path :: (RandomGen g) => Sprinkler -> Weather -> Rand g Path
path On _ = noise 0 Wet
path _ Rain = noise 0 Wet
path Off Sun = noise 0 Dry

slippy :: (RandomGen g) => Path -> Rand g Slippy
slippy Wet = noise 0 Slippy
slippy Dry = noise 0 Grippy

--want this to be an error function. Need some fancy type stuff first
noise _ x = return x

worldstate :: (RandomGen g) => Rand g WorldState
worldstate = do
  s <- season
  sp <- sprinkler s
  w <- weather s
  p <- path sp w
  sl <- slippy p
  return $! WorldState s sp w p sl
  
makeobservations :: Int -> IO [WorldState]
makeobservations n = evalRandIO $! sequence $! replicate n worldstate

hasSeason :: Season -> WorldState -> Bool 
hasSeason x (WorldState y _ _ _ _) = x==y

hasSprinkler :: Sprinkler -> WorldState -> Bool 
hasSprinkler x (WorldState _ y _ _ _) = x==y

hasWeather x (WorldState _ _ y _ _) = x==y

hasPath x (WorldState _ _ _ y _) = x==y

hasSlippy x (WorldState _ _ _ _ y) = x==y

-- P(X|Y) = P(X^Y)/P(Y)
pXGivenY :: (WorldState->Bool) -> (WorldState->Bool) -> [WorldState] -> Float        
pXGivenY hasX hasY obs = (fromIntegral numer)/(fromIntegral denom)
  where denom = length $! filter (hasY) obs 
        numer = length $! filter (\x -> hasX x && hasY x) obs
        
toDistribution :: (Bounded t, Enum t) =>
     (t -> WorldState -> Bool) -> [WorldState] -> [(t, Float)]
toDistribution hasProperty observations = map (\x -> (x, pXGivenY (hasProperty x) (const True) observations)) $! list 
  where list = enumFromTo minBound maxBound

instance (Bounded a, Enum a, Eq a, Bounded b, Enum b, Eq b) => Enum (a,b) where
-- could do this in the same way as fromEnum, but will get more useful error
-- message using succ  
  toEnum x = apply x succ $ (minBound,minBound)
    where apply 0 _ = id
          apply n x = x . (apply (n-1) x)
  fromEnum (a,b) = case elemIndex (a,b) list of
    Nothing -> error "Cannot find in list"
    Just n -> n
    where list = [(x,y)|x<-enumFrom  minBound,y<-enumFrom minBound]
  succ (a,b) = if (b==maxBound) then (succ a, minBound) else (a, succ b)
  pred (a,b) = if (b==minBound) then (pred a, maxBound) else (a, pred b) 
  

distributionXandY :: (Bounded a, Bounded b, Enum a, Enum b, Eq a, Eq b) =>
     (a -> WorldState -> Bool)-> 
     (b -> WorldState -> Bool) -> 
     [WorldState] -> 
     [((a, b), Float)]
distributionXandY hasX hasY obs = toDistribution (\(x,y) s -> hasX x s && hasY y s) obs
        
pYequals :: (Bounded a, Enum a, Eq a) =>
     (a -> WorldState -> Bool) -> a -> [WorldState] -> Float
pYequals hasY y obs = case (find (\(a,b)->a==y) $! toDistribution hasY obs) of   
  Nothing -> error "Cannot find in list"
  Just (y,p) -> p

-- P(X|Y) = P(X=x AND Y=y)/P(Y=y)
distributionXGivenY hasX hasY obs = concatMap (\(y,world)->tidyResult (y,toDistribution hasX world)) worldsWithY
  where allY = enumFromTo minBound maxBound
        worldsWithY = [(y,filter (hasY y) obs)|y<-allY]
        tidyResult (y,(x:xs)) = (fst x,y,snd x):(tidyResult (y,xs))
        tidyResult (y,[]) = []