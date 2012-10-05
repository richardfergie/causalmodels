{- 
Seeing if I can recreate a causal network using raw data.
First generate some data. Then do the analysis to see if
I can get back to the network.

The network looks like this:
          Season
          /    \
     Weather   Sprinkler
           \   /
         Path wet
             |
        Path slippy
-}

import Control.Monad.Random
import Data.List

-- data types representing the different states of the nodes
data Season = Winter|Spring|Summer|Autumn deriving (Show,Eq,Bounded,Enum)
data Sprinkler = On|Off deriving (Show,Eq,Bounded,Enum)
data Weather = Rain|Sun deriving (Show,Eq,Bounded,Enum)
data Path = Wet|Dry deriving (Show,Eq,Bounded,Enum)
data Slippy = Slippy|Grippy deriving (Show,Eq,Bounded,Enum)

-- data type representing the observed state of the world
data WorldState = WorldState !Season !Sprinkler !Weather !Path !Slippy deriving Show


-- pick a season at random
-- bet there is a better way to do this
season :: (RandomGen g) => Rand g Season
season = do
  i <- getRandomR (1::Int,4)
  case i of
    1 -> return Winter
    2 -> return Spring
    3 -> return Summer
    4 -> return Autumn
    
-- sprinkler is on in summer otherwise off
-- noise not used at the moment. Will represent exogenous
-- factors later on
sprinkler :: (RandomGen g) => Season -> Rand g Sprinkler
sprinkler Summer = noise 0 On 
sprinkler _ = noise 0 Off

-- weather depends on season
weather :: (RandomGen g) => Season -> Rand g Weather
weather Summer = noise 0 Sun
weather Spring = noise 0 Sun
weather Autumn = noise 0 Rain
weather Winter = noise 0 Rain

-- wetness of path depends on weather and sprinkler
path :: (RandomGen g) => Sprinkler -> Weather -> Rand g Path
path On _ = noise 0 Wet
path _ Rain = noise 0 Wet
path Off Sun = noise 0 Dry

slippy :: (RandomGen g) => Path -> Rand g Slippy
slippy Wet = noise 0 Slippy
slippy Dry = noise 0 Grippy

--want this to be an error function. Need some fancy type stuff first
noise _ x = return x

-- observe a random world
worldstate :: (RandomGen g) => Rand g WorldState
worldstate = do
  s <- season
  sp <- sprinkler s
  w <- weather s
  p <- path sp w
  sl <- slippy p
  return $! WorldState s sp w p sl
  
-- generate lots of observations
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
        
-- list properties of the world along with their probabilities
toDistribution :: (Bounded t, Enum t) =>
     (t -> WorldState -> Bool) -> [WorldState] -> [(t, Float)]
toDistribution hasProperty observations = map (\x -> (x, pXGivenY (hasProperty x) (const True) observations)) $! list 
  where list = enumFromTo minBound maxBound

-- need this instance to get conjoined distributions
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

-- P(X=x|Y=y)
-- returns NaN if Y is never seen in the observations
distributionXGivenY  :: (Bounded t, Bounded a, Enum t, Enum a) =>
     (a -> WorldState -> Bool) -> 
     (t -> WorldState -> Bool) -> 
     [WorldState] -> 
     [((a, t), Float)]
distributionXGivenY hasX hasY obs = concatMap (\(y,world)->tidyResult (y,toDistribution hasX world)) worldsWithY
  where allY = enumFromTo minBound maxBound --enumerate list of all Y
        worldsWithY = [(y,filter (hasY y) obs)|y<-allY] -- filter observations where Y=y
        tidyResult (y,(x:xs)) = ((fst x,y),snd x):(tidyResult (y,xs)) -- tidy up the results a bit
        tidyResult (y,[]) = []
        
-- need to test for conditional independence
-- X is independent of Y given Z if P(X|Z)=P(X|Z,Y)
-- So first need a way of checking two distributions are equal
-- A direct equality check won't work for two reasons:
--    1. Floating point numbers
--    2. The distribution P(X|Z) looks something like [((x,z),p)] 
--       but P(X|Y,Z) looks like [((x,(y,z)),p)]

nearlyEqual :: Float -> Float -> Float -> Bool
nearlyEqual threshold a b = (isNaN a && isNaN b) || abs (a-b) < threshold

{-
  > distributionXGivenY hasSlippy (\(x,y) s -> hasPath x s && hasWeather y s) obs
  [((Slippy,(Wet,Rain)),1.0),((Grippy,(Wet,Rain)),0.0),((Slippy,(Wet,Sun)),1.0),((Grippy,(Wet,Sun)),0.0),((Slippy,(Dry,Rain)),NaN),((Grippy,(Dry,Rain)),NaN),((Slippy,(Dry,Sun)),0.0),((Grippy,(Dry,Sun)),1.0)]

  > distributionXGivenY hasSlippy hasPath obs
  [((Slippy,Wet),1.0),((Grippy,Wet),0.0),((Slippy,Dry),0.0),((Grippy,Dry),1.0)]

  In what sense are these equal? P(X|Z) = sum over Y of P(X|Z,Y)?

  X,Y conditionally independent given Z if for all y P(X|Z)=P(X|Z AND Y=y)

  so for all x,y,z calculate pXGivenY x z and pXGivenY x (z and y) then check equal
-}

conditionallyIndependent hasX hasY hasZ obs = and $ map (\a -> check a xGivenZY) xGivenZ 
  where xGivenZ = [(x,z,pXGivenY (hasX x) (hasZ z) obs)|x<-[minBound..maxBound],z<-[minBound..maxBound]]
        xGivenZY = [(x,z,y,pXGivenY (hasX x) (\s -> hasY y s && hasZ z s) obs)|x<-[minBound..maxBound],z<-[minBound..maxBound],y<-[minBound,maxBound]]
        
check (x,z,p) lst = all (nearlyEqual 0.0005 p) $ map (\(x1,z1,y1,p1)->p1) $ filter (\(x1,z1,y1,p1)->x==x1 && z==z1 && (not $ isNaN p1)) lst
        
