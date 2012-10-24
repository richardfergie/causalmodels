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
import qualified Numeric.Probability.Distribution as Dist
import Numeric.Probability.Distribution ((??), (?=<<), )

-- data types representing the different states of the nodes
data Season = Winter|Spring|Summer|Autumn deriving (Show,Eq,Bounded,Enum,Ord)
data Sprinkler = On|Off deriving (Show,Eq,Bounded,Enum,Ord)
data Weather = Rain|Sun deriving (Show,Eq,Bounded,Enum,Ord)
data Path = Wet|Dry deriving (Show,Eq,Bounded,Enum,Ord)
data Slippy = Slippy|Grippy deriving (Show,Eq,Bounded,Enum,Ord)

-- data type representing the observed state of the world
data WorldState = WorldState { season::Season 
                              ,sprinkler::Sprinkler 
                              ,weather:: Weather 
                              ,path::Path 
                              ,slippy::Slippy 
                              } deriving (Show,Eq,Ord)

type Probability = Double
type Dist a = Dist.T Probability a

seasonD :: Dist Season
seasonD = Dist.uniform [Winter ..Autumn]

sprinklerD :: Season -> Dist Sprinkler
sprinklerD Winter = Dist.certainly Off
sprinklerD Summer = Dist.certainly On
sprinklerD Autumn = Dist.certainly Off
sprinklerD Spring = Dist.certainly Off

weatherD :: Season -> Dist Weather
weatherD Winter = Dist.certainly Rain
weatherD Summer = Dist.certainly Sun
weatherD Autumn = Dist.certainly Sun
weatherD Spring = Dist.certainly Rain

pathD :: Sprinkler -> Weather -> Dist Path
pathD On _ = Dist.certainly Wet
pathD _ Rain = Dist.certainly Wet
pathD Off Sun = Dist.certainly Dry

slippyD :: Path -> Dist Slippy
slippyD Wet = Dist.certainly Slippy
slippyD Dry = Dist.certainly Grippy

worldDistribution = do
  s <- seasonD
  sp <- sprinklerD s
  we <- weatherD s
  p <- pathD sp we
  sl <- slippyD p
  return $ WorldState s sp we p sl
  
{-
P(Path=Slippy)
(\x->slippy x==Slippy) ?? (\x -> True) ?=<< worldDistribution

P(Path=Grippy)
(\x->slippy x==Grippy) ?? (\x -> True) ?=<< worldDistribution

P(Season=Winter | Path=Slippy)
(\x->season x==Winter) ?? (\x -> slippy x==Slippy) ?=<< worldDistribution
-}

--Do foo and bar have the same distribution?
sameDist foo bar = Dist.approx 
                       ((\world-> foo world) ?=<< worldDistribution)
                       ((\world-> foo world && bar world) ?=<< worldDistribution)
                       
conditionallyIndependent foo bars = map (sameDist foo) bars