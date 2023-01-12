
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
module Tetris where

import Control.Applicative ((<|>))
import Control.Monad (guard)
import Data.Maybe (fromMaybe)

import Control.Lens hiding ((<|), (|>), (:>), (:<))
import Control.Monad.Trans.Maybe
import Control.Monad.Trans.State
import Control.Monad.Extra (orM)
import Data.Sequence (Seq(..), (<|))
import qualified Data.Sequence as S
import Linear.V2 (V2(..), _x, _y)
import System.Random (Random(..), newStdGen)

import qualified Graphics.Vty as V

-- Types

data Game = Game
  { _remain :: Remain       -- ^ remains blocks that have not been eliminated 
  , _dir    :: Direction    -- ^ direction
  , _food   :: Coord        -- ^ location of the food
  , _foods  :: Stream Coord -- ^ infinite list of random next food locations
  , _dead   :: Bool         -- ^ game over flag
  , _paused :: Bool         -- ^ paused flag
  , _score  :: Int          -- ^ score
  , _locked :: Bool         -- ^ lock to disallow duplicate turns between time steps
  } deriving (Show)

type Coord = V2 Int

type CoordWithAttr = (Coord, V.Attr)

data TetrisK = L | T | Skew | Square | Line 
data TetrisB = TetrisB { center :: Coord
, direction :: Direction
, kind :: TetrisK
, attr :: V.Attr
}


type Remain = Seq CoordWithAttr

data Stream a = a :| Stream a
  deriving (Show)


data Direction
  = North
  | South
  | East
  | West
  deriving (Eq, Show)

makeLenses ''Game

-- Constants

height, width :: Int
height = 20
width = 10

-- Functions

-- | Step forward in time
step :: Game -> Game
step s = flip execState s . runMaybeT $ do

  -- Make sure the game isn't paused or over
  MaybeT $ guard . not <$> orM [use paused, use dead]

  -- Unlock from last directional turn
  MaybeT . fmap Just $ locked .= False



-- | Turn game direction (only turns orthogonally)
--
-- Implicitly unpauses yet locks game
turn :: Direction -> Game -> Game
turn d g = if g ^. locked
  then g
  else g & dir %~ turnDir d & paused .~ False & locked .~ True

turnDir :: Direction -> Direction -> Direction
turnDir n c | c `elem` [North, South] && n `elem` [East, West] = n
            | c `elem` [East, West] && n `elem` [North, South] = n
            | otherwise = c

-- | Initialize a paused game with random food location
initGame :: IO Game
initGame = do
  (f :| fs) <-
    fromList . randomRs (V2 0 0, V2 (width - 1) (height - 1)) <$> newStdGen
  let xm = width `div` 2
      ym = height `div` 2
  return Game
        {_remain = S.empty
        , _food   = f
        , _foods  = fs
        , _score  = 80
        , _dir    = North
        , _dead   = False
        , _paused = True
        , _locked = False
        }


fromList :: [a] -> Stream a
fromList = foldr (:|) (error "Streams must be infinite")

