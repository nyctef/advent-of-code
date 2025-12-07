{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PointRC (PointRC (..), left, right, up, down, neighbor8) where

import Data.Hashable (Hashable)
import GHC.Generics (Generic)
import Text.Printf

data PointRC = PointRC {row :: Int, col :: Int} deriving (Eq, Ord, Generic, Hashable)

left :: PointRC -> PointRC
left p = PointRC (row p) (col p - 1)

right :: PointRC -> PointRC
right p = PointRC (row p) (col p + 1)

up :: PointRC -> PointRC
up p = PointRC (row p - 1) (col p)

down :: PointRC -> PointRC
down p = PointRC (row p + 1) (col p)

instance Show PointRC where
  show p = printf "(r%d c%d)" (row p) (col p)

neighbor8 :: PointRC -> [PointRC]
neighbor8 (PointRC r c) =
  [ PointRC (r - 1) (c - 1),
    PointRC (r - 1) c,
    PointRC (r - 1) (c + 1),
    PointRC r (c - 1),
    -- PointRC (r) c,
    PointRC r (c + 1),
    PointRC (r + 1) (c - 1),
    PointRC (r + 1) c,
    PointRC (r + 1) (c + 1)
  ]
