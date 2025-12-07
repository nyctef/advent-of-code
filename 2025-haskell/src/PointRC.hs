{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}

module PointRC (PointRC(..), row, col, left, right, up, down) where

import GHC.Generics (Generic)
import Data.Hashable (Hashable )
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
