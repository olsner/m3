
{-# LANGUAGE PatternGuards,DeriveDataTypeable #-}

module SourcePos where

import Data.Data (Data,Typeable)

data SourcePos = SourcePos { sourceName :: String, sourceLine :: Int, sourceColumn :: Int } deriving (Ord,Eq,Data,Typeable)
instance Show SourcePos where
  show (SourcePos name 0 0) = name
  show (SourcePos name line column) = name++" (line "++show line++", column "++show column++")"
initialPos source = SourcePos source 1 1
dummyPos source = SourcePos source 0 0
endOfFilePos = dummyPos "eof"

type Pos = SourcePos

