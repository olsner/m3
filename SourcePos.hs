
{-# LANGUAGE PatternGuards,DeriveDataTypeable #-}

module SourcePos where

import Data.Data (Data,Typeable)

data SourcePos = SourcePos { sourceName :: String, sourceLine :: Int, sourceColumn :: Int } deriving (Ord,Eq,Data,Typeable)
instance Show SourcePos where
  show (SourcePos name line column) = name++" (line "++show line++", column "++show column++")"
initialPos source = SourcePos source 1 1
endOfFilePos = SourcePos "eof" 0 0

type Pos = SourcePos

