{-# LANGUAGE
    EmptyDataDecls
  , FlexibleContexts
  , GADTs
  , StandaloneDeriving
  , TypeFamilies 
  , UndecidableInstances #-}
module Language.JVM.Earth.Insn
       ( Insn (..)
       , Narrow
       , Wide
       , Index
       , Const
       ) where

import Data.Int
import Data.Word

data Insn a where
  IINC :: Index a -> Const a -> Insn a
  ILOAD :: Index a -> Insn a
  WIDE :: Insn Wide -> Insn Narrow

deriving instance (Show (Index a), Show (Const a)) => Show (Insn a)

class Width a where
  type Index a
  type Const a

instance Width Narrow where
  type Index Narrow = Word8
  type Const Narrow = Word8

instance Width Wide where
  type Index Wide = Word16
  type Const Wide = Int16

data Narrow
data Wide