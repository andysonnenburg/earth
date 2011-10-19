{-# LANGUAGE
    FlexibleInstances
  , GeneralizedNewtypeDeriving
  , MultiParamTypeClasses
  , RecordWildCards
  , StandaloneDeriving
  , UndecidableInstances #-}
module Language.JVM.Earth.Verifier
       ( verify
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.Reader (MonadReader, ReaderT)
import qualified Control.Monad.Reader as Reader
import Control.Monad.State (MonadState, StateT)
import qualified Control.Monad.State as State
import Control.Monad.Trans

import Data.Array
import Data.Int hiding (Int)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import Data.Word

import Language.JVM.Earth.Insn hiding (Insn)
import qualified Language.JVM.Earth.Insn as Insn

import Prelude hiding (Int, read)

type Insn = Insn.Insn Narrow

data VerifyError
  = StackUnderflow
  | InappropriateStackType
  | InappropriateVariableType
  | UnusableVariable
  | UndefinedVariable
  | InconsistentOperandStacks
  | InconsistentLocalVariableArrays
  | FellOffLastInstruction

verify :: [(Word16, Insn)] -> Either VerifyError (Word16, Word16)
verify = undefined

newtype ErrorT e m a
  = ErrorT
    { runErrorT :: m (Either e a)
    }

instance Functor m => Functor (ErrorT e m) where
  fmap f = ErrorT . fmap (fmap f) . runErrorT

instance (Functor m, Monad m) => Applicative (ErrorT e m) where
  pure a = ErrorT $ return (Right a)
  f <*> v = ErrorT $ do
    mf <- runErrorT f
    case mf of
      Left e -> return (Left e)
      Right k -> do
        mv <- runErrorT v
        case mv of
          Left e -> return (Left e)
          Right x -> return (Right (k x))

instance Monad m => Monad (ErrorT e m) where
  return a = ErrorT $ return (Right a)
  m >>= k = ErrorT $ do
    a <- runErrorT m
    case a of
      Left l -> return (Left l)
      Right r -> runErrorT (k r)

instance Monad m => MonadError e (ErrorT e m) where
  throwError l = ErrorT $ return (Left l)
  m `catchError` h = ErrorT $ do
    a <- runErrorT m
    case a of
      Left l -> runErrorT (h l)
      Right r -> return (Right r)

instance MonadReader r m => MonadReader r (ErrorT e m) where
  ask = lift Reader.ask
  local f m = ErrorT $ Reader.local f (runErrorT m)

instance MonadState s m => MonadState s (ErrorT e m) where
  get = lift State.get
  put = lift . State.put

instance MonadTrans (ErrorT e) where
  lift m = ErrorT $ do
    a <- m
    return (Right a)

instance Monad m => MonadPlus (ErrorT e m) where
  mzero = ErrorT $ return (Left undefined)
  m `mplus` n = ErrorT $ do
    a <- runErrorT m
    case a of
      Left _ -> runErrorT n
      Right r -> return (Right r)

newtype VerifierT m a
  = VerifierT
    { unVerifierT :: ErrorT VerifyError (StateT S (ReaderT R m)) a
    } deriving ( Functor
               , Applicative
               , Monad
               , MonadPlus
               )

deriving instance Monad m => MonadError VerifyError (VerifierT m)

data S
  = S
    { currentState :: State
    , currentAddress :: Word16
    , states :: Map Word16 State
    }

data State
  = State
    { stack :: [Type]
    , locals :: Map Word16 (Maybe Type)
    }

type R = Array Word16 (Maybe Insn)

data Type = Int deriving Eq

iload :: Monad m => Word16 -> VerifierT m [Successor]
iload x = do
  read Int x
  push Int
  return [Next]

iinc :: Monad m => Word16 -> Int16 -> VerifierT m [Successor]
iinc x _ = do
  read Int x
  return [Next]

read :: Monad m => Type -> Word16 -> VerifierT m ()
read typ x = do
  State {..} <- getCurrentState
  case Map.lookup x locals of
    Nothing -> throwError UndefinedVariable
    Just Nothing -> throwError UnusableVariable
    Just (Just typ')
      | typ /= typ' -> throwError InappropriateVariableType
      | otherwise -> return ()

push :: Monad m => Type -> VerifierT m ()
push typ = modifyCurrentState f
  where
    f s@State {..} = s { stack = stack' }
      where
        stack' = typ:stack

getInsns :: Monad m => Successor -> VerifierT m [Insn]
getInsns x =
  case x of
    Next -> liftM (:[]) getNextInsn
    Target address -> liftM (:[]) (getTargetInsn address)
    ExceptionHandlers -> undefined -- TODO

getNextInsn :: Monad m => VerifierT m Insn
getNextInsn = do
  arr <- ask
  S {..} <- get
  let
    f i
      | not (inRange (bounds arr) i) = throwError FellOffLastInstruction
      | otherwise = return (arr!i)
        where
          b = bounds arr
  liftM fromJust (msum . map f $ [currentAddress..])

getTargetInsn :: Monad m => Int32 -> VerifierT m Insn
getTargetInsn address = undefined

getCurrentState :: Monad m => VerifierT m State
getCurrentState = do
  S {..} <- get
  return currentState

modifyCurrentState :: Monad m => (State -> State) -> VerifierT m ()
modifyCurrentState f = modify f'
  where
    f' s@S {..} = s { currentState = currentState' }
      where
        currentState' = f currentState

getCurrentAddress :: Monad m => VerifierT m Word16
getCurrentAddress = do
  S {..} <- get
  return currentAddress

get :: Monad m => VerifierT m S
get = VerifierT State.get

put :: Monad m => S -> VerifierT m ()
put = VerifierT . State.put

modify :: Monad m => (S -> S) -> VerifierT m ()
modify = VerifierT . State.modify

ask :: Monad m => VerifierT m R
ask = VerifierT Reader.ask

data Successor
  = Next
  | Target Int32
  | ExceptionHandlers