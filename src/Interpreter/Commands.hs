{-# LANGUAGE DeriveFunctor #-}

module Interpreter.Commands (
    Interaction(..)
  , Program
  , EWP
  , getLineAfterPrompt
  , deployTag
  , gitCheckoutTag
  , gitTags
  , gitCheckoutNewBranchFromTag
  , gitPushTags
  , gitTag
  ) where

import           Control.Monad.Free
import           Control.Monad.Trans.Class         (lift)

import           Control.Monad.Trans.Either        (EitherT)
import           Control.Monad.Trans.Writer.Strict (WriterT)

import           Types                             (Branch (..), Environment,
                                                    Tag (..))


data Interaction x
  = GetLineAfterPrompt String (String -> x)
  | DeployTag Tag Environment x
  | GitCheckoutTag Tag x
  | GitTags ([Tag] -> x)
  | GitCheckoutNewBranchFromTag Branch Tag x
  | GitPushTags String x
  | GitTag Tag x
  deriving Functor

type Program = Free Interaction
type EWP =  EitherT String (WriterT [String] Program)

getLineAfterPrompt :: String -> EWP String
getLineAfterPrompt prompt = lift $ liftF $ GetLineAfterPrompt prompt id

gitCheckoutTag :: Tag -> EWP ()
gitCheckoutTag tag = lift $ liftF $ GitCheckoutTag tag ()

deployTag :: Tag -> Environment -> EWP ()
deployTag tag env = lift $ liftF $ DeployTag tag env ()

gitTags :: EWP [Tag]
gitTags = lift $ liftF $ GitTags id

gitCheckoutNewBranchFromTag :: Branch -> Tag -> EWP ()
gitCheckoutNewBranchFromTag branch tag = lift $ liftF $ GitCheckoutNewBranchFromTag branch tag ()

gitPushTags :: String -> EWP ()
gitPushTags remote = lift $ liftF $ GitPushTags remote ()

gitTag :: Tag -> EWP ()
gitTag tag = lift $ liftF $ GitTag tag ()

