{-# LANGUAGE DeriveFunctor #-}

module Interpreter.Commands (
    Interaction(..)
  , Program
  , EWP
  , getReleaseBranch
  , deployTag
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
  = GetReleaseBranch (Branch -> x)
  | DeployTag Tag Environment x
  | GitTags ([Tag] -> x)
  | GitCheckoutNewBranchFromTag Branch Tag x
  | GitPushTags String Branch x
  | GitTag Tag x
  deriving Functor

type Program = Free Interaction

type EWP =  EitherT String (WriterT [String] Program)

getReleaseBranch :: EWP Branch
getReleaseBranch = lift $ liftF $ GetReleaseBranch id

deployTag :: Tag -> Environment -> EWP ()
deployTag tag env = lift $ liftF $ DeployTag tag env ()

gitTags :: EWP [Tag]
gitTags = lift $ liftF $ GitTags id

gitCheckoutNewBranchFromTag :: Branch -> Tag -> EWP ()
gitCheckoutNewBranchFromTag branch tag = lift $ liftF $ GitCheckoutNewBranchFromTag branch tag ()

gitPushTags :: String -> Branch -> EWP ()
gitPushTags remote branch = lift $ liftF $ GitPushTags remote branch ()

gitTag :: Tag -> EWP ()
gitTag tag = lift $ liftF $ GitTag tag ()

