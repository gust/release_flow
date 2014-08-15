{-# LANGUAGE DeriveFunctor #-}

module Interpreter.Commands (
    Interaction(..)
  , Program
  , EP
  , getLineAfterPrompt
  , gitCreateAndCheckoutBranch
  , gitCheckoutBranch
  , gitCheckoutTag
  , gitTags
  , gitBranches
  , gitCheckoutNewBranchFromTag
  , gitPushTags
  , gitRemoveTag
  , gitRemoveBranch
  , gitMergeNoFF
  , gitTag
  , outputMessage
  ) where

import           Control.Monad.Free
import           Control.Monad.Trans.Class         (lift)

import           Control.Monad.Trans.Either        (EitherT)
import           Control.Monad.Trans.Writer.Strict (WriterT)

import           Types                             (Branch (..), Environment,
                                                    Tag (..), ReleaseError(..))

data Interaction x
  = GetLineAfterPrompt String (String -> x)
  | GitCreateAndCheckoutBranch Branch x
  | GitCheckoutBranch Branch x
  | GitCheckoutTag Tag x
  | GitTags ([Tag] -> x)
  | GitBranches ([Branch] -> x)
  | GitCheckoutNewBranchFromTag Branch Tag x
  | GitPushTags String x
  | GitRemoveTag Tag x
  | GitRemoveBranch Branch x
  | GitTag Tag x
  | GitMergeNoFF Branch x
  | OutputMessage String x
  deriving Functor

instance Show (Interaction x) where
  show (GetLineAfterPrompt _ _)            = "GetLineAfterPrompt"
  show (GitCreateAndCheckoutBranch _ _)    = "GitCreateAndCheckoutBranch"
  show (GitCheckoutBranch _ _)             = "GitCheckoutBranch"
  show (GitCheckoutTag _ _)                = "GitCheckoutTag"
  show (GitTags _)                         = "GitTags"
  show (GitBranches _)                     = "GitBranches"
  show (GitCheckoutNewBranchFromTag _ _ _) = "GitCheckoutNewBranchFromTag"
  show (GitPushTags _ _)                   = "GitPushTags"
  show (GitRemoveTag _ _)                  = "GitRemoveTag"
  show (GitRemoveBranch _ _)               = "GitRemoveBranch"
  show (GitTag _ _)                        = "GitTag"
  show (GitMergeNoFF _ _)                  = "GitMergeNoFF"
  show (OutputMessage _ _)                 = "OutputMessage"

type Program = Free Interaction
type EP = EitherT ReleaseError Program


getLineAfterPrompt :: String -> EP String
getLineAfterPrompt prompt = lift $ liftF $ GetLineAfterPrompt prompt id

gitCreateAndCheckoutBranch :: Branch -> EP ()
gitCreateAndCheckoutBranch branch = lift $ liftF $ GitCreateAndCheckoutBranch branch ()

gitCheckoutBranch :: Branch -> EP ()
gitCheckoutBranch branch = lift $ liftF $ GitCheckoutBranch branch ()

gitCheckoutTag :: Tag -> EP ()
gitCheckoutTag tag = lift $ liftF $ GitCheckoutTag tag ()

gitTags :: EP [Tag]
gitTags = lift $ liftF $ GitTags id

gitBranches :: EP [Branch]
gitBranches = lift $ liftF $ GitBranches id

gitCheckoutNewBranchFromTag :: Branch -> Tag -> EP ()
gitCheckoutNewBranchFromTag branch tag = lift $ liftF $ GitCheckoutNewBranchFromTag branch tag ()

gitPushTags :: String -> EP ()
gitPushTags remote = lift $ liftF $ GitPushTags remote ()

gitRemoveTag :: Tag -> EP ()
gitRemoveTag tag = lift $ liftF $ GitRemoveTag tag ()

gitTag :: Tag -> EP ()
gitTag tag = lift $ liftF $ GitTag tag ()

gitRemoveBranch :: Branch -> EP ()
gitRemoveBranch branch = lift $ liftF $ GitRemoveBranch branch ()

gitMergeNoFF :: Branch -> EP ()
gitMergeNoFF branch = lift $ liftF $ GitMergeNoFF branch ()

outputMessage :: String -> EP ()
outputMessage message = lift $ liftF $ OutputMessage message ()


