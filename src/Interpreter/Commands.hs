{-# LANGUAGE DeriveFunctor #-}

module Interpreter.Commands (
    Interaction(..)
  , Program
  , EWP
  , getLineAfterPrompt
  , deployTag
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
                                                    Tag (..))

data Interaction x
  = GetLineAfterPrompt String (String -> x)
  | DeployTag Tag Environment x
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
  show (GetLineAfterPrompt _ _) = "GetLineAfterPrompt"
  show (DeployTag _ _ _) = "DeployTag"
  show (GitCreateAndCheckoutBranch _ _) = "GitCreateAndCheckoutBranch"
  show (GitCheckoutBranch _ _) = "GitCheckoutBranch"
  show (GitCheckoutTag _ _) = "GitCheckoutTag"
  show (GitTags _) = "GitTags"
  show (GitBranches _) = "GitBranches"
  show (GitCheckoutNewBranchFromTag _ _ _) = "GitCheckoutNewBranchFromTag"
  show (GitPushTags _ _) = "GitPushTags"
  show (GitRemoveTag _ _) = "GitRemoveTag"
  show (GitRemoveBranch _ _) = "GitRemoveBranch"
  show (GitTag _ _) = "GitTag"
  show (GitMergeNoFF _ _) = "GitMergeNoFF"
  show (OutputMessage _ _) = "OutputMessage"

type Program = Free Interaction
type EWP =  EitherT String (WriterT [String] Program)


getLineAfterPrompt :: String -> EWP String
getLineAfterPrompt prompt = lift $ liftF $ GetLineAfterPrompt prompt id

gitCreateAndCheckoutBranch :: Branch -> EWP ()
gitCreateAndCheckoutBranch branch = lift $ liftF $ GitCreateAndCheckoutBranch branch ()

gitCheckoutBranch :: Branch -> EWP ()
gitCheckoutBranch branch = lift $ liftF $ GitCheckoutBranch branch ()

gitCheckoutTag :: Tag -> EWP ()
gitCheckoutTag tag = lift $ liftF $ GitCheckoutTag tag ()

deployTag :: Tag -> Environment -> EWP ()
deployTag tag env = lift $ liftF $ DeployTag tag env ()

gitTags :: EWP [Tag]
gitTags = lift $ liftF $ GitTags id

gitBranches :: EWP [Branch]
gitBranches = lift $ liftF $ GitBranches id

gitCheckoutNewBranchFromTag :: Branch -> Tag -> EWP ()
gitCheckoutNewBranchFromTag branch tag = lift $ liftF $ GitCheckoutNewBranchFromTag branch tag ()

gitPushTags :: String -> EWP ()
gitPushTags remote = lift $ liftF $ GitPushTags remote ()

gitRemoveTag :: Tag -> EWP ()
gitRemoveTag tag = lift $ liftF $ GitRemoveTag tag ()

gitTag :: Tag -> EWP ()
gitTag tag = lift $ liftF $ GitTag tag ()


gitRemoveBranch :: Branch -> EWP ()
gitRemoveBranch branch = lift $ liftF $ GitRemoveBranch branch ()

gitMergeNoFF :: Branch -> EWP ()
gitMergeNoFF branch = lift $ liftF $ GitMergeNoFF branch ()

outputMessage :: String -> EWP ()
outputMessage message = lift $ liftF $ OutputMessage message ()


