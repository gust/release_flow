{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}


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
  , gitPush
  , gitRemoveTag
  , gitRemoveBranch
  , gitMergeNoFF
  , gitTag
  , gitPullRebase
  , outputMessage
  ) where

import           Control.Monad.Free
import           Control.Monad.Trans.Class         (lift)

import           Control.Monad.Trans.Either        (EitherT)
import           Control.Monad.Trans.Writer.Strict (WriterT)

import           Types                             (Branch (..), Environment,
                                                    Tag (..), ReleaseError(..), Message)

data Interaction x
  = GetLineAfterPrompt String (String -> x)
  | GitCreateAndCheckoutBranch Branch x
  | GitCheckoutBranch Branch x
  | GitCheckoutTag Tag x
  | GitTags ([Tag] -> x)
  | GitBranches ([Branch] -> x)
  | GitCheckoutNewBranchFromTag Branch Tag x
  | GitPushTags String x
  | GitPush String Branch x
  | GitRemoveTag Tag x
  | GitRemoveBranch Branch x
  | GitTag Tag x
  | forall a. Show a => GitMergeNoFF a x
  | GitPullRebase x
  | OutputMessage Message x

instance Functor Interaction where
  fmap f (GetLineAfterPrompt s g)              = GetLineAfterPrompt s (fmap f g)
  fmap f (GitCreateAndCheckoutBranch branch x) = GitCreateAndCheckoutBranch branch $ f x
  fmap f (GitCheckoutBranch b x)               = GitCheckoutBranch b $ f x
  fmap f (GitCheckoutTag t x)                  = GitCheckoutTag t $ f x
  fmap f (GitTags g)                           = GitTags $ fmap f g
  fmap f (GitBranches g)                       = GitBranches $ fmap f g
  fmap f (GitCheckoutNewBranchFromTag b t x)   = GitCheckoutNewBranchFromTag b t $ f x
  fmap f (GitPushTags s x)                     = GitPushTags s $ f x
  fmap f (GitPush s b x)                       = GitPush s b $ f x
  fmap f (GitRemoveTag t x)                    = GitRemoveTag t $ f x
  fmap f (GitRemoveBranch b x)                 = GitRemoveBranch b $ f x
  fmap f (GitTag t x)                          = GitTag t $ f x
  fmap f (GitMergeNoFF s x)                    = GitMergeNoFF s $ f x
  fmap f (GitPullRebase x)                     = GitPullRebase $ f x
  fmap f (OutputMessage s x)                   = OutputMessage s $ f x

instance Show (Interaction x) where
  show (GetLineAfterPrompt _ _)            = "GetLineAfterPrompt"
  show (GitCreateAndCheckoutBranch _ _)    = "GitCreateAndCheckoutBranch"
  show (GitCheckoutBranch _ _)             = "GitCheckoutBranch"
  show (GitCheckoutTag _ _)                = "GitCheckoutTag"
  show (GitTags _)                         = "GitTags"
  show (GitBranches _)                     = "GitBranches"
  show (GitCheckoutNewBranchFromTag _ _ _) = "GitCheckoutNewBranchFromTag"
  show (GitPushTags _ _)                   = "GitPushTags"
  show (GitPush _ _ _)                     = "GitPush"
  show (GitRemoveTag _ _)                  = "GitRemoveTag"
  show (GitRemoveBranch _ _)               = "GitRemoveBranch"
  show (GitTag _ _)                        = "GitTag"
  show (GitMergeNoFF _ _)                  = "GitMergeNoFF"
  show (GitPullRebase _)                   = "GitPullRebase"
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

gitPush :: String -> Branch -> EP ()
gitPush remote branch = lift $ liftF $ GitPush remote branch ()

gitRemoveTag :: Tag -> EP ()
gitRemoveTag tag = lift $ liftF $ GitRemoveTag tag ()

gitTag :: Tag -> EP ()
gitTag tag = lift $ liftF $ GitTag tag ()

gitRemoveBranch :: Branch -> EP ()
gitRemoveBranch branch = lift $ liftF $ GitRemoveBranch branch ()

gitMergeNoFF :: Show a => a -> EP ()
gitMergeNoFF commitish = lift $ liftF $ GitMergeNoFF commitish ()

outputMessage :: Message -> EP ()
outputMessage message = lift $ liftF $ OutputMessage message ()

gitPullRebase :: EP ()
gitPullRebase = lift $ liftF $ GitPullRebase ()


