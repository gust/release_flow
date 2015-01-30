{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}


module Interpreter.Commands where

import           Control.Monad.Free
import           Control.Monad.Trans.Class         (lift)

import           Control.Monad.Trans.Either        (EitherT)
import           Control.Monad.Trans.Writer.Strict (WriterT)
import Control.Monad.Reader (ReaderT)
import Data.Aeson (FromJSON(..), (.:), Value(..))
import Control.Applicative ((<$>), (<*>))
import Control.Monad (mzero)



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


data Config = Config {
    integrationBranch :: String
  , deployCommand     :: String
} deriving Show

instance FromJSON Config where
    parseJSON (Object v) = Config <$> v .: "integrationBranch" <*> v .: "deployCommand"

    -- A non-Object value is of the wrong type, so fail.
    parseJSON _          = mzero


type Program = Free Interaction
type REP = ReaderT Config (EitherT ReleaseError Program)


getLineAfterPrompt :: String -> REP String
getLineAfterPrompt prompt = lift $ liftF $ GetLineAfterPrompt prompt id

gitCreateAndCheckoutBranch :: Branch -> REP ()
gitCreateAndCheckoutBranch branch = lift $ liftF $ GitCreateAndCheckoutBranch branch ()

gitCheckoutBranch :: Branch -> REP ()
gitCheckoutBranch branch = lift $ liftF $ GitCheckoutBranch branch ()

gitCheckoutTag :: Tag -> REP ()
gitCheckoutTag tag = lift $ liftF $ GitCheckoutTag tag ()

gitTags :: REP [Tag]
gitTags = lift $ liftF $ GitTags id

gitBranches :: REP [Branch]
gitBranches = lift $ liftF $ GitBranches id

gitCheckoutNewBranchFromTag :: Branch -> Tag -> REP ()
gitCheckoutNewBranchFromTag branch tag = lift $ liftF $ GitCheckoutNewBranchFromTag branch tag ()

gitPushTags :: String -> REP ()
gitPushTags remote = lift $ liftF $ GitPushTags remote ()

gitRemoveTag :: Tag -> REP ()
gitRemoveTag tag = lift $ liftF $ GitRemoveTag tag ()

gitTag :: Tag -> REP ()
gitTag tag = lift $ liftF $ GitTag tag ()

gitRemoveBranch :: Branch -> REP ()
gitRemoveBranch branch = lift $ liftF $ GitRemoveBranch branch ()

gitMergeNoFF :: Branch -> REP ()
gitMergeNoFF branch = lift $ liftF $ GitMergeNoFF branch ()

outputMessage :: String -> REP ()
outputMessage message = lift $ liftF $ OutputMessage message ()


