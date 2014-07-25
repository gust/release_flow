{-# LANGUAGE TemplateHaskell #-}

module Interpreter.State (
    interpret
    , ES
    , defaultInput
    , Input(..)
    , initialOutput
    , Output(..)
    , defaultWorld
    , World(..)
    )
where

import           Control.Error              (throwT)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (EitherT, hoistEither)
import qualified Data.Map                   as M
import           Data.Maybe                 (fromJust)

import           Control.Monad.Free         (Free (..))
import           Control.Monad.State.Strict (State, get, put, runState)

import           Control.Lens               (makeLenses, (%=), (^.))
import           Data.Functor               ((<$>))
import           Interpreter.Commands       (Interaction (..), Program)
import           Parser.Tag                 (parsedTags)
import           Types                      (Branch (..), Environment (..), Tag)

data Input = Input {
    _iTags      :: [Tag]
  , _iBranches  :: [Branch]
  , _iUserInput :: [(String, String)]
} deriving (Eq, Show)

data Output = Output {
    _oCommands :: [String]
  , _oLog      :: [String]
} deriving (Eq, Show)

data World = World {
    _wInput  :: Input
  , _wOutput :: Output
} deriving (Eq, Show)

defaultInput = Input {
    _iTags              = []
  , _iBranches          = []
  , _iUserInput         = []
}

initialOutput = Output {
    _oCommands = []
  , _oLog      = []
}

defaultWorld = World {
    _wInput  = defaultInput
  , _wOutput = initialOutput
}

makeLenses ''Input
makeLenses ''Output
makeLenses ''World

type ES = EitherT String (State World)

interpret :: Program a -> ES a
interpret (Pure r) = return r
interpret (Free x) = case x of
  GetLineAfterPrompt prompt f               -> getLineAfterPrompt prompt              >>= interpret . f
  GitCreateAndCheckoutBranch branch x       -> gitCreateAndCheckoutBranch branch      >>  interpret x
  GitCheckoutBranch branch x                -> gitCheckoutBranch branch               >>  interpret x
  GitCheckoutTag tag x                      -> gitCheckoutTag tag                     >>  interpret x
  DeployTag tag env x                       -> deployTag tag env                      >>  interpret x
  GitTags f                                 -> gitTags                                >>= interpret . f
  GitBranches f                             -> gitBranches                            >>= interpret . f
  GitCheckoutNewBranchFromTag branch tag x  -> gitCheckoutNewBranchFromTag branch tag >>  interpret x
  GitPushTags remote x                      -> gitPushTags remote                     >>  interpret x
  GitRemoveBranch branch x                  -> gitRemoveBranch branch                 >>  interpret x
  GitRemoveTag tag x                        -> gitRemoveTag tag                       >>  interpret x
  GitTag tag x                              -> gitTag tag                             >>  interpret x
  GitMergeNoFF branch x                     -> gitMergeNoFF branch                    >>  interpret x
  _                                         -> error "command does not match in State interpreter"

  where
    getLineAfterPrompt :: String -> ES String
    getLineAfterPrompt prompt = do
      w <- get
      return $ fromJust $ M.lookup prompt $ M.fromList $ w^.wInput.iUserInput

    gitCheckoutBranch :: Branch -> ES ()
    gitCheckoutBranch branch =
      wOutput . oCommands %= (++ ["git checkout " ++ show branch])

    gitCreateAndCheckoutBranch :: Branch -> ES ()
    gitCreateAndCheckoutBranch branch =
      wOutput . oCommands %= (++ ["git checkout -b " ++ show branch])

    gitCheckoutTag :: Tag -> ES ()
    gitCheckoutTag tag =
      wOutput . oCommands %= (++ ["git checkout " ++ show tag])

    deployTag :: Tag -> Environment -> ES ()
    deployTag tag env = do -- executeExternal "DEPLOY_MIGRATIONS=true rake" [show env, "deploy:force[" ++ show tag ++ "]"] >> return ()
      wOutput . oCommands %= (++ ["deploy " ++ show tag])

    gitTags :: ES [Tag]
    gitTags = do -- git ["fetch", "--tags"] >> git ["tag"] >>= hoistEither . parsedTags
      wOutput . oCommands %= (++ ["git tags"])
      w <- get
      return $ w^.wInput.iTags

    gitBranches :: ES [Branch]
    gitBranches = do -- git ["fetch", "--tags"] >> git ["tag"] >>= hoistEither . parsedTags
      wOutput . oCommands %= (++ ["git branch"])
      w <- get
      return $ w^.wInput.iBranches

    gitCheckoutNewBranchFromTag :: Branch -> Tag -> ES ()
    gitCheckoutNewBranchFromTag (Branch name) tag = do -- git ["checkout", "-b", name, (show tag)] >> return ()
      wOutput . oCommands %= (++ ["checkout branch " ++ name ++ " from tag " ++ show tag])

    gitPushTags :: String -> ES ()
    gitPushTags remote = do -- git ["push", remote, show branch, "--tags"] >> return ()
      wOutput . oCommands %= (++ ["git push " ++ remote ++ " --tags"])

    gitRemoveTag :: Tag -> ES ()
    gitRemoveTag tag = do
      wOutput . oCommands %= (++ ["git tag -d " ++ show tag])
      wOutput . oCommands %= (++ ["git push origin :refs/tags/" ++ show tag])

    gitTag :: Tag -> ES ()
    gitTag tag = do -- git ["tag", show tag] >> return ()
      wOutput . oCommands %= (++ ["git tag " ++ show tag])


    gitRemoveBranch :: Branch -> ES ()
    gitRemoveBranch branch = do
      wOutput . oCommands %= (++ ["git branch -d " ++ show branch])
      wOutput . oCommands %= (++ ["git push origin :" ++ show branch])

    gitMergeNoFF :: Branch -> ES ()
    gitMergeNoFF branch = do
      wOutput . oCommands %= (++ ["git merge --no-ff " ++ show branch])



