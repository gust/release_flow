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
import           Data.List                  (intercalate)

import           Control.Monad.Free         (Free (..))
import           Control.Monad.State.Strict (State, get, put, runState)

import           Control.Lens               (makeLenses, (%=), (^.))
import           Data.Functor               ((<$>))
import           Interpreter.Commands       (Interaction (..), Program)
import           Parser.Tag                 (parsedTags)
import           Types                      (Branch (..), Environment (..), Tag, ReleaseError(..), Message)

data Input = Input {
    _iTags      :: [Tag]
  , _iBranches  :: [Branch]
  , _iUserInput :: [(String, String)]
} deriving (Eq, Show)

data Output = Output {
    _oCommands  :: [String]
  , _oStdOut    :: [String]
  , _oStdErr    :: [String]
} deriving (Eq, Show)

data World = World {
    _wInput     :: Input
  , _wOutput    :: Output
} deriving (Eq, Show)

defaultInput = Input {
    _iTags              = []
  , _iBranches          = []
  , _iUserInput         = []
}

initialOutput = Output {
    _oCommands  = []
  , _oStdOut    = []
  , _oStdErr    = []
}

defaultWorld = World {
    _wInput  = defaultInput
  , _wOutput = initialOutput
}

makeLenses ''Input
makeLenses ''Output
makeLenses ''World

type ES = EitherT ReleaseError (State World)

interpret :: Program a -> ES a
interpret (Pure r) = return r
interpret (Free x) = case x of
  GetLineAfterPrompt prompt f               -> getLineAfterPrompt prompt              >>= interpret . f
  GitCreateAndCheckoutBranch branch x       -> gitCreateAndCheckoutBranch branch      >>  interpret x
  GitCheckoutBranch branch x                -> gitCheckoutBranch branch               >>  interpret x
  GitCheckoutTag tag x                      -> gitCheckoutTag tag                     >>  interpret x
  GitTags f                                 -> gitTags                                >>= interpret . f
  GitBranches f                             -> gitBranches                            >>= interpret . f
  GitCheckoutNewBranchFromTag branch tag x  -> gitCheckoutNewBranchFromTag branch tag >>  interpret x
  GitPushTags remote x                      -> gitPushTags remote                     >>  interpret x
  GitPush remote branch x                   -> gitPush remote branch                  >>  interpret x
  GitRemoveBranch branch x                  -> gitRemoveBranch branch                 >>  interpret x
  GitRemoveTag tag x                        -> gitRemoveTag tag                       >>  interpret x
  GitTag tag x                              -> gitTag tag                             >>  interpret x
  GitMergeNoFF commitish x                  -> gitMergeNoFF commitish                 >>  interpret x
  GitPullRebase x                           -> gitPullRebase                          >>  interpret x
  OutputMessage message x                   -> logMessage message                     >>  interpret x
  _                                         -> error $ "Interpreter Error: no match for command in State interpreter: " ++ (show x)

  where
    getLineAfterPrompt :: String -> ES String
    getLineAfterPrompt prompt = do
      w <- get
      case M.lookup prompt (promptToAnswerMap w) of
        Just answer -> return answer
        Nothing -> error $ "No corresponding input for prompt: \"" ++ prompt ++ "\", available prompts are:\n" ++ intercalate "\n" (map (\s -> "\t- " ++ s) (availablePrompts w))
      where
        promptToAnswerMap w = M.fromList $ w^.wInput.iUserInput
        availablePrompts w = M.keys (promptToAnswerMap w) :: [String]

    gitCheckoutBranch :: Branch -> ES ()
    gitCheckoutBranch branch =
      wOutput . oCommands %= (++ ["git checkout " ++ show branch])

    gitCreateAndCheckoutBranch :: Branch -> ES ()
    gitCreateAndCheckoutBranch branch =
      wOutput . oCommands %= (++ ["git checkout -b " ++ show branch])

    gitCheckoutTag :: Tag -> ES ()
    gitCheckoutTag tag =
      wOutput . oCommands %= (++ ["git checkout " ++ show tag])

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
    gitPushTags remote = do -- git ["push", remote, "--tags"] >> return ()
      wOutput . oCommands %= (++ ["git push " ++ remote ++ " --tags"])

    gitPush :: String -> Branch -> ES ()
    gitPush remote branch = do -- git ["push", remote, show branch] >> return ()
      wOutput . oCommands %= (++ ["git push " ++ remote ++ " " ++ show branch])

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

    gitMergeNoFF :: Show a => a -> ES ()
    gitMergeNoFF commitish =
      wOutput . oCommands %= (++ ["git merge --no-ff " ++ show commitish])

    gitPullRebase :: ES ()
    gitPullRebase = 
      wOutput . oCommands %= (++ ["git pull --rebase"])

    logMessage :: String -> ES ()
    logMessage message = wOutput . oStdOut %= (++ [message])

