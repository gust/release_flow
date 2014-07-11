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

import           Control.Monad.Free         (Free (..))
import           Control.Monad.State.Strict (State, get, put, runState)

import           Interpreter.Commands       (Interaction (..), Program)
import           Parser.Tag                 (parsedTags)
import           Types                      (Branch (..), Environment (..), Tag)
import Control.Lens ((%=), (^.), makeLenses)
import Data.Functor ((<$>))

data Input = Input {
    _iReleaseBranchName :: String
  , _iTags              :: [Tag]
  , _iBranches          :: [(String, String)]
  , _iUserInput         :: [String]
} deriving (Eq, Show)

data Output = Output {
    _oCommands :: [String]
  , _oLog      :: [String]
} deriving (Eq, Show)

data World = World {
    _wInput   :: Input
  , _wOutput  :: Output
} deriving (Eq, Show)

defaultInput = Input {
    _iReleaseBranchName = "bananas"
  , _iTags              = []
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
  GetReleaseBranch f                        -> getReleaseBranch                       >>= interpret . f
  DeployTag tag env x                       -> deployTag tag env                      >>  interpret x
  GitTags f                                 -> gitTags                                >>= interpret . f
  GitCheckoutNewBranchFromTag branch tag x  -> gitCheckoutNewBranchFromTag branch tag >>  interpret x
  GitPushTags remote branch x               -> gitPushTags remote branch              >>  interpret x
  GitTag tag x                              -> gitTag tag                             >>  interpret x

  where
    getReleaseBranch :: ES Branch
    getReleaseBranch = do -- Branch . head <$> lift getArgs
      w <- get
      return $ Branch $ w^.wInput.iReleaseBranchName

    deployTag :: Tag -> Environment -> ES ()
    deployTag tag env = do -- executeExternal "DEPLOY_MIGRATIONS=true rake" [show env, "deploy:force[" ++ show tag ++ "]"] >> return ()
      wOutput . oCommands %= (++ ["deploy " ++ show tag])

    gitTags :: ES [Tag]
    gitTags = do -- git ["fetch", "--tags"] >> git ["tag"] >>= hoistEither . parsedTags
      w <- get
      return $ w^.wInput.iTags

    gitCheckoutNewBranchFromTag :: Branch -> Tag -> ES ()
    gitCheckoutNewBranchFromTag (Branch name) tag = do -- git ["checkout", "-b", name, (show tag)] >> return ()
      wOutput . oCommands %= (++ ["checkout branch " ++ name ++ " from tag " ++ show tag])

    gitPushTags :: String -> Branch -> ES ()
    gitPushTags remote branch = do -- git ["push", remote, show branch, "--tags"] >> return ()
      -- TODO
      return ()

    gitTag :: Tag -> ES ()
    gitTag tag = do -- git ["tag", show tag] >> return ()
      -- TODO
      return ()

