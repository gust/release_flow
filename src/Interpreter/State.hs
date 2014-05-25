module Interpreter.State (
    interpret
    , ES
    , defaultWorld
    , World(..)
    )
where

import Control.Monad.Trans.Either (EitherT, hoistEither)
import Control.Error (throwT)
import Control.Monad.Trans.Class (lift)

import Control.Monad.Free (Free(..))
import Control.Monad.State.Strict (State, runState, get, put)

import Types (Tag, Branch(..), Environment(..))
import Interpreter.Commands (Program, Interaction(..))
import Parser.Tag (parsedTags)


data World = World {
    wReleaseBranchName  :: String
  , wCurrentDeployment  :: Maybe Tag
  , wTags               :: [Tag]
  , wBranches           :: [(String, String)]
  , wLog                :: [String]
  } deriving (Eq, Show)

defaultWorld = World {
    wReleaseBranchName  = "bananas"
  , wCurrentDeployment  = Nothing
  , wTags               = []
  , wBranches           = []
  , wLog                = []
  }


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
      return $ Branch $ wReleaseBranchName w

    deployTag :: Tag -> Environment -> ES ()
    deployTag tag env = do -- executeExternal "DEPLOY_MIGRATIONS=true rake" [show env, "deploy:force[" ++ show tag ++ "]"] >> return ()
      w <- get
      put $! w{wCurrentDeployment = Just tag}

    gitTags :: ES [Tag]
    gitTags = do -- git ["fetch", "--tags"] >> git ["tag"] >>= hoistEither . parsedTags
      w <- get
      return $ wTags w

    gitCheckoutNewBranchFromTag :: Branch -> Tag -> ES ()
    gitCheckoutNewBranchFromTag (Branch name) tag = do -- git ["checkout", "-b", name, (show tag)] >> return ()
      w <- get
      put $! w{wBranches = (name, show tag):(wBranches w)}

    gitPushTags :: String -> Branch -> ES ()
    gitPushTags remote branch = do -- git ["push", remote, show branch, "--tags"] >> return ()
      -- TODO
      return ()

    gitTag :: Tag -> ES ()
    gitTag tag = do -- git ["tag", show tag] >> return ()
      -- TODO
      return ()

