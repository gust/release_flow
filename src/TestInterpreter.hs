module TestInterpreter
  (interpret, ES, defaultWorld, World(..))
where

import Control.Monad.Trans.Either (EitherT, hoistEither)
import Control.Error (throwT)
import Control.Monad.Trans.Class (lift)

import Control.Monad.Free (Free(..))
import Control.Monad.State

import Types (Tag, Branch(..), Environment(..))
import Commands -- (Program)
import TagParsers (parsedTags)


data World = World {
    wCurrentDeployment  :: Maybe Tag
  , wTags               :: [Tag]
  , wBranches           :: [(String, String)]
  , wErrors             :: [String]
  } deriving Show

defaultWorld = World {
    wCurrentDeployment  = Nothing
  , wTags               = []
  , wBranches           = []
  , wErrors             = []
  }


type ES = EitherT String (State World)

interpret :: Program a -> ES a
interpret (Pure r) = return r
interpret (Free x) = case x of
  DeployTag tag env x                       -> deployTag tag env                      >> interpret x
  GitTags f                                 -> gitTags                                >>= interpret . f
  GitCheckoutNewBranchFromTag branch tag x  -> gitCheckoutNewBranchFromTag branch tag >> interpret x
  GitPushTags remote branch x               -> gitPushTags remote branch              >> interpret x
  GitTag tag x                              -> gitTag tag                             >> interpret x

  where
    deployTag :: Tag -> Environment -> ES ()
    deployTag tag env = do -- executeExternal "DEPLOY_MIGRATIONS=true rake" [show env, "deploy:force[" ++ show tag ++ "]"] >> return ()
      w <- get
      put w{wCurrentDeployment = Just tag}

    gitTags :: ES [Tag]
    gitTags = do -- git ["fetch", "--tags"] >> git ["tag"] >>= hoistEither . parsedTags
      w <- get
      return $ wTags w

    gitCheckoutNewBranchFromTag :: Branch -> Tag -> ES ()
    gitCheckoutNewBranchFromTag (Branch name) tag = do -- git ["checkout", "-b", name, (show tag)] >> return ()
      w <- get
      put w{wBranches = (name, show tag):(wBranches w)}

    gitPushTags :: String -> Branch -> ES ()
    gitPushTags remote branch = do -- git ["push", remote, show branch, "--tags"] >> return ()
      return ()

    gitTag :: Tag -> ES ()
    gitTag tag = do -- git ["tag", show tag] >> return ()
      return ()

