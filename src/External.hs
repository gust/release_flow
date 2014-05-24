module External 
  (
    gitDeployTag
  , gitGetTags
  , gitCheckoutNewBranchFromTag
  , gitPushTags
  , gitTag
  , gitFetchTags
  )
where

import Control.Monad.Trans.Either (hoistEither)
import Control.Error (throwT)
import Control.Monad.IO.Class (liftIO)
{- import Control.Monad.Trans.Class (lift) -}

import System.Process (readProcessWithExitCode)
import System.Exit (ExitCode(..))

import Types (EIO, Tag, Branch(..), Environment(..))
import TagParsers (parsedTags)


gitDeployTag :: Tag -> Environment -> EIO ()
gitDeployTag tag env = executeExternal "DEPLOY_MIGRATIONS=true rake" [show env, "deploy:force[" ++ show tag ++ "]"] >> return ()

gitGetTags :: EIO [Tag]
gitGetTags = hoistEither . parsedTags =<< git ["tag"]

gitCheckoutNewBranchFromTag :: Branch -> Tag -> EIO ()
gitCheckoutNewBranchFromTag (Branch name) tag = git ["checkout", "-b", name, (show tag)] >> return ()

gitPushTags :: String -> Branch -> EIO ()
gitPushTags remote branch = git ["push", remote, show branch, "--tags"] >> return ()

gitTag :: Tag -> EIO ()
gitTag tag = git ["tag", show tag] >> return ()

gitFetchTags :: EIO ()
gitFetchTags = git ["fetch", "--tags"] >> return ()



git :: [String] -> EIO String
git args = executeExternal "git" args

executeExternal :: String -> [String] -> EIO String
executeExternal cmd args = do
  result <- liftIO $ readProcessWithExitCode cmd args ""
  case result of
    (ExitSuccess, stdout, _)  -> return stdout
    (ExitFailure _, _, err)   -> throwT $ "Error: " ++ err

