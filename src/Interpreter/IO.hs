module Interpreter.IO
  (interpret)
where

import           Control.Applicative        ((<$>))
import           Control.Error              (throwT)
import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Either (EitherT, hoistEither)

import           Control.Monad.Free         (Free (..))
import           System.Environment         (getArgs)
import           System.Exit                (ExitCode (..))
import           System.Process             (readProcessWithExitCode)

import           Interpreter.Commands       (Interaction (..), Program)
import           Parser.Tag                 (parsedTags)
import           Types                      (Branch (..), Environment (..), Tag)

type EIO = EitherT String IO

interpret :: Program a -> EitherT String IO a
interpret (Pure r) = return r
interpret (Free x) = case x of
  DeployTag tag env x                       -> deployTag tag env                      >>  interpret x
  GitTags f                                 -> gitTags                                >>= interpret . f
  GitCheckoutNewBranchFromTag branch tag x  -> gitCheckoutNewBranchFromTag branch tag >>  interpret x
  GitPushTags remote x                      -> gitPushTags remote                     >>  interpret x
  GitTag tag x                              -> gitTag tag                             >>  interpret x

  where
    deployTag :: Tag -> Environment -> EIO ()
    deployTag tag env = executeExternal "DEPLOY_MIGRATIONS=true rake" [show env, "deploy:force[" ++ show tag ++ "]"] >> return ()

    gitTags :: EIO [Tag]
    gitTags = git ["fetch", "--tags"] >> git ["tag"] >>= hoistEither . parsedTags

    gitCheckoutNewBranchFromTag :: Branch -> Tag -> EIO ()
    gitCheckoutNewBranchFromTag (Branch name) tag = git ["checkout", "-b", name, (show tag)] >> return ()

    gitPushTags :: String -> EIO ()
    gitPushTags remote = git ["push", remote, "--tags"] >> return ()

    gitTag :: Tag -> EIO ()
    gitTag tag = git ["tag", show tag] >> return ()


    git :: [String] -> EIO String
    git args = executeExternal "git" args

    executeExternal :: String -> [String] -> EIO String
    executeExternal cmd args = do
      result <- liftIO $ readProcessWithExitCode cmd args ""
      case result of
        (ExitSuccess, stdout, _)  -> return stdout
        (ExitFailure _, _, err)   -> throwT $ "Error: " ++ err

