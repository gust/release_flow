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
import           Parser.Branch              (parsedBranches)
import           Types                      (Branch (..), Environment (..), Tag)

type EIO = EitherT String IO

interpret :: Program a -> EitherT String IO a
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
  _                                         -> error "command does not match in IO interpreter"

  where
    getLineAfterPrompt :: String -> EIO String
    getLineAfterPrompt prompt = liftIO $ do
      putStrLn prompt
      getLine

    gitCheckoutTag :: Tag -> EIO ()
    gitCheckoutTag tag = git ["checkout", (show tag)] >> return ()

    gitCheckoutBranch :: Branch -> EIO ()
    gitCheckoutBranch branch = git ["checkout", (show branch)] >> return ()

    gitCreateAndCheckoutBranch :: Branch -> EIO ()
    gitCreateAndCheckoutBranch branch = git ["checkout", "-b", (show branch)] >> return ()

    gitBranches :: EIO [Branch]
    gitBranches = git ["branch"] >>= hoistEither . parsedBranches

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

    gitRemoveTag :: Tag -> EIO ()
    gitRemoveTag tag = do
      git ["tag", "-d", show tag] >> git ["push", "origin", ":refs/tags/" ++ (show tag)] >> return ()

    gitRemoveBranch :: Branch -> EIO ()
    gitRemoveBranch branch = do
      git ["branch", "-d", show branch] >> git ["push", "origin", ":" ++ (show branch)] >> return ()

    gitMergeNoFF :: Branch -> EIO ()
    gitMergeNoFF branch = do
      git ["merge", "--no-ff", show branch] >> return ()


    git :: [String] -> EIO String
    git args = executeExternal "git" args

    executeExternal :: String -> [String] -> EIO String
    executeExternal cmd args = do
      result <- liftIO $ readProcessWithExitCode cmd args ""
      case result of
        (ExitSuccess, stdout, _)  -> return stdout
        (ExitFailure _, _, err)   -> throwT $ "Error: " ++ err

