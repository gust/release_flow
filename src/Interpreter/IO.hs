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
import           System.IO                  (stdout, hFlush)

import           Interpreter.Commands       (Interaction (..), Program)
import           Parser.Tag                 (parsedTags)
import           Parser.Branch              (parsedBranches)
import           Types                      (Branch (..), Environment (..), Tag, ReleaseError(..))


type EIO = EitherT ReleaseError IO

interpret :: Program a -> EIO a
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
  GitRemoveBranch branch x                  -> gitRemoveBranch branch                 >>  interpret x
  GitRemoveTag tag x                        -> gitRemoveTag tag                       >>  interpret x
  GitTag tag x                              -> gitTag tag                             >>  interpret x
  GitMergeNoFF branch x                     -> gitMergeNoFF branch                    >>  interpret x
  OutputMessage message x                   -> outputMessage message                  >>  interpret x
  _                                         -> error $ "Interpreter Error: no match for command in IO interpreter: " ++ (show x)

  where
    getLineAfterPrompt :: String -> EIO String
    getLineAfterPrompt prompt = liftIO $ do
      putStr $ prompt ++ " "
      hFlush stdout
      getLine

    gitCheckoutTag :: Tag -> EIO ()
    gitCheckoutTag tag = git ["checkout", (show tag)] >> return ()

    gitCheckoutBranch :: Branch -> EIO ()
    gitCheckoutBranch branch = git ["checkout", (show branch)] >> return ()

    gitCreateAndCheckoutBranch :: Branch -> EIO ()
    gitCreateAndCheckoutBranch branch = git ["checkout", "-b", (show branch)] >> return ()

    gitBranches :: EIO [Branch]
    gitBranches = parsedBranches <$> git ["branch"]

    gitTags :: EIO [Tag]
    gitTags = parsedTags <$> (git ["fetch", "--tags"] >> git ["tag"])

    gitCheckoutNewBranchFromTag :: Branch -> Tag -> EIO ()
    gitCheckoutNewBranchFromTag (Branch name) tag = git ["checkout", "-b", name, (show tag)] >> return ()

    gitPushTags :: String -> EIO ()
    gitPushTags remote = git ["push", remote, "--tags"] >> return ()

    gitTag :: Tag -> EIO ()
    gitTag tag = git ["tag", show tag] >> return ()

    gitRemoveTag :: Tag -> EIO ()
    gitRemoveTag tag = do
      git ["tag", "-d", show tag] >> git_ ["push", "origin", ":refs/tags/" ++ (show tag)] >> return ()

    gitRemoveBranch :: Branch -> EIO ()
    gitRemoveBranch branch = do
      git ["branch", "-d", show branch] >> git_ ["push", "origin", ":" ++ (show branch)] >> return ()

    gitMergeNoFF :: Branch -> EIO ()
    gitMergeNoFF branch = do
      git ["merge", "--no-ff", show branch] >> return ()


    git :: [String] -> EIO String
    git = execOrThrow "git"

    git_ :: [String] -> EIO String
    git_ = execIgnoringErrors "git"

    execIgnoringErrors :: String -> [String]-> EIO String
    execIgnoringErrors cmd args = (exec cmd args) >>= either return return

    execOrThrow :: String -> [String] -> EIO String
    execOrThrow cmd args = (exec cmd args) >>= either (throwT . ExecutionError) return

    exec :: String -> [String] -> EIO (Either String String)
    exec cmd args = do
      result <- liftIO $ readProcessWithExitCode cmd args ""
      case result of
        (ExitSuccess, stdout, _)  -> return $ Right stdout
        (ExitFailure _, _, err)   -> return $ Left err

    outputMessage :: String -> EIO ()
    outputMessage = liftIO . putStrLn

