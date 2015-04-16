{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Program.Release (program, determineReleaseState, runProgram) where

import           Control.Applicative               ((<$>))
import           Control.Monad                     ((<=<))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Either        (hoistEither, runEitherT, EitherT(..))
import           Data.Maybe                        (fromMaybe, listToMaybe, isNothing)
import           Data.List                         (isPrefixOf, intercalate)
import Control.Monad.Reader (runReaderT, ask)
import Text.StringTemplate (newSTMP, render, setAttribute)

import           Types                             (Branch (..),
                                                    Environment (..),
                                                    ReleaseState (..), Tag (..),
                                                    Version (..), isNextPatchOf,
                                                    tmpBranch,
                                                    ReleaseError(..), infoMessage, promptMessage)

import           Interpreter.Commands              (REP, Program, Config(..),
                                                    outputMessage,
                                                    getLineAfterPrompt,
                                                    gitCheckoutNewBranchFromTag,
                                                    gitCreateAndCheckoutBranch,
                                                    gitCheckoutBranch,
                                                    gitCheckoutTag, gitPushTags,
                                                    gitRemoveTag, gitRemoveBranch, gitTag,
                                                    gitTags, gitBranches, gitMergeNoFF,
                                                    gitPush, gitPullRebase
                                                    )

import           Tags                              (ciTagFilter,
                                                    defaultReleaseCandidateTag,
                                                    defaultReleaseTag,
                                                    getAllCandidatesForRelease,
                                                    getReleaseTagFromCandidate,
                                                    isReleaseCandidateTag,
                                                    latestFilteredTag,
                                                    releaseCandidateTagFilter,
                                                    releaseTagFilter,
                                                    getNextMinorReleaseCandidateTag,
                                                    getNextPatchReleaseCandidateTag)

runProgram :: (Monad m, Functor m) => (Program (Either ReleaseError b) -> (EitherT ReleaseError m (Either ReleaseError b))) -> Program (Either ReleaseError b) -> m (Either ReleaseError b)
runProgram interpreter program =
  collapseErrors <$> (runEitherT $ interpreter program)
  where
    collapseErrors :: (Either ReleaseError (Either ReleaseError b)) -> (Either ReleaseError b)
    collapseErrors = (>>= id)

determineReleaseState :: [Tag] -> [Branch] -> ReleaseState
determineReleaseState tags branches =
  let
    latestReleaseCandidate = fromMaybe
      defaultReleaseCandidateTag $
      latestFilteredTag releaseCandidateTagFilter tags
    bugfixBranch = findReleaseCandidateBugfixBranch latestReleaseCandidate branches
    latestRelease = fromMaybe
      defaultReleaseTag $
      latestFilteredTag releaseTagFilter tags
    maybeHotfixBranch = findHotfixBranch latestRelease branches
  in
  case maybeHotfixBranch of
    Just hotfixBranch -> HotfixInProgress latestRelease hotfixBranch
    Nothing -> 
      case bugfixBranch of
        Just branch -> ReleaseInProgressBugfix latestReleaseCandidate branch
        Nothing ->
          if latestReleaseCandidate > latestRelease
          then ReleaseInProgress latestRelease latestReleaseCandidate
          else NoReleaseInProgress latestRelease

-- TODO handle case when there are multipe matching branches
findReleaseCandidateBugfixBranch :: Tag -> [Branch] -> Maybe Branch
findReleaseCandidateBugfixBranch releaseCandidateTag = 
  listToMaybe . filter (isPrefixOf ((show releaseCandidateTag) ++ "/bugs") . show)

findHotfixBranch :: Tag -> [Branch] -> Maybe Branch
findHotfixBranch releaseTag =
  listToMaybe . filter (isPrefixOf ((show releaseTag) ++ "/hotfix") . show)

data Adventure = StartNewRelease | StartHotfix deriving (Enum, Bounded)
instance Show Adventure where
  show StartNewRelease = "Start new release"
  show StartHotfix = "Start hotfix"

program :: Config -> Program (Either ReleaseError ())
program = runEitherT . runReaderT release

  where
    release :: REP ()
    release = do
      tags <- gitTags
      branches <- gitBranches
      case determineReleaseState tags branches of
        ReleaseInProgress latestReleaseTag latestReleaseCandidate -> do
          outputMessage $ "Release candidate found: " ++ (show latestReleaseCandidate)
          yesOrNo <- promptForYesOrNo "Is this release candidate good? y(es)/n(o)"


          case yesOrNo of
            True -> releaseCandidate latestReleaseTag latestReleaseCandidate
            False -> do
              bugBranchName <- getLineAfterPrompt "What bug are you fixing? (specify dash separated descriptor, e.g. 'theres-a-bug-in-the-code')"
              let bugFixBranch = Branch ((show latestReleaseCandidate) ++ "/bugs/" ++ bugBranchName)
              gitCheckoutTag latestReleaseCandidate
              gitCreateAndCheckoutBranch $ tmpBranch latestReleaseCandidate
              gitCreateAndCheckoutBranch bugFixBranch
              outputMessage $ "Created branch: " ++ (show bugFixBranch) ++ ", fix your bug!"

        NoReleaseInProgress latestReleaseTag -> do
          -- ask whether to do a new release or a hotfix
          choice <- promptForChoice "Choose your adventure" [StartNewRelease, StartHotfix]


          case choice of
            StartNewRelease -> do
              -- checkout latest green build
              lastGreenTag <- lift $ hoistEither $ maybeToEither (ProgramExpectationError "Could not find latest green tag") $ latestFilteredTag ciTagFilter tags
              gitCheckoutTag lastGreenTag
              let releaseCandidateTag = getNextMinorReleaseCandidateTag latestReleaseTag
              gitTag releaseCandidateTag
              outputMessage $ "Started new release: " ++ show releaseCandidateTag 
              outputMessage $ "Deploy to preproduction and confirm the release is good to go!"
              command <- (newSTMP . deployCommand) <$> ask 
              outputMessage $ render $ setAttribute "environment" "preproduction" $ setAttribute "tag" (show releaseCandidateTag) command
              gitPushTags "origin"
            StartHotfix -> do
              hotfixName <- getLineAfterPrompt "What is the hotfix for? (specify dash separated descriptor, e.g. 'signup-is-broken')"
              gitCheckoutTag latestReleaseTag
              let hotfixBranch = Branch ((show latestReleaseTag) ++ "/hotfix/" ++ hotfixName)
              gitCreateAndCheckoutBranch hotfixBranch 
              outputMessage $ "Started hotfix: " ++ (show hotfixBranch) ++ ", fix stuff!"

        ReleaseInProgressBugfix latestReleaseCandidate branch -> do
          outputMessage $ "Bugfix found: " ++ show branch
          yesOrNo <- promptForYesOrNo "Is the bug fixed? y(es)/n(o)"
          case yesOrNo of
            True -> do
              gitCheckoutBranch $ tmpBranch latestReleaseCandidate
              gitMergeNoFF branch
              gitRemoveBranch branch
              let nextReleaseCandidateTag = getNextMinorReleaseCandidateTag latestReleaseCandidate
              gitTag $ nextReleaseCandidateTag
              outputMessage $ "Created new release candidate: " ++ show nextReleaseCandidateTag ++ ", you'll get it this time!"
              gitPushTags "origin"
              gitCheckoutTag nextReleaseCandidateTag
              gitRemoveBranch $ tmpBranch latestReleaseCandidate
            False -> do
              gitCheckoutBranch branch
              outputMessage "Keep fixing that code!"

        HotfixInProgress releaseTag hotfixBranch -> do
          outputMessage $ "Hotfix found: " ++ show hotfixBranch
          gitCheckoutBranch hotfixBranch
          yesOrNo <- promptForYesOrNo "Is the hotfix complete? y(es)/n(o)"
          case yesOrNo of
            True -> do
              let nextReleaseCandidateTag = getNextPatchReleaseCandidateTag releaseTag
              gitTag nextReleaseCandidateTag -- "git tag release/1.2.4-rc1"
              gitPushTags "origin" -- "git push --tags"
              gitCheckoutTag nextReleaseCandidateTag -- "git checkout release/1.2.4-rc1"
              gitRemoveBranch hotfixBranch -- "git branch -d release/1.2.3/hotfix/hot-fixing", "git push origin :release/1.2.3/hotfix/hot-fixing"
              outputMessage $ "Started new release: " ++ show nextReleaseCandidateTag
              outputMessage $ "Deploy to preproduction and confirm the release is good to go!"

            False -> do
              outputMessage "Keep fixing that code!"


        unhandledState -> error $ "State is not handled: " ++ show unhandledState

      where
        promptForChoice :: (Show a, Enum a, Bounded a) => String -> [a] -> REP a
        promptForChoice prompt choices = do
          parseChoice <$> (getLineAfterPrompt promptWithChoices) >>= (maybe (promptForChoice prompt choices) return)
          where
            promptWithChoices :: String
            promptWithChoices = prompt ++ ": " ++ (intercalate ", " (map showChoice choices))

            showChoice :: (Show a, Enum a) => a -> String
            showChoice choice = (show choice) ++ " (" ++ (show $ fromEnum choice) ++ ")"

            parseChoice :: (Enum a, Bounded a) => String -> Maybe a
            parseChoice = safeToEnum . read

        promptForYesOrNo :: String -> REP Bool
        promptForYesOrNo prompt = do
          parseYesOrNo <$> (getLineAfterPrompt prompt) >>= (maybe (promptForYesOrNo prompt) return)

        parseYesOrNo :: String -> Maybe Bool
        parseYesOrNo "y"   = Just True
        parseYesOrNo "yes" = Just True
        parseYesOrNo "n"   = Just False
        parseYesOrNo "no"  = Just False
        parseYesOrNo  _    = Nothing

        releaseCandidate latestReleaseTag latestReleaseCandidate = do
          gitCheckoutTag latestReleaseCandidate
          let releaseTag = getReleaseTagFromCandidate latestReleaseCandidate
          gitTag releaseTag
          gitPushTags "origin"

          if  version latestReleaseCandidate `isNextPatchOf` version latestReleaseTag
            then do
              let integration = Branch "integration"
              gitCheckoutBranch integration -- "git checkout integration"
              gitPullRebase -- "git pull --rebase"
              gitMergeNoFF releaseTag -- "git merge --no-ff release/<patch release>
              gitPush "origin" integration

              gitCheckoutTag releaseTag
              outputReleaseMessages releaseTag
              command <- (newSTMP . deployCommand) <$> ask 
              outputMessage $ render $ setAttribute "environment" "production" $ setAttribute "tag" (show releaseTag) command

            else do
              gitCheckoutTag releaseTag
              outputReleaseMessages releaseTag
              command <- (newSTMP . deployCommand) <$> ask 
              outputMessage $ render $ setAttribute "environment" "production" $ setAttribute "tag" (show releaseTag) command

        outputReleaseMessages releaseTag = do
          outputMessage $ "Created tag: " ++ (show releaseTag)
          outputMessage "Deploy to production cowboy!"

        maybeToEither = flip maybe Right . Left

safeToEnum :: forall t . (Enum t, Bounded t) => Int -> Maybe t
safeToEnum i =
  if (i >= fromEnum (minBound :: t)) && (i <= fromEnum (maxBound :: t))
    then Just . toEnum $ i
    else Nothing
