{-# LANGUAGE MultiWayIf #-}

module Program.Release (program, determineReleaseState, runProgram) where

import           Control.Applicative               ((<$>))
import           Control.Monad                     ((<=<))
import           Control.Monad.Trans.Class         (lift)
import           Control.Monad.Trans.Either        (hoistEither, runEitherT, EitherT(..))
import           Data.Maybe                        (fromMaybe, fromJust, listToMaybe, isNothing)
import           Data.List                         (isPrefixOf)

import           Types                             (Branch (..),
                                                    Environment (..),
                                                    ReleaseState (..), Tag (..),
                                                    Version (..),
                                                    tmpBranch,
                                                    ReleaseError(..))

import           Interpreter.Commands              (EP, Program,
                                                    outputMessage,
                                                    getLineAfterPrompt,
                                                    gitCheckoutNewBranchFromTag,
                                                    gitCreateAndCheckoutBranch,
                                                    gitCheckoutBranch,
                                                    gitCheckoutTag, gitPushTags,
                                                    gitRemoveTag, gitRemoveBranch, gitTag,
                                                    gitTags, gitBranches, gitMergeNoFF
                                                    )

import           Tags                              (ciTagFilter,
                                                    defaultReleaseCandidateTag,
                                                    defaultReleaseTag,
                                                    getAllCandidatesForRelease,
                                                    getNextReleaseCandidateTag,
                                                    getReleaseTagFromCandidate,
                                                    isReleaseCandidateTag,
                                                    latestFilteredTag,
                                                    releaseCandidateTagFilter,
                                                    releaseTagFilter)

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
  in
  case bugfixBranch of
    Just branch -> ReleaseInProgressBugfix latestReleaseCandidate branch
    Nothing ->
      if latestReleaseCandidate > latestRelease
      then ReleaseInProgress latestReleaseCandidate
      else NoReleaseInProgress latestRelease

-- TODO handle case when there are multipe matching branches
findReleaseCandidateBugfixBranch :: Tag -> [Branch] -> Maybe Branch
findReleaseCandidateBugfixBranch releaseCandidateTag = 
  listToMaybe . filter (isPrefixOf ((show releaseCandidateTag) ++ "/bugs") . show)

program :: Program (Either ReleaseError ())
program = do
  runEitherT release

  where
    release :: EP ()
    release = do
      tags <- gitTags
      branches <- gitBranches
      case determineReleaseState tags branches of
        ReleaseInProgress latestReleaseCandidate -> do
          outputMessage $ "Release candidate found: " ++ (show latestReleaseCandidate)
          yesOrNo <- promptForYesOrNo "Is this release candidate good? y(es)/n(o):"
          case yesOrNo of
            True -> releaseCandidate latestReleaseCandidate
            False -> do
              bugBranchName <- getLineAfterPrompt "What bug are you fixing? (specify dash separated descriptor, e.g. 'theres-a-bug-in-the-code'): "
              let bugFixBranch = Branch ((show latestReleaseCandidate) ++ "/bugs/" ++ bugBranchName)
              gitCheckoutTag latestReleaseCandidate
              gitCreateAndCheckoutBranch $ tmpBranch latestReleaseCandidate
              gitCreateAndCheckoutBranch bugFixBranch
              outputMessage $ "Created branch: " ++ (show bugFixBranch) ++ ", fix your bug!"

        NoReleaseInProgress latestReleaseTag -> do
          -- checkout latest green build
          lastGreenTag <- hoistEither $ maybeToEither (ProgramExpectationError "Could not find latest green tag") $ latestFilteredTag ciTagFilter tags
          outputMessage $ "No outstanding release candidate found, starting new release candidate from: " ++ (show lastGreenTag)
          gitCheckoutTag lastGreenTag
          let releaseCandidateTag = getNextReleaseCandidateTag latestReleaseTag
          gitTag releaseCandidateTag
          outputMessage $ "Started new release: " ++ show releaseCandidateTag ++ ", deploy to preproduction and confirm the release is good to go!"
          gitPushTags "origin"

        ReleaseInProgressBugfix latestReleaseCandidate branch -> do
          outputMessage $ "Bugfix found: " ++ show branch
          yesOrNo <- promptForYesOrNo "Is the bug fixed? y(es)/n(o):"
          case yesOrNo of
            True -> do
              gitCheckoutBranch $ tmpBranch latestReleaseCandidate
              gitMergeNoFF branch
              gitRemoveBranch branch
              let nextReleaseCandidateTag = getNextReleaseCandidateTag latestReleaseCandidate
              gitTag $ nextReleaseCandidateTag
              outputMessage $ "Created new release candidate: " ++ show nextReleaseCandidateTag ++ ", you'll get it this time!"
              gitPushTags "origin"
              gitCheckoutTag nextReleaseCandidateTag
              gitRemoveBranch $ tmpBranch latestReleaseCandidate
            False -> do
              gitCheckoutBranch branch
              outputMessage "Keep fixing that code!"

      where
        releaseCandidate latestReleaseCandidate = do
          gitCheckoutTag latestReleaseCandidate
          let releaseTag = getReleaseTagFromCandidate latestReleaseCandidate
          gitTag releaseTag
          gitPushTags "origin"
          gitCheckoutTag releaseTag
          outputMessage $ "Created tag: " ++ (show releaseTag) ++ ", deploy to production cowboy!"

        maybeToEither = flip maybe Right . Left

        promptForYesOrNo :: String -> EP Bool
        promptForYesOrNo prompt = do
          parseYesOrNo <$> (getLineAfterPrompt prompt) >>= (\answer -> if isNothing answer then promptForYesOrNo prompt else return $ fromJust answer)

        parseYesOrNo :: String -> Maybe Bool
        parseYesOrNo "y"   = Just True
        parseYesOrNo "yes" = Just True
        parseYesOrNo "n"   = Just False
        parseYesOrNo "no"  = Just False
        parseYesOrNo  _    = Nothing

