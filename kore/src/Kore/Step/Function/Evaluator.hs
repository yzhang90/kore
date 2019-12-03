{-|
Module      : Kore.Step.Function.Evaluator
Description : Evaluates functions in a pattern.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Function.Evaluator
    ( evaluateApplication
    , evaluatePattern
    ) where

import Control.Error
    ( ExceptT
    , MaybeT (..)
    , exceptT
    , maybeT
    , throwE
    )
import Control.Exception
    ( assert
    )
import qualified Control.Monad.Trans as Trans
import qualified Data.Foldable as Foldable
import Data.Function
import qualified Data.Text.Prettyprint.Doc as Pretty

import qualified Branch as BranchT
import Kore.Attribute.Synthetic
import Kore.Debug
import qualified Kore.Internal.MultiOr as MultiOr
    ( flatten
    , merge
    )
import Kore.Internal.OrPattern
    ( OrPattern
    )
import qualified Kore.Internal.OrPattern as OrPattern
import Kore.Internal.Pattern
    ( Condition
    , Conditional (..)
    , Pattern
    )
import qualified Kore.Internal.Pattern as Pattern
import qualified Kore.Internal.Symbol as Symbol
import Kore.Internal.TermLike as TermLike
import Kore.Logger
    ( MonadLog (..)
    , withLogScope
    , logDebug
    )
import qualified Kore.Profiler.Profile as Profile
    ( axiomBranching
    , axiomEvaluation
    , equalitySimplification
    , mergeSubstitutions
    , resimplification
    )
import Kore.Step.Axiom.Identifier
    ( AxiomIdentifier
    )
import qualified Kore.Step.Axiom.Identifier as AxiomIdentifier
import qualified Kore.Step.Function.Memo as Memo
import Kore.Step.Simplification.Simplify as AttemptedAxiom
    ( AttemptedAxiom (..)
    )
import Kore.Step.Simplification.Simplify as Simplifier
import qualified Kore.Step.Simplification.Simplify as AttemptedAxiomResults
    ( AttemptedAxiomResults (..)
    )
import Kore.TopBottom
import Kore.Unparser

import qualified Data.Text as Text

{-| Evaluates functions on an application pattern.
-}
-- TODO (thomas.tuegel): Factor out a "function evaluator" object.
-- See also: Kore.Step.Function.Memo.Self
-- Then add a function,
--   memoize :: Evaluator.Self state -> Memo.Self state -> Evaluator.Self state
-- to add memoization to a function evaluator.
evaluateApplication
    :: forall variable simplifier
    .  ( SimplifierVariable variable
       , MonadSimplify simplifier
       )
    => Condition variable
    -- ^ The predicate from the configuration
    -> Condition variable
    -- ^ Aggregated children predicate and substitution.
    -> Application Symbol (TermLike variable)
    -- ^ The pattern to be evaluated
    -> simplifier (OrPattern variable)
evaluateApplication
    configurationCondition
    childrenCondition
    (evaluateSortInjection -> application)
  = finishT $ do
    Foldable.for_ canMemoize recallOrPattern
    results <-
        maybeEvaluatePattern
            childrenCondition
            termLike
            unevaluated
            configurationCondition
        & maybeT (return unevaluated) return
        & Trans.lift
    Foldable.for_ canMemoize (recordOrPattern results)
    return results
  where
    finishT :: ExceptT r simplifier r -> simplifier r
    finishT = exceptT return return

    Application { applicationSymbolOrAlias = symbol } = application

    termLike = synthesize (ApplySymbolF application)
    unevaluated =
        OrPattern.fromPattern
        $ Pattern.withCondition
            (markSimplifiedIfChildren termLike)
            childrenCondition

    markSimplifiedIfChildren =
        if all TermLike.isSimplified application
           then TermLike.markSimplified
           else id

    canMemoize
      | Symbol.isMemo symbol
      , isTop childrenCondition
      , isTop configurationCondition
      = traverse asConcrete application
      | otherwise
      = Nothing

    recallOrPattern key = do
        Memo.Self { recall } <- askMemo
        maybeTermLike <- recall key
        let maybeOrPattern =
                OrPattern.fromTermLike . fromConcrete <$> maybeTermLike
        Foldable.for_ maybeOrPattern throwE

    recordOrPattern orPattern key
      | [result] <- OrPattern.toPatterns orPattern
      , Just term <- asConcrete (Pattern.term result)
      -- If the pattern and predicate are concrete, then we expect the predicate
      -- to be fully-evaluated, so it must be Top. It may not be fully-evaluated
      -- due to some uninterpreted function or an unsolved unification problem;
      -- these are not errors, but they are unusual enough that we don't want to
      -- deal with them here.
      , isTop (Pattern.predicate result)
      -- We already checked that childrenCondition has no substitutions, so we
      -- don't expect the result to have any substitutions either. As with the
      -- predicate, it might be possible to have a substitution in some cases,
      -- but they are unusual enough that we don't need to deal with them here.
      , isTop (Pattern.substitution result)
      = do
        Memo.Self { record } <- askMemo
        record key term
      | otherwise
      = return ()

{-| Evaluates axioms on patterns.
-}
evaluatePattern
    :: forall variable simplifier
    .  SimplifierVariable variable
    => MonadSimplify simplifier
    => Condition variable
    -- ^ The predicate from the configuration
    -> Condition variable
    -- ^ Aggregated children predicate and substitution.
    -> TermLike variable
    -- ^ The pattern to be evaluated
    -> OrPattern variable
    -- ^ The default value
    -> simplifier (OrPattern variable)
evaluatePattern
    configurationCondition
    childrenCondition
    patt
    defaultValue
  =
    maybeEvaluatePattern
        childrenCondition
        patt
        defaultValue
        configurationCondition
    & maybeT (return defaultValue) return

{-| Evaluates axioms on patterns.

Returns Nothing if there is no axiom for the pattern's identifier.
-}
maybeEvaluatePattern
    :: forall variable simplifier
    .  SimplifierVariable variable
    => MonadSimplify simplifier
    => Condition variable
    -- ^ Aggregated children predicate and substitution.
    -> TermLike variable
    -- ^ The pattern to be evaluated
    -> OrPattern variable
    -- ^ The default value
    -> Condition variable
    -> MaybeT simplifier (OrPattern variable)
maybeEvaluatePattern
    childrenCondition
    termLike
    defaultValue
    configurationCondition
  = do
    BuiltinAndAxiomSimplifier evaluator <- lookupAxiomSimplifier termLike
    Trans.lift . tracing $ do
        let conditions = configurationCondition <> childrenCondition
        logAxiomIdentifier identifier
        result <-
            Profile.axiomEvaluation identifier
            $ evaluator termLike conditions
        flattened <- case result of
            AttemptedAxiom.NotApplicable ->
                return AttemptedAxiom.NotApplicable
            AttemptedAxiom.Applied AttemptedAxiomResults
                { results = orResults
                , remainders = orRemainders
                } -> do
                    simplified <-
                        Profile.resimplification
                            identifier (length orResults)
                        $ mapM simplifyIfNeeded orResults
                    let simplifiedResult = MultiOr.flatten simplified
                    Profile.axiomBranching
                        identifier
                        (length orResults)
                        (length simplifiedResult)
                    return
                        (AttemptedAxiom.Applied AttemptedAxiomResults
                            { results = simplifiedResult
                            , remainders = orRemainders
                            }
                        )
        merged <-
            Profile.mergeSubstitutions identifier
            $ mergeWithConditionAndSubstitution
                childrenCondition
                flattened
        case merged of
            AttemptedAxiom.NotApplicable ->
                return defaultValue
            AttemptedAxiom.Applied attemptResults ->
                return $ MultiOr.merge results remainders
                where
                AttemptedAxiomResults { results, remainders } =
                    attemptResults
  where
    identifier :: Maybe AxiomIdentifier
    identifier = AxiomIdentifier.matchAxiomIdentifier termLike

    tracing =
        traceNonErrorMonad
            D_Function_evaluatePattern
            [ debugArg "axiomIdentifier" identifier ]
        . Profile.equalitySimplification identifier termLike

    unchangedPatt =
        Conditional
            { term         = termLike
            , predicate    = predicate
            , substitution = substitution
            }
      where
        Conditional { term = (), predicate, substitution } = childrenCondition

    simplifyIfNeeded :: Pattern variable -> simplifier (OrPattern variable)
    simplifyIfNeeded toSimplify
      | toSimplify == unchangedPatt =
        return (OrPattern.fromPattern unchangedPatt)
      | otherwise =
        reevaluateFunctions configurationCondition toSimplify

    logAxiomIdentifier :: MonadLog m => Maybe AxiomIdentifier -> m()
    logAxiomIdentifier axiomId =
        withLogScope "Evaluator"
        . withLogScope "evaluatePattern"
        . logDebug
        . Text.pack . show . Pretty.sep
        $ ["Evaluating: ", (Pretty.pretty axiomId)]

evaluateSortInjection
    :: InternalVariable variable
    => Application Symbol (TermLike variable)
    -> Application Symbol (TermLike variable)
evaluateSortInjection ap
  | Symbol.isSortInjection apHead
  = case apChild of
    App_ apHeadChild grandChildren
      | Symbol.isSortInjection apHeadChild ->
        let
            (fromSort', toSort') = sortInjectionSorts apHeadChild
            apHeadNew = updateSortInjectionSource apHead fromSort'
            resultApp = apHeadNew grandChildren
        in
            assert (toSort' == fromSort) resultApp
    _ -> ap
  | otherwise = ap
  where
    apHead = applicationSymbolOrAlias ap
    (fromSort, _) = sortInjectionSorts apHead
    apChild = sortInjectionChild ap
    updateSortInjectionSource head1 fromSort1 children =
        Application
            { applicationSymbolOrAlias =
                Symbol.coerceSortInjection head1 fromSort1 toSort1
            , applicationChildren = children
            }
      where
        (_, toSort1) = sortInjectionSorts head1

sortInjectionChild :: Unparse a => Application Symbol a -> a
sortInjectionChild application =
    case applicationChildren application of
        [child] -> child
        _ ->
            (error . show . Pretty.vsep)
                [ "Sort injection pattern"
                , Pretty.indent 4 (unparse application)
                , "should have one argument."
                ]

sortInjectionSorts :: Symbol -> (Sort, Sort)
sortInjectionSorts symbol =
    case symbolParams symbol of
        [fromSort, toSort] -> (fromSort, toSort)
        _ ->
            (error . show . Pretty.vsep)
                [ "Sort injection symbol"
                , Pretty.indent 4 (unparse symbol)
                , "should have two sort parameters."
                ]

{-| 'reevaluateFunctions' re-evaluates functions after a user-defined function
was evaluated.
-}
reevaluateFunctions
    :: SimplifierVariable variable
    => MonadSimplify simplifier
    => Condition variable
    -> Pattern variable
    -- ^ Function evaluation result.
    -> simplifier (OrPattern variable)
reevaluateFunctions predicate rewriting = do
    let (rewritingTerm, rewritingCondition) = Pattern.splitTerm rewriting
    orResults <- BranchT.gather $ do
        simplifiedTerm <- simplifyConditionalTerm predicate rewritingTerm
        simplifyCondition
            $ Pattern.andCondition simplifiedTerm rewritingCondition
    return (OrPattern.fromPatterns orResults)

{-| Ands the given condition-substitution to the given function evaluation.
-}
mergeWithConditionAndSubstitution
    :: SimplifierVariable variable
    => MonadSimplify simplifier
    => Condition variable
    -- ^ Condition and substitution to add.
    -> AttemptedAxiom variable
    -- ^ AttemptedAxiom to which the condition should be added.
    -> simplifier (AttemptedAxiom variable)
mergeWithConditionAndSubstitution _ AttemptedAxiom.NotApplicable =
    return AttemptedAxiom.NotApplicable
mergeWithConditionAndSubstitution
    toMerge
    (AttemptedAxiom.Applied AttemptedAxiomResults { results, remainders })
  = do
    evaluatedResults <- fmap OrPattern.fromPatterns . BranchT.gather $ do
        result <- BranchT.scatter results
        simplifyCondition $ Pattern.andCondition result toMerge
    evaluatedRemainders <- fmap OrPattern.fromPatterns . BranchT.gather $ do
        remainder <- BranchT.scatter remainders
        simplifyCondition $ Pattern.andCondition remainder toMerge
    return $ AttemptedAxiom.Applied AttemptedAxiomResults
        { results = evaluatedResults
        , remainders = evaluatedRemainders
        }
