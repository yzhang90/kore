{-|
Module      : Kore.Step.Simplification.Not
Description : Tools for Not pattern simplification.
Copyright   : (c) Runtime Verification, 2018
License     : NCSA
Maintainer  : virgil.serbanuta@runtimeverification.com
Stability   : experimental
Portability : portable
-}
module Kore.Step.Simplification.Not
    ( makeEvaluate
    , simplify
    , simplifyEvaluated
    ) where

import qualified Data.Foldable as Foldable

import qualified Kore.Attribute.Symbol as Attribute
import           Kore.IndexedModule.MetadataTools
                 ( SmtMetadataTools )
import qualified Kore.Internal.Conditional as Conditional
import           Kore.Internal.MultiAnd
                 ( MultiAnd )
import qualified Kore.Internal.MultiAnd as MultiAnd
import           Kore.Internal.MultiOr
                 ( MultiOr )
import           Kore.Internal.OrPattern
                 ( OrPattern )
import qualified Kore.Internal.OrPattern as OrPattern
import           Kore.Internal.Pattern as Pattern
import           Kore.Internal.TermLike hiding
                 ( mkAnd )
import           Kore.Predicate.Predicate
                 ( makeAndPredicate, makeNotPredicate )
import qualified Kore.Predicate.Predicate as Predicate
import           Kore.Step.Axiom.Data
                 ( BuiltinAndAxiomSimplifierMap )
import qualified Kore.Step.Simplification.And as And
import           Kore.Step.Simplification.Data
                 ( BranchT, PredicateSimplifier, Simplifier,
                 TermLikeSimplifier, gather, scatter )
import           Kore.Unparser
import           Kore.Variables.Fresh
                 ( FreshVariable )

{-|'simplify' simplifies a 'Not' pattern with an 'OrPattern'
child.

Right now this uses the following:

* not top = bottom
* not bottom = top

-}
simplify
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> Not Sort (OrPattern variable)
    -> Simplifier (OrPattern variable)
simplify
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    Not { notChild }
  =
    simplifyEvaluated
        tools
        predicateSimplifier
        termSimplifier
        axiomSimplifiers
        notChild

{-|'simplifyEvaluated' simplifies a 'Not' pattern given its
'OrPattern' child.

See 'simplify' for details.
-}
{- TODO (virgil): Preserve pattern sorts under simplification.

One way to preserve the required sort annotations is to make 'simplifyEvaluated'
take an argument of type

> CofreeF (Not Sort) (Attribute.Pattern variable) (OrPattern variable)

instead of an 'OrPattern' argument. The type of 'makeEvaluate' may
be changed analogously. The 'Attribute.Pattern' annotation will eventually
cache information besides the pattern sort, which will make it even more useful
to carry around.

-}
simplifyEvaluated
    ::  ( FreshVariable variable
        , SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap
    -> OrPattern variable
    -> Simplifier (OrPattern variable)
simplifyEvaluated
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
    simplified
  =
    fmap OrPattern.fromPatterns $ gather $ do
        let not' = Not { notChild = simplified, notSort = () }
        andPattern <- scatterAnd (makeEvaluateNot <$> distributeNot not')
        mkMultiAndPattern' andPattern
  where
    mkMultiAndPattern' =
        mkMultiAndPattern
            tools
            predicateSimplifier
            termSimplifier
            axiomSimplifiers

{-|'makeEvaluate' simplifies a 'Not' pattern given its 'Pattern'
child.

See 'simplify' for details.
-}
makeEvaluate
    ::  ( SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => Pattern variable
    -> OrPattern variable
makeEvaluate = makeEvaluateNot . Not ()

makeEvaluateNot
    :: (Ord variable, Show variable, SortedVariable variable, Unparse variable)
    => Not sort (Pattern variable)
    -> OrPattern variable
makeEvaluateNot Not { notChild } =
    OrPattern.fromPatterns
        [ Pattern.fromTermLike $ makeTermNot term
        , Conditional
            { term = mkTop (termLikeSort term)
            , predicate =
                makeNotPredicate
                $ makeAndPredicate predicate
                $ Predicate.fromSubstitution substitution
            , substitution = mempty
            }
        ]
  where
    Conditional { term, predicate, substitution } = notChild

makeTermNot
    ::  ( SortedVariable variable
        , Ord variable
        , Show variable
        , Unparse variable
        )
    => TermLike variable
    -> TermLike variable
-- TODO: maybe other simplifications like
-- not ceil = floor not
-- not forall = exists not
makeTermNot term
  | isBottom term = mkTop    (termLikeSort term)
  | isTop term    = mkBottom (termLikeSort term)
  | otherwise     = mkNot term

{- | Distribute 'Not' over 'MultiOr' using de Morgan's identity.
 -}
distributeNot
    :: (Ord sort, Ord variable)
    => Not sort (OrPattern variable)
    -> MultiAnd (Not sort (Pattern variable))
distributeNot notOr@Not { notChild } =
    MultiAnd.make $ worker <$> Foldable.toList notChild
  where
    worker child = notOr { notChild = child }

{- | Distribute 'MultiAnd' over 'MultiOr'.
 -}
distributeAnd
    :: MultiAnd (OrPattern variable)
    -> MultiOr (MultiAnd (Pattern variable))
distributeAnd = sequenceA

{- | Distribute 'MultiAnd' over 'MultiOr' and 'scatter' into 'BranchT'.
 -}
scatterAnd
    :: Monad m
    => MultiAnd (OrPattern variable)
    -> BranchT m (MultiAnd (Pattern variable))
scatterAnd = scatter . distributeAnd

{- | Conjoin and simplify a 'MultiAnd' of 'Pattern'.
 -}
mkMultiAndPattern
    ::  ( FreshVariable variable
        , Ord variable
        , SortedVariable variable
        , Show variable
        , Unparse variable
        )
    => SmtMetadataTools Attribute.Symbol
    -> PredicateSimplifier
    -> TermLikeSimplifier
    -> BuiltinAndAxiomSimplifierMap

    -> MultiAnd (Pattern variable)
    -> BranchT Simplifier (Pattern variable)
mkMultiAndPattern
    tools
    predicateSimplifier
    termSimplifier
    axiomSimplifiers
  =
    Foldable.foldrM mkAnd Pattern.top
  where
    mkAnd =
        And.makeEvaluate
            tools
            predicateSimplifier
            termSimplifier
            axiomSimplifiers
