{-|
Description: The minimal matching logic proof system

This module defines the minimal matching logic proof system,
which does not assume the existence of a definedness symbol).
 -}
module Kore.MatchingLogic.ProofSystem.Minimal where

import           Data.Text.Prettyprint.Doc       (Pretty (pretty))

import           Data.Functor.Foldable           (Fix (..))

import           Kore.MatchingLogic.AST          as AST
import           Kore.MatchingLogic.HilbertProof

{-|
  This type has constructors for each rule of the
  proof system.
  It is parameterized over the exact types of parts of patterns
  to allow working with different signatures or implementations.
  The 'term' parameter is used where the rule must be written
  with a literal pattern.
  'hypothesis' refers to hypotheses of a proof rule, it can be
  instantiated with names of the hypotheses or with the actual
  formulas giving the conclusions of those hypotheses.
 -}
data MLRule sort label var term hypothesis =
   Propositional1 term term
 | Propositional2 term term term
 | Propositional3 term term
 | ModusPonens hypothesis hypothesis
 | Generalization var hypothesis
 | VariableSubstitution var hypothesis var
 | ForallRule var term term
 | Framing label Int hypothesis
 | PropagateOr label Int term term
     -- ^ sigma(before ..,\phi1 \/ \phi2,.. after) <->
     --     sigma(before ..,\phi1, .. after) <-> sigma(before ..,\phi2,.. after)
 | PropagateExists label Int var term
     -- ^ sigma(before ..,Ex x. phi,.. after) <-> Ex x.sigma(before ..,phi,.. after)
 | Existence var
     -- ^ Ex x.x
 | Singvar var term [Int] [Int]
 deriving (Functor, Foldable, Traversable, Show)

-- | The 'MLRuleSig' synonym instantiates 'MLRule' to use
-- the sorts, labels, and patterns from the 'IsSignature' instance 'sig'
type MLRuleSig sig var = MLRule (Sort sig) (Label sig) var (WFPattern sig var)

simpleFormulaVerifier :: formula -> Either (Error MLError) ()
simpleFormulaVerifier _ = return ()

-- | This instance is currently incomplete, it correctly checks
-- uses of propositional1 and propositional2 but rejects any other rules.
instance (IsSignature sig, Eq (Sort sig), Eq (Label sig), Eq var) =>
         ProofSystem (MLRuleSig sig var) (WFPattern sig var) where
  checkDerivation conclusion rule = case rule of
    Propositional1 phi1 phi2
      | wfPatSort phi1 == wfPatSort phi2
        -> let s = wfPatSort phi1
               phi1' = fromWFPattern phi1
               phi2' = fromWFPattern phi2
               statement = implies s phi1' (implies s phi2' phi1')
           in fromWFPattern conclusion == statement
    Propositional2 phi1 phi2 phi3
      | wfPatSort phi1 == wfPatSort phi2
      , wfPatSort phi1 == wfPatSort phi3
        -> let s = wfPatSort phi1
               phi1' = fromWFPattern phi1
               phi2' = fromWFPattern phi2
               phi3' = fromWFPattern phi3
               statement = implies s (implies s phi1' (implies s phi2' phi3'))
                               (implies s (implies s phi1' phi2') (implies s phi1' phi3'))
           in fromWFPattern conclusion == statement
    _ -> False
