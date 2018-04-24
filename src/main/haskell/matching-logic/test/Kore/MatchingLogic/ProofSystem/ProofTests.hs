module Kore.MatchingLogic.ProofSystem.ProofTests 
  (proofTests) where 

import           Test.Tasty                                              (TestTree,
                                                                          testGroup)

import           Kore.MatchingLogic.HilbertProof                         as HilbertProof (Proof (..))
import           Kore.MatchingLogic.ProofSystem.Minimal                  (MLRule (..))
import           Test.Tasty.HUnit
import           Kore.MatchingLogic.ProverRepl                           (checkProof, parseCommand)
import           Data.Kore.AST.Kore                                      (UnifiedPattern, UnifiedSort)
import           Data.Kore.Parser.ParserImpl                             (unifiedPatternParser)
import           Kore.MatchingLogic.ProofSystem.Minimal                  (MLRule)
import           Data.Kore.AST.Common                                    (SymbolOrAlias (..))
import           Kore.MatchingLogic.ProofSystem.MLProofSystem            (formulaVerifier)
import           Kore.MatchingLogic.ProofSystem.ProofTestUtils           (testRuleParser
                                                                          , testFormulaVerifier
                                                                          , testCommandParser)
import           Text.Megaparsec

proofTests :: TestTree

proofTests = 
  testGroup "MLProofTests"
  [ onePlusOneProofTest
  ]
              
onePlusOneProofTest :: TestTree

checkProofTest proofString = 
  case (checkProof testFormulaVerifier testCommandParser proofString) of
    (Success _) -> True
    _           -> False
    

{- 
 - The proof object is written completely in the object level.
 - Need to instantiate a parser for the object level, and the  
 - proof system 
 -
 - We used unifiedPatternParser as the parser for kore patterns
 - UnfiedPatterns as the type of Kore Patterns
 - UnifiedSort as the type of Sorts
 -}

onePlusOneProofTest = 
  testCaseSteps "OnePlusOneProofTest" $ \step -> do
  step "Loading 1+1=2.obj ..."
  x <- readFile "../../../test/resources/1+1=2.obj"
  step "Loading nat.kore  ..." 
  y <- readFile "../../../test/resources/nat.kore"
  assertEqual "1+1=2 proof check" (checkProofTest x) True 
  

