module Kore.MatchingLogic.ProofSystem.ProofTests (proofTests) 
  where 

import           Test.Tasty                                        (TestTree,
                                                                    testGroup)
import           Kore.MatchingLogic.HilbertProof              as HilbertProof (Proof (..),
                                                                               add,
                                                                               derive,
                                                                               emptyProof)
import           Kore.MatchingLogic.ProofSystem.Minimal       (MLRule (..), SubstitutedVariable (..),
                                                               SubstitutingVariable (..))
import           Test.Tasty.HUnit
import           Kore.MatchingLogic.ProverRepl                (checkProof)


proofTests :: TestTree

proofTests = 
  testGroup "MLProofTests"
  [ onePlusOneProofTest
  ]
              
onePlusOneProofTest :: TestTree

data CheckResult = Success | Failure String  deriving Show

type Id = Int

checkProofTest proofString = False 


onePlusOneProofTest = 
  testCaseSteps "OnePlusOneProofTest" $ \step -> do
  step "Loading 1+1=2.obj..."
  x <- readFile "test/resources/1+1=2.obj" 
  putStrLn x
  step "Running Prover   ..."
  assertEqual "1+1=2 proof check" (checkProofTest x) True 
  


