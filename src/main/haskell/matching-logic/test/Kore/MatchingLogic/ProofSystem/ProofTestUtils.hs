module Kore.MatchingLogic.ProofSystem.ProofTestUtils
  (GoalId(..), MLProof(..), NewGoalId(..)) where

import           Data.Kore.AST.Common                         (Application (..),
                                                               Attributes (..),
                                                               Definition (..),
                                                               Id (..), Meta,
                                                               Module (..),
                                                               ModuleName (..),
                                                               Pattern (..),
                                                               Sentence (..),
                                                               SentenceSymbol (..),
                                                               Sort (..),
                                                               Symbol (..),
                                                               SymbolOrAlias (..),
                                                               Variable)

import           Kore.MatchingLogic.HilbertProof               (Proof (..))
import           Kore.MatchingLogic.ProofSystem.Minimal        (MLRule (..))
import           Data.Text.Prettyprint.Doc
import           Data.Kore.MetaML.AST                          (MetaMLPattern)
import           Text.Megaparsec
import           Kore.MatchingLogic.ProofSystem.Minimal.Syntax
import           Data.Kore.Parser.ParserImpl                  (sortParser,
                                                               unifiedVariableOrTermPatternParser,
                                                               symbolParser)
import           Data.Kore.MetaML.MetaToKore
import           Kore.MatchingLogic.ProofSystem.MLProofSystem (formulaVerifier)

newtype NewGoalId = NewGoalId Int
newtype GoalId = GoalId Int
    deriving (Eq, Show, Ord)

instance Pretty GoalId where
    pretty (GoalId i) = pretty "goalId:" <> pretty i


type MLProof =
    Proof
        GoalId
        (MLRule
            (Sort Meta)
            (SymbolOrAlias Meta)
            (Variable Meta)
            (MetaMLPattern Variable)
        )
        (MetaMLPattern Variable)

type MLProofCommand = 
    Command 
        GoalId
        (MLRule
            (Sort Meta)
            (SymbolOrAlias Meta)
            (Variable Meta)
            (MetaMLPattern Variable)
        )
        


{-
 - Parsers for proof object
 - In order to process object level proof objects, we need the  
 - following parsers - 
 -  1. CommandParser: Add/Derive commands need
 -     - Parser Id -> Parser Formula -> Parser (rule id) 
 -     - Use parseCommand :: Parser Id ->  Parser Formula -> Parser Rule
 -
 -  2. RuleParser: Parser different rules     
 -     - Use parseMLRule :: Parser Sort -> Parser Label -> Parser Variable 
 -                          -> Parser Formula
 -  
 -}
goalIdParser              :: Parser GoalId
objectSortParser          :: Parser (Sort          Object)
metaViaObjectSortParser   :: Parser (Sort          Meta)
objectSymbolParser        :: Parser (SymbolOrAlias Object)
metaViaObjectSymbolParser :: Parser (SymbolOrAlias Meta)
formulaParser             :: Parser UnifiedPattern
metaViaFormulaParser      :: Parser (MetaMLPattern Variable)
testCommandParser         :: Parser MLProofCommand
testRuleParser            :: Parser (MLRule
                                      (Sort          Meta)
                                      (SymbolOrAlias Meta)
                                      (Variable      Meta)
                                      (MetaMLPattern Variable)
                                    )

goalIdParser               = GoalId                             $    read <$> some digitChar
objectSortParser           = sortParser Object
metaViaObjectSortParser    = objectSortParser                   >>= return $ patternKoreToMeta
objectSymbolParser         = symbolParser Object
metaViaObjectSymbolParser  = objectSymbolParser                 >>= return $ patternKoreToMeta
formulaParser              = unifiedVariableOrTermPatternParser
metaViaObjectPatternParser = formulaParser                      >>= return $ patternKoreToMeta

testRuleParser             = parseMLRule 
                              metaViaObjectSortParser 
                              metaViaObjectSymbolParser
                              metaViaVariableParser 
                              metaViaFormulaParser
                              goalIdParser

testCommandParser          = parseCommand goalIdParser metaViaFormulaParser ruleParser



testFormulaVerifier = formulaVerifier testIndexedModule
