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
 -}
goalIdParser       :: Parser GoalId 
objectSortParser   :: Parser (Sort Object)
objectSymbolParser :: Parser (SymbolOrAlias Object)
formulaParser      :: Parser UnifiedPattern
commandParser      :: Parser (MLProofCommand)
ruleParser         :: Parser (MLRule
                               (Sort Meta)
                               (SymbolOrAlias Meta)
                               (Variable Meta)
                               (MetaMLPattern Variable)
                             )


goalIdParser       = GoalId $ read <$> some digitChar
objectSortParser   = sortParser Object
objectSymbolParser = symbolParser Object
ruleParser         = toMeta $ parseMLRule objectSortParser objectSymbolParser goalIdParser
formulaParser      = unifiedVariableOrTermPatternParser
commandParser      = parseCommand goalIdParser formulaParser ruleParser 


