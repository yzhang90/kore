{-|
Copyright   : (c) Runtime Verification, 2019
License     : NCSA
-}
module Kore.AST.ApplicativeKore
    ( completeDefinition ) where

import           Control.Comonad
import           Data.Foldable
import           Kore.AST.Common
import           Kore.AST.MetaOrObject
import           Kore.AST.Sentence
import           Kore.AST.Valid
import           Kore.Step.Pattern hiding
                 ( freeVariables )
import qualified Kore.Verified as Verified

completeDefinition :: Definition Verified.Sentence -> Definition Verified.Sentence
completeDefinition Definition { definitionAttributes, definitionModules } =
    Definition
    { definitionAttributes
    , definitionModules = map completeModule definitionModules
    }

completeModule :: Module Verified.Sentence -> Module Verified.Sentence
completeModule Module { moduleName, moduleSentences, moduleAttributes } =
    Module
    { moduleName
    , moduleSentences = concatMap completeSentence moduleSentences
    , moduleAttributes
    }

completeSentence :: Verified.Sentence -> [Verified.Sentence]
completeSentence (SentenceAxiomSentence sentenceAxiom) =
    [ SentenceAxiomSentence sentenceAxiom
        { sentenceAxiomPattern = quantifiedAxiomPattern }
    ]
 where
   quantifiedAxiomPattern =
       quantifyFreeVariables (sentenceAxiomPattern sentenceAxiom)
completeSentence s = [s]

quantifyFreeVariables
    :: StepPattern Object Variable -> StepPattern Object Variable
quantifyFreeVariables p =
    foldl' wrapAndQuantify p freeVariables
  where
    Valid { freeVariables } = extract p

wrapAndQuantify
    :: StepPattern Object Variable
    -> Variable Object
    -> StepPattern Object Variable
wrapAndQuantify p var =
    mkForall var p