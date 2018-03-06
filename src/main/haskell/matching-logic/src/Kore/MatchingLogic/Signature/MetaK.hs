{-# LANGUAGE TypeFamilies #-}
{-|
Description: A signature for the meta-level Kore terms.

Defines a signature for the meta-level Kore terms, but
currently does not have space for the added symbols that
will be introduced by lifting object-level definitions.
-}
module Kore.MatchingLogic.Signature.MetaK where
import           Data.Coerce

import           Kore.MatchingLogic.AST (IsSignature)
import qualified Kore.MatchingLogic.AST as AST

data MetaSort =
    Char | CharList
  | Pattern | PatternList
  | SortSort | SortList
  | SymbolSort | SymbolList
  | VariableSort | VariableList
 deriving Eq

instance Show MetaSort where
    show Char         = "#Char"
    show CharList     = "#CharList"
    show Pattern      = "#Pattern"
    show PatternList  = "#PatternList"
    show SortSort     = "#Sort"
    show SortList     = "#SortList"
    show SymbolSort   = "#Symbol"
    show SymbolList   = "#SymbolList"
    show VariableSort = "#Variable"
    show VariableList = "#VariableList"

data MetaLabel =
   LChar Prelude.Char
 | ConsChars | NilChars
 | Sort
 | ConsSorts | NilSorts
 | AppendSortList | InSortList MetaSort | DeleteSortList
 | Symbol
 | ConsSymbols | NilSymbols
 | GetArgumentSorts | GetReturnSort
 | CeilSymbol
 | Variable
 | ConsVariables | NilVariables
 | Application | And | Not | Exists | VariableAsPattern
 | ConsPatterns | NilPatterns
 | VariablePattern
 | Or | Implies | Iff | Forall | Ceil | Floor | Equals | In | Top | Bottom
 | GetFV | GetFVFromPatterns
 | OccursFree MetaSort | FreshName
 | Substitute | SubstitutePatterns
 | SortDeclared MetaSort | SortsDeclared MetaSort
 | SymbolDeclared MetaSort | AxiomDeclared MetaSort
 | WellFormed MetaSort | WellFormedPatterns MetaSort
 | GetSort | GetSortsFromPatterns
 | Provable MetaSort
 deriving(Show,Eq)

metaLabelSignature :: MetaLabel -> (MetaSort,[MetaSort])
metaLabelSignature l = case l of
  LChar _              -> (Char,[])
  ConsChars            -> (CharList,[Char,CharList])
  NilChars             -> (CharList,[])
  Sort                 -> (SortSort,[CharList,SortList])
  ConsSorts            -> (SortList,[SortSort,SortList])
  NilSorts             -> (SortList,[])
  AppendSortList       -> (SortList,[SortList,SortList])
  InSortList s         -> (s,[SortSort,SortList])
  DeleteSortList       -> (SortList,[SortSort,SortList])
  Symbol               -> (SymbolSort,[CharList,SortList,SortList,SortSort])
  GetArgumentSorts     -> (SortList,[SymbolSort])
  GetReturnSort        -> (SortSort,[SymbolSort])
  CeilSymbol           -> (SymbolSort,[SortSort,SortSort])
  Variable             -> (VariableSort,[CharList,SortSort])
  Application          -> (Pattern,[SymbolSort,PatternList])
  And                  -> (Pattern,[SortSort,Pattern,Pattern])
  Not                  -> (Pattern,[SortSort,Pattern])
  Exists               -> (Pattern,[SortSort,VariableSort,Pattern])
  VariableAsPattern    -> (Pattern,[VariableSort])
  VariablePattern      -> (Pattern,[CharList,SortSort])
  Or                   -> (Pattern,[SortSort,Pattern,Pattern])
  Implies              -> (Pattern,[SortSort,Pattern,Pattern])
  Iff                  -> (Pattern,[SortSort,Pattern,Pattern])
  Forall               -> (Pattern,[SortSort,VariableSort,Pattern])
  Ceil                 -> (Pattern,[SortSort,SortSort,Pattern])
  Floor                -> (Pattern,[SortSort,SortSort,Pattern])
  Equals               -> (Pattern,[SortSort,SortSort,Pattern,Pattern])
  In                   -> (Pattern,[SortSort,SortSort,Pattern,Pattern])
  Top                  -> (Pattern,[SortSort])
  Bottom               -> (Pattern,[SortSort])
  GetFV                -> (VariableList,[Pattern])
  GetFVFromPatterns    -> (VariableList,[PatternList])
  OccursFree s         -> (s,[VariableSort,Pattern])
  FreshName            -> (CharList,[PatternList])
  Substitute           -> (Pattern,[Pattern,Pattern,VariableSort])
  SubstitutePatterns   -> (PatternList,[PatternList,Pattern,VariableSort])
  SortDeclared s       -> (s,[SortSort])
  SortsDeclared s      -> (s,[SortList])
  SymbolDeclared s     -> (s,[SymbolSort])
  AxiomDeclared s      -> (s,[Pattern])
  WellFormed s         -> (s,[Pattern])
  WellFormedPatterns s -> (s,[PatternList])
  GetSort              -> (SortSort,[Pattern])
  GetSortsFromPatterns -> (SortList,[PatternList])
  Provable s           -> (s,[Pattern])

data MetaK

instance IsSignature MetaK where
  newtype Sort MetaK = MetaSort MetaSort
  newtype Label MetaK = MetaLabel MetaLabel
  labelSignature (MetaLabel l) = coerce (metaLabelSignature l)

deriving instance Eq (AST.Sort MetaK)
deriving instance Eq (AST.Label MetaK)

instance Show (AST.Sort MetaK) where
  showsPrec p (MetaSort s) = showsPrec p s
instance Show (AST.Label MetaK) where
  showsPrec p (MetaLabel s) = showsPrec p s
