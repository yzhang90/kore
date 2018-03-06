{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}
{-|
Description: A generic signature for parameterized sorts and labels

A generic signature for parameterized sorts and labels.
This uses the reflection package to provide a type with
an 'IsSignature' instance from a set of textual label
and sort names, along with the number of parameters of
each sort and the number of sort parameters
and a parameterized signature for each label.
Functions are also provided to check and convert trees
over strings into the 'Label' or 'Sort' types associated
with that signature.
-}
module Kore.MatchingLogic.Signature.Poly
  (SignatureInfo(..),PolySignature) where
  -- ,ValidatedSignature,fromValidated,findLabel) where
import           Data.Char                 (isAlpha, isAlphaNum)
import           Data.Coerce
import           Data.Functor.Foldable
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Proxy
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text

import           Data.Reflection
import           Data.Text.Prettyprint.Doc

import           Kore.MatchingLogic.AST

data SortPat = PApp Text [SortPat] | Var Int
  deriving Show

data SignatureInfo =
  MkSignatureInfo
  { sortCons :: Map Text Int
  , labels   :: Map Text (Int,SortPat,[SortPat])
  }
  deriving Show

newtype ValidatedSignature =
  MkValidatedSignature SignatureInfo
instance Show ValidatedSignature where
  show (MkValidatedSignature sig) = show sig

fromValidated :: ValidatedSignature -> SignatureInfo
fromValidated (MkValidatedSignature info) = info

data PolySignature s

sortTermValid :: Map Text Int -> Int -> SortPat -> Bool
sortTermValid sortCons nargs t = check t
  where check (Var v) = 0 <= v && v < nargs
        check (PApp c ts) =
          case Map.lookup c sortCons of
            Just arity -> arity == length ts && all check ts
            Nothing    -> False

isValid :: SignatureInfo -> Bool
isValid sigInfo =
  all (\(arity,r,args) ->sortOk arity r && all (sortOk arity) args) (labels sigInfo)
 where sortOk arity t = sortTermValid (sortCons sigInfo) arity t

validate :: SignatureInfo -> Maybe ValidatedSignature
validate sig = if isValid sig then Just (MkValidatedSignature sig) else Nothing

data RawSort = RawSort Text [RawSort]
  deriving Eq
data RawLabel = RawLabel Text [RawSort]
  deriving Eq

prettyName :: Text -> Doc ann
prettyName name
  | isAlpha (Text.head name) && Text.all isAlphaNum name = pretty name
  | otherwise = pretty (show name)

prettyPolyTerm :: (Pretty t) => Text -> [t] -> Doc ann
prettyPolyTerm name args = prettyName name <> braced (map pretty args)

instance Show RawSort where
  showsPrec _ (RawSort con args) s = shows con (showsBraces args s)
instance Pretty RawSort where
  pretty (RawSort con args) = prettyName con

showsBraces :: (Show a) => [a] -> ShowS
showsBraces items s = '{':showsItems items ('}':s)
   where showsItems [] s          = s
         showsItems [x] s         = shows x s
         showsItems (x:l@(_:_)) s = shows x (',':showsItems l s)

instantiate :: [RawSort] -> SortPat -> RawSort
instantiate args (Var v)          = args !! v
instantiate args (PApp con cargs) = RawSort con (map (instantiate args) cargs)

findSort1 :: forall s . (Reifies s ValidatedSignature) =>
  Text -> [Sort (PolySignature s)] -> Maybe (Sort (PolySignature s))
findSort1 sortName args =
  case Map.lookupGE sortName (sortCons sig) of
    Just (sort',arity) | sort' == sortName, arity == length args
                         -> Just (PolySort (RawSort sort' (coerce args)))
    _ -> Nothing
 where sig = fromValidated (reflect @s Proxy)

findSort :: forall s . (Reifies s ValidatedSignature) =>
  RawSort -> Maybe (Sort (PolySignature s))
findSort (RawSort name args) = traverse findSort args >>= findSort1 name

findLabel1 :: forall s . (Reifies s ValidatedSignature) =>
  Text -> [Sort (PolySignature s)] -> Maybe (Label (PolySignature s))
findLabel1 labelName args =
  case Map.lookupGE labelName (labels sig) of
    Just (label',(arity,_,_)) | label' == labelName, arity == length args
                                -> Just (PolyLabel label' (coerce args))
    _ -> Nothing
 where sig = fromValidated (reflect @s Proxy)

findLabel :: forall s . (Reifies s ValidatedSignature) =>
  RawLabel -> Maybe (Label (PolySignature s))
findLabel (RawLabel name args) = traverse findSort args >>= findLabel1 name

deriving instance Eq (Label (PolySignature s))
instance Show (Label (PolySignature s)) where
  showsPrec _ (PolyLabel l args) s = shows l (showsBraces args s)
deriving instance Eq (Sort (PolySignature s))
instance Show (Sort (PolySignature s)) where
  show (PolySort s) = show s

braced :: [Doc ann] -> Doc ann
braced = group . encloseSep lbrace rbrace comma

instance Pretty (Label (PolySignature s)) where
  pretty (PolyLabel l args) = prettyName l <> braced (map pretty args)
instance Pretty (Sort (PolySignature s)) where
  pretty (PolySort s) = pretty s

instance (Reifies s ValidatedSignature) => IsSignature (PolySignature s) where
  data Label (PolySignature s) = PolyLabel Text [RawSort]
  newtype Sort (PolySignature s) = PolySort RawSort
  labelSignature (PolyLabel name sortArgs) =
    case Map.lookup name (labels sig) of
      Just (arity,return,args) | arity == length sortArgs -> coerce
        (instantiate sortArgs return, map (instantiate sortArgs) args)
      Nothing -> error $ "Encapsulation failure, invalid label "++show name++" found in a reflected signature"
   where sig = fromValidated (reflect @s Proxy)

reifySignature :: ValidatedSignature
               -> (forall s . (Reifies s ValidatedSignature)
                           => Proxy (PolySignature s) -> a)
               -> a
reifySignature sig f = reify sig (\(proxy :: Proxy s) -> f @s Proxy)

recognizeSignatureSyntax :: forall s var . (Reifies s ValidatedSignature)
                         => Pattern RawSort RawLabel var
                         -> Maybe (SigPattern (PolySignature s) var)
recognizeSignatureSyntax p = cata (\p -> recognizeLayer p >>= sequenceA >>= return . Fix) p
  where
    recognizeLayer :: PatternF RawSort RawLabel var p
                   -> Maybe (SigPatternF (PolySignature s) var p)
    recognizeLayer p = case p of
      Variable s v -> (\s' -> Variable s' v) <$> findSort s
      Application l ps -> (\l' -> Application l' ps) <$> findLabel l
      And s p1 p2 -> (\s' -> And s' p1 p2) <$> findSort s
      Not s p -> (\s' -> Not s' p) <$> findSort s
      Exists s1 s2 v p -> (\s1' s2' -> Exists s1' s2' v p) <$> findSort s1 <*> findSort s2

{-
let testSig = MkSignatureInfo  (Map.fromList [("Map",2),("Id",0),("Int",0)]) (Map.fromList [(".Map",(2,PApp "Map" [Var 0,Var 1],[])) ,("_|->_",(2,PApp "Map" [Var 0,Var 1],[Var 0,Var 1])) ,("_Map_",(2,PApp "Map" [Var 0,Var 1],[PApp "Map" [Var 0,Var 1],PApp "Map" [Var 0,Var 1]]))])
-}
