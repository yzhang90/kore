{-# LANGUAGE TypeApplications     #-}
{-# LANGUAGE TypeFamilies         #-}
{-|
Description: A generic signature for finite sets of simply-sorted labels

A generic signature for finite sets of simply-sorted labels.
This uses the reflection package to allow any set of string
sort names, string label names, and label signatures to
be used as an 'IsSignature' instance, on a type of the
form 'SimpleSignature s'.
Functions are also provided to check and convert strings
to the 'Label' or 'Sort' types associated with that signature.
-}
module Kore.MatchingLogic.Signature.Simple
  (SignatureInfo(..),SimpleSignature
  ,ValidatedSignature,fromValidated,validate,reifySignature
  ,findLabel,findSort,recognizeSignatureSyntax) where
import           Data.Char
import           Data.Coerce
import           Data.Functor.Foldable
import           Data.Map.Strict           (Map)
import qualified Data.Map.Strict           as Map
import           Data.Proxy
import           Data.Set                  (Set)
import qualified Data.Set                  as Set
import           Data.Text                 (Text)
import qualified Data.Text                 as Text
import           Data.Text.Prettyprint.Doc

import           Data.Reflection

import           Kore.MatchingLogic.AST

-- | A finite signatures of sorts and labels written as text
data SignatureInfo =
  MkSignatureInfo
  { sorts  :: Set Text
  , labels :: Map Text (Text,[Text])
  }
  deriving Show

-- | A newtype wrapper around signatures which are
-- valid in the sense the label sorts only mention
-- defined sorts.
newtype ValidatedSignature =
  MkValidatedSignature SignatureInfo
instance Show ValidatedSignature where
  show (MkValidatedSignature sig) = show sig

fromValidated :: ValidatedSignature -> SignatureInfo
fromValidated (MkValidatedSignature info) = info

data SimpleSignature s

isValid :: SignatureInfo -> Bool
isValid sigInfo =
  all (\(r,args) -> sortOk r && all sortOk args) (labels sigInfo)
 where sortOk s = Set.member s (sorts sigInfo)

validate :: SignatureInfo -> Maybe ValidatedSignature
validate sig = if isValid sig then Just (MkValidatedSignature sig) else Nothing

findSort :: forall s . (Reifies s ValidatedSignature) => Text -> Maybe (Sort (SimpleSignature s))
findSort sortName =
  case Set.lookupGE sortName (sorts sig) of
    Just sort' | sort' == sortName -> Just (SimpleSort sort')
    _          -> Nothing
 where sig = fromValidated (reflect @s Proxy)

findLabel :: forall s . (Reifies s ValidatedSignature) => Text -> Maybe (Label (SimpleSignature s))
findLabel labelName =
  case Map.lookupGE labelName (labels sig) of
    Just (label',_) | label' == labelName -> Just (SimpleLabel label')
    _               -> Nothing
 where sig = fromValidated (reflect @s Proxy)

instance Show (Label (SimpleSignature s)) where
  show (SimpleLabel l) = show l
instance Show (Sort (SimpleSignature s)) where
  show (SimpleSort l) = show l

instance Pretty (Label (SimpleSignature s)) where
  pretty (SimpleLabel l)
    | isAlpha (Text.head l) && Text.all isAlphaNum l = pretty l
    | otherwise = pretty (show l)
instance Pretty (Sort (SimpleSignature s)) where
  pretty (SimpleSort l)
    | isAlpha (Text.head l) && Text.all isAlphaNum l = pretty l
    | otherwise = pretty (show l)

instance (Reifies s ValidatedSignature) => IsSignature (SimpleSignature s) where
  newtype Label (SimpleSignature s) = SimpleLabel Text
  newtype Sort (SimpleSignature s) = SimpleSort Text
  labelSignature (SimpleLabel name) =
    case Map.lookup name (labels sig) of
      Just labelSignature -> coerce labelSignature
      Nothing -> error $ "Encapsulation failure, invalid label "++show name++" found in a reflected signature"
   where sig = fromValidated (reflect @s Proxy)
deriving instance (Eq (Label (SimpleSignature s)))
deriving instance (Eq (Sort (SimpleSignature s)))

reifySignature :: ValidatedSignature
               -> (forall s . (Reifies s ValidatedSignature)
                           => Proxy (SimpleSignature s) -> a)
               -> a
reifySignature sig f = reify sig (\(proxy :: Proxy s) -> f @s Proxy)

{-
testSig = MkSignatureInfo (Set.fromList ["A","L"]) (Map.fromList [("cons",("L",["A","L"])),("nil",("L",[]))])
 -}

recognizeSignatureSyntax :: forall s var . (Reifies s ValidatedSignature)
                         => Pattern Text Text var
                         -> Maybe (SigPattern (SimpleSignature s) var)
recognizeSignatureSyntax p = cata (\p -> recognizeLayer p >>= sequenceA >>= return . Fix) p
  where
    recognizeLayer :: PatternF Text Text var p
                   -> Maybe (SigPatternF (SimpleSignature s) var p)
    recognizeLayer p = case p of
      Variable s v -> (\s' -> Variable s' v) <$> findSort s
      Application l ps -> (\l' -> Application l' ps) <$> findLabel l
      And s p1 p2 -> (\s' -> And s' p1 p2) <$> findSort s
      Not s p -> (\s' -> Not s' p) <$> findSort s
      Exists s1 s2 v p -> (\s1' s2' -> Exists s1' s2' v p) <$> findSort s1 <*> findSort s2
