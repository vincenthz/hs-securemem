{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
module Data.SecureMemTests
  (
    main
  , defaultTestGroup
) where

import           Control.Applicative
import           Data.Byteable
import           Data.SecureMem
import           Data.String           (fromString)
import qualified Data.ByteString       as B
import qualified Data.Text             as T
import qualified Data.Text.Encoding    as TE
import qualified Test.QuickCheck       as QC
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck
import           Test.Tasty.TH

defaultTestGroup :: TestTree
defaultTestGroup = $(testGroupGenerator)

instance ToSecureMem T.Text where
        toSecureMem = secureMemFromByteString . TE.encodeUtf8

main :: IO ()
main = defaultMain defaultTestGroup

case_secureMemEq = do
    let l = "Joh1R2dYzkRvDkqv3sygm5YyK8Gi4ShZqbhK2gxcs2u" :: B.ByteString
        r = "Joh1R2dYzkRvDkqv3sygm5YyK8Gi4ShZqbhK2gxcs2U" :: B.ByteString
    (toSecureMem l /= toSecureMem r) @? "Unequal input should not be equal using SecureMem.Eq"

prop_secureMemEncodeDecodeText = f
  where
    f :: T.Text -> Bool
    f t = t == secureMemToText (toSecureMem t)
    secureMemToText :: SecureMem -> T.Text
    secureMemToText = TE.decodeUtf8 . toBytes

prop_secureMemEqText = f
  where
    f :: T.Text -> T.Text -> Bool
    f t1 t2 = let eqt = t1 == t2
                  eqs = toSecureMem t1 == toSecureMem t2
              in eqt == eqs

prop_secureMemEqText' = f
  where
    f :: T.Text -> T.Text -> Bool
    f tl tr = let t1 = ("Joh1R2dYzkRvDkqv3sygm5YyK8Gi4ShZqbhK2gxcs2" :: T.Text) `T.append` tl
                  t2 = ("Joh1R2dYzkRvDkqv3sygm5YyK8Gi4ShZqbhK2gxcs2" :: T.Text) `T.append` tr
                  eqt = t1 == t2
                  eqs = toSecureMem t1 == toSecureMem t2
              in eqt == eqs

prop_secureMemEqByteString = f
  where
    f :: B.ByteString -> B.ByteString -> Bool
    f tl tr = let t1 = ("Joh1R2dYzkRvDkqv3sygm5YyK8Gi4ShZqbhK2gxcs2" :: B.ByteString) `B.append` tl
                  t2 = ("Joh1R2dYzkRvDkqv3sygm5YyK8Gi4ShZqbhK2gxcs2" :: B.ByteString) `B.append` tr
                  eqt = t1 == t2
                  eqs = toSecureMem t1 == toSecureMem t2
              in eqt == eqs

instance Arbitrary T.Text where
    arbitrary = fromString <$> (arbitrary :: QC.Gen String)

instance Arbitrary B.ByteString where 
    arbitrary = B.pack <$> arbitrary


