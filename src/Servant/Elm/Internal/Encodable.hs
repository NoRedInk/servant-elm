{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Servant.Elm.Internal.Encodable (renderUrlEncoder, URLEncodable) where

import Data.Bifunctor (bimap)
import Data.Proxy
import Data.Text hiding (zip)
import Elm
import GHC.Generics
import Generic.Data
import Servant.API (
    ToHttpApiData (..),
 )
import Servant.Elm.Internal.Generate (docToText)
import Text.PrettyPrint.Leijen.Text hiding (
    (<$>),
 )

collectConstructors :: ElmDatatype -> [Text]
collectConstructors (ElmPrimitive _) = error "Invalid state"
collectConstructors (ElmDatatype _ constructors') = extractNames constructors'
  where
    extractNames :: ElmConstructor -> [Text]
    extractNames (NamedConstructor name ElmEmpty) = pure name
    extractNames (MultipleConstructors cons') = cons' >>= extractNames
    extractNames _ = error "Invalid state"

type URLEncodable a = (Generic a, GEnum StandardEnum (Rep a), ElmType a, ToHttpApiData a)

elmConstructorOf :: (URLEncodable a) => Proxy a -> Int -> Text
elmConstructorOf p i =
    names !! i
  where
    datatype = toElmType p
    names = collectConstructors datatype

constructors :: (URLEncodable a) => Proxy a -> [a]
constructors _ = genumFrom (gtoEnum 0)

toElmString :: (URLEncodable a) => Proxy a -> [(Text, Text)]
toElmString p = bimap (elmConstructorOf p) toQueryParam <$> zip [0 ..] (constructors p)

renderUrlEncoder :: (URLEncodable a) => Proxy a -> Text
renderUrlEncoder p =
    docToText $
        vsep
            [ hcat ["stringFrom", pretty datatypeName', " : ", pretty datatypeName', " -> String"]
            , hcat ["stringFrom", pretty datatypeName', " value = case value of"]
            , indent 4 $ vsep $ renderCase <$> toElmString p
            ]
  where
    datatypeName' = case toElmType p of
        ElmPrimitive _ -> error "Invalid state"
        ElmDatatype name _ -> name
    renderCase (constructor, result) = hsep [pretty constructor, "->", dquotes $ pretty result]
