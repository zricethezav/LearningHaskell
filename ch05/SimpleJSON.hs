-- file: ch05/SimpleJSON.hs
-- assuming typeclass Eq, Ord, Show
-- aka we can compare, order, and show a representation of the JSON

-- A Haskell source file contains a definition of a single module. A module lets
-- us determine which names inside the module are accessible from other modules

-- A Haskell source file begins with a module declaration. This must precede all
-- definitions in the source file

-- The word module is reserved. It is followed by the name of the module, which
-- must begin with  a capital letter. A source file must have the same BASE NAME
-- as the name of the module it contains, ie. SimpleJson and SimpleJson.hs 
module SimpleJSON
    (
        JValue(..)
      , getString
      , getInt
      , getDouble
      , getBool
      , getObject
      , getArray
      , isNull
    ) where
-- following the module name is a list of exports, enclosed in parens. The where
-- keyword indicates that the body of the module follows

-- The special notation (..) that follows the name JValue indicates that we are
-- exporting both the type and all of its constructors
data JValue = JString String
             | JNumber Double
             | JBool Bool
             | JNull
             | JObject [(String, JValue)]
             | JArray [JValue]
             deriving (Eq, Ord, Show)

getString :: JValue -> Maybe String
getString (JString s) = Just s
getString _           = Nothing

getInt :: JValue -> Maybe Int
getInt (JNumber n) = Just (truncate n)
getInt _           = Nothing

getDouble :: JValue -> Maybe Double
getDouble (JNumber n) = Just n
getDouble _           = Nothing

getBool :: JValue -> Maybe Bool
getBool (JBool b) = Just b
getBool _         = Nothing

getObject :: JValue -> Maybe [(String, JValue)]
getObject (JObject o) = Just o
getObject _           = Nothing

getArray :: JValue -> Maybe [JValue]
getArray (JArray a) = Just a
getArray _          = Nothing

isNull v            = v == JNull

