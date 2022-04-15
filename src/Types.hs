module Types where

-- name(s) : direction type := val
data Port = Port [String] String String (Maybe String)
    deriving Show

-- name : type := val
data Generic = Generic [String] String (Maybe String)
    deriving Show

data VhdlFile = VhdlFile String (Maybe [Generic]) [Port]

instance Show VhdlFile where
    show (VhdlFile name generics ports) =
        "file: " ++ name ++ "\n" ++ getGenerics ++ getPorts
        where
            getGenerics = maybe "" (\xs -> "generics:\n" ++ concatList xs) generics
            getPorts = "ports:\n" ++ concatList ports
            
            concatList :: Show a => [a] -> String
            concatList = concatMap (flip (++) "\n" . show)
