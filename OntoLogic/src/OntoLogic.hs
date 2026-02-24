{-# LANGUAGE DeriveGeneric, OverloadedStrings #-}
module OntoLogic where

import GHC.Generics
import Data.Hashable
import qualified Data.Map as Map

type Name = String

data FOL = Predicate Name [Term]
         | And FOL FOL
         | Or FOL FOL
         | Not FOL
         | Implies FOL FOL
         | Iff FOL FOL
         | ForAll Name FOL
         | Exists Name FOL
         deriving (Show, Eq, Generic)

data Term = Var Name | Constant String deriving (Show, Eq, Generic)

instance Hashable FOL
instance Hashable Term

data Registry = Registry 
    { concepts   :: [(Name, Maybe Name)] -- Simplified for Parsing, normally Map
    , axioms     :: [FOL]
    , properties :: [Property]
    } deriving (Show, Generic)

-- Helper to convert list to map if needed later
-- conceptsMap :: Registry -> Map.Map Name Name
-- conceptsMap r = Map.fromList (concepts r)

data Property = Property 
    { propName  :: Name
    , domain    :: Name
    , rangeType :: String 
    } deriving (Show, Generic)
