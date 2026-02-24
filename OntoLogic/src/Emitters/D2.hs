module Emitters.D2 where

import OntoLogic
import Data.List (intercalate)

emitD2 :: Registry -> String
emitD2 reg =
    intercalate "\n" $
    map emitClass (concepts reg) ++
    map emitRelation (concepts reg) ++
    map emitPropertyReference (properties reg)

-- Emit class definition: Name: { shape: class }
emitClass :: (Name, Maybe Name) -> String
emitClass (name, _) = name ++ ": { shape: class }"

-- Emit inheritance: Child -> Parent
emitRelation :: (Name, Maybe Name) -> String
emitRelation (name, Just parent) = name ++ " -> " ++ parent
emitRelation _ = ""

-- Emit attributes: Concept.propName: Type
-- D2 allows adding fields to the block via dot notation outside
emitPropertyReference :: Property -> String
emitPropertyReference p = domain p ++ "." ++ propName p ++ ": " ++ rangeType p
