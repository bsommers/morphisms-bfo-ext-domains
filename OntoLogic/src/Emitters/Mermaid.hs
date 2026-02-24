module Emitters.Mermaid where

import OntoLogic
import Data.List (intercalate)

emitMermaid :: Registry -> String
emitMermaid reg =
    "classDiagram\n" ++
    concatMap emitClass (concepts reg) ++
    concatMap emitRelation (concepts reg) ++
    concatMap emitProperty (properties reg)

-- Emit class definition (optional if relations exist, but good for orphans)
emitClass :: (Name, Maybe Name) -> String
emitClass (name, _) = "    class " ++ name ++ "\n"

-- Emit inheritance: Parent <|-- Child
emitRelation :: (Name, Maybe Name) -> String
emitRelation (name, Just parent) = "    " ++ parent ++ " <|-- " ++ name ++ "\n"
emitRelation _ = ""

-- Emit attributes: Class : +Type name
emitProperty :: Property -> String
emitProperty p = "    " ++ domain p ++ " : +" ++ rangeType p ++ " " ++ propName p ++ "\n"
