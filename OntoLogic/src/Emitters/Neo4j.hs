module Emitters.Neo4j where

import OntoLogic

emitCypher :: Registry -> String
emitCypher reg = 
    "// Nodes and Hierarchy\n" ++
    concatMap emitNode (concepts reg) ++
    "// Constraints\n" ++
    concatMap emitConstraint (properties reg)

emitNode :: (Name, Maybe Name) -> String
emitNode (name, parent) = 
    "MERGE (n:" ++ name ++ " {name: '" ++ name ++ "'})\n" ++
    maybe "" (\p -> "MERGE (n)-[:SUBCLASS_OF]->(:" ++ p ++ " {name: '" ++ p ++ "'})\n") parent

emitConstraint :: Property -> String
emitConstraint p = 
    "CREATE CONSTRAINT IF NOT EXISTS FOR (n:" ++ domain p ++ ") REQUIRE n." ++ propName p ++ " IS NOT NULL;\n"
