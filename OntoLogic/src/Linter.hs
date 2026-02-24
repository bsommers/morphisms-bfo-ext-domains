module Linter where

import qualified Data.Map as Map
import qualified Data.Set as Set
import OntoLogic

lintRegistry :: Registry -> [String]
lintRegistry reg = 
    checkCircularInheritance (Map.fromList $ concepts reg) ++ checkContradictoryAxioms (axioms reg)

checkCircularInheritance :: Map.Map Name (Maybe Name) -> [String]
checkCircularInheritance conceptMap = 
    concatMap (findCycle [] Set.empty) (Map.keys conceptMap)
  where
    findCycle path visited node
      | node `elem` path = ["Circular inheritance: " ++ show (node : path)]
      | Set.member node visited = []
      | otherwise = case Map.lookup node conceptMap of
          Nothing -> [] -- Node not found in map
          Just Nothing -> [] -- No parent
          Just (Just parent) -> findCycle (node : path) (Set.insert node visited) parent

checkContradictoryAxioms :: [FOL] -> [String]
checkContradictoryAxioms [] = []
checkContradictoryAxioms (ax:axs) = 
    case ax of
        (And p (Not q)) | p == q -> ["Axiom contradiction: P and Not P."]
        _ -> checkContradictoryAxioms axs
