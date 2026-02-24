module Emitters.Python where

import OntoLogic

emitPython :: Registry -> String
emitPython reg = 
    "from pydantic import BaseModel, Field, validator\n" ++
    "from typing import Optional, List\n\n" ++
    concatMap (emitPythonClass (properties reg)) (concepts reg)

emitPythonClass :: [Property] -> (Name, Maybe Name) -> String
emitPythonClass allProps (name, parent) = 
    "class " ++ name ++ "(" ++ maybe "BaseModel" id parent ++ "):\n" ++
    concatMap emitField (filter (\p -> domain p == name) allProps) ++
    "    pass\n\n"

emitField :: Property -> String
emitField p = "    " ++ propName p ++ ": " ++ mapType (rangeType p) ++ "\n"

mapType :: String -> String
mapType "Integer" = "int"
mapType "Boolean" = "bool"
mapType _         = "str"
