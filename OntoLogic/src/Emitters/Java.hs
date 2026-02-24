module Emitters.Java where

import OntoLogic

emitJava :: Registry -> String
emitJava reg = 
    "package com.hivemq.generated;\n" ++
    "import jakarta.validation.constraints.*;\n\n" ++
    concatMap (emitJavaClass (properties reg)) (concepts reg)

emitJavaClass :: [Property] -> (Name, Maybe Name) -> String
emitJavaClass allProps (name, parent) = 
    "public class " ++ name ++ " extends " ++ maybe "Object" id parent ++ " {\n" ++
    concatMap emitField (filter (\p -> domain p == name) allProps) ++
    "}\n\n"

emitField :: Property -> String
emitField p = "    @NotNull\n    private " ++ mapType (rangeType p) ++ " " ++ propName p ++ ";\n"

mapType :: String -> String
mapType "Integer" = "Integer"
mapType "Boolean" = "boolean"
mapType _         = "String"
