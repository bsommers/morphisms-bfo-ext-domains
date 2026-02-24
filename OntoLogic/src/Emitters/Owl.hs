module Emitters.Owl where

import OntoLogic

emitOwl :: Registry -> String
emitOwl reg = 
    xmlHeader ++
    ontologyHeader ++
    concatMap emitClass (concepts reg) ++
    concatMap emitProperty (properties reg) ++
    xmlFooter

xmlHeader :: String
xmlHeader = "<?xml version=\"1.0\"?>\n<rdf:RDF xmlns=\"http://www.hivemq.com/ontologies/fleet#\"\n"

ontologyHeader :: String
ontologyHeader = "    <owl:Ontology rdf:about=\"http://www.hivemq.com/ontologies/fleet\"/>\n"

xmlFooter :: String
xmlFooter = "</rdf:RDF>"

emitClass :: (Name, Maybe Name) -> String
emitClass (name, Nothing) = "    <owl:Class rdf:about=\"#" ++ name ++ "\"/>\n"
emitClass (name, Just parent) = "    <owl:Class rdf:about=\"#" ++ name ++ "\"><rdfs:subClassOf rdf:resource=\"#" ++ parent ++ "\"/></owl:Class>\n"

emitProperty :: Property -> String
emitProperty p = "    <owl:DatatypeProperty rdf:about=\"#" ++ propName p ++ "\">\n" ++
                 "        <rdfs:domain rdf:resource=\"#" ++ domain p ++ "\"/>\n" ++
                 "    </owl:DatatypeProperty>\n"
