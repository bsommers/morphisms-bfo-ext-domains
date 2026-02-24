# OntoLogic

```text
  ______           __           __                      __            
 /      \         /  |         /  |                    /  |           
/$$$$$$  |_______ $$ |_ ______ $$ |  ______   ______  $$/   _______   
$$ |  $$ |/      \$$   /      \$$ | /      \ /      \ /  | /       |  
$$ |  $$ |$$$$$$  |$$ |$$$$$$  |$$ |/$$$$$$  /$$$$$$  |$$ |/$$$$$$/   
$$ |  $$ |$$ |  $$ |$$ |$$ |  $$ |$$ |$$ |  $$ |$$ |  $$ |$$ |$$ |    
$$ \__$$ |$$ |  $$ |$$ |$$ \__$$ |$$ |$$ \__$$ |$$ \__$$ |$$ |$$ \_____ 
$$    $$/ $$ |  $$ |$$ |$$    $$/ $$ |$$    $$/ $$    $$ |$$ |$$       |
 $$$$$$/  $$/   $$/ $$/  $$$$$$/  $$/  $$$$$$/   $$$$$$$ |$$/  $$$$$$$/ 
                                                    /  \__$$ |            
                                                    $$    $$/             
                                                     $$$$$$/              
```

## Logic as Code for the IoT Edge

OntoLogic is a formal logic transpiler for IoT ecosystems.

## Goals

- **Cross-Platform Consistency**: Ensure logical rules are functionally identical across Java, Python, and Neo4j.
- **Formal Verification**: Use Haskell's type system and a custom linter to catch circular dependencies and logical contradictions at compile-time.
- **Edge Efficiency**: Generate lightweight, performant models suitable for deployment on low-power edge gateways.
- **Living Documentation**: Provide visual and structural representations (Mermaid, D2, OWL) of the logic for both humans and machines.

## Components

- **Haskell Compiler**: Parses .logic files and emits Pydantic, Java, Cypher, and OWL.
- **Linter**: Checks for circular inheritance and logical contradictions.
- **Integration**: Docker Compose stack with Neo4j.

## Usage

`stack run -- -i logic/fleet.logic -t java -o generated/`
