# Logic Visualizer

OntoLogic includes a built-in visualizer that converts your `.logic` files into standard diagram formats (Mermaid, D2 Lang) and can automatically render them to images.

## Prerequisites

To render images (convert text to SVG/PNG), you need the following CLI tools installed:

### Mermaid (for Class Diagrams)

Install the Mermaid CLI (`mmdc`):

```bash
npm install -g @mermaid-js/mermaid-cli
```

### D2 Lang (for Structural Diagrams)

Install D2:

```bash
curl -fsSL https://d2lang.com/install.sh | sh -s --
```

*Note: If you don't have these tools, OntoLogic will still generate the source code files (`.mmd`, `.d2`) which you can paste into online editors.*

## Usage

Build the tool first:

```bash
stack build
```

### Generate Mermaid Class Diagram

```bash
stack exec ontologic-exe -- --input examples/hivemq_ecosystem_unified.logic --target mermaid --output .
# Generates: class_diagram.mmd
```

To render automatically (requires `mmdc`):

```bash
stack exec ontologic-exe -- --input examples/hivemq_ecosystem_unified.logic --target mermaid --output . --render
# Generates: class_diagram.mmd.svg
```

### Generate D2 Diagram

```bash
stack exec ontologic-exe -- --input examples/hivemq_ecosystem_unified.logic --target d2 --output .
# Generates: diagram.d2
```

To render automatically (requires `d2`):

```bash
stack exec ontologic-exe -- --input examples/hivemq_ecosystem_unified.logic --target d2 --output . --render
# Generates: diagram.d2.svg
```

## Supported Visualizations

| Format | Description | File Extension |
| :--- | :--- | :--- |
| **Mermaid** | Standard UML Class Diagram showing inheritance (`<|--`) and properties. | `.mmd` |
| **D2**      | Modern diagramming language showing structural relationships and nested properties. | `.d2`  |
| **Neo4j**   | Cypher query language to populate a graph database. | `.cypher` |
