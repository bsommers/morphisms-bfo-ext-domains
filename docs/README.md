# Semantic AI Architecture for IoT

This repository demonstrates the convergence of high-velocity IoT metadata and Semantic AI logic. It solves the architectural "Taxonomy Trap" found in large-scale MQTT deployments by enforcing an **Ontology-Driven Semantic Gateway**.

## Overview

When IoT deployments scale, organizations often rely strictly on their MQTT topic structure (e.g., `factory/line1/boiler/42`) to define what an asset is. While simple, attempting to map highly interconnected physical factory graphs into strict hierarchical trees inevitably collapses logic and limits AI inference capabilities.

This project demonstrates a Python-based reasoning engine utilizing a **3-Tier Semantic Architecture** and mathematical morphisms to solve this disparity:

1. **Upper Ontology (Basic Formal Ontology - BFO):** The universal logical backbone.
2. **BFO Extensions (Plant Topology):** Domain-specific categorizations (e.g., Boiler, Sensor).
3. **Application Domain:** Logic constraints and reactive alerts (e.g., determining Critical States).

By combining pure Knowledge Graphs with these formal Ontologies, the system distinguishes between valid physical reality and corrupted/hallucinated sensor data.

## Contents

* `/ontology_reasoner.py` - Core logic reasoner and MQTT simulator powered by `owlready2`.
* `/article.md` - Technical deep-dive on Isomorphisms and Homomorphisms in semantic systems.
* `/OntoLogic/` - A git subtree integration of the OntoLogic engine repository.
* `/docs/` - System documentation encompassing architecture and execution profiles.

## More Information

* For a deep dive into the system design, consult [ARCHITECTURE.md](ARCHITECTURE.md).
* For instructions on testing the logic reasoner, see [RUNNING.md](RUNNING.md).

## License

Provided under the **GNU General Public License v3.0 (GPLv3)**. Please see the [LICENSE](../LICENSE) file for complete details.
