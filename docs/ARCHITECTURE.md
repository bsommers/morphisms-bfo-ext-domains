# System Architecture

The semantic architecture bridges the gap between raw, loosely-structured JSON-LD / MQTT messages and strict formal logic systems.

## The 3-Tier Logic Topology

To effectively manage industrial data at scale without manually hardcoding rules for each new sensor, the system employs a three-tier semantic stack:

### Tier 1: Basic Formal Ontology (BFO)

The baseline of reality. BFO separates concepts into `Continuants` (things that exist in physical space, like a piece of equipment) and `Occurrents` (things that unfold over time, like an MQTT telemetry measurement).

### Tier 2: Domain Extensions

This maps physical devices to broader semantic categories, extending the BFO definitions.

- e.g., `PlantEquipment`, `PlantSensor`.
This tier forms the "Homomorphic Bridge", utilizing properties like `has_mqtt_source` to bind arbitrary, "messy" device identifiers to clean logical structures.

### Tier 3: Application Domain

Where the active reasoning happens. We apply relational algebra and mathematical constraints here:

- e.g., "A CriticalBoiler is any Boiler fed by a Pump that is currently OFF."

## Morphisms in Ontology Mapping

### Isomorphisms

An isomorphism occurs when there is a 1:1 structural bijection between the physical reality (MQTT Topic length and namespaces) and the logical reality (Digital Twin classes and properties).

- When a topic `factory/pumps/pump_07/status` arrives, the system processes it immediately without requiring a heavy graph lookup because the system *isomorphicly* mirrors the logic graph.

### Homomorphisms

A homomorphism occurs when a multi-dimensional graph is "collapsed" into a flat identifier, losing contextual hierarchy.

- A telemetry reading arriving at `sensors/data/182736` lacks context. Does `182736` represent a thermometer, a valve, or a boiler?
- Our **Registration Service** rectifies this by formally registering the flat property `182736` to a structural graph instance (`PressureSensor`), thus bridging the gap.

## The Semantic Gateway

The system is executed through the `ontology_reasoner.py` script, which functions as a digital twin gateway.
It consumes raw topic/payload combinations from the edge environment and:

1. Maps structural properties to logical continuants.
2. Infers complex cross-branch relationships.
3. Alerts on formal semantic violations before the data reaches your Knowledge Graph.
4. Generates `.logic` flat files strictly for programmatic use by secondary frameworks like `OntoLogic`.
