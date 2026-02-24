# Running the Inference Engine

This document outlines the steps to verify the installation, execute the Semantic Reasoner, and view the automated logic `.logic` exports.

## Prerequisites

The logic reasoner operates purely on Python 3 and the `owlready2` semantic web engine library.

Since standard pip installations may be externally managed by your OS package system, it is recommended to run this inside a virtual environment.

```sh
# Set up the virtual environment
python3 -m venv venv

# Activate the environment
source venv/bin/activate

# Install the dependencies
pip install owlready2
```

## Running the Reasoner

From the root directory, simply invoke the reasoner script using Python:

```sh
python3 ontology_reasoner.py
```

### Execution Phases Explained

When you run the engine, you will observe the pipeline functioning in four distinct phases:

1. **[PHASE 1] Semantic Registration Phase:**
   Registers a non-isomorphic identifier (`182736`) and establishes the mathematical homolographic mapping bridging the topic to a formal OWL Entity.

2. **[PHASE 2] MQTT Telemetry Contextualization:**
   Simulates inbound MQTT messages interacting with the Digital Twin in-memory state. You will see an anomaly trigger representing a cross-branch validation error (e.g., catching a cascading failure of a Boiler caused by a Pump going down).

3. **[PHASE 3] Formal Logic Verification:**
   Executes `sync_reasoner()` on the local machine. This acts as the legal arbiter of logic. If no contradictions mathematically exist in the state, stability is verified.

4. **[PHASE 4] Reverse Engineering to .logic Files:**
   Dynamically reverse-engineers the in-memory graph into flat, OntoLogic-compliant `.logic` text files separating Upper BFO, Extension topologies, and Domain layers.

## Outputs

Upon a successful test, check your local root directory for the `*.logic` system exports:

- `bfo_core.logic`
- `bfo_extension_plant.logic`
- `domain_specific.logic`

You can verify their structure using standard cat command:

```sh
cat bfo_core.logic
```
