// Nodes and Hierarchy
MERGE (n:Asset {name: 'Asset'})
MERGE (n:Machine {name: 'Machine'})
MERGE (n)-[:SUBCLASS_OF]->(:Asset {name: 'Asset'})
MERGE (n:Sensor {name: 'Sensor'})
MERGE (n)-[:SUBCLASS_OF]->(:Asset {name: 'Asset'})
MERGE (n:Mixer {name: 'Mixer'})
MERGE (n)-[:SUBCLASS_OF]->(:Machine {name: 'Machine'})
MERGE (n:Oven {name: 'Oven'})
MERGE (n)-[:SUBCLASS_OF]->(:Machine {name: 'Machine'})
MERGE (n:ProofingChamber {name: 'ProofingChamber'})
MERGE (n)-[:SUBCLASS_OF]->(:Machine {name: 'Machine'})
MERGE (n:CuttingMachine {name: 'CuttingMachine'})
MERGE (n)-[:SUBCLASS_OF]->(:Machine {name: 'Machine'})
MERGE (n:BaggingMachine {name: 'BaggingMachine'})
MERGE (n)-[:SUBCLASS_OF]->(:Machine {name: 'Machine'})
MERGE (n:PackagingMachine {name: 'PackagingMachine'})
MERGE (n)-[:SUBCLASS_OF]->(:Machine {name: 'Machine'})
MERGE (n:Refrigerator {name: 'Refrigerator'})
MERGE (n)-[:SUBCLASS_OF]->(:Machine {name: 'Machine'})
MERGE (n:Camera {name: 'Camera'})
MERGE (n)-[:SUBCLASS_OF]->(:Sensor {name: 'Sensor'})
MERGE (n:SecurityCamera {name: 'SecurityCamera'})
MERGE (n)-[:SUBCLASS_OF]->(:Camera {name: 'Camera'})
// Constraints
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Machine) REQUIRE n.is_active IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Machine) REQUIRE n.temperature IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Machine) REQUIRE n.humidity IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Mixer) REQUIRE n.rpm IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Camera) REQUIRE n.is_recording IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:PackagingMachine) REQUIRE n.items_per_minute IS NOT NULL;
