// Nodes and Hierarchy
MERGE (n:Asset {name: 'Asset'})
MERGE (n:Vehicle {name: 'Vehicle'})
MERGE (n)-[:SUBCLASS_OF]->(:Asset {name: 'Asset'})
MERGE (n:SUV {name: 'SUV'})
MERGE (n)-[:SUBCLASS_OF]->(:Vehicle {name: 'Vehicle'})
MERGE (n:Sedan {name: 'Sedan'})
MERGE (n)-[:SUBCLASS_OF]->(:Vehicle {name: 'Vehicle'})
MERGE (n:Truck {name: 'Truck'})
MERGE (n)-[:SUBCLASS_OF]->(:Vehicle {name: 'Vehicle'})
MERGE (n:Equipment {name: 'Equipment'})
MERGE (n)-[:SUBCLASS_OF]->(:Asset {name: 'Asset'})
MERGE (n:Station {name: 'Station'})
MERGE (n)-[:SUBCLASS_OF]->(:Equipment {name: 'Equipment'})
MERGE (n:Robot {name: 'Robot'})
MERGE (n)-[:SUBCLASS_OF]->(:Equipment {name: 'Equipment'})
MERGE (n:WeldingRobot {name: 'WeldingRobot'})
MERGE (n)-[:SUBCLASS_OF]->(:Robot {name: 'Robot'})
MERGE (n:PaintingRobot {name: 'PaintingRobot'})
MERGE (n)-[:SUBCLASS_OF]->(:Robot {name: 'Robot'})
MERGE (n:AssemblyRobot {name: 'AssemblyRobot'})
MERGE (n)-[:SUBCLASS_OF]->(:Robot {name: 'Robot'})
MERGE (n:Scanner {name: 'Scanner'})
MERGE (n)-[:SUBCLASS_OF]->(:Equipment {name: 'Equipment'})
MERGE (n:OpticalScanner {name: 'OpticalScanner'})
MERGE (n)-[:SUBCLASS_OF]->(:Scanner {name: 'Scanner'})
MERGE (n:LaserScanner {name: 'LaserScanner'})
MERGE (n)-[:SUBCLASS_OF]->(:Scanner {name: 'Scanner'})
MERGE (n:BluewristScanner {name: 'BluewristScanner'})
MERGE (n)-[:SUBCLASS_OF]->(:LaserScanner {name: 'LaserScanner'})
MERGE (n:SafetySystem {name: 'SafetySystem'})
MERGE (n)-[:SUBCLASS_OF]->(:Equipment {name: 'Equipment'})
MERGE (n:LightCurtain {name: 'LightCurtain'})
MERGE (n)-[:SUBCLASS_OF]->(:SafetySystem {name: 'SafetySystem'})
// Constraints
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Vehicle) REQUIRE n.vin IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Vehicle) REQUIRE n.model_year IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Vehicle) REQUIRE n.weld_count IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Vehicle) REQUIRE n.is_painted IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Vehicle) REQUIRE n.is_quality_approved IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Vehicle) REQUIRE n.paint_color IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Station) REQUIRE n.station_id IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Equipment) REQUIRE n.is_active IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Equipment) REQUIRE n.temperature IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Robot) REQUIRE n.current_job IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Scanner) REQUIRE n.defect_detected IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:Scanner) REQUIRE n.scan_resolution_microns IS NOT NULL;
CREATE CONSTRAINT IF NOT EXISTS FOR (n:SafetySystem) REQUIRE n.barrier_active IS NOT NULL;
