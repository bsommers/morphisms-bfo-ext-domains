import json
from owlready2 import *
import datetime

def run_reasoner():
    print("Initializing Semantic Reasoner using Owlready2...")
    
    # 1. Setup the "World" and Ontology
    onto = get_ontology("http://factory.org/semantic_model.owl")

    with onto:
        # =====================================================================
        # TIER 1: UPPER ONTOLOGY (Basic Formal Ontology - BFO simulated)
        # =====================================================================
        class Entity(Thing): pass
        
        class Continuant(Entity): pass
        class MaterialEntity(Continuant): pass
        
        class Occurrent(Entity): pass
        class Process(Occurrent): pass
        
        class Property(Thing): pass
        class Quality(Property): pass
        class PressureQuality(Quality): pass

        class measures(ObjectProperty):
            domain = [MaterialEntity]
            range = [Quality]

        # =====================================================================
        # TIER 2: BFO EXTENSIONS (Plant Domain Topology)
        # =====================================================================
        class PlantEntity(MaterialEntity): pass
        
        class PlantEquipment(PlantEntity): pass
        class Boiler(PlantEquipment): pass
        class Pump(PlantEquipment): pass

        class PlantSensor(PlantEntity): pass

        # The 'Bridge' Property: Links the "Messy" MQTT world to the "Clean" OWL world
        # This relationship effectively establishes the homomorphism between the 
        # physical topic tree and the logical graph.
        class has_mqtt_source(DataProperty, FunctionalProperty):
            domain = [PlantSensor]
            range  = [str]
            
        class located_in(DataProperty, FunctionalProperty):
            domain = [PlantEntity]
            range = [str]

        # Cross-branch logic: Relationships that don't fit in a strict taxonomy hierarchy
        class _is_fed_by(ObjectProperty):
            domain = [Boiler]
            range = [Pump]

        class has_status(DataProperty, FunctionalProperty):
            domain = [PlantEquipment]
            range = [str]

        # =====================================================================
        # TIER 3: DOMAIN ONTOLOGIES (Specific Application Logic)
        # =====================================================================
        class PressureSensor(PlantSensor):
            # Isomorphic property mapping to BFO traits
            equivalent_to = [PlantSensor & (measures.some(PressureQuality))]

        class CriticalBoiler(Boiler):
            # A Boiler becomes a CriticalBoiler if it is fed by a pump that is OFF.
            # Using Python-based reactive reasoning for complex multi-property evaluation
            pass


    # 2. Registration Service: Aligning the Non-Isomorphic IDs
    class SensorRegistrar:
        def __init__(self, ontology):
            self.onto = ontology

        def register_device(self, mqtt_id, device_type_str, plant_location):
            with self.onto:
                try:
                    TargetClass = getattr(self.onto, device_type_str)
                except AttributeError:
                    return None, f"Error: {device_type_str} is not a valid mapped class."

                # Create Digital Twin Instance (Continuant)
                twin_name = f"twin_{mqtt_id}"
                digital_twin = TargetClass(twin_name)
                
                # Assign the Topic ID as a semantic property (Homomorphic mapping)
                digital_twin.has_mqtt_source = mqtt_id
                digital_twin.located_in = plant_location
                return digital_twin, f"Registered MQTT:{mqtt_id} -> logical BFO subclass:{device_type_str}"

    registrar = SensorRegistrar(onto)
    print("\n[PHASE 1] Semantic Registration Phase -----------------------")
    sensor_twin, msg1 = registrar.register_device("182736", "PressureSensor", "Zone_A")
    print(f"  {msg1}")

    with onto:
        boiler_42 = Boiler("unit_42")
        pump_07 = Pump("pump_07")
        boiler_42._is_fed_by.append(pump_07)
        pump_07.has_status = "ON"
        print(f"  Registered logical relationship: {boiler_42.name} is fed by {pump_07.name}")

    print("\n[PHASE 2] MQTT Telemetry Contextualization ------------------")
    
    def process_telemetry(topic, payload):
        print(f"\nIncoming Message -> Topic: '{topic}' | Payload: '{payload}'")
        topic_parts = topic.split('/')
        
        if len(topic_parts) == 3 and topic_parts[0] == "sensors" and topic_parts[1] == "data":
            # Resolving non-isomorphic topic (Structural Homomorphism)
            mqtt_id = topic_parts[2]
            matched_sensor = onto.search_one(has_mqtt_source=mqtt_id)
            if matched_sensor:
                print(f"  [Resolver] Topic '{topic}' is not an isomorphic path.")
                print(f"  [Resolver] Successfully mapped to Logical Instance: {matched_sensor.name} (Class: {matched_sensor.is_a})")
            else:
                print(f"  [Resolver] Unknown sensor ID: {mqtt_id}")
                
        elif len(topic_parts) == 4 and topic_parts[0] == "factory":
            # Processing isomorphic topic (Isomorphism)
            _, device_type, device_id, msg_type = topic_parts
            if msg_type == "status":
                data = json.loads(payload)
                
                # Isomorphic structural lookup: topic maps exactly to instance naming
                matched_equipment = onto.search_one(iri=f"*{device_id}")
                if matched_equipment:
                    matched_equipment.has_status = data.get("status")
                    print(f"  [Resolver] Isomorphic Lookup: Updated {matched_equipment.name} status to '{data.get('status')}'")
                    
                    # Graph Logic reasoning across branches:
                    if data.get("status") == "OFF" and "Pump" in [str(c) for c in matched_equipment.is_a]:
                        # Infer consequences on the larger graph geometry
                        affected_boilers = onto.search(_is_fed_by=matched_equipment)
                        for b in affected_boilers:
                            print(f"  🚨 [REASONER ALERT] Safety Violation! Cross-branch dependency broken.")
                            print(f"                      {b.name} is in danger because its feeder {matched_equipment.name} is OFF.")
                else:
                    print(f"  [Resolver] Unknown equipment: {device_id}")

    # Simulated MQTT stream demonstrating both Homomorphic registration resolution and Isomorphic explicit mapping
    process_telemetry("sensors/data/182736", '{"psi": 120}')
    process_telemetry("factory/pumps/pump_07/status", '{"status": "ON"}')
    process_telemetry("factory/pumps/pump_07/status", '{"status": "OFF"}')
    
    print("\n[PHASE 3] Formal Logic Verification -------------------------")
    print("  Running sync_reasoner() to assert baseline consistency...")
    try:
        sync_reasoner(onto, debug=0)
        print("  ✓ Formal logical consistency verified.")
    except Exception as e:
        print(f"  ❌ Inconsistency detected: {e}")

    print("\n[PHASE 4] Reverse Engineering to .logic Files ---------------")
    
    # Pre-calculate properties so we can assign them to domains during export
    object_properties = list(onto.object_properties())
    data_properties = list(onto.data_properties())

    def export_to_logic(filename, namespace_filter, is_upper=False):
        """
        Exports OWL classes and their relationships/attributes to a flat .logic file.
        """
        with open(filename, 'w') as f:
            f.write(f"// Auto-generated Semantic Logic Export: {filename}\n")
            f.write("namespace factory\n\n")
            
            for cls in onto.classes():
                if is_upper and cls.__name__ in ["Entity", "Continuant", "MaterialEntity", "Occurrent", "Process", "Property", "Quality", "PressureQuality"]:
                    f.write(f"type {cls.__name__}\n")
                    for parent in cls.is_a:
                        if hasattr(parent, '__name__'):
                            f.write(f"  sub {parent.__name__}\n")
                            
                    # Export Object Properties as Relationships (`rel`)
                    for op in object_properties:
                        if cls in getattr(op, 'domain', []):
                            ranges = getattr(op, 'range', [])
                            if ranges:
                                f.write(f"  rel {op.__name__} {ranges[0].__name__}\n")
                                
                    # Export Data Properties as Attributes (`has`)
                    for dp in data_properties:
                        if cls in getattr(dp, 'domain', []):
                            f.write(f"  has {dp.__name__}\n")
                            
                    f.write("\n")
                elif not is_upper and cls.__name__ in namespace_filter:
                    f.write(f"type {cls.__name__}\n")
                    for parent in cls.is_a:
                        if hasattr(parent, '__name__') and parent.__name__ != "Thing":
                            f.write(f"  sub {parent.__name__}\n")
                            
                    # Export Object Properties as Relationships (`rel`)
                    for op in object_properties:
                        if cls in getattr(op, 'domain', []):
                            ranges = getattr(op, 'range', [])
                            if ranges:
                                f.write(f"  rel {op.__name__} {ranges[0].__name__}\n")
                                
                    # Export Data Properties as Attributes (`has`)
                    for dp in data_properties:
                        if cls in getattr(dp, 'domain', []):
                            f.write(f"  has {dp.__name__}\n")
                            
                    f.write("\n")
                    
        print(f"  ✓ Exported {filename}")

    # BFO Core
    export_to_logic("bfo_core.logic", [], is_upper=True)
    # BFO Extension (Plant Topology)
    export_to_logic("bfo_extension_plant.logic", ["PlantEntity", "PlantEquipment", "Boiler", "Pump", "PlantSensor"])
    # App Domain Layer
    export_to_logic("domain_specific.logic", ["PressureSensor", "CriticalBoiler"])

if __name__ == '__main__':
    run_reasoner()
