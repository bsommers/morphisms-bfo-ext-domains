import csv
from neo4j import GraphDatabase
# from models import Sensor (Assuming generated models are in path)

def load_data(file_path):
    with GraphDatabase.driver("bolt://localhost:7687", auth=("neo4j", "password")) as driver:
        with driver.session() as session:
            reader = csv.DictReader(open(file_path))
            for row in reader:
                session.run("MERGE (n:Device {id: $id}) SET n += $props", id=row['id'], props=row)

if __name__ == "__main__":
    load_data("data.csv")
