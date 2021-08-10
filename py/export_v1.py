from pymongo import MongoClient
from pathlib import Path
import csv

def export(filepath, dbname, user, password):
    infile = Path(filepath)
    filename = infile.stem
    data = {}
    with open(infile) as f:
        comma = csv.DictReader(f)
        data = [row for row in comma]
    uri = f"mongodb+srv://{user}:{password}@cluster0.isyog.mongodb.net/myFirstDatabase?retryWrites=true&w=majority"
    client = MongoClient(uri)
    db = client[dbname]
    collection = db[filename]
    collection.insert_many(data)