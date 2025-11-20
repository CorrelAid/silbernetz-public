import os
import random
import polars as pl 
from faker import Faker
from datetime import datetime

# contact ids
n_calls = 800000
#n_calls = 80 #development

n_callers = 30000
contact_ids = range(1, n_calls + 1)

fake = Faker("de_DE")
Faker.seed(5432)

ortskennzahlen_map = pl.read_csv("../geo/mastertable_vorw_bdl.csv", schema_overrides = {
    "Ortsnetzkennzahl": pl.String,
    "Ortsnetzname": pl.String,
    "Bundesland": pl.String,
    "PLZ": pl.String
})
print(ortskennzahlen_map)
ortskennzahlen_map["Ortsnetzkennzahl"]

calls = {
    "date" : [fake.date_between(datetime(2020, 1, 1), datetime(2025, 8, 8)) for _ in range(n_calls)],
    "time" : [fake.time() for _ in range(n_calls)],
    "caller": [random.choice(range(0, n_callers)) for _ in range(n_calls)],
    "duration_inbound" : [random.randrange(0, 2000, 1) for _ in range(n_calls)],
    "duration_outbound" : [random.randrange(0, 2000, 1) for _ in range(n_calls)],
    "success" : [random.choices(["TRUE", "FALSE"], [0.61, 0.39])[0] for _ in range(n_calls)],
    "is_landline":  [random.choices(["TRUE", "FALSE"], [0.55, 0.45])[0] for _ in range(n_calls)],
    "Ortsnetzkennzahl": [random.choice(ortskennzahlen_map["Ortsnetzkennzahl"]) for _ in range(n_calls)],
    "firstcall" : [random.choices(["TRUE", "FALSE"], [0.61, 0.39])[0] for _ in range(n_calls)]
}

# Create a Polars DataFrame
df = pl.DataFrame(calls)

# join plz bundesland 
df = df.join(ortskennzahlen_map.select(["Ortsnetzkennzahl", "Ortsnetzname", "Bundesland", "PLZ"]), on = "Ortsnetzkennzahl")

# Write the DataFrame to a CSV file
df.write_csv("../raw/current_data_fake.csv")