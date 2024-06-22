
# Shell Script to Export and Import Data Between SQL Table and CSV File

## Introduction
This guide provides a shell script to export data from a specified SQL table to a CSV file and import data from a CSV file into the table. Additionally, it includes instructions to use Python libraries to create sample CSV and SQL files for testing.

## Prerequisites
- Ensure you have `sqlite3` installed on your system.
- Ensure you have Python and the following Python libraries installed:
  - `pandas`
  - `sqlite3`

## Shell Script

### Export Data from SQL Table to CSV
```sh
#!/bin/bash

# Parameters
DB_NAME="sample.db"
TABLE_NAME="sample_table"
CSV_FILE="exported_data.csv"

# Export data from SQL table to CSV file
sqlite3 -header -csv $DB_NAME "SELECT * FROM $TABLE_NAME;" > $CSV_FILE

echo "Data exported from $TABLE_NAME to $CSV_FILE"
```

### Import Data from CSV to SQL Table
```sh
#!/bin/bash

# Parameters
DB_NAME="sample.db"
TABLE_NAME="sample_table"
CSV_FILE="import_data.csv"

# Import data from CSV file to SQL table
sqlite3 $DB_NAME <<EOF
.mode csv
.import $CSV_FILE $TABLE_NAME
EOF

echo "Data imported from $CSV_FILE to $TABLE_NAME"
```

## Instructions to Create Sample CSV and SQL Files Using Python

### Create Sample CSV File
```python
import pandas as pd

# Create a sample DataFrame
data = {
    'id': [1, 2, 3],
    'name': ['Alice', 'Bob', 'Charlie'],
    'age': [25, 30, 35]
}
df = pd.DataFrame(data)

# Save the DataFrame to a CSV file
csv_file = 'import_data.csv'
df.to_csv(csv_file, index=False)

print(f"Sample CSV file created: {csv_file}")
```

### Create Sample SQL Database and Table
```python
import sqlite3

# Connect to the database
db_name = 'sample.db'
conn = sqlite3.connect(db_name)
cursor = conn.cursor()

# Create a sample table
table_name = 'sample_table'
cursor.execute(f'''
CREATE TABLE IF NOT EXISTS {table_name} (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL,
    age INTEGER NOT NULL
)
''')

# Insert sample data into the table
sample_data = [
    (1, 'Alice', 25),
    (2, 'Bob', 30),
    (3, 'Charlie', 35)
]
cursor.executemany(f'INSERT INTO {table_name} (id, name, age) VALUES (?, ?, ?)', sample_data)
conn.commit()

print(f"Sample SQL table created: {table_name}")

# Close the connection
conn.close()
```

## Usage
1. Run the Python scripts to create the sample CSV and SQL files.
   ```sh
   python create_sample_csv.py
   python create_sample_sql.py
   ```

2. Use the shell script to export data from the SQL table to a CSV file.
   ```sh
   ./export_sql_to_csv.sh
   ```

3. Use the shell script to import data from a CSV file to the SQL table.
   ```sh
   ./import_csv_to_sql.sh
   ```

Ensure you have the necessary permissions to execute the shell scripts. You can make the scripts executable by running:
```sh
chmod +x export_sql_to_csv.sh import_csv_to_sql.sh
```

By following these steps, you can easily export and import data between an SQL table and a CSV file using shell scripts and Python.
