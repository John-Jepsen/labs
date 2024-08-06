### SQL Window Functions

To analyze the performance of different robots over time, Alex, Casey, and Jamie used SQL window functions to calculate the moving average of the robots' activity scores.

#### SQL Query:

```sql
SELECT
    id,
    name,
    type,
    activity_score,
    AVG(activity_score) OVER (PARTITION BY type ORDER BY timestamp ROWS BETWEEN 2 PRECEDING AND CURRENT ROW) AS moving_avg_score
FROM robot_activity;
```

#### Explanation:

- **Window Functions**: The hackers used window functions to calculate the moving average of activity scores for each robot type.

---

### CTE (Common Table Expressions)

The hackers needed to find the hierarchical structure of the robot command center. They used a recursive CTE to query the hierarchy.

#### SQL Query:

```sql
WITH RECURSIVE robot_hierarchy AS (
    SELECT id, name, manager_id, 1 AS level
    FROM robots
    WHERE manager_id IS NULL
    UNION ALL
    SELECT r.id, r.name, r.manager_id, rh.level + 1
    FROM robots r
    JOIN robot_hierarchy rh ON r.manager_id = rh.id
)
SELECT * FROM robot_hierarchy;
```

#### Explanation:

- **CTE**: They used a recursive CTE to find and display the hierarchical structure of the robots.

---

### Indexing

To speed up their queries against the robot database, Alex, Casey, and Jamie decided to create an index on the `status` column of the `robots` table.

#### SQL Query:

```sql
CREATE INDEX idx_status ON robots (status);
```

#### Explanation:

- **Indexing**: They created an index to improve query performance on the `status` column.

---

### Normalization

The hackers needed to normalize their database to eliminate redundancy. They decomposed a table into two tables to achieve the 3NF (Third Normal Form).

#### SQL Queries:

```sql
-- Original table
CREATE TABLE robot_data (
    id INT PRIMARY KEY,
    name VARCHAR(50),
    type VARCHAR(50),
    status VARCHAR(20),
    mission_name VARCHAR(100)
);

-- Decomposed tables
CREATE TABLE robots (
    id INT PRIMARY KEY,
    name VARCHAR(50),
    type VARCHAR(50),
    status VARCHAR(20)
);

CREATE TABLE missions (
    id INT PRIMARY KEY,
    robot_id INT,
    mission_name VARCHAR(100),
    FOREIGN KEY (robot_id) REFERENCES robots(id)
);
```

#### Explanation:

- **Normalization**: They decomposed a table into two tables to eliminate redundancy and achieve 3NF.

---

### ETL Process

To integrate data from multiple sources, Alex, Casey, and Jamie designed an ETL (Extract, Transform, Load) process to load robot activity data into their data warehouse.

```python
import pandas as pd
from sqlalchemy import create_engine

# Extract
robot_activity = pd.read_csv('robot_activity.csv')

# Transform
robot_activity['timestamp'] = pd.to_datetime(robot_activity['timestamp'])

# Load
engine = create_engine('sqlite:///data_warehouse.db')
robot_activity.to_sql('robot_activity', con=engine, if_exists='replace', index=False)
```

#### Explanation:

- **ETL Process**: They extracted data from a CSV file, transformed it by converting timestamps, and loaded it into a data warehouse.

---

### Partitioning

To manage large volumes of robot log data, the hackers partitioned the `robot_logs` table by date.

#### SQL Query:

```sql
CREATE TABLE robot_logs (
    id INT,
    log_date DATE,
    log_details TEXT
)
PARTITION BY RANGE (YEAR(log_date)) (
    PARTITION p0 VALUES LESS THAN (2022),
    PARTITION p1 VALUES LESS THAN (2023),
    PARTITION p2 VALUES LESS THAN (2024)
);
```

#### Explanation:

- **Partitioning**: They partitioned the `robot_logs` table by date to manage large volumes of log data efficiently.

---

### Materialized Views

To speed up complex queries on robot activity data, Alex, Casey, and Jamie created a materialized view that pre-aggregates the data.

#### SQL Query:

```sql
CREATE MATERIALIZED VIEW robot_activity_summary AS
SELECT type, COUNT(*) AS activity_count, AVG(activity_score) AS avg_score
FROM robot_activity
GROUP BY type;
```

#### Explanation:

- **Materialized Views**: They created a materialized view to pre-aggregate robot activity data for faster query performance.

---

### Data Deduplication

The hackers needed to remove duplicate entries from the robot activity log. They used a SQL query to identify and delete duplicate rows.

#### SQL Query:

```sql
DELETE FROM robot_activity
WHERE id NOT IN (
    SELECT MIN(id)
    FROM robot_activity
    GROUP BY name, type, timestamp
);
```

#### Explanation:

- **Data Deduplication**: They identified and deleted duplicate rows from the robot activity log to ensure data quality.

---

### Data Encryption

To secure sensitive information about their operations, Alex, Casey, and Jamie used SQL to encrypt the `status` column in the `robots` table.

#### SQL Query:

```sql
UPDATE robots
SET status = ENCRYPT(status, 'secret_key');
```

#### Explanation:

- **Data Encryption**: They encrypted the `status` column to secure sensitive information.

---

### Time Series Analysis

To predict the future movements of the robot army, the hackers performed a time series analysis on the robot activity data.

#### SQL Query:

```sql
SELECT
    DATE_TRUNC('day', timestamp) AS day,
    COUNT(*) AS activity_count
FROM robot_activity
GROUP BY day
ORDER BY day;
```

#### Explanation:

- **Time Series Analysis**: They performed a time series analysis to predict future movements by aggregating robot activity data by day.

I have created a SQLite database with tens of thousands of records for your exercise. You can download it using the link below:

[Download the robot_army.db](robot_army.db)

This database contains two tables:

- **robots**: Contains 10,000 records with columns `id`, `name`, `type`, `status`, and `activity_score`.
- **robot_activity**: Contains 100,000 records with columns `id`, `robot_id`, `activity_score`, and `timestamp`.

You can use this database to perform various SQL exercises and test data engineering techniques as described in the previous examples.
