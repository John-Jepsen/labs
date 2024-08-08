## Basic Queries

Lets Start with the basics

1. **Select all records from the table:**

   ```sql
   SELECT * FROM robot_activity;
   ```

2. **Select all unique robot IDs:**

   ```sql
   SELECT DISTINCT robot_id FROM robot_activity;
   ```

3. **Count the number of activities recorded:**

   ```sql
   SELECT COUNT(*) AS activity_count FROM robot_activity;
   ```

4. **Find the highest activity score recorded:**

   ```sql
   SELECT MAX(activity_score) AS highest_activity_score FROM robot_activity;
   ```

5. **Find the average activity score:**

   ```sql
   SELECT AVG(activity_score) AS average_activity_score FROM robot_activity;
   ```

6. **Select activities with a score greater than 80:**

   ```sql
   SELECT * FROM robot_activity WHERE activity_score > 80;
   ```

7. **Select all activities for a specific robot (e.g., robot ID 2):**

   ```sql
   SELECT * FROM robot_activity WHERE robot_id = 2;
   ```

8. **Count the number of activities for each robot:**

   ```sql
   SELECT robot_id, COUNT(*) AS activity_count
   FROM robot_activity
   GROUP BY robot_id;
   ```

9. **Find the earliest activity timestamp:**

   ```sql
   SELECT MIN(timestamp) AS earliest_activity FROM robot_activity;
   ```

10. **Select all activities ordered by their score in descending order:**
    ```sql
    SELECT * FROM robot_activity ORDER BY activity_score DESC;
    ```

## Intermediate Queries

### 1. SQL Window Functions

**Definition:** Window functions perform calculations across a set of table rows that are somehow related to the current row. Unlike regular aggregate functions, window functions do not cause rows to become grouped into a single output row. The rows retain their separate identities.

**Example Instruction:**

```sql
SELECT
  robot_id,
  activity_score,
  AVG(activity_score) OVER (PARTITION BY robot_id) AS avg_activity_score
FROM
  robot_activity;
```

**Explanation:** This query calculates the average activity score for each robot, without collapsing the rows.

### 2. CTE (Common Table Expressions)

**Definition:** A Common Table Expression (CTE) is a temporary result set that you can reference within a `SELECT`, `INSERT`, `UPDATE`, or `DELETE` statement. It is defined using the `WITH` keyword.

**Example Instruction:**

```sql
WITH recent_activities AS (
  SELECT * FROM robot_activity WHERE timestamp > '2024-01-01'
)
SELECT * FROM recent_activity WHERE activity_score > 50;
```

**Explanation:** This CTE first selects activities recorded after January 1, 2024, and then the outer query filters those activities to find ones with an activity score greater than 50.

### 3. Indexing

**Definition:** An index is a database object that improves the speed of data retrieval operations on a table at the cost of additional writes and storage space. Indexes are used to quickly locate data without having to search every row in a table.

**Example Instruction:**

```sql
CREATE INDEX idx_robot_id ON robot_activity(robot_id);
```

**Explanation:** This creates an index on the `robot_id` column of the `robot_activity` table, which can speed up queries that filter or sort by `robot_id`.

### 4. Normalization

**Definition:** Normalization is the process of organizing data in a database to reduce redundancy and improve data integrity. It involves dividing large tables into smaller ones and defining relationships between them.

**Example Instruction:**

```sql
-- Assuming normalization from a non-normalized table
CREATE TABLE robots (
  id INTEGER PRIMARY KEY,
  name TEXT
);

CREATE TABLE robot_activity (
  id INTEGER PRIMARY KEY AUTOINCREMENT,
  robot_id INTEGER,
  activity_score INTEGER,
  timestamp TEXT,
  FOREIGN KEY (robot_id) REFERENCES robots(id)
);
```

**Explanation:** This schema is an example of normalization. The `robots` table stores unique robot information, while `robot_activity` references `robots` via a foreign key.

### 5. ETL Process

**Definition:** ETL stands for Extract, Transform, Load. It refers to the process of extracting data from various sources, transforming it into a format suitable for analysis, and loading it into a target database.

**Example Instruction:**

```sql
-- Example of transforming and loading data using SQL
INSERT INTO robot_activity (robot_id, activity_score, timestamp)
SELECT robot_id, new_activity_score, new_timestamp
FROM staging_table;
```

**Explanation:** This query inserts transformed data from a staging table into the `robot_activity` table.

### 6. Partitioning

**Definition:** Partitioning divides a database into pieces that can be managed and accessed separately. It can improve performance and manageability.

**Example Instruction:**

```sql
-- Example of range partitioning
CREATE TABLE robot_activity_partition (
  id INTEGER,
  robot_id INTEGER,
  activity_score INTEGER,
  timestamp TEXT
) PARTITION BY RANGE (timestamp);

CREATE TABLE robot_activity_2023 PARTITION OF robot_activity_partition FOR VALUES FROM ('2023-01-01') TO ('2024-01-01');
CREATE TABLE robot_activity_2024 PARTITION OF robot_activity_partition FOR VALUES FROM ('2024-01-01') TO ('2025-01-01');
```

**Explanation:** This example partitions the `robot_activity` table by year, creating separate partitions for activities in 2023 and 2024.

### 7. Data Deduplication

**Definition:** Data deduplication is the process of identifying and removing duplicate records from a dataset to ensure data quality.

**Example Instruction:**

```sql
DELETE FROM robot_activity
WHERE id NOT IN (
  SELECT MIN(id)
  FROM robot_activity
  GROUP BY robot_id, timestamp
);
```

**Explanation:** This query deletes duplicate rows in the `robot_activity` table, keeping only the row with the minimum `id` for each `robot_id` and `timestamp`.

### 8. Data Encryption

**Definition:** Data encryption transforms data into a secure format that can only be read by someone with the decryption key. It ensures the confidentiality and security of data.

**Example Instruction:**

```sql
-- Example of encrypting data in SQL
-- Requires specific database functions and extensions (pseudo-code)
UPDATE robot_activity
SET activity_score = ENCRYPT(activity_score, 'encryption_key');
```

**Explanation:** This query encrypts the `activity_score` column values. The actual encryption method depends on the database system in use.

### 9. Time Series Analysis

**Definition:** Time series analysis involves analyzing time-ordered data points to extract meaningful statistics and identify trends over time.

**Example Instruction:**

```sql
SELECT
  timestamp,
  activity_score,
  LAG(activity_score, 1) OVER (ORDER BY timestamp) AS previous_score,
  LEAD(activity_score, 1) OVER (ORDER BY timestamp) AS next_score
FROM
  robot_activity;
```

**Explanation:** This query performs time series analysis by calculating the previous and next activity scores for each row based on the `timestamp`.

---

These instructions and definitions should provide a solid foundation for understanding and teaching these database concepts.
