# Data Modeling and DAX

This section covers data modeling concepts and DAX (Data Analysis Expressions) in Power BI, including relationships, calculated columns, measures, and best practices.

## Data Modeling Basics

### Table Relationships

1. **Types of Relationships**

   - One-to-One (1:1)
   - One-to-Many (1:\*)
   - Many-to-Many (_:_)

2. **Creating Relationships**
   - Drag and drop fields
   - Set cardinality
   - Configure cross-filter direction

### Example Relationship

```dax
// Create relationship between Sales and Products
RELATIONSHIP(
    Sales[ProductID],
    Products[ProductID],
    "Many-to-One"
)
```

## DAX Fundamentals

### Basic Syntax

```dax
// Simple measure
Total Sales = SUM(Sales[Amount])

// Measure with filter
High Value Sales =
CALCULATE(
    SUM(Sales[Amount]),
    Sales[Amount] > 1000
)

// Time intelligence
YTD Sales =
TOTALYTD(
    SUM(Sales[Amount]),
    'Date'[Date]
)
```

### Common Functions

1. **Aggregation Functions**

   ```dax
   // Sum
   Total = SUM(Table[Column])

   // Average
   Average = AVERAGE(Table[Column])

   // Count
   Count = COUNT(Table[Column])
   ```

2. **Filter Functions**

   ```dax
   // Filter
   Filtered = CALCULATE(
       SUM(Sales[Amount]),
       Sales[Category] = "Electronics"
   )

   // All
   All Categories = CALCULATE(
       SUM(Sales[Amount]),
       ALL(Sales[Category])
   )
   ```

3. **Time Intelligence**

   ```dax
   // Year to Date
   YTD = TOTALYTD(SUM(Sales[Amount]), 'Date'[Date])

   // Month to Date
   MTD = TOTALMTD(SUM(Sales[Amount]), 'Date'[Date])

   // Previous Year
   PY = CALCULATE(
       SUM(Sales[Amount]),
       DATEADD('Date'[Date], -1, YEAR)
   )
   ```

## Calculated Columns vs Measures

### Calculated Columns

```dax
// Add profit margin column
Profit Margin =
DIVIDE(
    Sales[Profit],
    Sales[Revenue],
    0
)

// Add category grouping
Category Group =
SWITCH(
    TRUE(),
    Sales[Amount] > 1000, "High",
    Sales[Amount] > 500, "Medium",
    "Low"
)
```

### Measures

```dax
// Running total
Running Total =
CALCULATE(
    SUM(Sales[Amount]),
    FILTER(
        ALL('Date'),
        'Date'[Date] <= MAX('Date'[Date])
    )
)

// Market share
Market Share =
DIVIDE(
    SUM(Sales[Amount]),
    CALCULATE(
        SUM(Sales[Amount]),
        ALL(Sales[Product])
    ),
    0
)
```

## Advanced DAX Patterns

### Dynamic Measures

```dax
// Dynamic measure based on selection
Selected Measure =
SWITCH(
    SELECTEDVALUE(MeasureSelector[Measure]),
    "Sales", [Total Sales],
    "Profit", [Total Profit],
    "Units", [Total Units],
    BLANK()
)
```

### Complex Calculations

```dax
// Weighted average
Weighted Average =
DIVIDE(
    SUMX(
        Sales,
        Sales[Amount] * Sales[Weight]
    ),
    SUM(Sales[Weight]),
    0
)

// Moving average
Moving Average =
AVERAGEX(
    DATESINPERIOD(
        'Date'[Date],
        LASTDATE('Date'[Date]),
        -7,
        DAY
    ),
    [Total Sales]
)
```

## Performance Optimization

### Best Practices

1. **Measure Optimization**

   ```dax
   // Avoid
   Bad Measure =
   CALCULATE(
       SUM(Sales[Amount]),
       FILTER(
           ALL(Sales),
           Sales[Date] <= MAX('Date'[Date])
       )
   )

   // Better
   Good Measure =
   CALCULATE(
       SUM(Sales[Amount]),
       DATESYTD('Date'[Date])
   )
   ```

2. **Relationship Optimization**

   - Use appropriate cardinality
   - Set correct cross-filter direction
   - Avoid unnecessary relationships

3. **Data Model Optimization**
   - Remove unused columns
   - Use appropriate data types
   - Create hierarchies

## Common Patterns

### Time Intelligence

```dax
// Year over Year Growth
YoY Growth =
VAR CurrentYear = [Total Sales]
VAR PreviousYear =
    CALCULATE(
        [Total Sales],
        DATEADD('Date'[Date], -1, YEAR)
    )
RETURN
    DIVIDE(
        CurrentYear - PreviousYear,
        PreviousYear,
        0
    )
```

### Ranking

```dax
// Product ranking
Product Rank =
RANKX(
    ALL(Products),
    [Total Sales],
    ,
    DESC
)
```

## Troubleshooting

### Common Issues

1. **Circular Dependencies**

   - Check measure references
   - Review relationship chains
   - Use CALCULATE carefully

2. **Performance Issues**

   - Monitor measure complexity
   - Check relationship design
   - Review data model

3. **Calculation Errors**
   - Verify syntax
   - Check data types
   - Review filter context

## Next Steps

Now that you understand data modeling and DAX, proceed to [Visualization Development](04-visualizations.md) to learn about creating effective visualizations.

## Additional Resources

- [DAX Guide](https://dax.guide/)
- [Power BI DAX Documentation](https://docs.microsoft.com/dax/)
- [DAX Patterns](https://www.daxpatterns.com/)
- [Power BI Community](https://community.powerbi.com/)
