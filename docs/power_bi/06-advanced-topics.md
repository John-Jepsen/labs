# Advanced Topics

This section covers advanced Power BI features, including DAX optimization, performance tuning, custom tooltips, and paginated reports.

## DAX Optimization

### Performance Optimization

1. **Measure Optimization**

   ```dax
   // Inefficient
   Slow Measure =
   CALCULATE(
       SUM(Sales[Amount]),
       FILTER(
           ALL(Sales),
           Sales[Date] <= MAX('Date'[Date])
       )
   )

   // Optimized
   Fast Measure =
   CALCULATE(
       SUM(Sales[Amount]),
       DATESYTD('Date'[Date])
   )
   ```

2. **Variable Usage**
   ```dax
   // Using variables
   Optimized Measure =
   VAR CurrentDate = MAX('Date'[Date])
   VAR YTDDate = DATESYTD(CurrentDate)
   RETURN
       CALCULATE(
           [Total Sales],
           YTDDate
       )
   ```

### Advanced Patterns

1. **Dynamic Measures**

   ```dax
   // Dynamic measure selection
   Dynamic Measure =
   SWITCH(
       SELECTEDVALUE(MeasureSelector[Measure]),
       "Sales", [Total Sales],
       "Profit", [Total Profit],
       "Units", [Total Units],
       BLANK()
   )
   ```

2. **Complex Calculations**
   ```dax
   // Weighted moving average
   Weighted MA =
   VAR Period = 7
   RETURN
       DIVIDE(
           SUMX(
               DATESINPERIOD(
                   'Date'[Date],
                   LASTDATE('Date'[Date]),
                   -Period,
                   DAY
               ),
               [Total Sales] * (Period - DATEDIFF('Date'[Date], LASTDATE('Date'[Date]), DAY))
           ),
           SUMX(
               DATESINPERIOD(
                   'Date'[Date],
                   LASTDATE('Date'[Date]),
                   -Period,
                   DAY
               ),
               Period - DATEDIFF('Date'[Date], LASTDATE('Date'[Date]), DAY)
           )
       )
   ```

## Performance Tuning

### Model Optimization

1. **Data Model Design**

   - Use appropriate relationships
   - Optimize column data types
   - Remove unused columns
   - Create hierarchies

2. **Query Optimization**
   ```dax
   // Optimize filter context
   Optimized Filter =
   CALCULATE(
       [Total Sales],
       KEEPFILTERS(Sales[Category] = "Electronics")
   )
   ```

### Visual Optimization

1. **Visual Performance**

   - Limit data points
   - Use appropriate charts
   - Optimize measures
   - Cache results

2. **Interaction Design**
   ```dax
   // Optimize cross-filtering
   Cross Filter Measure =
   CALCULATE(
       [Total Sales],
       CROSSFILTER(Sales[ProductID], Products[ProductID], BOTH)
   )
   ```

## Custom Tooltips

### Tooltip Design

1. **Basic Tooltip**

   ```dax
   // Tooltip measure
   Tooltip Sales =
   VAR SelectedDate = SELECTEDVALUE('Date'[Date])
   RETURN
       CALCULATE(
           [Total Sales],
           'Date'[Date] = SelectedDate
       )
   ```

2. **Advanced Tooltip**
   ```dax
   // Multi-measure tooltip
   Advanced Tooltip =
   VAR SelectedDate = SELECTEDVALUE('Date'[Date])
   RETURN
       "Sales: " & FORMAT([Total Sales], "$#,##0") &
       " | Profit: " & FORMAT([Total Profit], "$#,##0") &
       " | Units: " & FORMAT([Total Units], "#,##0")
   ```

### Tooltip Configuration

1. **Tooltip Page**

   - Create dedicated page
   - Add visualizations
   - Configure settings
   - Test functionality

2. **Tooltip Settings**
   ```json
   {
   	"tooltip": {
   		"page": "Tooltip Page",
   		"measures": ["Total Sales", "Total Profit"],
   		"format": "Currency",
   		"position": "Right"
   	}
   }
   ```

## Paginated Reports

### Report Design

1. **Basic Report**

   ```sql
   -- SQL query for report
   SELECT
       ProductName,
       Category,
       SUM(SalesAmount) as TotalSales,
       COUNT(*) as OrderCount
   FROM Sales
   GROUP BY ProductName, Category
   ORDER BY TotalSales DESC
   ```

2. **Advanced Report**
   ```sql
   -- Parameterized query
   SELECT
       ProductName,
       Category,
       SUM(SalesAmount) as TotalSales,
       COUNT(*) as OrderCount
   FROM Sales
   WHERE
       SalesDate BETWEEN @StartDate AND @EndDate
       AND Category IN (@Categories)
   GROUP BY ProductName, Category
   ORDER BY TotalSales DESC
   ```

### Report Configuration

1. **Report Settings**

   ```json
   {
   	"report": {
   		"name": "Sales Report",
   		"format": "PDF",
   		"parameters": {
   			"StartDate": "2024-01-01",
   			"EndDate": "2024-12-31",
   			"Categories": ["Electronics", "Clothing"]
   		}
   	}
   }
   ```

2. **Export Options**
   - PDF
   - Excel
   - Word
   - CSV

## Best Practices

### Performance

1. **Model Optimization**

   - Use appropriate relationships
   - Optimize column data types
   - Remove unused columns
   - Create hierarchies

2. **Query Optimization**
   - Use variables
   - Optimize filter context
   - Cache results
   - Monitor performance

### Security

1. **Data Protection**

   - Implement RLS
   - Use appropriate roles
   - Monitor access
   - Regular review

2. **Access Control**
   - Secure connections
   - Encrypt data
   - Monitor usage
   - Regular audits

## Troubleshooting

### Common Issues

1. **Performance Problems**

   - Check data model
   - Review measures
   - Monitor refresh
   - Optimize queries

2. **Visual Issues**

   - Check data volume
   - Review formatting
   - Test interactions
   - Monitor performance

3. **Report Problems**
   - Verify parameters
   - Check queries
   - Test export
   - Review permissions

## Next Steps

Now that you understand advanced topics, proceed to [Troubleshooting and Best Practices](07-troubleshooting.md) to learn about common issues and solutions.

## Additional Resources

- [Power BI Performance Documentation](https://docs.microsoft.com/power-bi/guidance/power-bi-optimization)
- [DAX Patterns](https://www.daxpatterns.com/)
- [Power BI Custom Visuals](https://docs.microsoft.com/power-bi/developer/visuals/)
- [Power BI Community](https://community.powerbi.com/)
