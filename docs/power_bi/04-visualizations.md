# Visualization Development

This section covers creating effective visualizations in Power BI, including basic charts, custom visuals, interactive features, and best practices.

## Basic Visualizations

### Common Chart Types

1. **Bar and Column Charts**

   - Vertical bars
   - Horizontal bars
   - Stacked bars
   - Clustered bars

2. **Line Charts**

   - Single line
   - Multiple lines
   - Area charts
   - Combo charts

3. **Pie and Donut Charts**
   - Pie charts
   - Donut charts
   - Tree maps
   - Funnel charts

### Example Configuration

```dax
// Measure for chart
Sales by Category =
CALCULATE(
    SUM(Sales[Amount]),
    ALLSELECTED(Products[Category])
)

// Measure for trend
Sales Trend =
CALCULATE(
    [Total Sales],
    DATESYTD('Date'[Date])
)
```

## Custom Visuals

### Creating Custom Visuals

1. **Power BI Custom Visuals SDK**

   - Install SDK
   - Create project
   - Develop visual
   - Package and deploy

2. **Visual Settings**
   ```typescript
   // Example visual settings
   export class VisualSettings {
   	public dataColors: string[] = ["#1f77b4", "#ff7f0e"];
   	public fontSize: number = 12;
   	public showLabels: boolean = true;
   }
   ```

### Popular Custom Visuals

1. **Charts**

   - Bullet charts
   - Waterfall charts
   - Radar charts
   - Gantt charts

2. **Maps**
   - Custom map visuals
   - Shape maps
   - Filled maps
   - Bubble maps

## Interactive Features

### Drill-Through

1. **Configure Drill-Through**

   - Set target page
   - Define filters
   - Add drill-through fields

2. **Example Configuration**
   ```dax
   // Drill-through measure
   Drill Through Sales =
   CALCULATE(
       [Total Sales],
       ALLSELECTED()
   )
   ```

### Tooltips

1. **Custom Tooltips**

   - Create tooltip page
   - Add visualizations
   - Configure tooltip settings

2. **Example Tooltip**
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

## Best Practices

### Design Principles

1. **Color Usage**

   - Use consistent color scheme
   - Consider color blindness
   - Limit color palette
   - Use color for emphasis

2. **Layout**

   - Group related visuals
   - Use white space
   - Align elements
   - Maintain hierarchy

3. **Typography**
   - Use readable fonts
   - Consistent sizing
   - Clear labels
   - Proper spacing

### Performance

1. **Visual Optimization**

   - Limit data points
   - Use appropriate charts
   - Optimize measures
   - Cache results

2. **Interaction Design**
   - Clear navigation
   - Intuitive filters
   - Responsive design
   - Consistent behavior

## Advanced Features

### Conditional Formatting

```dax
// Conditional formatting measure
Color Scale =
SWITCH(
    TRUE(),
    [Total Sales] > 1000000, "Green",
    [Total Sales] > 500000, "Yellow",
    "Red"
)
```

### Dynamic Titles

```dax
// Dynamic title measure
Report Title =
"Sales Report - " &
FORMAT(MAX('Date'[Date]), "mmmm yyyy")
```

### Bookmarking

1. **Create Bookmarks**

   - Save view state
   - Configure actions
   - Add to report

2. **Bookmark Navigation**
   - Add buttons
   - Configure actions
   - Test navigation

## Troubleshooting

### Common Issues

1. **Visual Performance**

   - Check data volume
   - Review measures
   - Optimize filters
   - Use appropriate visuals

2. **Interaction Problems**

   - Verify relationships
   - Check filter context
   - Review drill-through
   - Test navigation

3. **Display Issues**
   - Check formatting
   - Verify data types
   - Review calculations
   - Test responsiveness

## Next Steps

Now that you understand visualization development, proceed to [Publishing and Sharing](05-publishing.md) to learn about deploying your reports.

## Additional Resources

- [Power BI Visualization Documentation](https://docs.microsoft.com/power-bi/visuals/)
- [Custom Visuals Gallery](https://appsource.microsoft.com/marketplace/apps?product=power-bi-visuals)
- [Power BI Design Guidelines](https://docs.microsoft.com/power-bi/guidelines/report-design/)
- [Power BI Community](https://community.powerbi.com/)
