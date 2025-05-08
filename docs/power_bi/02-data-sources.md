# Data Sources and Connections

This section covers how to connect to various data sources in Power BI, including SQL Server, Excel, SharePoint, and other common data sources.

## SQL Server Connection

### Basic Connection

1. **Connect to SQL Server**

   - Click "Get Data"
   - Select "SQL Server"
   - Enter server name
   - Choose authentication method

2. **Authentication Options**
   - Windows Authentication
   - SQL Server Authentication
   - Azure AD Authentication

### Connection String Example

```sql
Server=myserver.database.windows.net;Database=mydatabase;Authentication=Active Directory Integrated;
```

### Power Query M Example

```powerquery
let
    Source = Sql.Database("myserver.database.windows.net", "mydatabase"),
    SalesTable = Source{[Schema="dbo",Item="Sales"]}[Data]
in
    SalesTable
```

## Excel Files

### Local Excel Files

1. **Connect to Excel**

   - Click "Get Data"
   - Select "Excel"
   - Browse to file location
   - Select sheets/tables

2. **Data Loading Options**
   - Import
   - DirectQuery
   - Live Connection

### Power Query M Example

```powerquery
let
    Source = Excel.Workbook(File.Contents("C:\Data\Sales.xlsx"), null, true),
    SalesSheet = Source{[Item="Sales",Kind="Sheet"]}[Data],
    PromotedHeaders = Table.PromoteHeaders(SalesSheet, [PromoteAllScalars=true])
in
    PromotedHeaders
```

## SharePoint Lists

### Connection Steps

1. **Connect to SharePoint**

   - Click "Get Data"
   - Select "SharePoint Online List"
   - Enter site URL
   - Select lists

2. **Authentication**
   - Microsoft Account
   - Organizational Account

### Power Query M Example

```powerquery
let
    Source = SharePoint.Tables("https://company.sharepoint.com/sites/mysite", [ApiVersion = 15]),
    SalesList = Source{[Title="Sales"]}[Data]
in
    SalesList
```

## Other Data Sources

### Common Sources

1. **Web Data**

   - URLs
   - APIs
   - Web Services

2. **Files**

   - CSV
   - JSON
   - XML
   - PDF

3. **Databases**
   - Oracle
   - MySQL
   - PostgreSQL
   - Access

### Power Query M Examples

```powerquery
// CSV File
let
    Source = Csv.Document(File.Contents("C:\Data\sales.csv"),[Delimiter=",", Columns=5, Encoding=1252, QuoteStyle=QuoteStyle.None]),
    PromotedHeaders = Table.PromoteHeaders(Source, [PromoteAllScalars=true])
in
    PromotedHeaders

// JSON File
let
    Source = Json.Document(File.Contents("C:\Data\config.json")),
    Data = Source[data]
in
    Data

// Web API
let
    Source = Web.Contents("https://api.example.com/data"),
    JsonData = Json.Document(Source)
in
    JsonData
```

## Data Source Settings

### Privacy Levels

1. **Public**

   - Web data
   - Public APIs

2. **Organizational**

   - SharePoint
   - SQL Server

3. **Private**
   - Local files
   - Personal data

### Performance Options

1. **Import Mode**

   - Full data import
   - Better performance
   - Limited by refresh schedule

2. **DirectQuery**

   - Live connection
   - Real-time data
   - Limited by source performance

3. **Live Connection**
   - For Analysis Services
   - Real-time data
   - No data import

## Best Practices

1. **Connection Management**

   - Use consistent naming
   - Document connections
   - Monitor performance

2. **Data Refresh**

   - Schedule appropriately
   - Monitor failures
   - Handle errors

3. **Security**
   - Use appropriate authentication
   - Implement row-level security
   - Follow data governance

## Troubleshooting

### Common Issues

1. **Connection Errors**

   - Check credentials
   - Verify network
   - Review permissions

2. **Performance Issues**

   - Optimize queries
   - Use appropriate mode
   - Monitor refresh times

3. **Data Refresh**
   - Check schedules
   - Verify gateway
   - Review logs

## Next Steps

Now that you understand data sources, proceed to [Data Modeling and DAX](03-data-modeling.md) to learn about creating effective data models.

## Additional Resources

- [Power BI Data Sources](https://docs.microsoft.com/power-bi/connect-data/power-bi-data-sources)
- [Power Query Documentation](https://docs.microsoft.com/power-query/)
- [Data Gateway Documentation](https://docs.microsoft.com/data-integration/gateway/)
- [Power BI Community](https://community.powerbi.com/)
