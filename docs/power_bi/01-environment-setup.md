# Environment Setup

This guide will help you set up your Power BI development environment, including Power BI Desktop, Power BI Service, and necessary configurations.

## Installing Power BI Desktop

### System Requirements

- Windows 10/11 (64-bit)
- .NET Framework 4.7.2 or later
- 4GB RAM minimum (8GB recommended)
- 1GB free disk space

### Installation Steps

1. **Download Power BI Desktop**

   - Visit [Power BI Desktop Download](https://powerbi.microsoft.com/desktop)
   - Click "Download Free" button
   - Choose your preferred language

2. **Run the Installer**

   - Double-click the downloaded file
   - Accept the license terms
   - Choose installation location
   - Click "Install"

3. **Verify Installation**
   - Launch Power BI Desktop
   - Check for updates
   - Configure initial settings

## Setting Up Power BI Service

### Account Requirements

- Microsoft 365 account
- Valid email address
- Internet connection

### Registration Steps

1. **Sign Up for Power BI Service**

   - Visit [Power BI Service](https://app.powerbi.com)
   - Click "Sign up free"
   - Enter your work email
   - Follow verification steps

2. **Choose License Type**
   - Power BI Free
   - Power BI Pro
   - Power BI Premium Per User
   - Power BI Premium

### License Comparison

| Feature            | Free     | Pro      | Premium Per User | Premium       |
| ------------------ | -------- | -------- | ---------------- | ------------- |
| Report Creation    | ✓        | ✓        | ✓                | ✓             |
| Data Refresh       | 8/day    | 8/day    | 48/day           | 48/day        |
| Workspace          | Personal | Personal | Personal         | App Workspace |
| Sharing            | Limited  | ✓        | ✓                | ✓             |
| Row-level Security | -        | ✓        | ✓                | ✓             |
| Paginated Reports  | -        | -        | ✓                | ✓             |
| XMLA Endpoints     | -        | -        | ✓                | ✓             |

## Configuring Power BI Gateway

### Installation

1. **Download Gateway**

   - Visit [Power BI Gateway Download](https://aka.ms/gateway)
   - Choose "On-premises data gateway"
   - Run installer

2. **Configure Gateway**
   - Sign in with Power BI account
   - Register gateway
   - Configure data sources

### Data Source Configuration

```powershell
# Example PowerShell script for gateway configuration
$gatewayName = "MyGateway"
$gatewayRegion = "North Central US"

# Register gateway
Register-PowerBIGateway -Name $gatewayName -Region $gatewayRegion

# Add data source
Add-PowerBIGatewayDataSource -GatewayName $gatewayName -DataSourceType "SQL" -Server "myserver.database.windows.net"
```

## Initial Configuration

### Power BI Desktop Settings

1. **Options and Settings**

   - File > Options and settings > Options
   - Configure:
     - Regional settings
     - Privacy levels
     - DirectQuery options
     - Preview features

2. **Data Sources**
   - File > Options and settings > Data sources
   - Add common connections
   - Configure credentials

### Power BI Service Settings

1. **Workspace Setup**

   - Create new workspace
   - Configure access
   - Set up data sources

2. **Gateway Configuration**
   - Add data sources
   - Configure refresh schedule
   - Set up security

## Best Practices

1. **Installation**

   - Use latest version
   - Regular updates
   - Backup settings

2. **Configuration**

   - Document settings
   - Use consistent naming
   - Follow security guidelines

3. **Gateway**
   - Monitor performance
   - Regular maintenance
   - Backup configuration

## Troubleshooting

### Common Issues

1. **Installation Problems**

   - Check system requirements
   - Clear temporary files
   - Run as administrator

2. **Gateway Issues**

   - Check network connectivity
   - Verify credentials
   - Review logs

3. **Service Connection**
   - Check internet connection
   - Verify account status
   - Clear browser cache

## Next Steps

Now that your environment is set up, proceed to [Data Sources and Connections](02-data-sources.md) to learn about connecting to various data sources.

## Additional Resources

- [Power BI Desktop Download](https://powerbi.microsoft.com/desktop)
- [Power BI Service Documentation](https://docs.microsoft.com/power-bi/service-get-started)
- [Gateway Documentation](https://docs.microsoft.com/data-integration/gateway/service-gateway-install)
- [Power BI Community](https://community.powerbi.com/)
