# Publishing and Sharing

This section covers publishing reports to Power BI Service, managing workspaces, configuring permissions, and sharing options.

## Publishing to Power BI Service

### Publishing Process

1. **Prepare Report**

   - Check data sources
   - Verify refresh settings
   - Test performance
   - Review security

2. **Publish Steps**
   - Click "Publish"
   - Select workspace
   - Configure settings
   - Monitor status

### Workspace Management

1. **Workspace Types**

   - Personal workspace
   - Shared workspace
   - Premium workspace

2. **Workspace Settings**
   ```json
   {
   	"name": "Sales Analytics",
   	"type": "Workspace",
   	"capacity": "Premium",
   	"users": [
   		{
   			"email": "user@company.com",
   			"role": "Admin"
   		}
   	]
   }
   ```

## Permission Management

### Access Levels

1. **User Roles**

   - Admin
   - Member
   - Contributor
   - Viewer

2. **Permission Configuration**

   ```powershell
   # Example PowerShell script
   $workspaceName = "Sales Analytics"
   $userEmail = "user@company.com"
   $role = "Viewer"

   Set-PowerBIWorkspaceUserRole `
       -WorkspaceName $workspaceName `
       -UserEmailAddress $userEmail `
       -UserRole $role
   ```

### Row-Level Security

1. **RLS Configuration**

   ```dax
   // RLS filter
   [Region] = USERNAME()
   ```

2. **Role Definition**
   ```json
   {
   	"name": "Sales Region",
   	"table": "Sales",
   	"filter": "[Region] = USERNAME()"
   }
   ```

## Sharing Options

### Report Sharing

1. **Direct Sharing**

   - Share with users
   - Share with groups
   - Set permissions
   - Configure access

2. **Publish to Web**
   - Generate embed code
   - Configure settings
   - Monitor usage
   - Manage access

### App Workspaces

1. **Create App**

   - Configure settings
   - Add content
   - Set permissions
   - Publish

2. **App Settings**
   ```json
   {
   	"name": "Sales Dashboard",
   	"description": "Sales analytics dashboard",
   	"workspace": "Sales Analytics",
   	"access": "Organization"
   }
   ```

## Data Refresh

### Refresh Configuration

1. **Schedule Refresh**

   - Set frequency
   - Configure time
   - Set time zone
   - Monitor status

2. **Gateway Configuration**
   ```json
   {
   	"gateway": "OnPremisesGateway",
   	"datasource": "SQL Server",
   	"refresh": {
   		"frequency": "Daily",
   		"time": "02:00",
   		"timezone": "UTC"
   	}
   }
   ```

### Refresh Monitoring

1. **Status Tracking**

   - Check history
   - Monitor errors
   - Review logs
   - Set alerts

2. **Troubleshooting**
   - Verify credentials
   - Check connectivity
   - Review permissions
   - Monitor performance

## Best Practices

### Publishing

1. **Report Preparation**

   - Optimize performance
   - Check security
   - Verify data
   - Test functionality

2. **Workspace Organization**
   - Use consistent naming
   - Group related content
   - Document structure
   - Monitor usage

### Security

1. **Access Control**

   - Implement RLS
   - Use appropriate roles
   - Monitor access
   - Regular review

2. **Data Protection**
   - Secure connections
   - Encrypt data
   - Monitor usage
   - Regular audits

## Troubleshooting

### Common Issues

1. **Publishing Problems**

   - Check permissions
   - Verify workspace
   - Review errors
   - Test connectivity

2. **Refresh Issues**

   - Check gateway
   - Verify credentials
   - Review schedule
   - Monitor logs

3. **Access Problems**
   - Verify permissions
   - Check roles
   - Review RLS
   - Test access

## Next Steps

Now that you understand publishing and sharing, proceed to [Advanced Topics](06-advanced-topics.md) to learn about advanced features and optimizations.

## Additional Resources

- [Power BI Service Documentation](https://docs.microsoft.com/power-bi/service-get-started)
- [Workspace Management](https://docs.microsoft.com/power-bi/service-create-workspaces)
- [Row-Level Security](https://docs.microsoft.com/power-bi/admin/service-admin-rls)
- [Power BI Community](https://community.powerbi.com/)
