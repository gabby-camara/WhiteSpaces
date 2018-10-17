
# Determine correlation between Products

head(cust_full)

cust1 = cust_full %>% group_by(Major.Desk, Date) %>% summarise(Value = sum(Value)) %>% arrange(Value)

ggplot(cust1, aes(x = Date, y = Value, group = Major.Desk)) + 
  geom_line(aes(color = Major.Desk), size = 1) +
  scale_color_manual(values = c("#00AFBB", "#E7B800")) +
  theme_minimal()

db_connect_mssql (integratedSecurity = TRUE, 
                  server = 'SmartLeads', 
                  path_jar  = 'F:/NYDSA/WhiteSpaces 2.0/SQL/Microsoft JDBC Driver 6.0 for SQL Server/sqljdbc_6.0/enu/jre8/sqljdbc42.jar')
