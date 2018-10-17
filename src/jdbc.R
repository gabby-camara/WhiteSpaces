# manually setting the path because I'm having issues connecting 
path_jar = 'F:/NYDSA/WhiteSpaces 2.0/SQL/sqljdbc_6.0/enu/jre8/sqljdbc4.jar'

path_jar = 'C:/Program Files/R/R-3.5.1/library/RJDBC/java'
path_jar = 'C:\Program Files\R\R-3.5.1\library\RJDBC\java'

rJava::.jaddClassPath('C:\\Program Files\\R\\R-3.5.1\\library\\RJDBC\\java\\sqljdbc41.jar')
rJava::.jclassPath()

# connect to the db
# ========================================
conn = db_connect_mssql (integratedSecurity = TRUE, 
                         server = 'SmartLeads', 
                         path_jar  = 'C:\\Program Files\\R\\R-3.5.1\\library\\RJDBC\\java\\sqljdbc42.jar')
