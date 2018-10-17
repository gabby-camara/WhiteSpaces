
# connect to one of the our mssql db's
# ===============================================
db_connect_mssql = function(integratedSecurity = F, 
                            username , 
                            password , 
                            path_jar = 'lib//sqljdbc42.jar', 
                            server = c('SmartLeads','NAXIAN','WIMIProd')){
  
  # load required packages
  require(rJava)
  require(DBI)
  require(RJDBC)
  
  #Set java XMX parameter: maximum memory allocation pool for a java virtual machine (in this case 4GB)
  options(java.parameters = "- Xmx4096m")
  
  #Load the JDBC driver into memory
  drv <- JDBC("com.microsoft.sqlserver.jdbc.SQLServerDriver", path_jar)
  
  #Create a connection to the DB
  server_details = c(SmartLeads = '//22.241.86.56\\CLS_MAIN_DEV',
                     NAXIAN = '//XZAPBCC1SQL0184\\FMF_MAIN1_LIVE',
                     WIMIProd = '//XZAPBCC1SQL0221\\WIMI_MAIN1_PRD')[server]
  
  if(integratedSecurity){
    con_str = sprintf("jdbc:sqlserver:%s;integratedSecurity=true;", server_details)
    conn <- dbConnect(drv, con_str)
  }else{
    con_str = sprintf("jdbc:sqlserver:%s;", server_details)
    conn <- dbConnect(drv, 
                      con_str,
                      username,
                      password
    )
  }
  
  return(conn)
}
