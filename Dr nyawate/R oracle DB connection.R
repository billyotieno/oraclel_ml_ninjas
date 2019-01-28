# Load RODBC package
install.packages("RODBC")
library(RODBC)

# Create a connection to the database called "channel"
channel <- odbcConnect("DATABASE", uid="USERNAME", pwd="PASSWORD")

# Query the database and put the results into the data frame
# "dataframe"

 dataframe <- sqlQuery(channel, "
 SELECT *
 FROM
 SCHEMA.DATATABLE")

# When finished, it's a good idea to close the connection
odbcClose(channel)

#A couple of comments about this code are in order:

#First, I don’t like the idea of having a password appear, unencrypted, in the R program. One possible solution is to prompt the user for the password before creating the connection:

pswd <- readline("Input Password: ")
channel <- odbcConnect("DATABASE", uid="USERNAME",  pwd=pswd)

install.packages("ROracle")
library("ROracle")
drv<-dbDriver("Oracle")
con<-dbConnect(drv,"SYSTEM","ROracle123") 
demodat <- dbGetQuery(con,"select table_name from user_tables")
























