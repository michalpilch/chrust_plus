library(data.table)
library(DBI)
require(feather)
#dump data
system(command = "rsync -abviuzP pi@192.168.0.139:~/domoticz/domoticz.db /home/dsl/Dokumenty/R/mqtt/chrust_plus/")
# Create an ephemeral in-memory RSQLite database
#system(command = "rsync -abviuzP /home/dsl/Dokumenty/R/mqtt/chrust_plus/domoticz.db~ pi@192.168.0.139:~/domoticz/")
# 
#con <- dbConnect(RSQLite::SQLite(), "/home/dsl/Dokumenty/R/mqtt/domoticz.db")
con <- dbConnect(RSQLite::SQLite(), "domoticz.db")
#  read temperature data
# dbListTables(con)
dbReadTable(con, "Temperature") -> df1
# ## load older data !!! max 7 days old
#df2 <- feather::read_feather(path = "~/Dokumenty/R/mqtt/mqtt_mpm.feather")
df2 <- feather::read_feather(path = "~/Dokumenty/R/mqtt/mqtt_mpm.feather")
# 
df <- unique(rbindlist(list(df1,df2),fill = T))
#feather::write_feather(df, path = "~/Dokumenty/R/mqtt/mqtt_mpm.feather")
feather::write_feather(df, path = "~/Dokumenty/R/mqtt/mqtt_mpm.feather")



