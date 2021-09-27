library(DBI)

# Importando os dados sqlite
filename <- "data/DB_EIP/EIP_20210415.db"

# Create an ephemeral in-memory RSQLite database
con <- dbConnect(RSQLite::SQLite(),
                 ":memory:",
                 dbname = filename)

# Checar os elementos presentes dentro do SQLITE
dbListTables(con)

# Chamando um dataset dentro desse sqlite
mytbl <- dbReadTable(con,"dm_projeto")


