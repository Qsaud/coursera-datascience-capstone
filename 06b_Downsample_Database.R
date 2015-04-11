require(RSQLite)
require(dplyr)
require(magrittr)

assign('n_distinct', function(x) {build_sql("COUNT(DISTINCT ", x, ")")}, envir=base_agg)
assign('length', sql_prefix('length', 1), envir=base_agg)
assign('glob', sql_prefix('glob', 2), envir=base_agg)
assign('like', sql_prefix('like', 2), envir=base_agg)


dir.repo <- paste0(Sys.getenv('PathGitHubRepos'), '/coursera-datascience-capstone/') %T>% print()
setwd(dir.repo)

name.db.full <- 'en_US.scrub.sqlite'
db_en_us.full <- src_sqlite(name.db.full) %T>% print() %T>% str()

name.db <- 'en_US.sample.sqlite'
if(file.exists(name.db)){file.remove(name.db)}
system2('sqlite3.exe', name.db, stdin = '06a_Create_DB_Small.sql')

db_en_us <- src_sqlite(name.db) %T>% print() %T>% str()
names.tbls <- src_tbls(db_en_us) %>% setNames(., .) %T>% print()

for(i.tbl in src_tbls(db_en_us.full)){ #i.tbl <- 'twitter'
  print(i.tbl)
  i.results <- db_en_us.full$con %>%
    dbSendQuery(paste(
      'select *'
      ,'from', i.tbl
      ,'order by random()'
      ,'limit 100000'
      ))
  
  i.fetch <- dbFetch(i.results, 1000)
  while(nrow(i.fetch)){
    dbWriteTable(
      db_en_us$con
      ,names.tbls
      ,i.fetch
      ,append=TRUE
      )
    i.fetch <- dbFetch(i.results, 1000)
  }
  
}

tbl(db_en_us, names.tbls)
