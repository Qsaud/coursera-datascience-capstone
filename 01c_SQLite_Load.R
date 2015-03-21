require(RSQLite)
require(dplyr)
require(magrittr)

assign('n_distinct', function(x) {build_sql("COUNT(DISTINCT ", x, ")")}, envir=base_agg)
assign('length', sql_prefix('length', 1), envir=base_agg)
assign('glob', sql_prefix('glob', 2), envir=base_agg)
assign('like', sql_prefix('like', 2), envir=base_agg)

dir.repo <- paste0(Sys.getenv('PathGitHubRepos'), '/coursera-datascience-capstone/') %T>% print()
setwd(dir.repo)

db_en_us <- src_sqlite('en_US.sqlite') %T>% print() %T>% str()
names.tbls <- src_tbls(db_en_us) %>% setNames(., .) %T>% print()

BatchImport <- function(name.table, nrow.cache=10000) {
  con.text <- paste0("en_US.", name.table, ".txt") %>% file("r")
  cache <- con.text %>% readLines(nrow.cache)
  while(length(cache)) {
    dbWriteTable(
      db_en_us$con
      ,name.table
      ,value = data.frame(document=cache)
      ,append = TRUE
      )
    cache <- con.text %>% readLines(nrow.cache)
  }
  con.text %>% close()
  return(tbl(db_en_us, name.table))
}

names.tbls %>% lapply(BatchImport)


## EDA for Quiz #1
tbl(db_en_us, 'twitter') %>% nrow()

tbl(db_en_us, 'blogs') %>% mutate(len=length(document)) %>% summarize(len_max = max(len))
tbl(db_en_us, 'twitter') %>% mutate(len=length(document)) %>% summarize(len_max = max(len))
tbl(db_en_us, 'news') %>% mutate(len=length(document)) %>% summarize(len_max = max(len))

tbl(db_en_us, 'twitter') %>% filter(document %glob% '*love*') %>% nrow()
tbl(db_en_us, 'twitter') %>% filter(document %glob% '*hate*') %>% nrow()

tbl(db_en_us, 'twitter') %>% filter(document %like% '%biostats%')

tbl(db_en_us, 'twitter') %>% filter(document == 'A computer once beat me at chess, but it was no match for me at kickboxing')
