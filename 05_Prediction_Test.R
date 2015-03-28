require(RSQLite)
require(dplyr)
require(magrittr)
require(tm)
require(SnowballC)

assign('n_distinct', function(x) {build_sql("COUNT(DISTINCT ", x, ")")}, envir=base_agg)
assign('length', sql_prefix('length', 1), envir=base_agg)
assign('glob', sql_prefix('glob', 2), envir=base_agg)
assign('like', sql_prefix('like', 2), envir=base_agg)

dir.repo <- paste0(Sys.getenv('PathGitHubRepos'), '/coursera-datascience-capstone/') %T>% print()
setwd(dir.repo)

name.db <- 'en_US.scrub.sqlite'
db_en_us <- src_sqlite(name.db) %T>% print() %T>% str()
names.tbls <- src_tbls(db_en_us) %>% setNames(., .) %T>% print()

ParseInput <- function(x) { #x <- 'I complete You'
  stopifnot(length(x)==1)
  x %>% 
    PlainTextDocument() %>% 
    stripWhitespace() %>% 
    removeNumbers() %>% 
    removePunctuation() %>% 
    stemDocument() %>%
    tolower()
}


QueryNextWords <- function(input, sample.size=1000) {

  BuildSingleTable <- function(name.table) {#name.table <- 'news'; input <- 'complete th'; sample.size <- 100
    paste0(
      'select case when instr(remaining, " ") = 0 then remaining'
      ,' else substr(remaining, 1, instr(remaining, " ")-1) end as word_next'
      #'select *'
      ,' from ('
        ,'select substr(document, instr(document, "',input,' ") + ', nchar(input) + 1, ') as remaining'
        ,' from ', name.table
        ,' where document glob "*', input, ' *"'
        ,' order by random()'
        ,' limit ', sample.size
      ,')'
      ,' where length(remaining) > 0'
    )
  }
  
  sql(paste(lapply(names.tbls, BuildSingleTable), collapse = ' union all '))
}

QueryNextWords('I love you')

tbl(
  db_en_us
  ,'me about his' %>% ParseInput() %>% QueryNextWords(10000)
  ) %>% collect() %$% word_next %>% table() %>% sort()
