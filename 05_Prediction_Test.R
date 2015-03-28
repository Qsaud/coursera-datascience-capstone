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

ScrubInput <- function(x) { #x <- 'I complete You'
  stopifnot(length(x)==1)
  x %>% 
    iconv(to = 'ASCII' ,sub = '') %>%
    PlainTextDocument() %>% 
    stripWhitespace() %>% 
    removeNumbers() %>% 
    removePunctuation() %>% 
    stemDocument() %>%
    tolower()
}

ScrubInput('I love to')

SampleNextWords <- function(input.raw, sample.size=10000) {
  
  input.scrub <- ScrubInput(input.raw)

  BuildSingleTable <- function(name.table) {#name.table <- 'news'; input <- 'complete th'; sample.size <- 100
    paste0(
      'select case when instr(remaining, " ") = 0 then remaining'
      ,' else substr(remaining, 1, instr(remaining, " ")-1) end as word_next'
      #'select *'
      ,' from ('
        ,'select substr(document, instr(document, "',input.scrub,' ") + ', nchar(input.scrub) + 1, ') as remaining'
        ,' from ', name.table
        ,' where document glob "* ', input.scrub, ' *"'
        ,' or substr(document,', nchar(input.scrub), ') = "', input.scrub, '"'
        #,' order by random()' # Give stochastic results, but slower
        ,' limit ', sample.size
      ,')'
      ,' where length(remaining) > 0'
    )
  }
  
  all.tables <- paste(lapply(names.tbls, BuildSingleTable), collapse = ' union all ')
  
  sql(paste(
    'select word_next, count(*) as freq'
    ,'from ('
    ,all.tables
    ,') group by word_next'
    ))
}

SampleNextWords('I love you')

tbl(
  db_en_us
  ,'he' %>% SampleNextWords(10000)
  ) %>% arrange(desc(freq)) %>% collect()
