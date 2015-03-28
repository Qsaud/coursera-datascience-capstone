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

SampleNextWords <- function(input, sample.size=10000) {
  
  BuildSingleTable <- function(name.table) {#name.table <- 'news'; input <- 'complete th'; sample.size <- 100
    paste0(
      'select case when instr(remaining, " ") = 0 then remaining'
      ,' else substr(remaining, 1, instr(remaining, " ")-1) end as word_next'
      #'select *'
      ,' from ('
        ,'select substr(document, instr(document, "',input,' ") + ', nchar(input) + 1, ') as remaining'
        ,' from ', name.table
        ,' where document glob "* ', input, ' *"'
        ,' or substr(document,', nchar(input), ') = "', input, '"'
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
  ,'he' %>% ScrubInput() %>% SampleNextWords(10000)
  ) %>% arrange(desc(freq)) %>% collect()


PredictNextWord <- function(input.raw) {#input.raw <- 'i like strawberry'
  input.considered <- input.raw %>%
    ScrubInput() %>%
    strsplit('\\s+') %>% 
    '[['(1) %>%
    tail(3)
  
  next.words <- db_en_us %>% 
    tbl(
      input.considered %>%
        paste(collapse=' ') %>%
        SampleNextWords(1000)
    ) %>% 
    collect()
  
  if(length(input.considered) == 3){
    back.two <- db_en_us %>% 
      tbl(
        input.considered %>%
          tail(2) %>%
          paste(collapse=' ') %>%
          SampleNextWords(1000)
      ) %>% 
      collect() %>%
      mutate(
        prop = freq/sum(freq)
        ,ridge = 100 * prop
        ) %>%
      select(word_next, ridge)
    next.words <- next.words %>%
      full_join(back.two, 'word_next') %>%
      mutate(
        freq = ifelse(is.na(freq), 0, freq)
        ,ridge = ifelse(is.na(ridge), 0, ridge)
        ,freq = freq + ridge
      ) %>%
      select(word_next, freq)
  }
  
  if(length(input.considered) > 1){
    back.one <- db_en_us %>% 
      tbl(
        input.considered %>%
          tail(1) %>%
          SampleNextWords(1000)
      ) %>% 
      collect() %>%
      mutate(
        prop = freq/sum(freq)
        ,ridge = 42 * prop
      ) %>%
      select(word_next, ridge)
    next.words <- next.words %>%
      full_join(back.one, 'word_next') %>%
      mutate(
        freq = ifelse(is.na(freq), 0, freq)
        ,ridge = ifelse(is.na(ridge), 0, ridge)
        ,freq = freq + ridge
      ) %>%
      select(word_next, freq)
  }
  
  return(next.words %>% arrange(desc(freq)) %$% word_next[1])

}

PredictNextWord('i like strawberry')
PredictNextWord('real')

strsplit('I love  you','\\s+')

