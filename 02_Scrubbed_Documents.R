require(RSQLite)
require(dplyr)
require(magrittr)
require(tm)
require(SnowballC)

profanity <- c(
  'ass'
  ,'asshol'
  ,'bastard'
  ,'bitch'
  ,'cunt'
  ,'damn'
  ,'ffs'
  ,'fuck'
  ,'goddamn'
  ,'goddam'
  ,'hell'
  ,'lmao'
  ,'motherfuck'
  ,'shit'
  ,'shitass'
  ,'whore'
  ,'wtf'
)

profanity %>%
  VectorSource() %>%
  VCorpus() %>%
  tm_map(stemDocument) %>%
  inspect()

assign('n_distinct', function(x) {build_sql("COUNT(DISTINCT ", x, ")")}, envir=base_agg)
assign('length', sql_prefix('length', 1), envir=base_agg)
assign('glob', sql_prefix('glob', 2), envir=base_agg)
assign('like', sql_prefix('like', 2), envir=base_agg)

dir.repo <- paste0(Sys.getenv('PathGitHubRepos'), '/coursera-datascience-capstone/') %T>% print()
setwd(dir.repo)

name.db <- 'en_US.scrub.sqlite'
if(file.exists(name.db)){file.remove(name.db)}
system2('sqlite3.exe', name.db, stdin = '01a_Create_DB.sql')

db_en_us <- src_sqlite(name.db) %T>% print() %T>% str()
names.tbls <- src_tbls(db_en_us) %>% setNames(., .) %T>% print()

BatchImport <- function(name.table, nrow.cache=1000) { #name.table <- 'blogs'; nrow.cache=10
  
  FetchChunk <- . %>%
    readLines(
      n = nrow.cache
      ,skipNul = TRUE
      ,encoding = 'UTF-8'
    ) %>%
    iconv(
      from = 'UTF-8'
      ,to = 'ASCII'
      ,sub = ''
    )
  
  ScrubCorpus <- . %>%
    tm_map(stripWhitespace) %>%
    tm_map(removeNumbers) %>%
    tm_map(removePunctuation) %>%
    tm_map(content_transformer(tolower)) %>% 
    tm_map(stemDocument) %>%
    tm_map(removeWords, profanity)
  
  CorpusToVector <- . %>%
    lapply('[','content') %>%
    unlist()
  
  con.text <- paste0(
    "en_US."
    ,name.table
    ,".txt"
  ) %T>%
    print() %>%
    file("r")
  
  cache <- con.text %>% FetchChunk()
  
  while(length(cache)) {
    tryCatch({
      scrubbed <- cache %>%
        #sample(100) %>%
        VectorSource() %>%
        VCorpus() %>%
        ScrubCorpus()
      
      dbWriteTable(
        db_en_us$con
        ,name.table
        ,value = data.frame(
          document=(scrubbed %>% CorpusToVector)
          ,stringsAsFactors = FALSE
        )
        ,append = TRUE
      )
    }
    ,error = function(e) e
    )
    
    cache <- con.text %>% FetchChunk()
  }
  
  con.text %>% close()
  
  return(tbl(db_en_us, name.table))
}

time.import <- system.time(names.tbls %>% lapply(BatchImport))

tbl(db_en_us,'blogs') %>% print()