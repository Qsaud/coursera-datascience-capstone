require(RSQLite)
require(dplyr)
require(magrittr)
require(tm)
require(slam)
require(RWeka)

assign('n_distinct', function(x) {build_sql("COUNT(DISTINCT ", x, ")")}, envir=base_agg)
assign('length', sql_prefix('length', 1), envir=base_agg)
assign('glob', sql_prefix('glob', 2), envir=base_agg)
assign('like', sql_prefix('like', 2), envir=base_agg)

dir.repo <- paste0(Sys.getenv('PathGitHubRepos'), '/coursera-datascience-capstone/') %T>% print()
setwd(dir.repo)

name.db <- 'en_US.scrub.sqlite'
db_en_us <- src_sqlite(name.db) %T>% print() %T>% str()
names.tbls <- src_tbls(db_en_us) %>% setNames(., .) %T>% print()

GetSample <- function(name.tbl, sample.size) {
  tbl(db_en_us, sql(paste(
    'select * from'
    ,name.tbl
    ,'order by random() limit'
    ,sample.size
  ))) %>%
    collect() %$%
    document
}

CalcWordFreq <- . %>%
  VectorSource() %>% 
  VCorpus() %>% 
  TermDocumentMatrix() %>%
  removeSparseTerms(1-1e-4) %>%
  row_sums() %>%
  prop.table()

samp <- GetSample('twitter',10000) %>%
  CalcWordFreq() %>%
  sort() %T>%
  {tail(., 42) %>% print()}

NGramFactory <- function(n.words) {
  function(x) {
    NGramTokenizer(
      x
      ,Weka_control(min=n.words, max=n.words)
    )
  }
}

Calc2GramFreq <- . %>%
  VectorSource() %>% 
  VCorpus() %>% 
  TermDocumentMatrix(control=list(tokenize=NGramFactory(3))) %>%
  removeSparseTerms(1-1e-4) %>%
  row_sums() %>%
  prop.table()

samp <- GetSample('blogs',1000) %>%
  Calc2GramFreq() %>%
  sort() %T>%
  {tail(., 42) %>% print()}

