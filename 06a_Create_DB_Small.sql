.echo ON
.timer ON

pragma synchronous = OFF;
pragma page_size = 4096;
pragma cache_size = 100000;

create table samples (document TEXT);

.exit
