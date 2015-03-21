.echo ON
.timer ON

pragma synchronous = OFF;
pragma page_size = 4096;
pragma cache_size = 100000;

create table blogs (document TEXT);

create table news (document TEXT);

create table twitter (document TEXT);

.exit
