.separator "≡"
.echo ON
.timer ON

pragma synchronous = OFF;
pragma page_size = 4096;
pragma cache_size = 100000;

create table blogs (document TEXT);
.import "en_US.blogs.txt" blogs

create table news (document TEXT);
.import "en_US.news.txt" news

create table twitter (document TEXT);
.import "en_US.twitter.txt" twitter

.exit
