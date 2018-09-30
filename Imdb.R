install.packages('rvest')

library('rvest')
url <- 'https://www.imdb.com/chart/top?pf_rd_m=A2FGELUUNOQJNL&pf_rd_p=cb6cf75a-1a51-49d1-af63-8202cfc3fb01&pf_rd_r=98HN5EKJ1GEM57R9D9MY&pf_rd_s=right-4&pf_rd_t=15506&pf_rd_i=moviemeter&ref_=chtmvm_ql_3'

web <- read_html(url)

rating_data <- html_nodes(web,'.titleColumn a')

rate_data <- html_text(rating_data)

head(rate_data)

rate_data 

rating <- html_nodes(web, '.ratingColumn.imdbRating strong')

rate <- html_text(rating)

rate

year <- html_nodes(web,'.titleColumn span')

year_data <- html_text(year)

year_data

rating <- as.numeric(rate_data)

rate <- as.numeric(rate)

rate

year_data <- gsub('\\(|\\)','', year_data)

year_data <- as.numeric(year_data)

movie_ratings <- data.frame(Title = rate_data, Year = year_data, Rating = rate)

str(movie_ratings)

install.packages('ggplot2')

library(ggplot2)

qplot(data = movie_ratings, Year, fill = Rating, bins=30)

ggplot(movie_ratings,aes(x=Year,y=Rating)) 

movie_ratings_year <- data.frame(Year = year_data, Rating = rate)

aggregate(Rating ~ Year, data = movie_ratings_year, sum)

qplot(data = movie_ratings_year, Year, fill = Rating, bins= 30)

hist(movie_ratings_year$Year, breaks = 10, col = "red")

d <- density(movie_ratings_year$Year)
plot(d)

url1 <- 'https://www.imdb.com/search/title?title_type=feature&release_date=2017-01-01,2017-12-31'

web1 <- read_html(url1)

movie_title <- html_nodes(web1, '.lister-list .lister-item-header a')

title_movie <- html_text(movie_title)

title_movie

title_year <- html_nodes(web1, '.lister-item-year.text-muted.unbold')

year_movie <- html_text(title_year)

year_movie

movie_rating <- html_nodes(web1, ' .lister-list .inline-block.ratings-imdb-rating strong')

rating_movie <- html_text(movie_rating)

rating_movie

rating_movie <- as.numeric(rating_movie)

movie_votes <- html_nodes(web1, '.sort-num_votes-visible span:nth-child(5)')

votes_movie <- html_text(movie_votes)

votes_movie

votes_movie <- as.numeric(votes_movie)

votes_movie <- gsub('M','',votes_movie)

votes_movie

votes_movie <- gsub('\\$','', votes_movie)

votes_movie

votes_movie <- as.numeric(votes_movie)

votes_movie

gross_movie <- votes_movie

gross_movie
length(gross_movie)

movie_vote <- html_nodes(web1, '.sort-num_votes-visible span:nth-child(2)')

vote_movie <- html_text(movie_vote)

vote_movie <- gsub(',','',vote_movie)

vote_movie

vote_movie <- as.numeric(vote_movie)

vote_movie

movie_gener <- html_nodes(web1, '.genre')

gener_movie <- html_text(movie_gener)

gener_movie

gener_movie <- gsub("\n","", gener_movie)

gener_movie

gener_movie <- gsub(" ","", gener_movie)

gener_movie

gener_movie <- gsub(",.*","", gener_movie)

gener_movie

movies_data <- data.frame(Title = title_movie, Year = year_movie, Rating = rating_movie, Votes = vote_movie, Gross = gross_movie, Genre = gener_movie)

qplot(data = movies_data, Rating, fill = Genre, bins= 30)

qplot(data = movies_data, Votes, fill = Genre, bins= 30)

ggplot(movies_data,aes(x=Gross,y=Rating))+
  geom_point(aes(size=Votes,col=Genre))

aggregate(Rating ~ Year, data = movie_ratings_year, sum)

gross_movie <- "insert"(1:50, 47, value = NA)

for (i in c(47,48)){
  
  a<-gross_movie[1:(i-1)]
  
  b<-gross_movie[i:length(gross_movie)]
  
  gross_movie<-append(a,list("NA"))
  
  gross_movie<-append(gross_movie,b)
}

length(gross_movie)

gross_movie <- as.numeric(gross_movie)

gross_movie 

str(movies_data)
             
movies_data <- data.frame(Title = title_movie, Year = year_movie, Rating = rating_movie, Votes = vote_movie, Gross = gross_movie, Genre = gener_movie)             

ggplot(movies_data,aes(x=Rating,y=Gross))+
  geom_point(aes(size=Votes,col=Genre))

ggplot(movies_data,aes(x=Rating,y=Votes))+
  geom_point(aes(size=Gross,col=Genre))
