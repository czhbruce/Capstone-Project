#Check if you need these
#install.packages("devtools")
#install.packages("dplyr") #for big data management i.e. merge, change column number
#install.packages("pbapply")
#install.packages("stringr")

#devtools::install_github("hrbrmstr/omdbapi")

library(dplyr)
library(pbapply)
library(omdbapi)

getTv <- function(OUR_TITLE,OUR_YEAR=NA){
  x <- c()
  x.season <- c()
  x.episode <- c()
  
  #loop over seasons (Assuming maximum of 20 seasons)
  for(this.season in 1:20){
    #check if this season exists, otherwise break the for loop
    if(dim(find_by_title(OUR_TITLE,type="episode",
                         season=this.season,
                         episode=1,
                         year_of_release = OUR_YEAR))[1] == 0){
      break
    } else {
      #now go over the episodes
      
      #first wait for .2 seconds (this amount probably needs to be higher) Change the wait time to 0.05s to speed up
      #(we don't want to get blacklisted from the API)
      print("Waiting for .2 seconds...")
      Sys.sleep(0.2)
      
      #looping over episodes (maximum is 50)
      for(this.episode in 1:50){
        if(dim(find_by_title(OUR_TITLE, 
                             type="episode",
                             season=this.season,
                             episode=this.episode,
                             year_of_release = OUR_YEAR))[1] == 0){
          break
        } else {
          if(this.episode %% 9 ==0){
            print("Waiting for .2 seconds...")
            Sys.sleep(.2)
          }
          this.rating <- find_by_title(OUR_TITLE, 
                                       type="episode",
                                       season=this.season,
                                       episode=this.episode,
                                       year_of_release = OUR_YEAR)$imdbRating
          x <- c(x,this.rating)
          x.season <- c(x.season,this.season)
          x.episode <- c(x.episode,this.episode) 
        }
        
      }
      
    }
  }
  
  return(data.frame(x=x,season=x.season,episode=x.episode))
}

#First, check if this tv series is included
OUR_TITLE <- "Game of Thrones"


res.1 <- search_by_title(OUR_TITLE,type="series")
res.1


#Check if it has ratings
find_by_title(OUR_TITLE, type="episode", season=1, episode=1)$imdbRating
#Source: local data frame [3 x 5]

#Title  Year    imdbID   Type
#(chr) (chr)     (chr)  (chr)
#1                    Game of Thrones 2011– tt0944947 series
#2         Stupid for Game of Thrones 2012– tt2143796 series
#3 World's Deadliest: Game of Thrones 2014– tt4845438 series

#Extract the rating for each episode/season
OUR_TITLE <- "Game of Thrones"
TVseries1 <-getTv(OUR_TITLE)
GameOfThrones <- na.omit(TVseries1)
str(GameOfThrones)
GameOfThrones["RatingCategory"] <- NA
GameOfThrones$RatingCategory <- round(GameOfThrones$x)
library(ggplot2)
#Plot1
ggplot(GameOfThrones, aes(factor(1), fill=factor(RatingCategory))) + 
  geom_bar(width=1) + coord_polar(theta = "y") +
  ggtitle("Overall Ratings for Game of Thrones") +labs(x=" ",y=" ")
#Plot2
ggplot(GameOfThrones, aes(x=episode, y=x, group=season, color = x)) + 
  geom_line() + facet_wrap(~season) + labs(x=" Episode",y="Rating") +
  ggtitle("Season-wise Ratings for Game of Thrones")

ggplot(GameOfThrones,aes(y=x,color=factor(season),x=episode,group=factor(season)))+
  geom_line(size=1)+geom_point(size=1)+geom_smooth(alpha=.3,method="lm")+
  ggtitle("Overall Ratings for Game of Thrones") +labs(x="Episode",y="Rating")
#Analysis
x <- GameOfThrones$x
acf(x)
pacf(x)
####Too many NA in Game of Thrones, Try House of Cards####
#####AR model, MA model, determine the lowers AIC#####
OUR_TITLE <- "House of Cards"
TVseries1 <-getTv(OUR_TITLE,OUR_YEAR = 2013)
HoC <- TVseries1
x <- TVseries1$x
max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}


best.model <- arima(x,order=c(0,0,0))
best.model2 <- arima(x,order=c(0,0,1)) #ma(1)
best.model3 <- arima(x,order=c(1,0,0)) #ar(1)
tinfoil.model <- arima(x,order=c(2,0,6)) #arma(2,6) (arima(2,0,6))
#acf(residuals(best.model))
HoCA <- data.frame(x=predict(tinfoil.model,n.ahead = 13)$pred,
                   season=5,
                   episode=1:13,
                   episodeT=53:65)
str(HoCA2)
HoCA2 <- data.frame(x=(predict(tinfoil.model,n.ahead = 26)$pred)[14:26],
                    season=6,
                    episode=1:13,
                    episodeT=66:78)


HoC2 <- rbind(HoC,HoCA)

HoC3 <- rbind(HoC2,HoCA2)

ggplot(HoC3,aes(y=x,color=factor(season),x=episodeT,group=factor(season)))+
  geom_line(size=2)+geom_point(size=2)+geom_smooth(alpha=.3,method="lm")


###########LM model and GLS Model#######
lm1 <- lm(x~factor(season)-1,data=GameOfThrones)
summary(lm1)

x <- lm1$residuals
ts.plot(x)
acf(x)

max.order <- 10
AIC.matrix <- matrix(0,nrow = max.order+1,ncol= max.order+1)

for(i in 1:(max.order+1)){
  for(j in 1:(max.order+1)){
    currentArima <- arima(x,order=c(i-1,0,j-1))
    AIC.matrix[i,j] <- AIC(currentArima) 
  }
}

best.model <- arima(x,order=c(1,0,3))
install.packages("nlme")
library(nlme)
gls1 <- gls(x~factor(season)-1,
            data=HoC,
            correlation = corARMA(value = coef(best.model)[1:4],p=1,q=3))
summary(lm1)