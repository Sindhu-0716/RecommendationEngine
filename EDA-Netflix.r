#BASIC EDA-----
#Checking data & remove any duplicates: Found no missing values
movies<-read.csv(path,header=TRUE,sep=",",stringsAsFactors = FALSE,na.strings = c("NA", ""))
data.frame("variable"=c(colnames(movies)), 
           "missing values count"=sapply(movies,
                                         function(x) sum(is.na(x))),
           row.names=NULL)


###The attribute $ rating has a scale of five. The below figure entails the frequency of each point on the rating scale.---
ratingsorted<-sort(table(titles$rating), decreasing = T)
df<-data.frame(ratingsorted, stringsAsFactors = FALSE)
figure000 <- ggplot(data = df, aes(x= Var1, y= Freq, fill=type))+ geom_bar(colour ="black", size= 0.8, fill = "blue" , stat = "identity")+ guides (fill= FALSE)+
xlab("Netflix Content rating scale") + ylab("count of ratings")+ ggtitle("Distribution of ratings")

#Next checking on the attribute $ userId:
d<-length(unique(titles$userId)) — 671 unique users.
f<-length(titles$userId) -- 100004 Number of times the users have submitted the ratings. Avgratingfreqbyuser – 149.
useridsfreq<-str (base:::as.data.frame(count_users,stringsAsFactors = FALSE))
#The below figure entails the frequency of each point on the rating delivered by each user.
figure001 <- ggplot(data = df1, aes(x= Var1, y= Freq, fill=type))+ geom_bar(colour ="green", size= 0.5, fill = "blue" , stat = "identity")+ guides(fill= FALSE)+
xlab("userId's") + ylab("individual ratings")+ ggtitle("Ratings per user")

#Next checking on the attribute $ movieId:
#Number of movies: 9066
#Average Number of Reviews per Movie: 11.030664019413193
#Running one-sample Wilcoxon test as the data related to how frequently a particular movie is rated by any given user doesn’t follow a normal distribution (as shown in the figure below).
ggqqplot(df2$Freq, ylab = "rate of freq", ggtheme = theme_minimal())

#The figure below shows the rating frequency on each movie—out of 9066 movies 1950 movies have been rated by users more than the average rating frequency of movie. e<-length(unique(titles$movieId))
f<-length(titles$userId)
avgfreqmovierating<- f/e
figure002 <- ggplot(data = df2[c(9000:9066),c(1:2)], aes(x= Var1, y= Freq, fill=type)) + geom_bar(colour ="green", size= 0.5, fill = "blue" , stat = "identity")+
guides(fill= FALSE)+
xlab("movieId's") + ylab("frequency rate of ratings")+ ggtitle("Ratings per movie") + geom_density(fill="red", color="#e9ecef", alpha=0.8)

##one-sample Wilcoxon test
res <- wilcox.test(df2$Freq, mu = 11)
## Since p-value less than Alpha, reject null by concluding every movie has been rates more than 11 times or less than that.
