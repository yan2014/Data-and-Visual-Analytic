#GTID: 903295488
#1.	Professional Employment by State
#install and load required libraries
install.packages("ggplot2")
library (ggplot2)
library (reshape2)
data("midwest")
#get the precprof for each state
newdata1 <- aggregate(percprof*popadults~state, midwest, sum)
newdata2 <- aggregate(popadults~state, midwest, sum)
newdata1
newdata2
newdata <- merge(newdata1,newdata2)
newdata 
newdata['percprofs'] <- (newdata['percprof * popadults']/newdata['popadults'])
newdata
#plot out the precrpof for each state in order
p <- ggplot(newdata, aes(x= reorder (state, percprofs), y=percprofs)) + 
  geom_bar(stat = "identity") 
p


#2.	School and College Education by State
#get two datasets: one contains state and perchsds; The other contains state and percolleges
newdata3 <- aggregate(perchsd*popadults~state, midwest, sum)
newdata4 <- aggregate(percollege*popadults~state, midwest, sum)
newdata_2 <- merge(newdata3,newdata2)
newdata_2
newdata_2['perchsds'] <- (newdata_2['perchsd * popadults']/newdata_2['popadults'])
newdata_2
newdata_3 <- merge(newdata4,newdata2)
newdata_3
newdata_3['percolleges'] <- (newdata_3['percollege * popadults']/newdata_3['popadults'])
newdata_3
#plot out the relationships between perchsd vs. state
p_2 <- ggplot(newdata_2, aes(x= reorder (state, perchsds), y=perchsds)) + 
  geom_bar(stat = "identity") 
p_2
#plot out the relationships between percollege  vs. state
p_3 <- ggplot(newdata_3, aes(x= reorder (state, percolleges), y=percolleges)) + 
  geom_bar(stat = "identity") 
p_3
#get a dataset that contains state, perchsds, and percolleges
newdata_5 <- merge(newdata_2,newdata_3)
newdata_5
# plot out perchsd vs. percollege within each state
p_4 <- ggplot(newdata_5, aes(perchsds,percolleges))
p_4 + geom_point(aes(colour = state))

#3.	Comparison of Visualization Techniques
randomdata <- data.frame(customer = sample(letters[1:5], size = 20, replace = TRUE),
                sales = rnorm(20, 8000, 2000),
                profit = rnorm(20, 40, 15))
ggplot(data=randomdata,aes(x=factor(customer),y=sales))+
geom_boxplot(color='grey')
#4.	Random Scatterplots
#get the size of files in each format (pdf, png, jpg, and eps)
N=25000
x_1 <- seq(1, N, 50)
x_1r <- length(x_1)
pdf <- array()
png <- array()
jpg <- array()
eps <- array()
for (n in 1:x_1r) {
  temp <- x_1[n]
  cat(temp)
  x=floor(runif(temp,0,37))
  y=floor(runif(temp,0,37))
  df <- data.frame(x = x, y = y)
  p_5 <- ggplot(data = df, mapping = aes(x = x, y = y)) + geom_point()
  p_5
  filenamepng=paste(c(temp,".png"), collapse = "")
  filenamepdf=paste(c(temp,".pdf"), collapse = "")
  filenamejpg=paste(c(temp,".jpg"), collapse = "")
  filenamebmp=paste(c(temp,".eps"), collapse = "")
  ggsave(p_5, file=filenamepdf)
  pdf[n] <- file.size(filenamepdf)/1000
  ggsave(p_5, file=filenamepng)
  png[n] <- file.size(filenamepng)/1000
  ggsave(p_5, file=filenamejpg)
  jpg[n] <- file.size(filenamejpg)/1000
  ggsave(p_5, file=filenamebmp)
  eps[n] <- file.size(filenamebmp)/1000
}
x_1
df_5 <- data.frame(x_1,pdf,png,jpg,eps)
df_5
randomdatareshape <- melt(df_5, id.vars = "x_1")
randomdatareshape
#plot out the relationship between file size and N
p <- ggplot(randomdatareshape,aes(x=x_1, y=value))+geom_line(aes(color=variable))
p

#5.	Diamonds
library (ggplot2)
data (diamonds)
#distribution of the three variables
ggplot(diamonds, aes(x=price)) + geom_histogram()
ggplot(diamonds, aes(x=carat)) + geom_histogram()
barplot(table(diamonds['color']))
#get a smaple of 100
diamondample <- diamonds[sample(nrow(diamonds), 100), ]
#plot a regression line to show the relationship between carat and price
p0 <- ggplot(data = diamondample, aes(carat, price, color)) 
p1 <- p0 + geom_point(aes(color = color), size = 3)
p1 
p2 <- p1 + geom_smooth()
p2 + ggtitle("Differentiable Curve")
p3 <- p1 + geom_smooth(method = "lm", se = FALSE)
p3 + ggtitle("Regression Line")
#plot a box plot to show the relationship between color and carat
pcarat <- ggplot(data = diamonds, aes(x = color, y = carat)) 
pcarat + geom_boxplot(aes(fill = color))
#plot a box plot to show the relationship between color and price
pprice <- ggplot(data = diamonds, aes(x = color, y = price)) 
pprice + geom_boxplot(aes(fill = color))
