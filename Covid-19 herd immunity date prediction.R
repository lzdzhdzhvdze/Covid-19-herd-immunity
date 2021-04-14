getwd()
setwd('C:/Users/Luiza/Data Mining')
data <- read.csv(file = "country_vaccinations.csv", header = TRUE)
margins.default = par("mar") 
par(mar = c(5, 4, 4, 2))
#Data preprocessing

#Dropping colomns 'source name' and 'source website'which are not useful for the study
data1 <- data [,-c(14,15)]

# Dropping all raws with no "total vaccinations" value
data_subset <- data1[, c("total_vaccinations")]
data2 <- data1[complete.cases(data_subset), ]

#How many countries have started vaccinations?
data3 <- data2[data2$"total_vaccinations">0, ]
data4 <- unique(data3[c("country")])
n <- nrow(data4) 
cat (n, 'countries have started vaccinations')

#Which country has administered the most number of vaccines?
data5 <- data2 [order(-data2$total_vaccinations), ]
cat (data5$country[1],'has administered', data5$total_vaccinations[1]/1000000,'million vaccinations which is the most number of vaccines')

#TOP-10 countries with the most number of vaccines?
data6 <- unique(data5[c("country")])
for(i in 1:10) {
        print (data6$country[i])
}

#The TOP-20 countries of total number of vaccines administered graph
library(dplyr)
#windows()
data7<- data5 %>% group_by(country) %>% filter(total_vaccinations==max(total_vaccinations)) %>% as.data.frame
data8<- data7[1:20,]
barplot(data8$total_vaccinations/1000000, names.arg = data8$country,main ='Top 20 Countries by Total Vaccinations',ylab= 'Number of total vaccinations',las = 2,col = 'violet')

#Top 20 Countries by Total Vaccinations per 100 Population
data9 <- data2 [order(-data2$total_vaccinations_per_hundred), ]
data10<- data9 %>% group_by(country) %>% filter(total_vaccinations_per_hundred==max(total_vaccinations_per_hundred)) %>% as.data.frame
data11<- data10[1:20,]
barplot(data11$total_vaccinations_per_hundred, names.arg = data11$country,main ='Top 20 Countries by Total Vaccinations per hundred',ylab= 'Number of total vaccinations per hundred population',las = 2,col = 'blue')


#PREDICTION when we can expect countries 
#to administer enough vaccines for achieving a herd immunity

#Prediction on USA
#Making DB just with USA
US_data <- subset(data2,country=='United States',1:13)
US_data[,14] <- as.POSIXct(US_data[,3]) #formatting dates for plotting

#Daily Vaccinations in the USA
xrange = range(US_data[2:84,14])
yrange = range(US_data[2:84,8])
plot(xrange,
     yrange,
     main="Daily number of vaccinations administered in USA", 
     xlab="Date", 
     ylab="Number of Vaccinations (x 1M)",
     type = "n")
points(US_data[2:84,14],US_data[2:84,8], pch=20, col="forestgreen")
lines(US_data[2:84,14],US_data[2:84,8], pch=20, col="forestgreen")

#Total number of vaccinations administered in USA graph
xrange = range(US_data[,14])
yrange = range(US_data[,4])
plot(xrange,
     yrange,
     main="Total number of vaccinations administered in USA", 
     xlab="Date", 
     ylab="Total Vaccinations (x 10M)",
     type = "n")
points(US_data[,14],US_data[,4], pch=20, col="red3")
lines(US_data[,14],US_data[,4], pch=20, col="red3")

#Data preparation for the regression task (USA)
US_data_small <- US_data[,3:4]
startdate <- as.Date(US_data_small[1,1])
US_data_small[,3] <- as.numeric (difftime(US_data_small[,1],startdate ,units="days"))

# Implementation of the Linear Regression (USA)
X <- US_data_small$total_vaccinations #predictor
y <- US_data_small$V3 #target
cat (cor(X, y))# checking that i chose the right predictor
model <- lm(y~X, data =US_data_small)#making the model
summary(model)#checking the scores
plot(X,y, pch = 16, col = "blue",main='Linear Regression (USA)') #Plot the results
abline(model, col = "red")#Plot the results
plot(model$residuals, pch = 16, col = "red", main ='Residuals (USA)')#plot the residuals

#Current population in the USA is 332 millions
#To have 70% vaccination rate for the herd immunity, 
#70% * 332 = 232 millions of the population need to be vaccinated
#Each person needs 2 doses, so the total number of vaccinations needs to be > 464 millions
#making the prediction (USA)
number_vacc <- data.frame(X = 464000000)
z<- predict(model, number_vacc)
date_herd <- as.Date(z,origin='2020-12-20') 
print(paste(date_herd, "is the earliest date that the USA will be able to achieve herd immunity "))

#Prediction on France
France_data <- subset(data2,country=='France',1:13)
France_data[,14] <- as.POSIXct(France_data[,3]) #formatting dates for plotting

#Daily Vaccinations in France 
xrange = range(France_data[2:87,14])
yrange = range(France_data[2:87,8])
plot(xrange,
     yrange,
     main="Daily number of vaccinations administered in France", 
     xlab="Date", 
     ylab="Number of Vaccinations (x 1M)",
     type = "n")
points(France_data[2:87,14],France_data[2:87,8], pch=20, col="forestgreen")
lines(France_data[2:87,14],France_data[2:87,8], pch=20, col="forestgreen")

#Total number of vaccinations administered in France 
xrange = range(France_data[,14])
yrange = range(France_data[,4])
plot(xrange,
     yrange,
     main="Total number of vaccinations administered in France", 
     xlab="Date", 
     ylab="Total Vaccinations (x 10M)",
     type = "n")
points(France_data[,14],France_data[,4], pch=20, col="red3")
lines(France_data[,14],France_data[,4], pch=20, col="red3")

#Data preparation for the regression task (France)
France_data_small <- France_data[,3:4]
startdate <- as.Date(France_data_small[1,1])
#France_data_small[,3] <- as.numeric(as.POSIXct(France_data_small[,1]))
France_data_small[,3] <- as.numeric (difftime(France_data_small[,1],startdate ,units="days"))

# Implementation of the Linear Regression (France)
X <- France_data_small$total_vaccinations #predictor
y <- France_data_small$V3 #target
cat (cor(X, y))# checking that i chose the right predictor
model <- lm(y~X, data =France_data_small)#making the model
summary(model)#checking the scores
plot(X,y, pch = 16, col = "blue",main='Linear Regression (France)') #Plot the results
abline(model, col = "red")#Plot the results
plot(model$residuals, pch = 16, col = "red", main ='Residuals (France)')#plot the residuals

#Current population in France is 68 millions
#To have 70% vaccination rate for the herd immunity, 
#70% * 68 = 47.6 millions of the population need to be vaccinated
#Each person needs 2 doses, so the total number of vaccinations needs to be > 95 millions
#making the prediction (France)
number_vacc <- data.frame(X = 95000000)
z<- predict(model, number_vacc)
date_herd <- as.Date(z,origin='2020-12-20') 
print(paste(date_herd, "is the earliest date that France will be able to achieve herd immunity "))

#Prediction on Germany
Germany_data <- subset(data2,country=='Germany',1:13)
Germany_data[,14] <- as.POSIXct(Germany_data[,3]) #formatting dates for plotting

#Daily Vaccinations in Germany
windows() #creating a window to plot because "Plots" pane is too small for this graph
xrange = range(Germany_data[2:90,14])
yrange = range(Germany_data[2:90,8])
plot(xrange,yrange,
     main="Daily number of vaccinations administered in Germany", 
     xlab="Date", 
     ylab="Number of Vaccinations (x 1M)",
     type = "n")
points(Germany_data[2:90,14],Germany_data[2:90,8], pch=20, col="forestgreen")
lines(Germany_data[2:90,14],Germany_data[2:90,8], pch=20, col="forestgreen")
dev.off() #closing the window with the graph

#Total number of vaccinations administered in Germany
windows() #creating a window to plot because "Plots" pane is too small for this graph
xrange = range(Germany_data[,14])
yrange = range(Germany_data[,4])
plot(xrange,
     yrange,
     main="Total number of vaccinations administered in Germany", 
     xlab="Date", 
     ylab="Total Vaccinations (x 10M)",
     type = "n")
points(Germany_data[,14],Germany_data[,4], pch=20, col="red3")
lines(Germany_data[,14], Germany_data[,4], pch=20, col="red3")
dev.off() 

#Data preparation for the regression task (Germany)
Germany_data_small <- Germany_data[,3:4]
startdate <- as.Date(Germany_data_small[1,1])
#Germany_data_small[,3] <- as.numeric(as.POSIXct(Germany_data_small[,1]))
Germany_data_small[,3] <- as.numeric (difftime(Germany_data_small[,1],startdate ,units="days"))

# Implementation of the Linear Regression (Germany)
X <- Germany_data_small$total_vaccinations #predictor
y <- Germany_data_small$V3 #target
cat (cor(X, y))# checking that i chose the right predictor
model <- lm(y~X, data =France_data_small)#making the model
summary(model)#checking the scores
windows()
plot(X,y, pch = 16, col = "blue",main='Linear Regression (Germany)') #Plot the results
abline(model, col = "red")#Plot the results
plot(model$residuals, pch = 16, col = "red",main ='Residuals (Germany)')#plot the residuals
dev.off() 

#Current population in Germany is 84 millions
#To have 70% vaccination rate for the herd immunity, 
#70% * 84 = 58.8 millions of the population need to be vaccinated
#Each person needs 2 doses, so the total number of vaccinations needs to be > 117.6 millions
#making the prediction (Germany)
number_vacc <- data.frame(X = 117600000)
z<- predict(model, number_vacc)
date_herd <- as.Date(z,origin='2020-12-20') 
print(paste(date_herd, "is the earliest date that Germany will be able to achieve herd immunity "))
