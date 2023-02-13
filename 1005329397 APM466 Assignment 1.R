#Unless otherwise stated, from left to right, the data is in order of maturity 
#from least to greatest maturity.
#Question 4a)
#cpn stands for the coupon rates of the 10 bonds that I selected. 
cpn <- c(1.75, 0.50, 2.25, 1.50, 1.25, 0.50, 0.25, 1.00, 1.25, 2.75)

#The vector accruedtime represents the amount of days that each bond has accrued
#interest, starting from January 16. I obtained this by counting the number of
#January 16 to March 1 and January 16 to May 1 since these are all the next 
#coupon payment dates for bonds I chose (actually first bond has coupon on 
#February 28 but that is a minor detail that I will ignore. Then I subtracted it 
#from 181 to get fraction out of 365. 181 is actually the number of days from 
#September 1 2022 to March 1 2023 and also from November 1, 2022 to May 1, 2023. 
#The accruedinterestmatrix shows the accrued interest for each bond on each day.

accruedtime <- c(138/365, 77/365, 138/365, 138/365, 138/365, 138/365, 138/365, 
                 138/365, 138/365, 138/365)
accruedinterestonjan16 <- cpn*accruedtime
days  <- c(0, 1, 2, 3, 4, 7, 8, 9, 10, 11)
accruedinterestmatrix <- matrix(ncol=10, nrow=10)
for(i in 1:10){
  for(j in 1:10){
    accruedinterestmatrix[i,j] <- accruedinterestonjan16[i]+days[j]/365}}

#Maturity is approximate number of days until maturity.
maturity <- c(43/365, 288/365, 408/365, 590.5/365, 773/365, 955.5/365, 
              1138/365, 1320.5/365, 1503/365, 1685.5/365)

#Below is the bond price data for each day.
jan16 <- c(99.71, 97.02, 97.93, 96.48, 95.38, 92.74, 91.08, 92.72, 93.14, 99.10)
jan17 <- c(99.70, 97.01, 97.87, 96.45, 95.42, 92.75, 91.19, 92.84, 93.25, 99.22)
jan18 <- c(99.72, 97.05, 97.95, 96.58, 95.61, 93.03, 91.43, 93.24, 93.73, 99.71)
jan19 <- c(99.72, 97.05, 97.92, 96.50, 95.59, 93.03, 91.51, 93.29, 93.78, 99.71)
jan20 <- c(99.74, 97.01, 97.87, 96.40, 95.44, 92.87, 91.32, 92.95, 93.39, 99.24)
jan23 <- c(99.75, 97.03, 97.86, 96.40, 95.38, 92.82, 91.23, 92.90, 93.33, 99.14)
jan24 <- c(99.76, 97.06, 97.87, 96.39, 95.34, 92.81, 91.21, 92.91, 93.35, 99.15)
jan25 <- c(99.77, 97.11, 97.92, 96.47, 95.47, 92.97, 91.41, 93.05, 93.50, 99.29)
jan26 <- c(99.78, 97.10, 97.92, 96.48, 95.50, 93.00, 91.45, 93.04, 93.46, 99.24)
jan27 <- c(99.79, 97.07, 97.84, 96.29, 95.30, 92.72, 91.17, 92.77, 93.14, 98.86)

#I combine the columns to form a matrix called bond data, each row represents
#each bond and each column represents the day of January. I then calculate the
#dirty prices for each day of every bond.
bonddata <- cbind(jan16, jan17, jan18, jan19, jan20, jan23, jan24, jan25, jan26, 
                  jan27)
cleanprices <- bonddata
prices <- cleanprices + accruedinterestmatrix

#4a) I now calculate the yield to maturity (ytm curve). First I used the ytm
#equation from class lectures and the dirty price and the payments all to one
#side. The I recognize that the equation is a function of yield to maturity and
#I use the uniroot function to find a solution to this equation. I use the
#geometric series formula to simply some of the equations.

ytmCAN1.75Mar23 <- matrix(ncol=10, nrow=1)
for(i in 1:10){
  f1 <- function(x) (100+0.5*cpn[1])*(1+x)^((days[i]-43)/365)-prices[1,i]
  ytmCAN1.75Mar23[i] <- uniroot(f1, c(0,1), tol=0.0000001)$root}

ytmCAN0.5Nov23 <- matrix(ncol=10, nrow=1)
for(i in 1:10){
  f1 <- function(x) (0.5*cpn[2])*(1+x)^((days[i]-105)/365)+
    (100+0.5*cpn[2])*(1+x)^((days[i]-288)/365)-prices[2,i]
  ytmCAN0.5Nov23[i] <- uniroot(f1, c(0,1), tol=0.0000001)$root}

partialytm <- matrix(ncol=10, nrow=8) 
for(j in 3:10){
  for(i in 1:10){
    f1 <- function(x) ((1-(1+x)^(-(j-1)/2))/(1-(1+x)^(-1/2)))*(0.5*cpn[j])*
      (1+x)^((days[i]-43)/365)+(100+0.5*cpn[j])*
      (1+x)^((days[i]-(43+182.5*(j-1)))/365)-prices[j,i]
    partialytm[j-2,i] <- uniroot(f1, c(0.001,1), tol=0.0000001)$root}}

ytmmatrixdecimal <- rbind(ytmCAN1.75Mar23, ytmCAN0.5Nov23, partialytm)
ytmmatrix<- 100*ytmmatrixdecimal

#I then plot each column of the yield to matrix dependent variable and the time 
#until maturity as the independent variable. Each column represents the
#yields of all 10 bonds on a specific day. I should add that I did not know how
#to plot all the scatterplots on the same plot so I used the answer that was
#written by csgillespie on the following link.
#https://stackoverflow.com/questions/19120949/how-to-overlay-scatterplots-in-r

plot(maturity, ytmmatrix[1:10,1], pch=NA_integer_, xlim=c(0,5), 
     ylim=c(2.7,4.6), xlab='Maturity', ylab='Yield to Maturity', 
     main='Yield to Maturity Cruve')
points(maturity, ytmmatrix[1:10,1], col='black')
points(maturity, ytmmatrix[1:10,2], col='red')
points(maturity, ytmmatrix[1:10,3], col='blue')
points(maturity, ytmmatrix[1:10,4], col='gold')
points(maturity, ytmmatrix[1:10,5], col='green')
points(maturity, ytmmatrix[1:10,6], col='grey')
points(maturity, ytmmatrix[1:10,7], col='purple')
points(maturity, ytmmatrix[1:10,8], col='pink')
points(maturity, ytmmatrix[1:10,9], col='orange')
points(maturity, ytmmatrix[1:10,10], col='turquoise')

#After this, I interpolate the data by using linear interpolation. I included a 
#legend so that the each day was indicated by a certain colour. I referred to 
#the following link to help me learn how to create the legend in my plot.
#https://r-coder.com/add-legend-r/

lines(maturity, ytmmatrix[1:10,1], col='black', lty=2)
lines(maturity, ytmmatrix[1:10,2], col='red', lty=2)
lines(maturity, ytmmatrix[1:10,3], col='blue', lty=2)
lines(maturity, ytmmatrix[1:10,4], col='gold', lty=2)
lines(maturity, ytmmatrix[1:10,5], col='green', lty=2)
lines(maturity, ytmmatrix[1:10,6], col='grey', lty=2)
lines(maturity, ytmmatrix[1:10,7], col='purple', lty=2)
lines(maturity, ytmmatrix[1:10,8], col='pink', lty=2)
lines(maturity, ytmmatrix[1:10,9], col='orange', lty=2)
lines(maturity, ytmmatrix[1:10,10], col='turquoise', lty=2)
legend("topright", legend=c("Jan 16", "Jan 17", "Jan 18", "Jan 19", "Jan 20", 
                            "Jan 23", "Jan 24", "Jan 25", "Jan 26", "Jan 27"),
       lty = 2, col=c("black", "red", "blue", "gold", "green", 
                                  "grey", "purple", "pink", "orange", "turquoise"))

#4b) Now I will find the spot curve, but sometimes I refer to the spot rate as 
#yield, as something distinct from yield to maturity (ytm).
#I calculate the yields for CAN 1.75 Mar 23 and CAN 0.5 Nov 23. Notice that
#I discounted the first coupon payment for the November yields using the March 23
#yield rate, instead of interpolating a 3 month yield. The reason why was because
#there was only one data point so far for each day so there's no best linear fit.

yieldmar23 <- matrix(ncol=10, nrow=1)
yieldnov23 <- matrix(ncol=10, nrow=1)
empty <- matrix(ncol=10, nrow=8)

e = exp(1)
c = 100+0.5*cpn
for(i in 1:10)
  yieldmar23[i]= -(365/(43-days[i]))*log(prices[1,i]/c[1])
for(i in 1:10)
  yieldnov23[i]= -(365/(288-days[i]))*log((prices[2,i]-0.5*cpn[2]*
                                            e^(yieldmar23[i]*(-104/365)))/c[2])  

#Now that every remaining bond matures either on March or September, it becomes 
#easier to write a nested loop to calculate all remaining yields using the
#same bootstrapping method as above. In retrospect, I think it was not necessary
#to solve for the yields of first two bonds and then write a for loop for the
#rest. I think I could have solved for all the yields all at once if I created
#matrices that contained the times for each payments for each bond.

newmatrix <- matrix(nrow=10, ncol=10)
for(i in 1:10){
  newmatrix[1,i] <- yieldmar23[i]
  newmatrix[2,i] <-yieldnov23[i]}

vect <- matrix(nrow=1, ncol=2)
p <- matrix(nrow=1, ncol=1)
for(i in 3:10){
  for(j in 1:10){
    for(m in 1:(i-1))
    {vect[m] = 0.5*cpn[i]*e^((43+182.5*(m-1)-days[j])*(-1/365)*newmatrix[m,j])}
    p[1,1] <- prices[i,j]-sum(vect[1:i-1])
    newmatrix[i,j] = -(365/(43+182.5*(i-1)-days[j]))*log(p[1,1]/c[i])
    vect <- matrix(ncol=i-1, nrow=1)
    p[1,1] <- prices[i,j]}}

#I then convert the matrix from decimal form to percents by 
#multiplying each index by 100. I call the resulting matrix percentyields.
percentyields <- 100*newmatrix

#I then plot each bonds yield/spot rate as the dependent variable and the time 
#until maturity as the independent variable. Each column represents the percent
#yields of all 10 bonds on a specific day. 

plot(maturity, percentyields[1:10,1], pch=NA_integer_, xlim=c(0,5), 
     ylim=c(2.7,4.6), xlab='Maturity', ylab='Yield', main='Spot Curves')
points(maturity, percentyields[1:10,1], col='black')
points(maturity, percentyields[1:10,2], col='red')
points(maturity, percentyields[1:10,3], col='blue')
points(maturity, percentyields[1:10,4], col='gold')
points(maturity, percentyields[1:10,5], col='green')
points(maturity, percentyields[1:10,6], col='grey')
points(maturity, percentyields[1:10,7], col='purple')
points(maturity, percentyields[1:10,8], col='pink')
points(maturity, percentyields[1:10,9], col='orange')
points(maturity, percentyields[1:10,10], col='turquoise')

#After this, I interpolate the data by using linear interpolation. I included a 
#legend so that the each day was indicated by a certain colour. 

lines(maturity, percentyields[1:10,1], col='black', lty=2)
lines(maturity, percentyields[1:10,2], col='red', lty=2)
lines(maturity, percentyields[1:10,3], col='blue', lty=2)
lines(maturity, percentyields[1:10,4], col='gold', lty=2)
lines(maturity, percentyields[1:10,5], col='green', lty=2)
lines(maturity, percentyields[1:10,6], col='grey', lty=2)
lines(maturity, percentyields[1:10,7], col='purple', lty=2)
lines(maturity, percentyields[1:10,8], col='pink', lty=2)
lines(maturity, percentyields[1:10,9], col='orange', lty=2)
lines(maturity, percentyields[1:10,10], col='turquoise', lty=2)
legend("topright", legend=c("Jan 16", "Jan 17", "Jan 18", "Jan 19", "Jan 20", 
                         "Jan 23", "Jan 24", "Jan 25", "Jan 26", "Jan 27"),
       lty = 2, col=c("black", "red", "blue", "gold", "green", 
       "grey", "purple", "pink", "orange", "turquoise"))

#Question 4c)

#To obtain the forward rates, I take the partial derivative of the yield curve 
#with respect to T variable, time until maturity. I will take the averages of 
#the left and right derivatives for the non-differentiable points. For the left 
#endpoint and right endpoint, I take the right and left derivatives respectively. 
#For any other value of time until maturity, the partial derivative is simply 
#the slope between the yields of the two bonds. 

#tdistance is just a vector representing the amount of time between the maturity 
#dates of each adjacent pair of bonds. It is not exact, and out of convenience
#I wrote 182.5 to represent number of days in 6 months. The derivative matrix is 
#the matrix of the slopes of each yield/spot curve. The semiderivative matrix 
#gives a rough notion of a derivative for the points whose derivative is not 
#defined, in the ordinary sense.

derivative <- matrix(ncol=10, nrow=9)
tdistance <- c(62, 365/3, 182.5, 182.5, 182.5, 182.5, 182.5, 182.5, 182.5)
for(i in 1:9){
  for(j in 1:10){
    derivative[i,j] <- (percentyields[i+1,j]-percentyields[i,j])/tdistance[i]}}
semiderivative <- matrix(ncol=10, nrow=8)
for(i in 1:8){
  for(j in 1:10){
    semiderivative[i,j] <- (derivative[i,j]+derivative[i+1,j])/2}}

derivative1 <- matrix(nrow=24, ncol=10)
for(i in 1:8){
  for(j in 1:10){
    derivative1[1+3*(i-1),j] <- derivative[i,j]
    derivative1[2+3*(i-1),j] <- semiderivative[i,j]
    derivative1[3+3*(i-1),j] <- derivative[i+1,j]}}

#Multiplied derivatives by 365 to get forward rate per year instead of per day.
derivativematrix <- 365*rbind(derivative[1,1:10], derivative1, derivative[9,1:10])

#maturity2 is a vector I made in order that the forward curves appear more like
#a continuous function of time (to remove the jump discontinuities). 
maturity2 <- c(44/365, 287/365, 288/365, 289/365, 407/365, 408/365,
              409/365, 589.5/365, 590.5/365, 591.5/365, 772/365, 773/365, 
              774/365, 954.5/365, 955.5/365, 956.5/365, 1137/365, 1138/365, 
              1139/365, 1319.5/365, 1320.5/365, 1321.5/365, 1502/365, 1503/365, 
              1504/365, 1684.5/365)

#I then plotted the forward curves using similar codes as before. However for
#legibility I increased the thickness of the lines and varied the line types.
plot(maturity2, derivativematrix[1:26, 1], pch=NA_integer_, xlim=c(0,5.5), 
     ylim=c(-365*0.0026,365*0.0016), xlab='Maturity', ylab='Forward Rate', 
     main='Forward Curves')
lines(maturity2, derivativematrix[1:26,1], col='black', lty=1, lwd=2)
lines(maturity2, derivativematrix[1:26,2], col='red', lty=2, lwd=2)
lines(maturity2, derivativematrix[1:26,3], col='blue', lty=3, lwd=2)
lines(maturity2, derivativematrix[1:26,4], col='gold', lty=4, lwd=2)
lines(maturity2, derivativematrix[1:26,5], col='green', lty=5, lwd=2)
lines(maturity2, derivativematrix[1:26,6], col='grey', lty=6, lwd=2)
lines(maturity2, derivativematrix[1:26,7], col='purple', lty=7, lwd=2)
lines(maturity2, derivativematrix[1:26,8], col='pink', lty=8, lwd=2)
lines(maturity2, derivativematrix[1:26,9], col='orange', lty=9, lwd=2)
lines(maturity2, derivativematrix[1:26,10], col='turquoise', lty=10, lwd=2)
legend("topright", legend=c("Jan 16", "Jan 17", "Jan 18", "Jan 19", "Jan 20", 
                            "Jan 23", "Jan 24", "Jan 25", "Jan 26", "Jan 27"),
       lty = c(1,2,3,4,5,6,7,8,9,10), 
       col=c("black", "red", "blue", "gold", "green", 
                                  "grey", "purple", "pink", "orange", "turquoise"))


#Question 5. I chose the 5 random variables to be bonds that have similar coupon
#rates. In order I chose: CAN 1.75 Mar 23, CAN 1.50 Sept 24, CAN 1.25 Mar 25,
#CAN 1.00 Sept 26, and CAN 1.25 Mar 27.

choice <- c(1,4,5,8,9)
xijs <- matrix(ncol=9, nrow=5)
for(i in 1:5){
  for(j in 1:9){
    xijs[i,j] <- log(percentyields[choice[i],j+1]/percentyields[choice[i],j])}}

mean1 <- mean(xijs[1,1:9])
difference1 <- xijs[1,1:9]-mean1
mean2 <- mean(xijs[2,1:9])
difference2 <- xijs[2,1:9]-mean2
mean3 <- mean(xijs[3,1:9])
difference3 <- xijs[3,1:9]-mean3
mean4 <- mean(xijs[4,1:9])
difference4 <- xijs[4,1:9]-mean4
mean5 <- mean(xijs[5,1:9])
difference5 <- xijs[1,1:9]-mean5
differencematrix <- rbind(difference1, difference2, difference3, difference4,
                          difference5)

variancecovariancelogdailyreturn <- matrix(ncol=5, nrow=5)
for(i in 1:5){
  for(j in 1:5){variancecovariancelogdailyreturn[i,j] <- 
    sum(differencematrix[i,1:9]*differencematrix[j,1:9])/8}}

#Now I will compute the variance-covariance matrix for the forward rates from
#1yr-1yr to 1yr-4yr.For the forward rates, I could have interpolated the yield 
#rates for 1 to 5 years but for sake of simplicity and because interpolation may 
#be inaccurate, I will use actual observed yield rates corresponding to 0.5 to 1 
#years, 1.5 to 2 years, 2.5 to 3 years, 3.5 to 4 years and 4.5 to 5 years.

oneyroneyr <- matrix(ncol=10, nrow=1)
for(j in 1:10)
  {oneyroneyr[j] <- (newmatrix[4,j]*2-newmatrix[2,j])/(2-1)}
diff1 <- oneyroneyr-mu1
oneyrtwoyr <- matrix(ncol=10, nrow=1)
for(j in 1:10)
  {oneyrtwoyr[j] <- (newmatrix[6,j]*3-newmatrix[2,j])/(3-1)}
oneyrthreeyr <- matrix(ncol=10, nrow=1)
for(j in 1:10)
{oneyrthreeyr[j] <- (newmatrix[8,j]*4-newmatrix[2,j])/(4-1)}
oneyrfouryr <- matrix(ncol=10, nrow=1)
for(j in 1:10)
{oneyrfouryr[j] <- (newmatrix[10,j]*5-newmatrix[2,j])/(5-1)}
mu1 <- mean(oneyroneyr)
diff1 <- oneyroneyr-mu1
mu2 <- mean(oneyrtwoyr)
diff2 <- oneyrtwoyr-mu2
mu3 <- mean(oneyrthreeyr)
diff3 <- oneyrthreeyr-mu3
mu4 <- mean(oneyrfouryr)
diff4 <- oneyrfouryr-mu4
diffmatrix <- rbind(diff1, diff2, diff3, diff4)
variancecovarianceforwardrates <- matrix(ncol=4, nrow=4)

for(i in 1:4){
  for(j in 1:4){variancecovarianceforwardrates[i,j] <- 
    sum(diffmatrix[i,1:10]*diffmatrix[j,1:10])/9}}

#Question 6
#Eigenvalues and their eigenvectors for the variance covariance matrix of the
#daily log returns of yield.
eigenvalues1 <- eigen(variancecovariancelogdailyreturn, symmetric=TRUE)$values
eigenvectors1 <- eigen(variancecovariancelogdailyreturn, symmetric=TRUE)$vectors

#Eigenvalues and their eigenvectors for the variance-covariance matrix of the
#daily 1yr-1yr to 1yr-4yr forward rates, treated as random variables.
eigenvalues2 <- eigen(variancecovarianceforwardrates, symmetric=TRUE)$values
eigenvectors2 <- eigen(variancecovarianceforwardrates, symmetric=TRUE)$vectors