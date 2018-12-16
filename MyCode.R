#read the data 
myData = read.csv("TPTP-data-201718.csv")

#print the statistical summary
print(summary(myData))

#convert to a matrix and then plot it
new_Data = data.matrix(myData)
hist(new_Data, xlim = c(0, 0.5), breaks = 100, col = rgb(221,160,221, maxColorValue = 255), xlab = "Values", main = "TPTP Dataset Histogram")

#initialize constants
n = 458
alpha = 5
denom = 0

#calculate betaCap denominator
for (i in 1:n)
  {
    denom = denom + (1/myData[i, 1])
  }
print(denom)

#calculate betaCap using formula betaCap = (n * alpha)/sum of all from 1 to n(1/x_i)
betaCap = (n * alpha) / denom
print(betaCap)

#Confidence Interval for Beta
beta_L = betaCap - 1.96 * (betaCap / sqrt(n*alpha))
beta_U = betaCap + 1.96 * (betaCap / sqrt(n*alpha))

#create empty array for simulations, each with a different Y'
Y_dash = numeric(10000)

#initialize y
y = sum(myData)

#CASE 1: 10000 simulations where we use betaCap
count = 0
for (j in 1:10000) 
{
  x = invgamma::rinvgamma(n, 5, betaCap)
  Y_dash[j] = sum(x)
  
  if (Y_dash[j] < y) {
    count = count+1
  }
}

#calculate probability where we use betaCap
prob_betaCap = count/10000

#CASE 2: 10000 simulations where we use lower confidence interval
count = 0
for (j in 1:10000) 
{
  x = invgamma::rinvgamma(458, 5, beta_L)
  Y_dash[j] = sum(x)
  
  if (Y_dash[j] < y) {
    count = count+1
  }
}

#calculate probability where we use lower confidence interval
prob_beta_L = count/10000

#CASE 3: 10000 simulations where we use upper confidence interval
count = 0
for (j in 1:10000) 
{
  x = invgamma::rinvgamma(458, 5, beta_U)
  Y_dash[j] = sum(x)
  
  if (Y_dash[j] < y) {
    count = count+1
  }
}

#plot the probabilities to show effect of different Beta Estimator
vector.numeric = c(prob_beta_L*100, prob_betaCap*100, prob_beta_U*100)
vector.numeric
barplot(vector.numeric, names.arg = c("Beta Lower", "Beta Cap", "Beta Upper"), xlab = "Beta values", ylab = "Probabilities", main  = "Visualization", col = "lightpink")

#calculate probability where we use upper confidence interval
prob_beta_U = count/10000

#required details
hist(Y_dash, breaks = c(41,41.5,42, 42.5, 43, 43.5, 44, 44.5, 45, 45.5,46, 46.5, 47, 47.5, 48, 48.5, 49, 49.5, 50, 50.5, 51, 51.5, 52, 52.5, 53),col = rgb(255, 207, 158, maxColorValue = 255))
summary(Y_dash)
rgb
cat(prob_betaCap * 100, "%", "\n" ) 
cat(prob_beta_L * 100, "%", "\n" )
cat(prob_beta_U * 100, "%", "\n" )