# ---
#   title: "STAT509-001-HW1-Xinye Xu"
# output: word_document
# ---
  
  ## Q1
#   Suppose X is double exponential with mean 0 and standard deviation of 2.
# (a) As Y = X + 2 is a strictly increasing function, Yq = Xq + 2 = -0.2760889
# 
source('startup.R') # for DExp
library(fGarch) # cal for GED-Dist
library(ggplot2)
library(fBasics) # cal for skewness and kurtosis
lambda = sqrt(2) / 2 
qdexp(p = 0.1, mu = 0, lambda) + 2 # (a)

lambda2 = sqrt(2) / 2
internal = log(1 / (5 * (exp(-2 * lambda2) + exp(2 * lambda2))))
Xt = (lambda2)^2 / (internal)^2 # (b)


## Q2
# 2. Suppose have portfolio of 100 million dollars. Compute the value-at-risk and relative value-at-risk, at α = .002 for the following cases.
# (a) norm mean of 0 and std of .025. : Var_rel= 0.06942634; Var = 100m*Var= 6.94 million
# (b) GED-dist with a mean of 0 and std of .025 for the cases of ν = 0.5, 0.9, 1.4.:Var_rel= 0.1260278, 0.09758018, 0.08011714; and Var = 12.60, 9.76, 8.01 million.
# (a)
norm_q <- qnorm(0.002, 0, 0.025) 
Var_a <- - (exp(norm_q) - 1)

# (b) qged from packate fGarch:
l <-  data.frame(0.5, 0.9, 1.4)
l <- rbind(l, rapply(l, function(x) qged(0.002, mean = 0, sd = 0.025, nu = x)))
l <- rbind(l, rapply(l[2,], function(x) -(exp(x) - 1)))


## Q3
# (a) From the graph below, it suggests that NASDAQ has postive increasing trend during the most of time between Jan/2015 to Dec/2018. It seems to be a little stable index of 5000 until a big fall at the beginning of 2016.Then it increased to the peak of 8000 and this upward trend ended nearly on the second half year of 2018. After that, it decreased to about 6000 on Dec 2018.  
fin_dat <- read.csv("/Users/xuxinye/Desktop/Umich classes/STATS 509/Data/Nasdaq_daily_Jan1_2015-Dec31_2018.csv")
fin_dat$Date <- as.Date(fin_dat$Date, format = "%m/%d/%Y")
ggplot(data = fin_dat, aes(x = Date, y = Adj.Close))+geom_line() 


# (b) From below, it suggets that the log daily return has range form -0.04 to 0.05, both of these extreme points happened in 2018. And distributions for log-return has a mean that is close to 0 (0.0003). Also, by calculating the standard deviations(0.01030379), it's quite small. The daily log returns are quite symmetric with a slight negative skewness =  -0.4940055 and its excess kurtosis is large, which is 3.187553, suggesting a heavier tail.So it can be modeled based on Double Exp distribution since Kurt of it is 3 and skew is 0. Plus, using boxplot, it suggests a negative heavy tail. So there might be ourliers. 
log_ret <- diff(log(fin_dat$Adj.Close))
summary(log_ret)
sd(log_ret)
skewness(log_ret)
kurtosis(log_ret)
fin_dat$log_ret <- c(NA, log_ret) 
ggplot(data = fin_dat, aes(x = Date, y = log_ret))+geom_line()
hist(log_ret, breaks = 200, main='log-daily-ret distribution', freq = FALSE)
boxplot(log_ret)


# (c) From (b), the estimate of the mean = 0.0003375 and standard deviation = 0.01030379, Relative VaR = 0.03424105。 
# Based on the .004-quantile data of log-returns, Relative VaR = 0.03820149, It suggests that Rel-VaR is a little bigger than one simulated by Double Exp. It's also in consistant with the conclusion that we have in (b) that excess kurtosis (3.187553) of data is larger than DExp's.
mu <- 0.0003375 # P0 <- fin_dat$Adj.Close[1]
std <- 0.01030379
lambda = sqrt(2) / std
dexp_q <- qdexp(p = 0.004, mu, lambda)
Var <- - (exp(dexp_q) - 1)
Var_o <- - (exp(quantile(log_ret, 0.004)) - 1) # quantile of log return data


# (d) Based on the double exponential model in (c), derive (analytically) an estimate of the expected shortfall.
1 / (2 * 0.004) * exp(-lambda*(-dexp_q - mu))*(-dexp_q + 1/lambda)


## Q4
It can be converted to Pr(log-ret >= log(110/100)), log-ret ~ N(0.1, 0.2^2); Then the prob = 0.509354 that it can selling at $110 or more.
{r}
pnorm(log(110/100), mean=0.1, sd=0.2, lower.tail = FALSE) 
# lower.tail = FALSE: Pr(X > x); True :  Pr(X < x);


## Q5
# We know sum of iid normal dist is normal dist.So two year log-ret (R1 + R2) ~ N(0.08*2,2*0.15^2). It can be converted to Pr(two-year log-ret >= log(90/80)), Then the prob = 0.5788736 that it can selling at $90 or more.
pnorm(log(90/80), mean=0.08*2, sd=sqrt(2*0.15^2), lower.tail = FALSE) 
