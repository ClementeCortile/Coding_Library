#Q1 Probability of answering 4 or less correct answers in a multiple choice test made by 12 questions with 5 possible answers.

#P(X <= 4)

#Computing probability by binomial distribution
P1 = dbinom(x = 0:4, size = 12, prob = 1/5)
#Printing results
P1
#cumulative prob function (shortcut to sum)
pbinom(4, size=12, prob=1/5) 
#--------------------------------------------------------------------------------------------------------#


#Q2 Probability of check-out time being less than 2 min where average rate is 3 per minute

#Computing probability with exp distribution (time interval between two events)
P2 = pexp(2,1/3)
#Printing results
P2
#--------------------------------------------------------------------------------------------------------#

#Q3 find the 2.5th and 97.5th percentiles of the student's T distribution with 5 degrees of freedom 
qt(c(.025, .975), df=5)

#--------------------------------------------------------------------------------------------------------#

#Q4 Hypothesis testing with a sampled mean of 9900, sample size of 30, population std.dev of 120 hours 
"
Null H: mean = 10000
Alt H: mean < 10000
Lower tail test
"
#declaring variables
n = 30
s = 120
mu_0 = 10000
x_bar = 9900


#Computing z-score to find the deviation of x_bar from the mean in terms of the sample variance
z = (x_bar - mu_0)/((s/sqrt(n)))
cat("z-score:", z)
#NB a positive Z score would gauge the area on the right of the crit value. We are searching for the area on the left.

#Computing P-value
pval = pnorm(z)
cat("P-value:", pval)


if (pval < alpha) {
  print("Reject H_0")
} else {
  print("Fail to reject H_0")
}
#Sol: Reject H_0

#--------------------------------------------------------------------------------------------------------#

#Q5 Categorical data Hypothesis testing on repeated sampling. 
"
Sample proportion 60%. Sample proportion of the sampled size 85/148
Confidence level = 95%.

Null H_0: p0 = 0.6
Altn H_a: p0 > 0.6
"

#declaring variables
n = 148
p0 = 0.6
pbar = 86/148
alpha = 0.05
sigma = sqrt(p0*((1-p0)/n)) #Stdev estimate from Bernoulli categorical distribution

#Computing z score (sampled size first since we use pnorm to evaluate p-value)
z = (pbar - p0)/sigma
cat("z-score:", z)

#Computing P-value
pval = pnorm(z)
cat("P-value:", pval)

if (pval < alpha) {
  print("Reject H_0")
} else {
  print("Fail to reject H_0")
}
#Sol: "Fail to reject H_0"

#--------------------------------------------------------------------------------------------------------#

#Q6 
"Assuming that the data in immer follows the normal distribution, find the 95% confidence interval estimate
of the difference between the mean barley yields between years 1931 and 1932.
"
library(MASS)
head(immer)
"
The two distribution come from the same sample The paired t-test function will:
1) consider the sum of the two distribution as one and compute their joint mean and joint variance
2) consider the distance in the two means (d = m_Y1 - m_Y1 )
3) consider the new mean (m_Y1 - m_Y1) as the mean of the joint distribution and compute a confidence
   interval that contains it 95% of the time (using the std normal z-value = 1,96, 1,96*sum of their variance)
4)test whenever the new mean has a random chance of less than 5% of ending in it 
"
#Calling paired t-test function, results are returned in a list
sol6 = t.test(immer$Y1, immer$Y2, paired=TRUE)

sol6
sol6[4]

#--------------------------------------------------------------------------------------------------------#

#Q7
"Assuming that the data in mtcars follows the normal distribution, find the 95% confidence interval 
estimate of the difference between the mean gas mileage of manual and automatic transmissions.
"

#Printing dataset
mtcars

#Locating the columns miles per gallon and trasmission dummy [auto = 0, man = 1]
mtcars$mpg
mtcars$gear

#Selecting subsets of car by transmission dummy
A = mtcars$am == 0 #Auto
M = mtcars$am == 1 #Manual

#Crossing transmission selection with mileage column
mileage_A = mtcars[A,]$mpg
mileage_M = mtcars[M,]$mpg

mileage_A
mileage_M

#calling unpaired t-test function
sol7 = t.test(mileage_A, mileage_M )

sol7

#--------------------------------------------------------------------------------------------------------#

#Q8
"
Without assuming the data to have normal distribution, test at .05 significance level if the barley yields 
of 1931 and 1932 in data set immer have identical data distributions (Identical population is the null hypothesis)

Samples come from the same distribution but without normality assumption parametric methods cannot be used.
Wilcoxon signed-rank test can be used to test if the distributions are identical

Stating the hypothesis testing for the two distributions
Null H_0: m_Y1  =  m_Y2
Altn H_a: m_Y1 !=  m_Y2

The function will consider the conditional probability of P( m_Y1 - m_Y1 | H_0) < 5%

if the random chance of having a difference between the two means, when H_0 is true,
is smaller than 5% then H_0 is rejected.

the function will print the 95% confidence interval estimate of the joint distribution
"

sol8 = wilcox.test(immer$Y1, immer$Y2, paired=TRUE)

sol8[3] 

if (sol8[3] < alpha) {
  print("Reject H_0")
} else {
  print("Fail to reject H_0")
}
#Sol: "reject H_0"


write.csv(immer, file = "immer.csv")
write.csv(mtcars, file = "mtcars.csv")



