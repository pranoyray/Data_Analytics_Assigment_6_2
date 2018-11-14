#Data Analytics Assignment 6_2 Session 6

#Import the bank marketing data set
bank <- read.csv("bank-additional.csv", sep=";")
View(bank)
dim(bank) 
str(bank) 

library(readr)
bank_full <- read_delim("bank-additional.csv",
                        ";", escape_double = FALSE, trim_ws = TRUE)
#Lets look at dataset and generate initial understanding about the column types
str(bank_full)
#A deep check for NA in a particular column let say age
if(length(which(is.na(bank_full$age)==TRUE)>0)){
  print("Missing Value found in the specified column")
} else
  print("All okay: No Missing Value found in the specified column")


# Check another example say
if(length(which(is.na(bank_full$campaign)==TRUE)>0)){print("Missing Value found in the specified
                                                           column")} else
                                                             print("All okay: No Missing Value found in the specified column")
head(bank_full) ## Displays first 6 rows for each variable
str(bank_full) ## Describes each variables
summary(bank_full) ## Provides basic statistical information of each variable
## DATA EXPLORATION - Check for Missing Data
## Option 1
is.na(bank_full) ## Displays True for a missing value
## Since it is a large dataset, graphical display of missing values will prove to be easier
##Option 2
require(Amelia)
missmap(bank_full,main="Missing Data - Bank ", col=c("red","grey"),legend=FALSE)
## No red colour stripes are visible. hence no missing values.
summary(bank_full) ## displays missing values if any under every variable
#The Pearson's chi-squared test of independence is one of the most basic and common hypothesis tests
#in the statistical analysis of categorical data. It is a significance test. Given two categorical random
#variables, X and Y, the chi-squared test of independence determines whether or not there exists a
#statistical dependence between them. Formally, it is a hypothesis test. The chi-squared test assumes a
#null hypothesis and an alternate hypothesis. The general practice is, if the p-value that comes out in the
#result is less than a pre-determined significance level, which is 0.05 usually, then we reject the null
#hypothesis.
#H0: The The two variables are independent
#H1: The The two variables are dependent
#The null hypothesis of the chi-squared test is that the two variables are independent and the alternate
#hypothesis is that they are related.
#To establish that two categorical variables (or predictors) are dependent, the chi-squared statistic must
#have a certain cutoff. This cutoff increases as the number of classes within the variable (or predictor)
#increases.

#i. Pearson's chi-squared test of independence (significance test)

#Q1 (a): Is there any association between Job and default?

#Solution:
  with(bank_full, chisq.test( job, default))
with(bank_full, table( job, default) )
# OR
with(bank_full, prop.table(table( job,default)))
#Pearson's Chi-squared test 

#since the p-value is < 2.2e-16 is less than the cu$t-off value of 0.05, we can reject the null hypothesis in
#favor of alternative hypothesis and conclude, that the variables,( job & default- p-value = 8.008e-09) are
#dependent to each other.

#Q 1 (b). Is there any significant difference in duration of last call between
#people having housing loan or not?

#Solution:
  with(bank_full, chisq.test(duration,housing))
with(bank_full, table( duration,housing) )
# OR
with(bank_full, prop.table(table(duration, housing)))
#data: duration and housing
#X-squared = 3162.3, df = 3086, p-value = 0.1657
#P value is above 0.05#	

#Q1 (c): Is there any association between consumer price index and consumer?

#Solution:
  with(bank_full, chisq.test(cons.price.idx,cons.conf.idx))
with(bank_full, table(cons.price.idx,cons.conf.idx))
# OR
with(bank_full, prop.table(table(cons.price.idx,cons.conf.idx)))
#p-value < 2.2e-16 and it is very much less than 0.05.we can reject the null hypothesis in favor
#of alternative hypothesis and conclude, that the variables, (job & Marital-p-value < 2.2e16),(con.price.idx
#consumer- are dependent to each other. 

#Q1 (d): the employment variation rate consistent across job types?
#SOlution:

  with(bank_full, chisq.test( job,emp.var.rate))
with(bank_full, table( job,emp.var.rate) )
# OR
with(bank_full, prop.table(table( job,emp.var.rate)))
#p-value < 2.2e-16 is very much less than 0.05 


#Q1 (e): Is the employment variation rate same across education?

#Solution:
  with(bank_full, chisq.test( education,emp.var.rate))
with(bank_full, table( education, emp.var.rate) )
# OR
with(bank_full, prop.table(table( education,emp.var.rate)))  
