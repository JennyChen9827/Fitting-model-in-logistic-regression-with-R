##fitting model in logistic regression

##create two binary vectors of length 100
x <- sample(c(0,1),100, replace=T)
x
y <- sample(c(0,1),100, replace=T)

##x and y is tabular form, create a 2x2 table with count, that data is grouped
xytab=table(x,y)
xytab

##y is response variable such as function of x that has two column (success=1, failure=0). We create count table such as column 2 is y=1
count <-cbind(xytab[,2], xytab[,1])
count

## We need a categorical predictor variable "0" & "1"
xfactor <- factor(c("0","1"))
xfactor

##specify y's ditribution and a link with x
tmp3 <- glm(count~xfactor,family=binomial("logit"))
tmp3

summary(tmp3)

##Data come in a matrix form, where data are ungrounped
xydata <- cbind(x,y)
xydata
tmp1=glm(y~x, family=binomial("logit"))
tmp1
_________________________________________________________

##smoking case: Y=student smoke x=1 is one or more parent smoke & x=0 is no parents smok

parentsmoke <- as.factor(c(1,0)) #create categorical varibale instead of a numeric
response <- cbind(yes=c(816,188), no=c(3203,1168)) #create Y vector that counts for both success and failure
response

##model for chinldren smoke b0=-1.82 says that log-odds of children smoke vs not smoke if none of parent smoke is satistially significant
##b1=0.459 says the log-odd of children smoke same statistically significant between x1=0 and x1=1 and eastimated odd ration is exp(0.459)=1.58 
summary(smoke.logistic <- glm(response~parentsmoke, family=binomial(link=logit))) #x1=0 is baseline that neither smoke, x1=1 is at least one smoke

##Only intercept model
summary(glm(response~1, family=binomial(link=logit)))
