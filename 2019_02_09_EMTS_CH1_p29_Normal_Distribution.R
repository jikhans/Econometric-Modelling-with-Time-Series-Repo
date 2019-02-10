## 2019_02_09_EMTS_CH1_p29_Normal_Distribution
## Reference: http://www.cambridge.org/features/econmodelling/exercises.htm


#=======================================================================
#
#    Program to estimate a Normal model with unknown mean 
#   and known variance equal to one
#
#=======================================================================

# Assume that ¥ò2 = 1.
# A sample of T = 5 observations consisting of the values {1, 2, 5, 1, 2}
# p15


getwd()
setwd("C:/rstudio/0_2019_spring/model_ts")
getwd()

# clear all
rm(list = ls(all=TRUE))
graphics.off()

# sample data for y
y <-cbind(1,2,5,1,2)
class(y) # matrix
t <-length(y)
t

theta <- mean(y)

cat('\nMLE\n')
cat('---\n')
cat(theta, '\n')
?cat # don't change the line of the output, but if we want then we use \n to go next line

# Define the log of the likelihood at each observation
lnlt <- -0.5*log(2*pi) - 0.1*(y - theta)^2 # p15, given code from the book is wrong for this probelm so I modified it.

# Evaluate log like at each obs
cat('\n')
cat('\nLog like at MLE for each obsservation\n')
cat('----------------------------\n')

seq(t) #t = sample size =5, making a sequence from 1 to 5
rnames <-seq(t) #1 2 3 4 5
rnames
cnames <- c("yt","lnlt")
cnames
print(matrix(c(y, lnlt), nrow=t, dimnames = list(rnames,cnames))) # dimnames list(c("rowname",),c("colnames",""))

lnlt
class(lnlt) # matrix
cat('\nLog Likelihood at MLE\n')
cat('---------------------\n')
cat(mean(lnlt), 'is the average value of loglikelihood \n') #p12

# Evaluate the gradient at the MLE  for each obs

g_t <- y- theta # theta is mean(y)

# Gradient means differentiationg -> gradient = d(lnL(parameter))/d(parameter) =0, parameter_hat = argmax(gradient)
cat('\nGradient of the log like at MLE for each obs\n')
cat('--------------------------------------------\n')
cat(g_t, '\n')

# Evaluate the gradient at the MLE
g <- (1/t)*mean(g_t) # gradient by mean
g # means
g2<- -0.5+1/2*1/5*mean((y-theta)^2)
g2 #-0.284, variance


cat('\nGradient of the log likelihood at MLE\n')
cat('-------------------------------------\n')
tol <- 1e-9
cat(ifelse(g < tol,0.0,g), '\n')



# Evaluate the Hessian at the MLE  for each obs
h <- -t

cat('\nHessian of the log likelihood at MLE\n')
cat('------------------------------------\n')
cat(h, '\n') #hesssian H11 -t/variance(=1 in given), t may come from sampling variance compare to population variance



