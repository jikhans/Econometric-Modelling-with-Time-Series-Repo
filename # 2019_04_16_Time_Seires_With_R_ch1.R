# 2019_04_16_Time_Seires_With_R_ch1

# reference: http://www.kocw.net/home/search/kemView.do?kemId=977301

library(astsa)
plot(jj, type="o", ylab ="Quarterly EPS")
par(mfrow = c(2,1))
plot(soi,main ="a-")
plot(rec, main="b-")

# ma

w = rnorm(500,0,1)
v = filter(w, sides=2, rep(1/3,3))
par(mfrow=c(2,1))
plot.ts(w, main ="white noise")
plot.ts(v, main="moving average")

set.seed(1)
w = rnorm(200,0,1) ; x=cumsum(w)
wd = w +.2 
xd = cumsum(wd)
plot.ts(xd, ylim=c(-5,55), main ='random walk')
lines(x)
lines(.2*(1:200), lty ="dashed") #lty line style

ls()

data(jj)
jj


ls(all.names=TRUE)
data("WBC")
WBC


class(jj)
jjm=as.matrix(jj)
class(jj)
dim(jjm)

z = c(1,2)
class(z)

mydir

colnames('jj')
jj
colnames(jjm)

rep(3,10)

rcauchy(10)

crazy <- function(num) {
  x <- rep(NA, num)
  for (n in 1:num) x[n] <- mean(rcauchy(n))
  plot(x)
}
crazy(100)
