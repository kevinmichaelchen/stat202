x1 <- rnorm(n=100,mean=0,sd=2)
x2 <- runif(n=100)
y <- 3*x1 - x2 + rnorm(n=100)

if (!"gvlma" %in% rownames(installed.packages())) {
  install.packages("gvlma")  
}
library("gvlma", lib.loc=.libPaths()[1])

m <- lm(y ~ x1 + x2)
g <- gvlma(m)

summary(m)
summary(g)
plot(g)
g.del <- deletion.gvlma(g)
summary(g.del)
