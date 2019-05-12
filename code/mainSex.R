

fet <- lapply(train, feature) #estimate hyperparameter


# Plot fitted
ageseq <- seq(min(train$fem$age), max(train$fem$age), length = 100)

plot(fet$fem$trainx, fet$fem$trainy, pch = 16, col = 1, 
     cex=.5, xlab='Age', ylab='Spinal bone mineral density')
points(fet$mal$trainx, fet$mal$trainy, pch = 16, col = 2, cex=.5)
lines(ageseq, gpsmooth(ageseq, fet$fem), type='l', lwd = 2, col = 1)
lines(ageseq, gpsmooth(ageseq, fet$mal), type='l', lwd = 2, col = 2)
#legend('bottomright', c('Male', 'Female'), col = c(1,'grey'), lty=1, bty = 'n')


#------------------------------


log.prob <- c()

for(i in unique(test$idnum)) {
  
  fit1 <- fit.gp(fet$fem, testx = test[test$idnum == i, 3], testy = test[test$idnum == i, 5])
  fit2 <- fit.gp(fet$mal, testx = test[test$idnum == i, 3], testy = test[test$idnum == i, 5])
  log.prob <- rbind(log.prob, c(fit1, fit2))
  
}

y <- c()
for (i in unique(test$idnum)) {
  y <- rbind(y, as.character((test[test$idnum == i, 4])[1]))
}

y.pred <- log.prob[,1] > log.prob[,2]
pred <- ifelse(y.pred == 0, 'mal', 'fem')
table(y.pred, y)
err <- mean(pred != y)
err
