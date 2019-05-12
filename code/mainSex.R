

fet <- lapply(train, feature)

ageseq <- seq(min(train$fem$age), max(train$fem$age), length = 100)

plot(fet$fem$trainx, fet$fem$trainy, pch = 16, col = 1, 
     cex=.5, xlab='Age', ylab='Spinal bone mineral density')
points(fet$mal$trainx, fet$mal$trainy, pch = 16, col = 2, cex=.5)
lines(ageseq, gpsmooth(ageseq, fet$fem), type='l', lwd = 2, col = 1)
lines(ageseq, gpsmooth(ageseq, fet$mal), type='l', lwd = 2, col = 2)
#legend('bottomright', c('Male', 'Female'), col = c(1,'grey'), lty=1, bty = 'n')
