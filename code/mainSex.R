

fet <- lapply(train, feature)

hyper <- lapply(fet, esthyper)

ageseq <- seq(min(bone$age), max(bone$age), length = 100)

plot(fet$fem$trainx, fet$fem$trainy, pch = 16, col = 1, 
     cex=.5, xlab='Age', ylab='Spinal bone mineral density')
points(fet$mal$trainx, fet$mal$trainx, pch = 16, col = 2, cex=.5)
lines(ageseq, gpsmooth(fet$fem$trainx, fet$fem$trainy, rep1, hyp1, N1), type='l', lwd = 2, col = 1)
lines(ageseq, gpsmooth(train.x2, train.y2, rep2, hyp2, N2), type='l', lwd = 2, col = 2)
#legend('bottomright', c('Male', 'Female'), col = c(1,'grey'), lty=1, bty = 'n')
