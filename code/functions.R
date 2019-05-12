# Estimate hypoerparameter

Hyper <- function(trainx, trainy, repnum, N) {
  
  marlik <- function(theta) {
    theta <- theta^2
    kxx <- covmat(trainx = trainx, repnum = repnum, theta = theta[1:4])
    -dmvnorm(x = trainy, sigma = kxx + theta[5] * diag(N), log = T)
  }
  
  hyp <- optim(par=rep(1, 5), fn = marlik, method = 'BFGS',
               control=list(maxit = 10000))
  print(hyp)
  return(hyp$par^2)
  
}


# Fit a smooth curve--------------------------------

gpsmooth <- function(x, trainlist) {
  
  kxx <- covmat(trainx = trainlist$trainx, repnum = trainlist$rep, 
                theta = trainlist$hyper[1:4])
  kx <-  testcov(x = x, y = trainlist$trainx, theta = trainlist$hyper[1:4])
  kinv <-  chol2inv(chol(kxx + trainlist$hyper[5] * diag(trainlist$N)))
  k <- kx %*% kinv
  pred <- k %*% as.matrix(trainlist$trainy)
  return(pred)
  
}


# Posterior mean and cov matrix for test curve

fit.gp <- function(trainx, trainy, testx, repnum, theta, kinv, N) {
  
  n <- length(testx)
  #kxx <- covmat(trainx, repnum , theta = theta[1:4])
  kx <-  testcov(x = testx, y = trainx, theta = theta[1:4])
  k <- kx %*% kinv
  mu <- k %*% as.matrix(trainy)    
  sigma <- testmat(x = testx, theta = theta[1:4]) + theta[5] * diag(n) - (k %*% t(kx))
  sigma <- as.matrix(forceSymmetric(sigma))
  
  return(list(mu = mu, sigma = sigma))
}


# Extract relevant feature of the data

feature <- function(train) {
  
  n <- length(unique(train$idnum))
  N <- dim(train)[1]
  trainx <- train$age
  trainy <- train$spnbmd
  rep <- as.numeric(table(train$idnum))
  hyper <- Hyper(trainx = trainx, trainy = trainy, repnum = rep, N = N)
  return(list(n = n,N = N,rep = rep, trainx = trainx, trainy = trainy, hyper = hyper))
  
}