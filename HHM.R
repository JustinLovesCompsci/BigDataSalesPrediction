DELTA <- matrix(0, nrow = 1115, ncol = 2)
PI <- list()
STAT <- list()
MU <- matrix(0, nrow = 1115, ncol = 2)
SD <- matrix(0, nrow = 1115, ncol = 2)
Pr.1 <- rep(0, max(train$Store))
Pr.2 <- rep(0, max(train$Store))

for ( i in 1 : max(train$Store) ) {
  
  obs <- trainstore[[i]]$Sales[ trainstore[[i]]$Sales > 1 ]
  
  trainstore[[i]] <- train[which(train$Store == i),]
  

  while (i == 103) { i <- 104 }; while ( i == 349 ) { i <- 350 };
  while (i == 708) { i <- 709}; while ( i == 972 ) { i <- 973 }
  
  Pi <- matrix(
    rep(1/2, 4),
    byrow = 2, nrow = 2
  )
  
  delta <- c(1/2, 1/2)
  
  parameters <- list(
    mean = as.numeric( quantile( trainstore[[i]]$Sales, c(0.25, 0.75 )) ),
    sd = 0.5*as.numeric( quantile( trainstore[[i]]$Sales, c(0.25, 0.75 )) )
  )
  
  x <- dthmm(obs, Pi, delta, "norm", parameters)
  
  y <- BaumWelch(x, control = bwcontrol(maxiter = 500))
  
  DELTA[i,] <- y$delta
  PI[[i]] <- matrix(y$Pi, byrow = T, nrow = 2)
  Pr.1[i] <- PI[[i]][1]
  Pr.2[i] <- PI[[i]][2]
  STAT[[i]] <- statdistr(y$Pi)
  MU[i,] <- y$pm$mean
  SD[i,] <- y$pm$sd
  
}

mu.good <- rep(0, 1115)

for ( i in 1 : 1115 ) { mu.good[i] <- MU[[i]][2]}
