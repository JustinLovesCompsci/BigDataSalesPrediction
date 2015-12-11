require(HiddenMarkov)
require(DTMCPack)

DELTA <- matrix(0, nrow = 1115, ncol = 2) # Saves initial distribution
PI <- list() # Saves transition matrix
STAT <- matrix(0, nrow = 115, ncol = 2) # Saves Stationary distribution
MU <- matrix(0, nrow = 1115, ncol = 2) # Saves state-means
SD <- matrix(0, nrow = 1115, ncol = 2) # Saves standard deviations
Pr.1 <- rep(0, max(train$Store)) # Extracts Prs from Stat
Pr.2 <- rep(0, max(train$Store))

for ( i in 1 : max(train$Store) ) {
  trainstore[[i]] <- train[which(train$Store == i),] # Get store alone
  
  obs <- trainstore[[i]]$Sales[ trainstore[[i]]$Sales > 1 ] # Suppresses days of 0 sales, which resulted in singularities
  
  Pi <- matrix( # Initial transition
    rep(1/2, 4),
    byrow = 2, nrow = 2
  )
  
  delta <- c(1/2, 1/2) # Initial dist
  
  parameters <- list( # Parameters for models 1 and 2, respectively
    mean = as.numeric( quantile( trainstore[[i]]$Sales, c(0.25, 0.75 )) ),
    sd = 0.5*as.numeric( quantile( trainstore[[i]]$Sales, c(0.25, 0.75 )) )
  )
  
  x <- dthmm(obs, Pi, delta, "norm", parameters) # Creates Discrete Time HMM Object; observations obs, initial transition matrix Pi, initial dist delta, normal, and the parameters
  
  y <- BaumWelch(x, control = bwcontrol(maxiter = 500)) # Runs the forward-backward algorithm to extract parameters
  
  DELTA[i,] <- y$delta
  PI[[i]] <- matrix(y$Pi, byrow = T, nrow = 2)
  STAT[[i]] <- statdistr(y$Pi)
  Pr.1[i] <- STAT[[i,]][1]
  Pr.2[i] <- STAT[[i,]][2]
  MU[i,] <- y$pm$mean
  SD[i,] <- y$pm$sd
  
}
