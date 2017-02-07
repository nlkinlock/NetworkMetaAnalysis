# Simulate hypothetical plant interaction networks

library(MASS)

# Inputs
s <- 5  # species richness
LL <- -1  # bounds for interaction coefficients (uniform distribution, lower and upper limit)
UL <- 1
a <- 0.5  # asymmetry
iter <- 10

M.all <- vector("list", iter)
M.t.all <- vector("list", iter)
counter <- seq(0.5, 1.0, length.out = iter)

for (h in 1:iter) {

# Create blank matrix
M.b <- matrix(data = NA, nrow = s, ncol = s)

# Create transitivity matrix with winners and losers of interaction (1 = winner, 0 = loser)

# Winners or losers in upper triangle using a random Bernoulli variate
# Probability of Bernouilli determines transitivity of matrix (winners in upper triangular) 
M.u <- ifelse(test = upper.tri(M.b), yes = apply(X = M.b, MARGIN = c(1, 2), FUN = function(M.b) rbinom(n = 1, size = 1, prob = counter[h])), no = NA)

# Fill in lower triangular, if upper triangular [i, j] = 1, lower triangular [j, i] = 0 (create winner/loser pairs)
# Diagonal is ignored (NA)
M.l <- matrix(data = NA, nrow = s, ncol = s)
for(i in 1:nrow(M.l)){
  for (j in 1:nrow(M.l)){
    if(i == j){
      next
    }
    if(i >= j){
      next
    }
    if(M.u[i, j] == 1) {
      M.l[j, i] <- 0
    }else{
      M.l[j, i] <- 1
    }
  }
}

M.u[is.na(M.u)] <- 0
M.l[is.na(M.l)] <- 0
M.t <- M.l + M.u  # Merge upper and lower triangular
diag(M.t) <- NA  # Set diagonal back to NA


# Create interaction matrix based on transitivity matrix

# Ignore diagonal
# Fill in winners (t[i, j] = 1) with a random uniform variate that repesents interaction strength
M <- matrix(data = NA, nrow = s, ncol = s)
for(i in 1:nrow(M)){
  for (j in 1:ncol(M)){
    if(i == j){
      next
    }
    if(M.t[i, j] == 1) {
      M[i, j] <- runif(n = 1, min = LL, max = UL)
    }else{
      next
    }
  }
}

# Ignore diagonal
# Fill in losers (t[i, j] = 0) with a random normal variate with a mean equal to some proportion of the winner's interaction strength
for(i in 1:nrow(M)){
  for (j in 1:ncol(M)){
    if(i == j){
      next
    }
    if(is.na(M[i, j])) {
    M[i, j] <- rnorm(n = 1, mean = a * M[j, i], sd = 0.1 * a * abs(M[j, i]))
    }else{
      next
    }
  }
}

diag(M) <- 1  # Set diagonal elements equal to 1

M.t.all[[h]] <- M.t
M.all[[h]] <- M
}
# visualize and compare transitivity matrix with interaction matrix
# M.t
# round(M, digits = 3)


# write all matrices to .txt files so that I can run louvain community detection algorithm in python
M.pos.all <- list()
M.neg.all <- list()
# each matrix must be split into positive and negative elements because louvain takes them as separate layers with different weights
for (a in 1:length(M.all)) {
  pos.ind <- which(M.all[[a]] > 0, arr.ind = TRUE)  # pull out positive indices for each matrix
  neg.ind <- which(M.all[[a]] <= 0, arr.ind = TRUE)  # pull out negative indices for each matrix
  # write positive matrices
  M.pos <- matrix(NA, nrow = nrow(M.all[[a]]), ncol = ncol(M.all[[a]]))  # blank
  M.pos[pos.ind] <- M.all[[a]][pos.ind]  # add in values
  zero <- which(is.na(M.pos), arr.ind = TRUE)  # fill missing values with zeros (igraph reads this as a missing edge)
  M.pos[zero] <- 0
  M.pos.all[[a]] <- M.pos  # save each matrix to a list
  write.matrix(M.pos, paste("./mat/pos", sprintf(fmt = "%02d", a), ".txt", sep = ""), sep = "\t")  #write to file
  #write negative matrices
  M.neg <- matrix(NA, nrow = nrow(M.all[[a]]), ncol = ncol(M.all[[a]]))
  M.neg[neg.ind] <- M.all[[a]][neg.ind]
  zero <- which(is.na(M.neg), arr.ind = TRUE)
  M.neg[zero] <- 0
  M.neg.all[[a]] <- M.neg
  write.matrix(M.neg, paste("./mat/neg", sprintf(fmt = "%02d", a), ".txt", sep = ""), sep = "\t")
}








