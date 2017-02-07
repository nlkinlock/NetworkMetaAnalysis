# Measure relative intransitivity index of plant interaction networks

# turn observed network into a competitive outcomes matrix
s <- nrow(M)
M.u <- matrix(data = NA, nrow = s, ncol = s)
M.l <- matrix(data = NA, nrow = s, ncol = s)
for(i in 1:nrow(M.l)){
  for (j in 1:nrow(M.l)){
    if(i == j){  # skip intraspecific interactions
      next
    }
    if(i >= j){  # only work in the lower triangle
      next
    }
    if(M[i, j] < M[j, i]) {  # if row species outcompetes column species (more negative = better)
      M.l[j, i] <- 0  # column species wins
      M.u[i, j] <- 1  # row species wins
    }else{
      M.l[j, i] <- 1  # column species loses
      M.u[i, j] <- 0  # row species loses
    }
  }
}
M.u[is.na(M.u)] <- 0
M.l[is.na(M.l)] <- 0
M.t <- M.l + M.u  # combine upper and lower triangles
diag(M.t) <- NA
rank.obs <- rank(-rowSums(M.t, na.rm = TRUE), ties.method = "average")
var.obs <- var(rowSums(M.t, na.rm = TRUE))  # observed variance

# loops to get min and max variance are identical to the loop used in "simulate"
# min variance - set probability of a 1 in the upper diagonal to 0.5 (random)
M.t.min <- vector("list", 1000)
var.min.all <- vector(length = 1000)

for (h in 1:1000) {
  M.b <- matrix(data = NA, nrow = s, ncol = s)
  M.u <- ifelse(test = upper.tri(M.b), yes = apply(X = M.b, MARGIN = c(1, 2), FUN = function(M.b) rbinom(n = 1, size = 1, prob = 0.5)), no = NA)
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
  M.t <- M.l + M.u
  diag(M.t) <- NA
  M.t.min[[h]] <- M.t
  var.min.all[h] <- var(rowSums(M.t, na.rm = TRUE))
}

var.min <- min(var.min.all)


# max variance - set probability of a 1 in the upper triangular to 1
M.t.max <- vector("list", 1000)
var.max.all <- vector(length = 1000)

for (h in 1:1000) {
  M.b <- matrix(data = NA, nrow = s, ncol = s)
  M.u <- ifelse(test = upper.tri(M.b), yes = apply(X = M.b, MARGIN = c(1, 2), FUN = function(M.b) rbinom(n = 1, size = 1, prob = 1)), no = NA)
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
  M.t <- M.l + M.u
  diag(M.t) <- NA
  M.t.max[[h]] <- M.t
  var.max.all[h] <- var(rowSums(M.t, na.rm = TRUE))
}

var.max <- max(var.max.all)

# formula for relative intransitivity index from Laird and Schamp (2006)
r.intrans <- 1 - (var.obs - var.min)/(var.max - var.min)






