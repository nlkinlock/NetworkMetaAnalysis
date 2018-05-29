# Measure relative intransitivity index of plant interaction networks

# turn observed network into a competitive outcomes matrix
s <- nrow(M)
M.u <- matrix(data = NA, nrow = s, ncol = s)
M.l <- matrix(data = NA, nrow = s, ncol = s)
for (i in 1:(nrow(M.l) - 1)) {
  for (j in (i + 1):nrow(M.l)) {
    if (is.na(M[i, j]) | is.na(M[j, i])) {
      M.l[i, j] <- NA
      M.u[j, i] <- NA
    } else if (M[i, j] < M[j, i]) {  # if row species outcompetes column species (more negative = better)
      M.l[j, i] <- 0  # column species wins
      M.u[i, j] <- 1  # row species wins
    } else {
      M.l[j, i] <- 1  # column species loses
      M.u[i, j] <- 0  # row species loses
    }
  }
}
# combine upper and lower triangles
M.t <- matrix(NA, s, s)
M.t[which(!is.na(M.l), arr.ind = TRUE)] <- M.l[which(!is.na(M.l), arr.ind = TRUE)]
M.t[which(!is.na(M.u), arr.ind = TRUE)] <- M.u[which(!is.na(M.u), arr.ind = TRUE)]
M.t.eff <- M.t[rowSums(is.na(M.t)) != ncol(M.t),]  # remove rows with all NAs
var.obs <- var(rowSums(M.t.eff, na.rm = TRUE))  # observed variance

# min and max variance for matrices of size s by s
# use effective number of species (number of pairs)
obs.mixed.pairs <- sum(!is.na(M.t[upper.tri(M.t)]))
# length of upper triangular for a matrix of a given size, find matrix that best matches data (with missing elements removed)
mixed.pairs <- c()
for (s in 1:10) {
  mixed.pairs[s] <- (s^2 - s) / 2
}
s.obs <- which.min(abs(mixed.pairs - obs.mixed.pairs))

# minimum and maximum possible variances for a matrix of this size (from transitivityinputs.R)
var.min <- var.min.s[s.obs]
var.max <- var.max.s[s.obs]

# formula for relative intransitivity index from Laird and Schamp (2006)
r.intrans <- 1 - ((var.obs - var.min) / (var.max - var.min))




# 
# n <- nrow(M)
# 
# 
# M.h <- matrix(data = NA, nrow = n, ncol = n)
# M.h[upper.tri(M.h)] <- 1
# M.h[lower.tri(M.h)] <- 0
# diag(M.h) <- 0
# M.h.temp <- matrix(data = NA, nrow = n, ncol = n)
# M.h.test <- matrix(data = NA, nrow = n, ncol = n)
# 
# 
# 
# permutations <- function(n){
#   if(n==1){
#     return(matrix(1))
#   } else {
#     sp <- permutations(n-1)
#     p <- nrow(sp)
#     A <- matrix(nrow=n*p,ncol=n)
#     for(i in 1:n){
#       A[(i-1)*p+1:p,] <- cbind(i,sp+(sp>=i))
#     }
#     return(A)
#   }
# }
# s <- 0
# cand.s <- c()
# cand.s2 <- c()
# all.perm <- permutations((n * (n - 1)) / 2)
# for(i in 1:nrow(all.perm)) {
#   M.temp <- M.h[all.perm[i, ], ]
#   M.test <- M.h[, all.perm[i, ]]
#   cand.s[i] <- sum(M.temp[upper.tri(M.temp)] != M.t[upper.tri(M.t)])
#   cand.s2[i] <- sum(M.temp[upper.tri(M.test)] != M.t[upper.tri(M.t)])
#   if(i == 1) {
#     s <- cand.s[i]
#   } else if(cand.s[i] < s) {
#     s <- cand.s[i]
#   } else if(cand.s2[i] < s) {
#     s <- cand.s2[i]
#   }
# }
# 
# 
# l <- rep(list(0:1), (n * (n - 1)) / 2)
# expand.grid()
# 
# tournaments <- list(1 = 0, 2 = 0, 3 = matrix(c(0, 0, 0, 1, 0, 1), ncol = 3, byrow = TRUE))
# 
# tab <- unname(read.table(file = "/Users/nicolekinlock/Downloads/tourn4.txt", colClasses = "character"))
# tab2 <- lapply(tab, function(x) strsplit(x, split = ""))
# tab3 <- lapply(tab2[[1]], function(x) as.numeric(x))
# tournaments <- data.frame(matrix(unlist(tab3), nrow = 4, byrow = TRUE))
# 
# 
# 
# s <- 0
# cand.s <- c()
# cand.s2 <- c()
# s.meta <- c()
# all.perm <- permutations(n)
# for(t in 1:nrow(tournaments)) {
#   for(i in 1:nrow(all.perm)) {
#     M.temp <- M.h[all.perm[i, ], ]
#     M.test <- M.h[, all.perm[i, ]]
#     cand.s[i] <- sum(M.temp[upper.tri(M.temp)] != tournaments[t, ])
#     cand.s2[i] <- sum(M.temp[upper.tri(M.test)] != tournaments[t, ])
#     if(i == 1) {
#       s <- cand.s[i]
#     } else if(cand.s[i] < s) {
#       s <- cand.s[i]
#     } else if(cand.s2[i] < s) {
#       s <- cand.s2[i]
#     }
#   }
#   s.meta[t] <- s 
# }



