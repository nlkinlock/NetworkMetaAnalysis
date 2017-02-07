# Indirect effect
# developed from Scotti et al 2007

# n step effect of species i on species j
# effect of species j on all species aside from species i (the direct effect) multiplied by each species's own effect on species i
# e.g. indirect effect of species b on species a (4 spp. total) = alpha(bc) * alpha (ca) + alpha(bd) * alpha(da)
# each element: sum is the total indirect effects acted upon species a by all other species in network

# use this data if needed
# M <- matrix(data = c(1, 2, 3, 2, 3, 1, 1, 2, 2, 1, 1, 2, 1, 2, 2, 1), nrow = 4, ncol = 4)

level1 <- c() #3 blank vectors
level2 <- c()
level3 <- c()
for (j in 1:ncol(M)) {  # outer loop is the species upon whom indirect effects are acting (species j)
  for (i in 1:nrow(M)) {  # center loop is the species indirect effects towards j
    if (i == j) {  # skip self loop of j on j
      next
    } else {
      for (h in 1:ncol(M)) { # inner loop is the effect of species i on all other species (each one is species h)
        if (h == i) {  # skip self loop of i on i
          next
        }
        if (h == j) {  # skip direct effect of i on j
          next
        } else {
           level1[h] <- M[i, h] * M[h, j]  # effect of j on h multiplied by h's effect on a
        }
      }
    }
    level2[i] <- sum(level1, na.rm = TRUE) # indirect effect of i on j
    level1 <- c()  # need to clear vectors each iteration because of NAs
  }
  level3[j] <- sum(level2, na.rm = TRUE) # indirect effect of all species on j
  level2 <- c()
}
level3

ind.eff <- level3
mean.ind.eff <- mean(ind.eff)
ratio.ind.eff <- length(which(ind.eff > 0)) / length(which(ind.eff < 0))






