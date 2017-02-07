# Optional step 2 for simulating hypothetical plant interaction networks
# Simulating SD for each element of the matrix
# Used for creating fake data I can pass to meta analysis

M.sd.all <- list()
for (i in 1:length(M.all)) {
  sd <- rgamma(n = s * s, rate = 1, shape = 0.2)
  M.sd <- matrix(data = sd, nrow = s, ncol = s)
  M.sd.all[[i]] <- M.sd
}

