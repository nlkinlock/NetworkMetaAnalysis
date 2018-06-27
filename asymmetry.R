# Measure asymmetry of interactions in plant interaction networks

# is the effect of species i on species j equal to the effect of species j on species i (symmetric)
# or is the effect of species i on species j much greater than the effect of species j on species i (asymmetric) 

M.r <- M
index <- which(upper.tri(M.r))
flat <- c(M.r)  # flatten matrix and its transpose, this gives w(ij) and w(ji) in order (ignoring diagonal)
flat.t <- c(t(M.r))
w.ij <- flat[index]
w.ji <- flat.t[index]
asymm.diff <- mean(abs(w.ij - w.ji), na.rm = TRUE)




