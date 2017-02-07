# Measure asymmetry of interactions in plant interaction networks

# is the effect of species i on species j equal to the effect of species j on species i (symmetric)
# or is the effect of species i on species j much greater than the effect of species j on species i (asymmetric) 

index <- which(upper.tri(M))
flat <- c(M)  # flatten matrix and its transpose, this gives w(ij) and w(ji) in order (ignoring diagonal)
flat.t <- c(t(M))
w.ij <- flat[index]
w.ji <- flat.t[index]
r <- cor(w.ij, w.ji)  # correlation coefficient





