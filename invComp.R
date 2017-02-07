#
# LOAD DATA ------------------------------------------------------------
#
# load coding table with extracted data from each case
df.init <- read.csv("CodingTable.csv")
col.remove <- c("Title", "doi", "Abstract", "Status", "OtherTreatments", "Metric", "Variation", "Variation.1")  # unwanted columns
df <- df.init[df.init$Status == "Complete" & df.init$RII == 1, !(names(df.init) %in% col.remove)]  # only completed cases with RII

# set up list with Unique ID of case and all species
spp.list <- setNames(split(as.character(df$Species), seq(nrow(df))), df$UniqueID)  # split df into list where each element is string with species
spp.list <- lapply(spp.list, function(x) strsplit(x, ", "))  # split string into vector
spp.list <- lapply(spp.list, unlist)
inv.list <- setNames(split(as.character(df$Invasive), seq(nrow(df))), df$UniqueID)  # split df into list where each element is string with invasives
inv.list <- lapply(inv.list, function(x) strsplit(x, ", "))  # split string into vector
inv.list <- lapply(inv.list, function(x) as.numeric(unlist(x)))

# loop to create a list with the unique case ID and the invasive status of each species from each case in each element
inv.output <- list()
for (i in 1:length(spp.list)) {
  if (all(inv.list[[i]] == 0)) {
    uniqueid <- rep(as.integer(names(spp.list)[i]), length(spp.list[[i]]))
    invasive <- rep(0, length(spp.list[[i]]))
    temp <- data.frame(UniqueID = uniqueid, Invasive = invasive)
    inv.output[[i]] <- temp
  } else {
    uniqueid <- rep(as.integer(names(spp.list)[i]), length(spp.list[[i]]))
    invasive <- integer(length = length(spp.list[[i]]))
    invasive[inv.list[[i]]] <- 1
    invasive[!inv.list[[i]]] <- 0
    temp <- data.frame(UniqueID = uniqueid, Invasive = invasive)
    inv.output[[i]] <- temp
  }
}

# load respective networks for each case in both MixMono and Cntrl
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/MixMono")
files.init <- dir(pattern = "*.csv", full.names = TRUE)
files <- grep(pattern = "RIIsd", files.init, value = TRUE, invert = TRUE)
M.inv <- lapply(files, function(x) read.csv(x, header = FALSE))
names(M.inv) <- files
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis/Networks/Complete/RII/Cntrl")
files.init2 <- dir(pattern = "*.csv", full.names = TRUE)
files2 <- grep(pattern = "RIIsd", files.init2, value = TRUE, invert = TRUE)
M.inv.2 <- lapply(files2, function(x) read.csv(x, header = FALSE))
names(M.inv.2) <- files2
M.inv <- append(M.inv, M.inv.2)
files.tot <- c(files, files2)
file.idx <- c()
# loop to match case number with invasive status df/list
for (z in 1:length(files.tot)) {
  if (substring(files.tot[z], 6, 6) == "-") {
    file.idx[z] <- as.integer(substring(files.tot[z], 3, 5))
  } else if (substring(files.tot[z], 4, 4) == "-") {
    file.idx[z] <- as.integer(substring(files.tot[z], 3, 3))
  } else {
    file.idx[z] <- as.integer(substring(files.tot[z], 3, 6))
  }
}
# order networks in the same way as inv.output
M.inv <- M.inv[order(file.idx)]
for (r in 1:length(M.inv)) {
  M.inv[[r]][which(is.na(M.inv[[r]]), arr.ind = TRUE)] <- 0
}

#
# EXTRACT COMP. IN/OUT STRENGTHS ------------------------------------------------------------
#
# run through all networks to get competitive in and out strength for all invasive and native species
setwd("/Users/nicolekinlock/Documents/Plant Ecology/NetworkMetaAnalysis")
s.out.store.inv <- c()
s.out.store.nat <- c()
s.in.c.store.inv <- c()
s.in.c.store.nat <- c()
s.out.c.store.inv <- c()
s.out.c.store.nat <- c()
for (m in 1:length(M.inv)) {
  # is species in position x invasive or native
  status <- inv.output[[m]]$Invasive
  M <- t(M.inv[[m]])  # transpose to fit network convention
  # node characteristics (competitive in/out strength)
  s.out <- rowSums(M) / nrow(M)
  M.c <- ifelse(test = M > 0, yes = 0, no = M)
  s.out.c <- rowSums(M.c, na.rm = TRUE) / nrow(M)
  s.in.c <- colSums(M.c, na.rm = TRUE) / nrow(M)
  s.out.store.inv <- c(s.out.store.inv, s.out[status == 1])
  s.out.store.nat <- c(s.out.store.nat, s.out[status == 0])
  s.in.c.store.inv <- c(s.in.c.store.inv, s.in.c[status == 1])
  s.in.c.store.nat <- c(s.in.c.store.nat, s.in.c[status == 0])
  s.out.c.store.inv <- c(s.out.c.store.inv, s.out.c[status == 1])
  s.out.c.store.nat <- c(s.out.c.store.nat, s.out.c[status == 0])
}

#
# PERMUTATION TEST ------------------------------------------------------------
#
# permutation test is a nonparametric t test
# set number of iterations
iterations <- 10000
# permute native invasive status among all species, preserving number of invasive and natives
# calculate t value in means between "invasives" and "natives" in each iterations and store
# repeat test for out strength, competitive out strength, and competitive in strength
out.strength <- data.frame(Status = c(rep(1, length = length(s.out.store.inv)), rep(0, length = length(s.out.store.nat))), OutStrength = c(s.out.store.inv, s.out.store.nat))
t.out <- c()
for (k in 1:iterations) {
  inv.idx <- sample(out.strength$Status, size = length(out.strength$Status), replace = FALSE)
  t.out[k] <- t.test(out.strength$OutStrength[inv.idx == 1], out.strength$OutStrength[inv.idx == 0])$statistic
}

c.in.strength <- data.frame(Status = c(rep(1, length = length(s.in.c.store.inv)), rep(0, length = length(s.in.c.store.nat))), InStrength = c(s.in.c.store.inv, s.in.c.store.nat))
t.c.in <- c()
for (k in 1:iterations) {
  inv.idx <- sample(in.strength$Status, size = length(in.strength$Status), replace = FALSE)
  t.c.in[k] <- t.test(in.strength$InStrength[inv.idx == 1], in.strength$InStrength[inv.idx == 0])$statistic
}

c.out.strength <- data.frame(Status = c(rep(1, length = length(s.out.c.store.inv)), rep(0, length = length(s.out.c.store.nat))), OutStrength = c(s.out.c.store.inv, s.out.c.store.nat))
t.c.out <- c()
for (k in 1:iterations) {
  inv.idx <- sample(c.out.strength$Status, size = length(c.out.strength$Status), replace = FALSE)
  t.c.out[k] <- t.test(c.out.strength$OutStrength[inv.idx == 1], c.out.strength$OutStrength[inv.idx == 0])$statistic
}

# two tailed t test on actual data (not permuted)
# p value by comparing distribution from permutation test to true values
t.out.true <- t.test(out.strength$OutStrength[out.strength$Status == 1], out.strength$OutStrength[out.strength$Status == 0])$statistic
p.out <- (length(which(t.out > abs(t.out.true))) + length(which(t.out < (-abs(t.out.true))))) / iterations

t.c.in.true <- t.test(c.in.strength$InStrength[c.in.strength$Status == 1], c.in.strength$InStrength[c.in.strength$Status == 0])$statistic
p.c.in <- (length(which(t.c.in > abs(t.c.in.true))) + length(which(t.c.in < (-abs(t.c.in.true))))) / iterations

t.c.out.true <- t.test(c.out.strength$OutStrength[c.out.strength$Status == 1], c.out.strength$OutStrength[c.out.strength$Status == 0])$statistic
p.c.out <- (length(which(t.c.out > abs(t.c.out.true))) + length(which(t.c.out < (-abs(t.c.out.true))))) / iterations


