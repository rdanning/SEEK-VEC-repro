source("./seekvec/main.R")
set.seed(78)


# results from paper
load("./MADStaText/1-Text abstracts/TextCorpusFinal.RData")
load("./MADStaText/3-Topic modeling results/TopicResults.RData")
D.all <- as.matrix(TDM)
terms <- dimnames(D.all)$Terms
D <- D.all[,paperInfo$inCorpus == 2]
vocab <- dictionary

# SEEK-VEC comparison
A.hats <- sapply(4:16, get.topic.matrix, D, "ts")
B.hats <- lapply(topic.matrices, prop.table, 1)

# t = 0.2
Hs.0.2 <- lapply(B.hats, get.hallmark.matrix, 0.2)
HHTs.0.2 <- lapply(Hs.0.2, XXT)
SV.0.2 <- SE.eigenscore(HHTs.0.2)
O.0.2 <- SV.0.2$O

# n = 20
H.hats.20 <- lapply(B.hats, get.hallmark.matrix, 20)
HHTs.20 <- lapply(Hs.20, XXT)
SV.20 <- SE.eigenscore(HHTs.20)
O.20 <- SV.20$O
