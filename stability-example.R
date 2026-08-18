library(tidyverse)
library(pheatmap)
library(cowplot)
library(seekvec)



get.keep.idx <- function(K,HHT.hats){
  TWT <- HHT.hats[[K-2]]
  order <- hclust(dist(TWT))$order
  keep.idx <- intersect(order,which(diag(TWT) > 0))
  return(keep.idx)
}

inflate.A.row <- function(i,A,scale.factor,inflate.sample){
  row <- A[i,]
  n.idx <- inflate.sample
  inflate.idx <- sample(1:ncol(A),n.idx)
  inflation <- rep(1,ncol(A))
  inflation[inflate.idx] <- scale.factor
  inflated.row <- row*inflation
  return(inflated.row)
}

get.A.imu <- function(p,K,stopwords,scale.factor, do.inflate = TRUE, inflate.sample = 1){
  probs <- (1:p + stopwords)^-1.07
  A <- matrix(rep(probs,each=K),ncol=K,byrow = TRUE)
  if(do.inflate){
    A.scaled <- t(sapply(1:nrow(A),inflate.A.row,A,scale.factor,inflate.sample))
    return(A.scaled)
  } else{return(A)}
}

get.A.miu <- function(p,K,i.pct,m.pct,scale.factor,stopwords){
  
  i.n <- round(p*i.pct)
  m.n <- round(p*m.pct)
  u.n <- p-i.n-m.n
  
  A.i <- get.A.imu(i.n,K,stopwords,scale.factor)
  A.m <- get.A.imu(m.n,K,stopwords,scale.factor,inflate.sample = 2)
  A.u <- get.A.imu(u.n,K,stopwords,do.inflate=FALSE)
  A.all <- rbind(A.i,A.m,A.u)
  A <- prop.table(A.all,2)
  
  return(A)
  
}

get.W <- function(K,n){
  W <- matrix(runif(n*K), K, n)
  return(prop.table(W,2))
}

get.D.vec <- function(i,D0,N){
  col <- D0[,i]
  new.col <- rmultinom(1,N,col)
  return(new.col)
}

make.stability.plot <- function(O,keep.idx){
  heatmap.m <- O[keep.idx,keep.idx]
  pm <- pheatmap::pheatmap(heatmap.m,
                           color = colorRampPalette(c("white", "navy"))(50),
                           breaks = seq(0,1,length.out = 50),
                           treeheight_row = 0,
                           treeheight_col = 0,
                           show_rownames = FALSE,
                           show_colnames = FALSE,
                           cluster_rows = FALSE,
                           cluster_cols = FALSE,
                           fs = 8,
                           border_color = NA,
                           legend = FALSE)
  
  return(pm[[4]])
}

set.seed(18)

# toy data
A <- get.A.miu(p=500,K=6,i.pct = 0.1, m.pct = 0.2, scale.factor = 10, stopwords = 50)
W <- get.W(K=6,n=1000)
D0 <- A %*% W
ND <- sapply(1:ncol(D0),get.D.vec,D0,N=5000)

# run SEEK-VEC
SV <- run_SEEK(ND,3:12,threshold = 10)
HHT.hats <- SV$hs.matrices


# analyze stability
idx3 <- get.keep.idx(3,HHT.hats)
idx4 <- get.keep.idx(4,HHT.hats)
idx5 <- get.keep.idx(5,HHT.hats)
idx6 <- get.keep.idx(6,HHT.hats)
idx7 <- get.keep.idx(7,HHT.hats)
idx8 <- get.keep.idx(8,HHT.hats)

O <- SV$O
p3 <- make.stability.plot(O,idx3)
p4 <- make.stability.plot(O,idx4)
p5 <- make.stability.plot(O,idx5)
p6 <- make.stability.plot(O,idx6)
p7 <- make.stability.plot(O,idx7)
p8 <- make.stability.plot(O,idx8)



plot_grid(p3,p4,p5,p6,p7,p8, nrow = 2, labels = paste0("K = ",3:8), label_x = 0.8, label_y = 0.98) +
  theme(
    panel.grid.major = element_blank(), 
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = "grey92", colour = NA),
    plot.background = element_rect(fill = "grey92", colour = NA),
    axis.line = element_line(colour = "black")
  )


          