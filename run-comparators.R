library(textmineR)
library(topicmodels)
library(tidytext)
library(fastTopics)
library(tidyverse)

# M: transpose of corpus matrix
# k: number of topics
# p: vocabulary size

get.A.lda <- function(M,k,p){
  print(paste("lda",k))
  lda.fit <- LDA(M,k=k)
  lda.topics <- tidy(lda.fit, matrix = "beta")
  lda.topics$term <- rep(1:p,each=k)
  A.lda <- lda.topics %>%
    pivot_wider(names_from = topic, values_from = beta) %>%
    select(-term)
  return(as.matrix(A.lda))
}

get.A.ctm <- function(M,k,p){
  print(paste("ctm",k))
  ctm.fit <- CTM(M,k=k)
  ctm.topics <- tidy(ctm.fit, matrix = "beta")
  ctm.topics$term <- rep(1:p,each=k)
  A.ctm <- ctm.topics %>%
    pivot_wider(names_from = topic, values_from = beta) %>%
    select(-term)
  return(as.matrix(A.ctm))
}

get.A.nmf <- function(M,k){
  nmf.fit <- fit_topic_model(M,k = k)
  A.nmf <- nmf.fit$F
  return(A.nmf)
}


