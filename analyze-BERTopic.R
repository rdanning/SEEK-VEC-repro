source("seekvec/ensemble.R")
library(tidyverse)
library(pheatmap)

# function to convert BERTTopic output to structure matrix
bt_to_hht <- function(i,bt){
  v <- bt %>% pull(i)
  df <- data.frame(v) %>%
    mutate(v = str_replace_all(v,"\\[|\\]|'","")) %>%
    separate(v, into = paste0("w",1:10),sep = ",") %>%
    mutate(topic = paste0("T",1:n())) %>%
    pivot_longer(starts_with("w")) %>%
    mutate(word = str_trim(value)) %>%
    select(topic,word) %>%
    mutate(dummy = 1) %>%
    distinct() %>%
    filter(word != "") %>%
    pivot_wider(names_from = "topic", values_from = "dummy", values_fill = 0) %>%
    column_to_rownames("word") %>%
    as.matrix()
  hht <- df %*% t(df)
  return(hht)
}

# function to select words for plotting
keep_top_words <- function(i,bt,n = 5){
  v <- bt %>% pull(i)
  df <- data.frame(v) %>%
    mutate(v = str_replace_all(v,"\\[|\\]|'","")) %>%
    separate(v, into = paste0("w",1:10),sep = ",") %>%
    mutate(topic = 1:n()) %>%
    filter(topic <= n) %>%
    mutate(topic = paste0("Topic ",topic)) %>%
    pivot_longer(starts_with("w")) %>%
    mutate(word = str_trim(value)) %>%
    select(topic,word) %>%
    filter(word != "")
  wc <- df %>% 
    group_by(word) %>%
    count() %>%
    filter(n == 1)
  df <- df %>%
    filter(word %in% wc$word) %>%
    column_to_rownames("word") %>%
    rename(Topic = topic)
  df$Topic <- factor(df$Topic, levels = paste0("Topic ",1:n))
  return(df)
}

# function to add in missing words
pad_hht <- function(hht, all.vocab){
  Y <- matrix(0, nrow = length(all.vocab), ncol = length(all.vocab), 
                dimnames= list(all.vocab, all.vocab))
  Y[rownames(hht), colnames(hht)] <- hht
  return(Y)
}

bt <- read_csv("topics-seed.csv")

# convert to Phi matrix
hhts <- lapply(1:ncol(bt), bt_to_hht, bt)

# unify vocabulary
all.vocab <- unique(unlist(lapply(hhts, rownames)))
hhts.padded <- lapply(hhts, pad_hht, all.vocab)
phis <- lapply(hhts.padded, function(x){x[all.vocab,all.vocab]})


# ensemble matrix
sv <- SE.eigenscore(phis)
sv.mat <- sv$O

# select top words and annotate for plotting
keep.df <- keep_top_words(5,bt,n=10)
keep.words <- intersect(rownames(keep.df),rownames(sv.mat))
plot.mat <- sv.mat[keep.words,keep.words]
diag(plot.mat) <- NA
pheatmap(plot.mat,
         annotation_row = keep.df,
         legend = F,
         cluster_rows = F,
         cluster_cols = F,
         color = colorRampPalette(c("white","navy"))(100),
         breaks = seq(0, 1, length.out = 100))
