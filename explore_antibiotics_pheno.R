#load data 
library(dplyr)
lf <- list.files("E:/pheno_combined", ".pheno", full.names = T)
drug <- stringr::str_match(lf, "\\/([a-zA-Z-_]+)\\.pheno")[ , 2]
drug <- sub("Para-aminosalisylic_acid", "PAA", drug)

data <- read.table(lf[1], stringsAsFactors = F, header = F)
colnames(data) <- c("id", drug[1])
for (i in 2:length(lf)) {
  d <- read.table(lf[i], stringsAsFactors = F, header = F)
  colnames(d) <- c("id", drug[i])
  data <- merge(data, d, by = "id", all=T)
  
}

pMiss <- function(x){sum(is.na(x))/length(x)*100}
apply(data[ , -1],2,pMiss)
s <- apply(data[ , -1],1,pMiss)
hist(s, breaks = 30)

df <- data[ , -1]
df[sapply(df, is.integer)] <- lapply(df[sapply(df, is.integer)], 
                                       as.factor)
require(mice)
imp <- mice(df, seed = 1233, method = "logreg") 

# logistic regression for binary variable 
#Generates multiple imputations for incomplete multivariate data by Gibbs sampling

complete.df <- complete(imp)
mdf <- data.matrix(complete.df)
cor(mdf)
matrix <- cbind(data[1],mdf)

library(VIM)
mice_plot <- aggr(df, col=c('navyblue','yellow'),
                  numbers=TRUE, sortVars=TRUE,
                  labels=names(df), cex.axis=.7,
                  gap=3, ylab=c("Missing data","Pattern"))

summary(aggr(df))
data.table::fwrite(matrix, "E:/R_project/MPTI/imp_matrix.txt", col.names = T, sep = " ")
