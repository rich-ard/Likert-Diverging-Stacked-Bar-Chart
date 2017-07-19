data1 <- read.csv('sample likert test data.csv')

data1 <- data1[,-1]

library(HH)

# Likert levels: low, high

low <- 1

high <- 5

# Coerce all columns to levels
data2 <- data.frame(lapply(data1, factor))

# Warn user if levels are not consistent across columns.
# It's possible that one might have a five-point scale but only record
# values from 1:4, but the user should make sure that the same scale
# was used across the question set.
for (i in ncol(data2)){
  if (nlevels(data2[,1])!=nlevels(data2[,i])){
    warning("Likert values are not consistent - confirm that identical
            scales were used across questions.")
  }
}

# Create Likert collection df
data3 <- data.frame(matrix(NA, ncol = high-low+1))

for (i in (1:ncol(data2))){
  for (j in (1:ncol(data3))){
    data3[i,j]<-sum(data2[,i]==j, na.rm=TRUE)
  }
}

rownames(data3)<-colnames(data2)

mycolor<-c("#006AB2", "#83C0E1", "#E3E3E3", "#f4a582", "#C7021D")

plot.likert(data3, col=mycolor, reference.line.col="black")