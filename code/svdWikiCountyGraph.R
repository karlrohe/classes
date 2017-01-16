library(rARPACK)
library(choroplethr)
rm(list = ls())
load("data/wikiCountyGraph.RData")
str(A)
cs = colSums(A)
rs =rowSums(A)
A = A[, cs > 5 &  cs< (length(rs)/2)]
dim(A)
rs = rowSums(A)

L = Diagonal(length(rs), 1/sqrt(rs+2))%*%A%*% Diagonal(length(cs), 1/sqrt(cs+2))
?svds
s = svds(L,k = 100)
plot(s$d)
hist(s$d)

u = s$u[,1:100]
u = t(apply(u, 1, function(x) return(x/sqrt(sum(x^2) + 10^(-10)))))

df = cbind(as.numeric(rownames(A)), u[,45]) 
colnames(df) = c("region", "value")
df %>%  as.data.frame %>% county_choropleth()
