###### GIBBS SAMPLING #######
## Read in Data and Drop Duplicate Columns
setwd('/Users/saulforman/Desktop/project')


proj = data.frame(
  pa = rep(0, 3000),
  bip = rep(0, 3000),
  h = rep(0, 3000),
  x1b = rep(0, 3000),
  x2b = rep(0, 3000),
  x3b = rep(0, 3000),
  hr = rep(0, 3000),
  k = rep(0, 3000),
  bb = rep(0, 3000),
  attempt = rep(0, 3000),
  sb = rep(0, 3000)
)

library(dplyr)
join1 = read.csv('join1.csv')

join2 = read.csv('join2.csv')
join2 = select(join2, -c(PA, AVG, Team, Name))

join3 = read.csv('join3.csv')
join3 = select(join3, -c(Team, BABIP, Name))
join3 = rename(join3, FlyB=FB.)

join4 = read.csv('join4.csv')
join4 = select(join4, -c(Team, Name))
join4 = rename(join4, FastB=FB.)

join5 = read.csv('join5.csv')
join5 = select(join5, -c(Team, Name))

## Join All Metrics Together
data_full = join1 %>% left_join(join2, by = c('Season','playerid')) %>%
  left_join(join3, by = c('Season','playerid')) %>%
  left_join(join4, by = c('Season','playerid')) %>%
  left_join(join5, by = c('Season','playerid'))

## Use Output Data from 2013 and After
data_output = filter(data_full, Season >= 2013)

## Join Stats for T-1, T-2, and T-3 Seasons
data_join1 = data_full %>%
  mutate(Season = Season + 1)
colnames(data_join1)[c(4:23, 25:85)] = paste(colnames(data_join1)[c(4:23, 25:85)], 't1', sep = '_')
data_join1 = data_join1 %>%
  select(-c(Name, Team))

data_join2 = data_full %>%
  mutate(Season = Season + 2)
colnames(data_join2)[c(4:23, 25:85)] = paste(colnames(data_join2)[c(4:23, 25:85)], 't2', sep = '_')
data_join2 = data_join2 %>%
  select(-c(Name, Team))

data_join3 = data_full %>%
  mutate(Season = Season + 3)
colnames(data_join3)[c(4:23, 25:85)] = paste(colnames(data_join3)[c(4:23, 25:85)], 't3', sep = '_')
data_join3 = data_join3 %>%
  select(-c(Name, Team))

## Join T-1, T-2, and T-3 Data to Output Data
data_pred = data_output %>% left_join(data_join1, by = c('Season','playerid')) %>%
  left_join(data_join2, by = c('Season','playerid')) %>%
  left_join(data_join3, by = c('Season','playerid'))

## Eliminate Advanced Stats for Output Year
data_pred2 = data_pred[, -c(24:86)]

## AB >= 15 to eliminate extreme probabilities
data_pred2 = data_pred2 %>%
  filter(AB >= 15)

## Create Predictors Matrix
X = data_pred2[, c(24:265)]

## Turn Percentages into Factors
for(i in 1:length(X)) {
  if(class(X[,i]) == 'factor') {
    X[,i] = as.numeric(sub('%', '', as.character(X[,i])))/100
  }
}

## KNN Imputation
library(bnstruct)
X = as.matrix(X)
X_imputed = knn.impute(X, k = 10, cat.var = NA, to.impute = 1:nrow(X),
                       using = 1:nrow(X))

## PCA
X_pca = prcomp(X_imputed, center = TRUE, scale = TRUE)
summary(X_pca)

head(data_pred2)
## Data Pred 3
data_pred3 = cbind(data_pred2[,1:2], X_pca$x[,1:10])

# Gibbs Sampling
library(rjags)
library(runjags)
mod_string = " model {

  for (a in 1:length(PA)) {
    PA[a] ~ dpois(lam[a])
    log(lam[a]) = a1 + b1[1]*PC1[a] + b1[2]*PC2[a] + b1[3]*PC3[a] + 
      b1[4]*PC4[a] + b1[5]*PC5[a] + b1[6]*PC6[a] + b1[7]*PC7[a] +
      b1[8]*PC8[a] + b1[9]*PC9[a] + b1[10]*PC10[a]
  }
  a1 ~ dnorm(0.0, 1.0/1e6)
  for (b in 1:10) {
    b1[b] ~ ddexp(0.0, sqrt(2.0))
  }


  for (c in 1:length(BIP)) {
    BIP[c] ~ dbinom(p2[c], PA[c])
    logit(p2[c]) = a2 + b2[1]*PC1[c] + b2[2]*PC2[c] + b2[3]*PC3[c] + 
      b2[4]*PC4[c] + b2[5]*PC5[c] + b2[6]*PC6[c] + b2[7]*PC7[c] +
      b2[8]*PC8[c] + b2[9]*PC9[c] + b2[10]*PC10[c] + b2[11]*PA[c]
  }
  a2 ~ dnorm(0.0, 1.0/1e6)
  for (d in 1:11) {
    b2[d] ~ ddexp(0.0, sqrt(2.0))
  }


  for (e in 1:length(H)) {
    H[e] ~ dbinom(p3[e], BIP[e])
    logit(p3[e]) = a3 + b3[1]*PC1[e] + b3[2]*PC2[e] + b3[3]*PC3[e] + 
      b3[4]*PC4[e] + b3[5]*PC5[e] + b3[6]*PC6[e] + b3[7]*PC7[e] +
      b3[8]*PC8[e] + b3[9]*PC9[e] + b3[10]*PC10[e] + b3[11]*PA[e] +
      b3[12]*BIP[e]
  }
  a3 ~ dnorm(0.0, 1.0/1e6)
  for (f in 1:12) {
    b3[f] ~ ddexp(0.0, sqrt(2.0))
  }


  for (g in 1:length(x1B)) {
    x1B[g] ~ dbinom(p4[g], H[g])
    logit(p4[g]) = a4 + b4[1]*PC1[g] + b4[2]*PC2[g] + b4[3]*PC3[g] + 
      b4[4]*PC4[g] + b4[5]*PC5[g] + b4[6]*PC6[g] + b4[7]*PC7[g] +
      b4[8]*PC8[g] + b4[9]*PC9[g] + b4[10]*PC10[g] + b4[11]*PA[g] +
      b4[12]*H[g]
  }
  a4 ~ dnorm(0.0, 1.0/1e6)
  for (i in 1:12) {
    b4[i] ~ ddexp(0.0, sqrt(2.0))
  }

  for (h in 1:length(x2B)) {
    x2B[h] ~ dbinom(p5[h], H[h])
    logit(p5[h]) = a5 + b5[1]*PC1[h] + b5[2]*PC2[h] + b5[3]*PC3[h] + 
      b5[4]*PC4[h] + b5[5]*PC5[h] + b5[6]*PC6[h] + b5[7]*PC7[h] +
      b5[8]*PC8[h] + b5[9]*PC9[h] + b5[10]*PC10[h] + b5[11]*PA[h] +
      b5[12]*H[h]
  }
  a5 ~ dnorm(0.0, 1.0/1e6)
  for (u in 1:12) {
    b5[u] ~ ddexp(0.0, sqrt(2.0))
  }

  for (l in 1:length(K)) {
    K[l] ~ dbinom(p6[l], TTO[l])
    logit(p6[l]) = a6 + b6[1]*PC1[l] + b6[2]*PC2[l] + b6[3]*PC3[l] + 
      b6[4]*PC4[l] + b6[5]*PC5[l] + b6[6]*PC6[l] + b6[7]*PC7[l] +
      b6[8]*PC8[l] + b6[9]*PC9[l] + b6[10]*PC10[l] + b6[11]*PA[l] +
      b6[12]*BIP[l] + b6[13]*H[l]
  }
  a6 ~ dnorm(0.0, 1.0/1e6)
  for (m in 1:13) {
    b6[m] ~ ddexp(0.0, sqrt(2.0))
  }


  for (n in 1:length(HR)) {
    HR[n] ~ dbinom(p7[n], PA[n]-BIP[n])
    logit(p7[n]) = a7 + b7[1]*PC1[n] + b7[2]*PC2[n] + b7[3]*PC3[n] + 
      b7[4]*PC4[n] + b7[5]*PC5[n] + b7[6]*PC6[n] + b7[7]*PC7[n] +
      b7[8]*PC8[n] + b7[9]*PC9[n] + b7[10]*PC10[n] + b7[11]*PA[n] +
      b7[12]*BIP[n] + b7[13]*H[n] + b7[14]*K[n]
  }
  a7 ~ dnorm(0.0, 1.0/1e6)
  for (o in 1:14) {
    b7[o] ~ ddexp(0.0, sqrt(2.0))
  }

  
  for (r in 1:length(SB)) {
    SB[r] ~ dbinom(p8, Attempts[r])
  }
  p8 ~ dbeta(.5, .5)


  for (s in 1:length(Attempts)) {
    Attempts[s] ~ dpois(lam2[s])
    log(lam2[s]) = a8 + b8[1]*PC1[s] + b8[2]*PC2[s] + b8[3]*PC3[s] + 
      b8[4]*PC4[s] + b8[5]*PC5[s] + b8[6]*PC6[s] + b8[7]*PC7[s] +
      b8[8]*PC8[s] + b8[9]*PC9[s] + b8[10]*PC10[s]
  }
  a8 ~ dnorm(0.0, 1.0/1e6)
  for (t in 1:14) {
    b8[t] ~ ddexp(0.0, sqrt(2.0))
  }
} "

X_pca_m = as.matrix(X_pca$x[,1:10])

data_jags = list(PA=data_pred2$PA,
                 BIP=(data_pred2$PA-(data_pred2$SO + data_pred2$BB + data_pred2$HR)),
                 H=(data_pred2$H - data_pred2$HR),
                 TTO=(data_pred2$SO + data_pred2$BB + data_pred2$HR),
                 x1B=(data_pred2$H-(data_pred2$HR + data_pred2$X2B + data_pred2$X3B)),
                 x2B=data_pred2$X2B,
                 HR=data_pred2$HR,
                 K=data_pred2$SO,
                 SB=data_pred2$SB,
                 Attempts=(data_pred2$SB + data_pred2$CS),
                 PC1=X_pca_m[,1],
                 PC2=X_pca_m[,2],
                 PC3=X_pca_m[,3],
                 PC4=X_pca_m[,4],
                 PC5=X_pca_m[,5],
                 PC6=X_pca_m[,6],
                 PC7=X_pca_m[,7],
                 PC8=X_pca_m[,8],
                 PC9=X_pca_m[,9],
                 PC10=X_pca_m[,10])


params = c("a1","b1",
           "a2","b2","p2",
           "a3","b3","p3",
           "a4","b4","p4",
           "a5","b5","p5",
           "a6","b6","p6",
           "a7","b7","p7",
           "a8","b8","p8")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

rm("data_full", "data_join1", "data_join2", "data_join3", "data_output",
   "data_pred", "join1", "join2", "join3", "join4", "join5", "X",
   "X_imputed")

update(mod, 5e2)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=1e3)

mod_csim = as.mcmc(do.call(rbind, mod_sim))

par(mar=c(2,2,2,2))
plot(mod_sim, ask = TRUE)

summary(mod_sim)
data_pred3 = data_pred3[which(data_pred3$Season == 2018),]

save(mod_csim, file = "mod_csim.rda")
write.csv(data_pred3, file = "data_pred.csv")
write.csv(data_pred2, file = "data_pred2.csv")
