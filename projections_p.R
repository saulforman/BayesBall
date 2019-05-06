setwd('/Users/saulforman/Desktop/project/BayesBall')
library(dplyr)
join1 = read.csv('pitch1.csv')

join2 = read.csv('pitch2.csv')
join2 = select(join2, -c(Team, Name)) %>%
  select(-ERA)

join3 = read.csv('pitch3.csv')
join3 = select(join3, -c(Team, Name)) %>%
  rename(FlyB = FB.) %>%
  select(-BABIP)

join4 = read.csv('pitch4.csv')
join4 = select(join4, -c(Team, Name))

join5 = read.csv('pitch5.csv')
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
colnames(data_join1)[c(4:25, 27:90)] = paste(colnames(data_join1)[c(4:25, 27:90)], 't1', sep = '_')
data_join1 = data_join1 %>%
  select(-c(Name, Team))

data_join2 = data_full %>%
  mutate(Season = Season + 2)
colnames(data_join2)[c(4:25, 27:90)] = paste(colnames(data_join2)[c(4:25, 27:90)], 't2', sep = '_')
data_join2 = data_join2 %>%
  select(-c(Name, Team))

data_join3 = data_full %>%
  mutate(Season = Season + 3)
colnames(data_join3)[c(4:25, 27:90)] = paste(colnames(data_join3)[c(4:25, 27:90)], 't3', sep = '_')
data_join3 = data_join3 %>%
  select(-c(Name, Team))

## Join T-1, T-2, and T-3 Data to Output Data
data_pred = data_output %>% left_join(data_join1, by = c('Season','playerid')) %>%
  left_join(data_join2, by = c('Season','playerid')) %>%
  left_join(data_join3, by = c('Season','playerid'))

## Eliminate Advanced Stats for Output Year
data_pred2 = data_pred[, -c(26:47,50:90)]

## AB >= 15 to eliminate extreme probabilities
data_pred2 = data_pred2 %>%
  filter(TBF >= 30)

## Create Predictors Matrix
X = data_pred2[, c(26:283)]

## Turn Percentages into Factors
for(i in 1:length(X)) {
  if(class(X[,i]) == 'factor') {
    X[,i] = as.numeric(sub('%', '', as.character(X[,i])))/100
  }
}

data_pred2$Name = as.character(data_pred2$Name)
data_pred2$Team = as.character(data_pred2$Team)

for(i in 1:length(data_pred2)) {
  if(class(data_pred2[,i]) == 'factor') {
    data_pred2[,i] = as.numeric(sub('%', '', as.character(data_pred2[,i])))/100
  }
}


library(stringr)
data_pred2$IP = as.character(data_pred2$IP)
data_pred2$IP = str_replace(data_pred2$IP, '0.1', '0.3333')
data_pred2$IP = str_replace(data_pred2$IP, '0.2', '0.6666')
data_pred2$IP = as.numeric(data_pred2$IP)
data_pred2$Outs = data_pred2$TBF-data_pred2$H-data_pred2$BB-data_pred2$HBP-data_pred2$IBB
write.csv(data_pred2, 'data_pred2_pitch.csv')

## KNN Imputation
library(bnstruct)
library(missMDA)
X = as.matrix(X)
X_impute = imputePCA(X)
X_imp = as.matrix(X_impute$completeObs)

## PCA
X_pca = prcomp(X_imp, center = TRUE, scale = TRUE)
summary(X_pca)

## Data Pred 3
which(data_pred3_p$Name == 'A.J. Cole')
data_pred3 = cbind(data_pred2[,1:2], X_pca$x[,1:10])


# Gibbs Sampling
library(rjags)
library(runjags)
mod_string = " model {

  for (a in 1:length(TBF)) {
  TBF[a] ~ dpois(lam[a])
  log(lam[a]) = a1 + b1[1]*PC1[a] + b1[2]*PC2[a] + b1[3]*PC3[a] + 
    b1[4]*PC4[a] + b1[5]*PC5[a] + b1[6]*PC6[a] + b1[7]*PC7[a] +
    b1[8]*PC8[a] + b1[9]*PC9[a] + b1[10]*PC10[a]
  }
  a1 ~ dnorm(0.0, 1.0/1e6)
  for (b in 1:10) {
    b1[b] ~ ddexp(0.0, sqrt(2.0))
  }
  
  
  for (c in 1:length(BIP)) {
  BIP[c] ~ dbinom(p2[c], TBF[c])
  logit(p2[c]) = a2 + b2[1]*PC1[c] + b2[2]*PC2[c] + b2[3]*PC3[c] + 
    b2[4]*PC4[c] + b2[5]*PC5[c] + b2[6]*PC6[c] + b2[7]*PC7[c] +
    b2[8]*PC8[c] + b2[9]*PC9[c] + b2[10]*PC10[c] + b2[11]*TBF[c]
  }
  a2 ~ dnorm(0.0, 1.0/1e6)
  for (d in 1:11) {
    b2[d] ~ ddexp(0.0, sqrt(2.0))
  }
  
  
  for (e in 1:length(H)) {
  H[e] ~ dbinom(p3[e], BIP[e])
  logit(p3[e]) = a3 + b3[1]*PC1[e] + b3[2]*PC2[e] + b3[3]*PC3[e] + 
    b3[4]*PC4[e] + b3[5]*PC5[e] + b3[6]*PC6[e] + b3[7]*PC7[e] +
    b3[8]*PC8[e] + b3[9]*PC9[e] + b3[10]*PC10[e] + b3[11]*BIP[e]
  }
  a3 ~ dnorm(0.0, 1.0/1e6)
  for (f in 1:11) {
    b3[f] ~ ddexp(0.0, sqrt(2.0))
  }
  
  for (l in 1:length(K)) {
  K[l] ~ dbinom(p6[l], TTO[l])
  logit(p6[l]) = a6 + b6[1]*PC1[l] + b6[2]*PC2[l] + b6[3]*PC3[l] + 
    b6[4]*PC4[l] + b6[5]*PC5[l] + b6[6]*PC6[l] + b6[7]*PC7[l] +
    b6[8]*PC8[l] + b6[9]*PC9[l] + b6[10]*PC10[l] + b6[11]*TBF[l] +
    b6[12]*BIP[l]
  }
  a6 ~ dnorm(0.0, 1.0/1e6)
  for (m in 1:12) {
    b6[m] ~ ddexp(0.0, sqrt(2.0))
  }
  
  
  for (n in 1:length(HR)) {
  HR[n] ~ dbinom(p7[n], TBF[n]-BIP[n])
  logit(p7[n]) = a7 + b7[1]*PC1[n] + b7[2]*PC2[n] + b7[3]*PC3[n] + 
    b7[4]*PC4[n] + b7[5]*PC5[n] + b7[6]*PC6[n] + b7[7]*PC7[n] +
    b7[8]*PC8[n] + b7[9]*PC9[n] + b7[10]*PC10[n] + b7[11]*TBF[n] +
    b7[12]*BIP[n]
  }
  a7 ~ dnorm(0.0, 1.0/1e6)
  for (o in 1:12) {
    b7[o] ~ ddexp(0.0, sqrt(2.0))
  }


  for (u in 1:length(IFFB)) {
    IFFB[u] ~ dbinom(p9[u], BIP[u])
    logit(p9[u]) = a9 + b9[1]*PC1[u] + b9[2]*PC2[u] + b9[3]*PC3[u] + 
      b9[4]*PC4[u] + b9[5]*PC5[u] + b9[6]*PC6[u] + b9[7]*PC7[u] +
      b9[8]*PC8[u] + b9[9]*PC9[u] + b9[10]*PC10[u] + b9[11]*TBF[u] +
      b9[12]*BIP[u] + b9[13]*K[u] + b9[14]*HR[u]
  }
  a9 ~ dnorm(0.0, 1.0/1e6)
  for (v in 1:14) {
    b9[v] ~ ddexp(0.0, sqrt(2.0))
  }


  for (w in 1:length(Outs)) {
    Outs[w] ~ dbinom(p11[w], TBF[w])
    logit(p11[w]) = a10 + b10[1]*PC1[w] + b10[2]*PC2[w] + b10[3]*PC3[w] + 
      b10[4]*PC4[w] + b10[5]*PC5[w] + b10[6]*PC6[w] + b10[7]*PC7[w] +
      b10[8]*PC8[w] + b10[9]*PC9[w] + b10[10]*PC10[w] + b10[11]*TBF[w] +
      b10[12]*BIP[w] + b10[13]*K[w] + b10[14]*HR[w]
    }
    a10 ~ dnorm(0.0, 1.0/1e6)
    for (x in 1:14) {
      b10[x] ~ ddexp(0.0, sqrt(2.0))
    }


  for (aa in 1:length(ER)) {
	  ER[aa] ~ dbinom(p12[aa], TBF[aa])
    logit(p12[aa]) = a12 + b12[1]*PC1[aa] + b12[2]*PC2[aa] + b12[3]*PC3[aa] + 
      b12[4]*PC4[aa] + b12[5]*PC5[aa] + b12[6]*PC6[aa] + b12[7]*PC7[aa] +
      b12[8]*PC8[aa] + b12[9]*PC9[aa] + b12[10]*PC10[aa] + b12[11]*TBF[aa] +
      b12[12]*HR[aa]
  }
  a12 ~ dnorm(0.0, 1.0/1e6) # intercept
  for (aa in 1:15){
    b12[aa] ~ ddexp(0.0, sqrt(2.0))
  }


  for (bb in 1:length(GS)) {
    GS[bb] ~ dpois(lam4[bb])
    log(lam4[bb]) = a13 + b13[1]*PC1[bb] + b13[2]*PC2[bb] + b13[3]*PC3[bb] + 
      b13[4]*PC4[bb] + b13[5]*PC5[bb] + b13[6]*PC6[bb] + b13[7]*PC7[bb] +
      b13[8]*PC8[bb] + b13[9]*PC9[bb] + b13[10]*PC10[bb] + b13[11]*TBF[bb] +
      b13[12]*BIP[bb] + b13[13]*K[bb] + b13[14]*HR[bb] + b13[15]*Outs[bb]
  }
  a13 ~ dnorm(0.0, 1.0/1e6)
  for (dd in 1:15){
    b13[dd] ~ ddexp(0.0, sqrt(2.0))
  }
  
  for (y in 1:length(W)) {
    W[y] ~ dbinom(p10[y], G[y])
    logit(p10[y]) = a11 + b11[1]*PC1[y] + b11[2]*PC2[y] + b11[3]*PC3[y] + 
      b11[4]*PC4[y] + b11[5]*PC5[y] + b11[6]*PC6[y] + b11[7]*PC7[y] +
      b11[8]*PC8[y] + b11[9]*PC9[y] + b11[10]*PC10[y] + b11[11]*TBF[y] +
      b11[12]*BIP[y] + b11[13]*K[y] + b11[14]*HR[y] + b11[15]*Outs[y]
  }
  a11 ~ dnorm(0.0, 1.0/1e6)
  for (z in 1:15) {
    b11[z] ~ ddexp(0.0, sqrt(2.0))
  }
  

  for (ff in 1:length(SV)) {
    SV[ff] ~ dpois(lam6[ff])
    log(lam6[ff]) = a15 + b15[1]*PC1[ff] + b15[2]*PC2[ff] + b15[3]*PC3[ff] + 
      b15[4]*PC4[ff] + b15[5]*PC5[ff] + b15[6]*PC6[ff] + b15[7]*PC7[ff] +
      b15[8]*PC8[ff] + b15[9]*PC9[ff] + b15[10]*PC10[ff]
  }
  a15 ~ dnorm(0.0, 1.0/1e6)
  for (ii in 1:10){
    b15[ii] ~ ddexp(0.0, sqrt(2.0))
  }

  for (gg in 1:length(G)) {
    G[gg] ~ dpois(lam7[gg])
    log(lam7[gg]) = a16 + b16[1]*PC1[gg] + b16[2]*PC2[gg] + b16[3]*PC3[gg] + 
      b16[4]*PC4[gg] + b16[5]*PC5[gg] + b16[6]*PC6[gg] + b16[7]*PC7[gg] +
      b16[8]*PC8[gg] + b16[9]*PC9[gg] + b16[10]*PC10[gg]
  }
  a16 ~ dnorm(0.0, 1.0/1e6)
  for (hh in 1:10){
    b16[hh] ~ ddexp(0.0, sqrt(2.0))
  }

  for (cc in 1:length(L)) {
    L[cc] ~ dbinom(p14[cc], G[cc])
    logit(p14[cc]) = a14 + b14[1]*PC1[cc] + b14[2]*PC2[cc] + b14[3]*PC3[cc] + 
      b14[4]*PC4[cc] + b14[5]*PC5[cc] + b14[6]*PC6[cc] + b14[7]*PC7[cc] +
      b14[8]*PC8[cc] + b14[9]*PC9[cc] + b14[10]*PC10[cc] + b14[11]*TBF[cc] +
      b14[12]*BIP[cc] + b14[13]*K[cc] + b14[14]*HR[cc] + b14[15]*Outs[cc]
  }
  a14 ~ dnorm(0.0, 1.0/1e6)
  for (ee in 1:15){
    b14[ee] ~ ddexp(0.0, sqrt(2.0))
  }
} "

X_pca_m = as.matrix(X_pca$x[,1:10])
data_jags = list(TBF=data_pred2$TBF,
                 BIP=(data_pred2$TBF-(data_pred2$SO + data_pred2$BB + data_pred2$HR)),
                 IFFB=round((data_pred2$TBF-(data_pred2$SO + data_pred2$BB))*data_pred2$FlyB*data_pred2$IFFB., 0),
                 H=(data_pred2$H - data_pred2$HR),
                 TTO=(data_pred2$SO + data_pred2$BB + data_pred2$HR),
                 HR=data_pred2$HR,
                 K=data_pred2$SO,
                 Outs=data_pred2$Outs,
                 GS=data_pred2$GS,
                 G=data_pred2$G,
                 SV=data_pred2$SV,
                 W=data_pred2$W,
                 L=data_pred2$L,
                 ER=data_pred2$ER,
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
           "a6","b6","p6",
           "a7","b7","p7",
           "a9","b9","p9",
           "a10", "b10",
           "a11", "b11",
           "a12", "b12",
           "a13", "b13",
           "a14", "b14",
           "a15", "b15",
           "a16", "b16")

mod = jags.model(textConnection(mod_string), data=data_jags, n.chains=3)

update(mod, 5e2)

mod_sim = coda.samples(model=mod,
                       variable.names=params,
                       n.iter=1e3)

mod_csim_p = as.mcmc(do.call(rbind, mod_sim))

data_pred3 = data_pred3[which(data_pred3$Season == 2018),]

save(mod_csim_p, file = "mod_csim_p.rda")
write.csv(data_pred3, file = "data_pred_pitch.csv")
