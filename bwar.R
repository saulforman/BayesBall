mat = matrix(0, ncol = 676, nrow = 3000)
bwar = as.data.frame(mat)
for(i in 1:nrow(data_pred3)){
  name = as.character(data_pred3[i,]$Name)
  row = data_pred3[which(data_pred3$Name == name),]
  x = as.numeric(row[,4:13])
  
  loglam = mod_csim[,1] + mod_csim[,9:18] %*% x
  lam = exp(loglam)
  ## num_sims
  n_sim = length(lam)
  
  ## plot PAs
  pa = rpois(n_sim, lam)
  
  ## plot BIP
  logit_p1 = mod_csim[,2] + mod_csim[,19:28] %*% x + as.matrix(mod_csim[,29]) * as.matrix(pa)
  
  p1 = exp(logit_p1)/(1+exp(logit_p1))
  bip = rbinom(n_sim, pa, p1)
  
  ## plot H - HR
  logit_p2 = mod_csim[,3] + mod_csim[,30:39] %*% x + as.matrix(mod_csim[,40]) * as.matrix(pa) +
    as.matrix(mod_csim[,41]) * as.matrix(bip)
  
  p2 = exp(logit_p2)/(1+exp(logit_p2))
  h2 = rbinom(n_sim, bip, p2)
  ## plot 1B
  logit_p3 = mod_csim[,4] + mod_csim[,42:51] %*% x + as.matrix(mod_csim[,52]) * as.matrix(pa) +
    as.matrix(mod_csim[,53]) * as.matrix(h)
  
  p3 = exp(logit_p3)/(1+exp(logit_p3))
  x1b = rbinom(n_sim, h2, p3)
  ## plot 2B
  logit_p4 = mod_csim[,5] + mod_csim[,54:63] %*% x + as.matrix(mod_csim[,64]) * as.matrix(pa) +
    as.matrix(mod_csim[,65]) * as.matrix(h)
  
  p4 = exp(logit_p4)/(1+exp(logit_p4))
  x2b = rbinom(n_sim, h2, p4)
  ## plot 3b
  x3b = rbinom(n_sim, h2, max(1-p3-p4, 0))
  ## plot H
  h = h2 + hr
  ## plot Ks
  logit_p6 = mod_csim[,6] + mod_csim[,66:75] %*% x + as.matrix(mod_csim[,76]) * as.matrix(pa) +
    as.matrix(mod_csim[,77]) * as.matrix(bip) + as.matrix(mod_csim[,78]) * as.matrix(h2)
  p6 = exp(logit_p6)/(1+exp(logit_p6))
  k = rbinom(n_sim, (pa-bip), p6)
  ## plot HR
  logit_p7 = mod_csim[,7] + mod_csim[,79:88] %*% x + as.matrix(mod_csim[,89]) * as.matrix(pa) +
    as.matrix(mod_csim[,90]) * as.matrix(bip) + as.matrix(mod_csim[,91]) * as.matrix(h2) +
    as.matrix(mod_csim[,92]) * as.matrix(k)
  
  p7 = exp(logit_p7)/(1+exp(logit_p7))
  hr = rbinom(n_sim, (pa-bip), p7)
  
  ## plot BBs
  bb = (pa-bip)-(hr+k)
  ## plot Attempts
  loglam2 = mod_csim[,8] + mod_csim[,93:102] %*% x
  lam2 = exp(loglam2)
  attempt = rpois(n_sim, lam2)
  ## plot SBs
  sb = rpois(n_sim, (lam2*0.7))
  cs = attempt - sb
  ## plot ABs
  ab = pa - bb
  ## plot AVG
  avg = h/ab
  ## plot OBP
  obp = (h+bb)/pa
  ## plot SLG
  slg = (x1b + 2*x2b + 3*x3b + 4*hr)/ab
  ## plot OPS
  ops = obp + slg
  ## plot wOBA
  woba = (0.696*bb + 0.881*x1b + 1.238*x2b + 1.560*x3b + 1.987*hr)/pa
  wraa = (woba - .317)/1.195 * pa
  lgrpa = 21630/185139
  row2 = data_pred2[which(data_pred2$Name == name),]
  row2 = row2[which(row2$Season == 2018),]
  rownum = which(as.character(pf$Team) == as.character(row2$Team))
  pf2 = pf[rownum,]$Basic
  if (as.character(row2$Team) %in% c('Yankees', 'Orioles', 'Red Sox', 'Blue Jays', 'Rays',
                                     'Indians', 'White Sox', 'Twins', 'Tigers', 'Royals',
                                     'Athletics', 'Astros', 'Angels', 'Mariners', 'Rangers')){
    natamrpa = 10982/91924
  } else {
    natamrpa = 10432/88080
  }
  
  if (length(pf2) == 0) {
    pf2 = 100
  }
  
  br = wraa + (lgrpa - ((pf2/100)*lgrpa))*pa + (lgrpa - natamrpa)*pa
  rpw = 10
  batwar = br/rpw
  
  bwar[,i] = batwar
  colnames(bwar)[i] = name
}

t = round(psych::describe(bwar), 3)
t2 = t[-which(rownames(t) %in% data_pred3_p$Name),]
t2$Name = rownames(t2)
rownames(t2) = 1:nrow(t2)
t2[,1] = t2$Name
t2 = t2[,-14]
colnames(t2)[1] = 'Name'
t2 = t2 %>% arrange(-skew)
write.csv(t2, 'bwar.csv')
