#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
library(shiny)
library(ggplot2)
library(plotly)
library(psych)
library(reshape2)
library(pracma)
library(robustbase)
data_pred3 = read.csv('data_pred.csv')
data_pred3_p = read.csv('data_pred_pitch.csv')
data_pred2 = read.csv('data_pred2.csv')
data_pred2_p = read.csv('data_pred2_pitch.csv')
data_pred2$Name = as.character(data_pred2$Name)
data_pred2$Season = as.character(data_pred2$Season)
t2 = read.csv('bwar.csv')
t2 = t2[,-1]
pf = read.csv('pf.csv')
load('mod_csim.rda')
load('mod_csim_p.rda')
league = colMeans(data_pred3[,4:13])
league_p = colMeans(data_pred3_p[,4:13])
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
  
  output$distPlot <- shiny::renderPlot({
    
    if(input$pos == 'Hitters'){
      ## Get PCs for specific player
      name = input$name
      stat = input$stat
      row = data_pred3[which(data_pred3$Name == input$name),]
      x = as.numeric(row[,4:13])
      
      loglam = mod_csim[,1] + mod_csim[,9:18] %*% x
      lam = exp(loglam)
      ## num_sims
      n_sim = length(lam)
      
      ## plot PAs
      if(input$prorated == 'Yes'){
        lam = 600
      }
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
        as.matrix(mod_csim[,53]) * as.matrix(h2)
      
      p3 = exp(logit_p3)/(1+exp(logit_p3))
      x1b = rbinom(n_sim, h2, p3)
      ## plot 2B
      logit_p4 = mod_csim[,5] + mod_csim[,54:63] %*% x + as.matrix(mod_csim[,64]) * as.matrix(pa) +
        as.matrix(mod_csim[,65]) * as.matrix(h2)
      
      p4 = exp(logit_p4)/(1+exp(logit_p4))
      x2b = rbinom(n_sim, h2, p4)
      ## plot 3b
      x3b = rbinom(n_sim, h2, max(1-p3-p4, 0))
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
      
      ## plot H
      h = h2 + hr
      
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
      sharpe = (batwar-1.3)/sd(batwar)
      
      data = as.data.frame(cbind(pa, ab, h, x1b, x2b, x3b, hr, bb, k, sb, cs, avg, obp, slg, ops, woba,
                                 batwar, sharpe))
      # draw the kernel density plot with the specified number of bins
      colnum = which(tolower(stat) == colnames(data))
      title = paste('Kernel Density Plot of Projected', toupper(stat), 'for', name)

      
          if (input$comp == 'Display League Average (2018)'){
            loglam_2 = mod_csim[,1] + mod_csim[,9:18] %*% league
            lam_2 = exp(loglam_2)
            ## num_sims
            n_sim_2 = length(lam_2)
            
            ## plot PAs
            pa_2 = rpois(n_sim, lam_2)
            ## plot BIP
            logit_p1_2 = mod_csim[,2] + mod_csim[,19:28] %*% league + as.matrix(mod_csim[,29])*as.matrix(pa_2)
            p1_2 = exp(logit_p1_2)/(1+exp(logit_p1_2))
            bip_2 = rbinom(n_sim, pa_2, p1_2)
            
            ## plot H - HR
            logit_p2_2 = mod_csim[,3] + mod_csim[,30:39] %*% league + as.matrix(mod_csim[,40])*as.matrix(pa_2) +
              as.matrix(mod_csim[,41])*as.matrix(bip_2)
            p2_2 = exp(logit_p2_2)/(1+exp(logit_p2_2))
            h2_2 = rbinom(n_sim, bip_2, p2_2)
            ## plot 1B
            logit_p3_2 = mod_csim[,4] + mod_csim[,42:51] %*% league + 
              as.matrix(mod_csim[,52])*as.matrix(pa_2) + as.matrix(mod_csim[,53])*as.matrix(h2_2)
            p3_2 = exp(logit_p3_2)/(1+exp(logit_p3_2))
            x1b_2 = rbinom(n_sim, h2_2, p3_2)
            ## plot 2B
            logit_p4_2 = mod_csim[,5] + mod_csim[,54:63] %*% league +
              as.matrix(mod_csim[,64])*as.matrix(pa_2) + as.matrix(mod_csim[,65])*as.matrix(h2_2)
            p4_2 = exp(logit_p4_2)/(1+exp(logit_p4_2))
            x2b_2 = rbinom(n_sim, h2_2, p4_2)
            ## plot 3b
            x3b_2 = rbinom(n_sim, h2_2, (1-p3_2-p4_2))
            x3b_2[which(is.na(x3b_2))] = 0
            
            ## plot Ks
            logit_p6_2 = mod_csim[,6] + mod_csim[,66:75] %*% league + as.matrix(mod_csim[,76]) * as.matrix(pa_2) +
              as.matrix(mod_csim[,77]) * as.matrix(bip_2) + as.matrix(mod_csim[,78]) * as.matrix(h2_2)
            p6_2 = exp(logit_p6_2)/(1+exp(logit_p6_2))
            k_2 = rbinom(n_sim, max(pa_2-bip_2, 0), p6_2)
            ## plot HR
            logit_p7_2 = mod_csim[,7] + mod_csim[,79:88] %*% league + as.matrix(mod_csim[,89]) * as.matrix(pa_2) +
              as.matrix(mod_csim[,90]) * as.matrix(bip_2) + as.matrix(mod_csim[,91]) * as.matrix(h2_2) +
              as.matrix(mod_csim[,92]) * as.matrix(k_2)
            p7_2 = exp(logit_p7_2)/(1+exp(logit_p7_2))
            hr_2 = rbinom(n_sim, max(pa_2-bip_2, 0), p7_2)
            ## plot H
            h_2 = h2_2 + hr_2
            ## plot BBs
            bb_2 = (pa_2-bip_2)-(hr_2+k_2)
            ## plot Attempts
            loglam2_2 = mod_csim[,8] + mod_csim[,93:102] %*% league
            lam2_2 = exp(loglam2_2)
            attempt_2 = rpois(n_sim, lam2_2)
            ## plot SBs
            sb_2 = rpois(n_sim, (lam2_2*0.7))
            cs_2 = attempt_2 - sb_2
            ## plot ABs
            ab_2 = pa_2 - bb_2
            ## plot AVG
            avg_2 = h_2/ab_2
            ## plot OBP
            obp_2 = (h_2+bb_2)/pa_2
            ## plot SLG
            slg_2 = (x1b_2 + 2*x2b_2 + 3*x3b_2 + 4*hr_2)/ab_2
            ## plot OPS
            ops_2 = obp_2 + slg_2
            ## plot wOBA
            woba_2 = (0.696*bb_2 + 0.881*x1b_2 + 1.238*x2b_2 + 1.560*x3b_2 + 1.987*hr_2)/pa_2
            
            wraa_2 = (woba_2 - .317)/1.195 * pa_2
            lgrpa_2 = 21630/185139
            natamrpa_2 = lgrpa_2
            pf2_2 = 1
            
            br_2 = wraa_2 + (lgrpa_2 - (pf2_2*lgrpa_2))*pa_2 + (lgrpa_2 - natamrpa_2)*pa_2
            rpw_2 = 10
            batwar_2 = br_2/rpw_2
            sharpe_2 = (batwar_2-1.3)/sd(batwar_2)
            
            data = as.data.frame(cbind(pa, ab, h, x1b, x2b, x3b, hr, bb, k, sb, cs, avg, obp, slg, ops, woba,
                                       batwar, sharpe,
                                       pa_2, ab_2, h_2, x1b_2, x2b_2, x3b_2, hr_2, bb_2, k_2, sb_2, cs_2, 
                                       avg_2, obp_2, slg_2, ops_2, woba_2, batwar_2, sharpe_2))
            colnum1 = which(colnames(data) == tolower(stat))
            stat2 = paste(tolower(stat), '2', sep = '_')
            colnum2 = which(colnames(data) == tolower(stat2))
            comp = data[,c(colnum1, colnum2)]
            colnames(comp) = c(name, 'League Average (2018)')
            melt_comp = melt(comp)
            
            text1 = paste('Player Mean:', round(mean(data[,colnum1]),2))
            text2 = paste('League Mean:', round(mean(data[,colnum2]),2))
            text3 = paste('Probability Player 1 > League Avg:', 
                          round(mean(data[,colnum1] > data[,colnum2]),2))
            
            p = ggplot(melt_comp, aes(x = value, fill = variable)) + 
              geom_histogram(aes(y = ..density..),  alpha = 0.4, position = "identity") +
              geom_density(alpha = 0.4) +
              ggtitle(title) +
              xlab(toupper(stat)) +
              ylab('Density') +
              theme(plot.title = element_text(hjust = 0.5, size=14, face='bold'),
                    axis.title = element_text(size=14,face="bold")) +
              geom_vline(aes(xintercept=mean(data[,colnum1])),
                         color="red", linetype="dashed", size=1) +
              geom_vline(aes(xintercept=mean(data[,colnum2])),
                         color="blue", linetype="dashed", size=1) +
              annotate("text", x=Inf, y=Inf, label = text1, vjust = 1, hjust = 1) +
              annotate("text", x=Inf, y=Inf, label = text2, vjust = 3, hjust = 1) +
              annotate("text", x=Inf, y=Inf, label = text3, vjust = 5, hjust = 1)
            
          } else if (input$comp == 'Compare Two Players') {
            name2 = input$name2
            row2 = as.numeric(data_pred3[which(data_pred3$Name == name2), 4:13])
            
            loglam_2 = mod_csim[,1] + mod_csim[,9:18] %*% row2
            lam_2 = exp(loglam_2)
            ## num_sims
            n_sim_2 = length(lam_2)
            
            ## plot PAs
            pa_2 = rpois(n_sim_2, lam_2)
            
            ## plot BIP
            logit_p1_2 = mod_csim[,2] + mod_csim[,19:28] %*% row2 + as.matrix(mod_csim[,29]) * as.matrix(pa_2)
            p1_2 = exp(logit_p1_2)/(1+exp(logit_p1_2))
            bip_2 = rbinom(n_sim_2, pa_2, p1_2)
            
            ## plot H - HR
            logit_p2_2 = mod_csim[,3] + mod_csim[,30:39] %*% row2 + as.matrix(mod_csim[,40]) * as.matrix(pa_2) +
              as.matrix(mod_csim[,41]) * as.matrix(bip_2)
            
            p2_2 = exp(logit_p2_2)/(1+exp(logit_p2_2))
            h2_2 = rbinom(n_sim_2, bip_2, p2_2)
            ## plot 1B
            logit_p3_2 = mod_csim[,4] + mod_csim[,42:51] %*% row2 + as.matrix(mod_csim[,52]) * as.matrix(pa_2) +
              as.matrix(mod_csim[,53]) * as.matrix(h2_2)
            
            p3_2 = exp(logit_p3)/(1+exp(logit_p3))
            x1b_2 = rbinom(n_sim, h2, p3)
            ## plot 2B
            logit_p4_2 = mod_csim[,5] + mod_csim[,54:63] %*% row2 + as.matrix(mod_csim[,64]) * as.matrix(pa_2) +
              as.matrix(mod_csim[,65]) * as.matrix(h2_2)
            
            p4_2 = exp(logit_p4_2)/(1+exp(logit_p4_2))
            x2b_2 = rbinom(n_sim_2, h2_2, p4_2)
            ## plot 3b
            x3b_2 = rbinom(n_sim_2, h2_2, max(1-p3_2-p4_2, 0))
            ## plot Ks
            logit_p6_2 = mod_csim[,6] + mod_csim[,66:75] %*% row2 + as.matrix(mod_csim[,76]) * as.matrix(pa_2) +
              as.matrix(mod_csim[,77]) * as.matrix(bip_2) + as.matrix(mod_csim[,78]) * as.matrix(h2_2)
            p6_2 = exp(logit_p6_2)/(1+exp(logit_p6_2))
            k_2 = rbinom(n_sim_2, (pa_2-bip_2), p6_2)
            ## plot HR
            logit_p7_2 = mod_csim[,7] + mod_csim[,79:88] %*% row2 + as.matrix(mod_csim[,89]) * as.matrix(pa_2) +
              as.matrix(mod_csim[,90]) * as.matrix(bip_2) + as.matrix(mod_csim[,91]) * as.matrix(h2_2) +
              as.matrix(mod_csim[,92]) * as.matrix(k_2)
            
            p7_2 = exp(logit_p7_2)/(1+exp(logit_p7_2))
            hr_2 = rbinom(n_sim_2, (pa_2-bip_2), p7_2)
            
            ## plot H
            h_2 = h2_2 + hr_2
            
            ## plot BBs
            bb_2 = (pa_2-bip_2)-(hr_2+k_2)
            ## plot Attempts
            loglam2_2 = mod_csim[,8] + mod_csim[,93:102] %*% row2
            lam2_2 = exp(loglam2_2)
            attempt_2 = rpois(n_sim_2, lam2_2)
            ## plot SBs
            sb_2 = rpois(n_sim_2, (lam2_2*0.7))
            cs_2 = attempt_2 - sb_2
            ## plot ABs
            ab_2 = pa_2 - bb_2
            ## plot AVG
            avg_2 = h_2/ab_2
            ## plot OBP
            obp_2 = (h_2+bb_2)/pa_2
            ## plot SLG
            slg_2 = (x1b_2 + 2*x2b_2 + 3*x3b_2 + 4*hr_2)/ab_2
            ## plot OPS
            ops_2 = obp_2 + slg_2
            ## plot wOBA
            woba_2 = (0.696*bb_2 + 0.881*x1b_2 + 1.238*x2b_2 + 1.560*x3b_2 + 1.987*hr_2)/pa_2
            wraa_2 = (woba_2 - .317)/1.195 * pa_2
            lgrpa_2 = 21630/185139
            row2_2 = data_pred2[which(data_pred2$Name == name2),]
            row2_2 = row2_2[which(row2_2$Season == 2018),]
            rownum_2 = which(as.character(pf$Team) == as.character(row2_2$Team))
            pf2_2 = pf[rownum_2,]$Basic
            if (as.character(row2_2$Team) %in% c('Yankees', 'Orioles', 'Red Sox', 'Blue Jays', 'Rays',
                                                 'Indians', 'White Sox', 'Twins', 'Tigers', 'Royals',
                                                 'Athletics', 'Astros', 'Angels', 'Mariners', 'Rangers')){
              natamrpa_2 = 10982/91924
            } else {
              natamrpa_2 = 10432/88080
            }
            
            if (length(pf2_2) == 0) {
              pf2_2 = 100
            }
            
            br_2 = wraa_2 + (lgrpa_2 - ((pf2_2/100)*lgrpa_2))*pa_2 + (lgrpa_2 - natamrpa_2)*pa_2
            rpw_2 = 10
            batwar_2 = br_2/rpw_2
            sharpe_2 = (batwar_2-1.3)/sd(batwar_2)
            
            data = as.data.frame(cbind(pa, ab, h, x1b, x2b, x3b, hr, bb, k, sb, cs, avg, obp, slg, ops, woba,
                                       batwar, sharpe,
                                       pa_2, ab_2, h_2, x1b_2, x2b_2, x3b_2, hr_2, bb_2, k_2, sb_2, cs_2, 
                                       avg_2, obp_2, slg_2, ops_2, woba_2, batwar_2, sharpe_2))
            colnum1 = which(colnames(data) == tolower(stat))
            stat2 = paste(tolower(stat), '2', sep = '_')
            colnum2 = which(colnames(data) == tolower(stat2))
            comp = data[,c(colnum1, colnum2)]
            colnames(comp) = c(name, name2)
            melt_comp = melt(comp)
            text1 = paste('Player 1 Mean:', round(mean(data[,colnum1]),2))
            text2 = paste('Player 2 Mean:', round(mean(data[,colnum2]),2))
            text3 = paste('Probability Player 1 > Player 2:', 
                          round(mean(data[,colnum1] > data[,colnum2]),2))
            
            p = ggplot(melt_comp, aes(x = value, fill = variable)) + 
              geom_histogram(aes(y = ..density..),  alpha = 0.4, position = "identity") +
              geom_density(alpha = 0.4) +
              ggtitle(title) +
              xlab(toupper(stat)) +
              ylab('Density') +
              theme(plot.title = element_text(hjust = 0.5, size=14, face='bold'),
                    axis.title = element_text(size=14,face="bold")) +
              geom_vline(aes(xintercept=mean(data[,colnum1])),
                         color="red", linetype="dashed", size=1) +
              geom_vline(aes(xintercept=mean(data[,colnum2])),
                         color="blue", linetype="dashed", size=1) +
              annotate("text", x=Inf, y=Inf, label = text1, vjust = 1, hjust = 1) +
              annotate("text", x=Inf, y=Inf, label = text2, vjust = 3, hjust = 1) +
              annotate("text", x=Inf, y=Inf, label = text3, vjust = 5, hjust = 1)
      } else {
        text = paste('Player Mean:', round(mean(data[,colnum])),3)
        p = ggplot(data, aes(x = data[,colnum])) + 
          geom_histogram(aes(y = ..density..), fill = 'dark blue',  alpha = 0.4, position = "identity") +
          geom_density(fill = 'blue', alpha = 0.4) +
          ggtitle(title) +
          xlab(toupper(stat)) +
          ylab('Density') +
          theme(plot.title = element_text(hjust = 0.5, size=14, face='bold'),
                axis.title = element_text(size=14,face="bold")) +
          geom_vline(aes(xintercept=mean(data[,colnum])),
                     color="blue", linetype="dashed", size=1) +
          annotate("text", x=Inf, y=Inf, label = text, vjust = 1, hjust = 1)
      }
      
      p
      
    } else {
      name_p = input$name_p
      stat_p = input$stat_p
      row = data_pred3_p[which(data_pred3_p$Name == name_p), ]
      x = as.numeric(row[,4:13])
      
      loglam = mod_csim_p[,1] + mod_csim_p[,14:23] %*% x
      lam = exp(loglam)
      n_sim = length(lam)
      if(input$prorated == 'Yes'){
        lam = 600
        n_sim = 3000
      }
      ## plot PAs
      tbf = rpois(n_sim, lam)
      ## plot BIP
      logit_p1 = mod_csim_p[,9] + mod_csim_p[,118:127] %*% x + mod_csim_p[,128] * tbf
      p1 = exp(logit_p1)/(1+exp(logit_p1))
      bip = rbinom(n_sim, tbf, p1)
      
      ## plot H - HR
      logit_p2 = mod_csim_p[,10] + mod_csim_p[,129:138] %*% x + mod_csim_p[,139] * bip
      p2 = exp(logit_p2)/(1+exp(logit_p2))
      h2 = rbinom(n_sim, bip, p2)
      ## plot K
      logit_p3 = mod_csim_p[,11] + mod_csim_p[,140:149] %*% x + mod_csim_p[,150] * tbf + 
        mod_csim_p[,151] * bip
      p3 = exp(logit_p3)/(1+exp(logit_p3))
      k = rbinom(n_sim, (tbf-bip), p3)
      ## plot HR
      logit_p4 = mod_csim_p[,12] + mod_csim_p[,152:161] %*% x + mod_csim_p[,162]*tbf +
        mod_csim_p[,163]*bip
      p4 = exp(logit_p4)/(1+exp(logit_p4))
      hr = rbinom(n_sim, (tbf-bip), p4)
      ## plot H
      h = h2 + hr
      ## plot BBs
      bb = (tbf-bip)-(hr+k)
      ## plot IFFB
      logit_p5 = mod_csim_p[,13] + mod_csim_p[,164:173] %*% x + mod_csim_p[,174]*tbf +
        mod_csim_p[,175]*bip + mod_csim_p[,176]*k + mod_csim_p[,177]*hr
      p5 = exp(logit_p5)/(1+exp(logit_p5))
      iffb = rbinom(n_sim, (tbf-bip), p5)
      ## plot ABs
      ab = tbf - bb
      ## plot Outs
      colnames(mod_csim_p)[24:33]
      loglam2 = mod_csim_p[,2] + mod_csim_p[,24:33] %*% x + mod_csim_p[,34]*tbf +
        mod_csim_p[,35]*bip + mod_csim_p[,36]*k + mod_csim_p[,37]*hr
      lam2 = exp(loglam)
      outs = rpois(n_sim, lam2)
      ## plot G
      loglam7 = mod_csim_p[,8] + mod_csim_p[,108:117] %*% x
      lam7 = exp(loglam7)
      g = rpois(n_sim, lam7)
      ip = outs/3
      fip = (13*hr + 3*(bb) - 2*k)/ip + 3.140
      ## plot W
      colnames(mod_csim_p)[38:47]
      logitp10 = mod_csim_p[,3] + mod_csim_p[,38:47] %*% x + mod_csim_p[,48]*tbf +
        mod_csim_p[,49]*bip + mod_csim_p[,50]*k + mod_csim_p[,51]*hr + mod_csim_p[,52]*outs
      p10 = exp(logitp10)/(1+exp(logitp10))
      w = rbinom(n_sim, g, p10)
      ## plot ER
      logitp12 = mod_csim_p[,4] + mod_csim_p[,53:62] %*% x + mod_csim_p[,63]*tbf +
        mod_csim_p[,64]*hr
      p12 = exp(logitp12)/(1+exp(logitp12))
      er = rbinom(n_sim, tbf, p12)
      era = er/(ip/9)
      ## plot L
      loglam5 = mod_csim_p[,6] + mod_csim_p[,83:92] %*% x + mod_csim_p[,93]*tbf +
        mod_csim_p[,94]*bip + mod_csim_p[,95]*k + mod_csim_p[,96]*hr + mod_csim_p[,97]*outs
      lam5 = exp(loglam5)
      l = rpois(n_sim, lam5)
      ## plot SV
      loglam6 = mod_csim_p[,7] + mod_csim_p[,98:107] %*% x
      lam6 = exp(loglam6)
      sv = rpois(n_sim, lam6)
      
      
      iffipc = 3.34
      iffip = ((13*hr)+(3*(bb))-(2*(k+iffb)))/(outs/3) + iffipc
      k9 = k/(ip/9)
      bb9 = bb/(ip/9)
      whip = (bb+h)/ip
      
      adjust = 4.52 - 4.15
      fipr9 = iffip + adjust
      row2 = data_pred2_p[which(data_pred2_p$Name == name_p),]
      row2 = row2[which(row2$Season == 2018),]
      rownum = which(as.character(pf$Team) == as.character(row2$Team))
      pf2 = pf[rownum,]$FIP
      if (length(pf2) == 0) {
        pf2 = 100
      }
      pfipr9 = fipr9/(pf2/100)
      aliffb = (92439-20425-7666-319-996)*.364*.105
      nliffb = (92700-20782-20782-610-926)*.343*.101
      if (as.character(row2$Team) %in% c('Yankees', 'Orioles', 'Red Sox', 'Blue Jays', 'Rays',
                                         'Indians', 'White Sox', 'Twins', 'Tigers', 'Royals',
                                         'Athletics', 'Astros', 'Angels', 'Mariners', 'Rangers')){
        lgfipr9 = ((13*2932)+(3*(7666 + 996))-(2*(20425+aliffb)))/ 21660.66666 + iffipc 
      } else {
        lgfipr9 = ((13*2653)+(3*(8020+926))-(2*(20782+nliffb)))/ 21828.3333 + iffipc 
      }
      
      ipg = (outs/3)/32
      drpw = (((((18-ipg)*lgfipr9)+(((outs/3)/row2$G)*pfipr9)/18) + 2)*1.5)
      
      raap9 = lgfipr9 - pfipr9
      wpgaa = raap9/drpw
      repl = 0.03*(1-(row2$GS/32)) + 0.12*(row2$GS/32)
      wpgar = wpgaa + repl
      warf = wpgar * (outs/27)
      correction = -0.000682902*(outs/3)
      war = warf + correction
      sharpe = (war-1.3)/sd(war)
      data_p = as.data.frame(cbind(w, l, era, g, sv, ip, h, er, hr,
                                   k, bb, whip, k9, bb9, fip, war, sharpe))
      
      colnum = which(tolower(stat_p) == colnames(data_p))
      title = paste('Kernel Density Plot of Projected', toupper(stat_p), 'for', name_p)
      if (input$comp == 'Display League Average (2018)'){
        loglam_2 = mod_csim_p[,1] + mod_csim_p[,14:23] %*% league_p
        lam_2 = exp(loglam_2)
        n_sim_2 = length(lam_2)
        if(input$prorated == 'Yes'){
          lam_2 = 600
          n_sim_2 = 3000
        }
        ## plot PAs
        tbf_2 = rpois(n_sim_2, lam_2)
        ## plot BIP
        logit_p1_2 = mod_csim_p[,9] + mod_csim_p[,118:127] %*% league_p + 
          mod_csim_p[,128] * tbf_2
        p1_2 = exp(logit_p1_2)/(1+exp(logit_p1_2))
        bip_2 = rbinom(n_sim, tbf_2, p1_2)
        
        ## plot H - HR
        logit_p2_2 = mod_csim_p[,10] + mod_csim_p[,129:138] %*% league_p + 
          mod_csim_p[,139] * bip_2
        p2_2 = exp(logit_p2_2)/(1+exp(logit_p2_2))
        h2_2 = rbinom(n_sim, bip_2, p2_2)
        ## plot K
        logit_p3_2 = mod_csim_p[,11] + mod_csim_p[,140:149] %*% league_p + 
          mod_csim_p[,150] * tbf_2 + 
          mod_csim_p[,151] * bip_2
        p3_2 = exp(logit_p3_2)/(1+exp(logit_p3_2))
        k_2 = rbinom(n_sim_2, (tbf_2-bip_2), p3_2)
        ## plot HR
        logit_p4_2 = mod_csim_p[,12] + mod_csim_p[,152:161] %*% league_p + 
          mod_csim_p[,162]*tbf_2 +
          mod_csim_p[,163]*bip_2
        p4_2 = exp(logit_p4_2)/(1+exp(logit_p4_2))
        hr_2 = rbinom(n_sim_2, (tbf_2-bip_2), p4_2)
        ## plot H
        h_2 = h2_2 + hr_2
        ## plot BBs
        bb_2 = (tbf_2-bip_2)-(hr_2+k_2)
        ## plot IFFB
        logit_p5_2 = mod_csim_p[,13] + mod_csim_p[,164:173] %*% league_p + 
          mod_csim_p[,174]*tbf_2 +
          mod_csim_p[,175]*bip_2 + mod_csim_p[,176]*k_2 + mod_csim_p[,177]*hr_2
        p5_2 = exp(logit_p5_2)/(1+exp(logit_p5_2))
        iffb_2 = rbinom(n_sim, (tbf_2-bip_2), p5_2)
        ## plot ABs
        ab_2 = tbf_2 - bb_2
        ## plot Outs
        loglam2_2 = mod_csim_p[,2] + mod_csim_p[,24:33] %*% league_p + 
          mod_csim_p[,34]*tbf_2 +
          mod_csim_p[,35]*bip_2 + mod_csim_p[,36]*k_2 + mod_csim_p[,37]*hr_2
        lam2_2 = exp(loglam_2)
        outs_2 = rpois(n_sim, lam2_2)
        ## plot G
        loglam7_2 = mod_csim_p[,8] + mod_csim_p[,108:117] %*% league_p
        lam7_2 = exp(loglam7_2)
        g_2 = rpois(n_sim_2, lam7_2)
        ip_2 = outs_2/3
        fip_2 = (13*hr_2 + 3*(bb_2) - 2*k_2)/ip_2 + 3.140
        ## plot W
        logitp10_2 = mod_csim_p[,3] + mod_csim_p[,38:47] %*% league_p + 
          mod_csim_p[,48]*tbf_2 +
          mod_csim_p[,49]*bip_2 + mod_csim_p[,50]*k_2 + mod_csim_p[,51]*hr_2 + 
          mod_csim_p[,52]*outs_2
        p10_2 = exp(logitp10_2)/(1+exp(logitp10_2))
        w_2 = rbinom(n_sim_2, g_2, p10_2)
        ## plot ER
        logitp12_2 = mod_csim_p[,4] + mod_csim_p[,53:62] %*% league_p + 
          mod_csim_p[,63]*tbf_2 +
          mod_csim_p[,64]*hr_2
        p12_2 = exp(logitp12_2)/(1+exp(logitp12_2))
        er_2 = rbinom(n_sim_2, tbf_2, p12_2)
        era_2 = er_2/(ip_2/9)
        ## plot L
        loglam5_2 = mod_csim_p[,6] + mod_csim_p[,83:92] %*% league_p + 
          mod_csim_p[,93]*tbf_2 +
          mod_csim_p[,94]*bip_2 + mod_csim_p[,95]*k_2 + mod_csim_p[,96]*hr_2 + 
          mod_csim_p[,97]*outs_2
        lam5_2 = exp(loglam5_2)
        l_2 = rpois(n_sim_2, lam5_2)
        ## plot SV
        loglam6_2 = mod_csim_p[,7] + mod_csim_p[,98:107] %*% league_p
        lam6_2 = exp(loglam6_2)
        sv_2 = rpois(n_sim_2, lam6_2)
        
        
        iffipc_2 = 3.34
        iffip_2 = ((13*hr_2)+(3*(bb_2))-(2*(k_2+iffb_2)))/(outs_2/3) + iffipc_2
        k9_2 = k_2/(ip_2/9)
        bb9_2 = bb_2/(ip_2/9)
        whip_2 = (bb_2+h_2)/ip_2
        
        adjust_2 = 4.52 - 4.15
        fipr9_2 = iffip_2 + adjust_2
        pf2_2 = 100
        pfipr9_2 = fipr9_2
        aliffb_2 = (92439-20425-7666-319-996)*.364*.105
        nliffb_2 = (92700-20782-20782-610-926)*.343*.101
        lgfipr9_2 = ((13*2932)+(3*(7666 + 996))-(2*(20425+(aliffb_2+nliffb_2)/2)))/ 21660.66666 + iffipc_2 
        
        ipg_2 = (outs_2/3)/32
        drpw_2 = (((((18-ipg_2)*lgfipr9_2)+(((outs_2/3)/g_2)*pfipr9_2)/18) + 2)*1.5)
        
        raap9_2 = lgfipr9_2 - pfipr9_2
        wpgaa_2 = raap9_2/drpw_2
        repl_2 = 0.12
        wpgar_2 = wpgaa_2 + repl_2
        warf_2 = wpgar_2 * (outs_2/27)
        correction_2 = -0.000682902*(outs_2/3)
        war_2 = warf_2 + correction_2
        sharpe_2 = (war_2-1.3)/sd(war_2)
        data_p = as.data.frame(cbind(w, l, era, g, sv, ip, h, er, hr,
                                     k, bb, whip, k9, bb9, fip, war, sharpe,
                                     w_2, l_2, era_2, g_2, sv_2, ip_2, h_2, er_2, hr_2,
                                     k_2, bb_2, whip_2, k9_2, bb9_2, fip_2, war_2, sharpe_2))
        colnum1 = which(colnames(data_p) == tolower(stat_p))
        stat2 = paste(tolower(stat_p), '2', sep = '_')
        colnum2 = which(colnames(data_p) == tolower(stat2))
        comp = data_p[,c(colnum1, colnum2)]
        colnames(comp) = c(name_p, 'League Average (2018)')
        melt_comp = melt(comp)
        
        text1 = paste('Player Mean:', round(mean(data_p[,colnum1]),2))
        text2 = paste('League Mean:', round(mean(data_p[,colnum2]),2))
        text3 = paste('Probability Player 1 > League Avg:', 
                      round(mean(data_p[,colnum1] > data_p[,colnum2]),2))
        
        p = ggplot(melt_comp, aes(x = value, fill = variable)) + 
          geom_histogram(aes(y = ..density..),  alpha = 0.4, position = "identity") +
          geom_density(alpha = 0.4) +
          ggtitle(title) +
          xlab(toupper(stat_p)) +
          ylab('Density') +
          theme(plot.title = element_text(hjust = 0.5, size=14, face='bold'),
                axis.title = element_text(size=14,face="bold")) +
          geom_vline(aes(xintercept=mean(data_p[,colnum1])),
                     color="red", linetype="dashed", size=1) +
          geom_vline(aes(xintercept=mean(data_p[,colnum2])),
                     color="blue", linetype="dashed", size=1) +
          annotate("text", x=Inf, y=Inf, label = text1, vjust = 1, hjust = 1) +
          annotate("text", x=Inf, y=Inf, label = text2, vjust = 3, hjust = 1) +
          annotate("text", x=Inf, y=Inf, label = text3, vjust = 5, hjust = 1)
        
        } else if (input$comp == 'Compare Two Players') {
            name2 = input$name2_p
            row2 = as.numeric(data_pred3_p[which(data_pred3_p$Name == name2), 4:13])
            loglam_2 = mod_csim_p[,1] + mod_csim_p[,14:23] %*% row2
            lam_2 = exp(loglam_2)
            n_sim_2 = length(lam_2)
            if(input$prorated == 'Yes'){
              lam_2 = 600
              n_sim_2 = 3000
            }
            ## plot PAs
            tbf_2 = rpois(n_sim_2, lam_2)
            ## plot BIP
            logit_p1_2 = mod_csim_p[,9] + mod_csim_p[,118:127] %*% row2 + 
              mod_csim_p[,128] * tbf_2
            p1_2 = exp(logit_p1_2)/(1+exp(logit_p1_2))
            bip_2 = rbinom(n_sim, tbf_2, p1_2)
            
            ## plot H - HR
            logit_p2_2 = mod_csim_p[,10] + mod_csim_p[,129:138] %*% row2 + 
              mod_csim_p[,139] * bip_2
            p2_2 = exp(logit_p2_2)/(1+exp(logit_p2_2))
            h2_2 = rbinom(n_sim, bip_2, p2_2)
            ## plot K
            logit_p3_2 = mod_csim_p[,11] + mod_csim_p[,140:149] %*% row2 + 
              mod_csim_p[,150] * tbf_2 + 
              mod_csim_p[,151] * bip_2
            p3_2 = exp(logit_p3_2)/(1+exp(logit_p3_2))
            k_2 = rbinom(n_sim_2, (tbf_2-bip_2), p3_2)
            ## plot HR
            logit_p4_2 = mod_csim_p[,12] + mod_csim_p[,152:161] %*% row2 + 
              mod_csim_p[,162]*tbf_2 +
              mod_csim_p[,163]*bip_2
            p4_2 = exp(logit_p4_2)/(1+exp(logit_p4_2))
            hr_2 = rbinom(n_sim_2, (tbf_2-bip_2), p4_2)
            ## plot H
            h_2 = h2_2 + hr_2
            ## plot BBs
            bb_2 = (tbf_2-bip_2)-(hr_2+k_2)
            ## plot IFFB
            logit_p5_2 = mod_csim_p[,13] + mod_csim_p[,164:173] %*% row2 + 
              mod_csim_p[,174]*tbf_2 +
              mod_csim_p[,175]*bip_2 + mod_csim_p[,176]*k_2 + mod_csim_p[,177]*hr_2
            p5_2 = exp(logit_p5_2)/(1+exp(logit_p5_2))
            iffb_2 = rbinom(n_sim, (tbf_2-bip_2), p5_2)
            ## plot ABs
            ab_2 = tbf_2 - bb_2
            ## plot Outs
            loglam2_2 = mod_csim_p[,2] + mod_csim_p[,24:33] %*% row2 + 
              mod_csim_p[,34]*tbf_2 +
              mod_csim_p[,35]*bip_2 + mod_csim_p[,36]*k_2 + mod_csim_p[,37]*hr_2
            lam2_2 = exp(loglam_2)
            outs_2 = rpois(n_sim, lam2_2)
            ## plot G
            loglam7_2 = mod_csim_p[,8] + mod_csim_p[,108:117] %*% row2
            lam7_2 = exp(loglam7_2)
            g_2 = rpois(n_sim_2, lam7_2)
            ip_2 = outs_2/3
            fip_2 = (13*hr_2 + 3*(bb_2) - 2*k_2)/ip_2 + 3.140
            ## plot W
            logitp10_2 = mod_csim_p[,3] + mod_csim_p[,38:47] %*% row2 + 
              mod_csim_p[,48]*tbf_2 +
              mod_csim_p[,49]*bip_2 + mod_csim_p[,50]*k_2 + mod_csim_p[,51]*hr_2 + 
              mod_csim_p[,52]*outs_2
            p10_2 = exp(logitp10_2)/(1+exp(logitp10_2))
            w_2 = rbinom(n_sim_2, g_2, p10_2)
            ## plot ER
            logitp12_2 = mod_csim_p[,4] + mod_csim_p[,53:62] %*% row2 + 
              mod_csim_p[,63]*tbf_2 +
              mod_csim_p[,64]*hr_2
            p12_2 = exp(logitp12_2)/(1+exp(logitp12_2))
            er_2 = rbinom(n_sim_2, tbf_2, p12_2)
            era_2 = er_2/(ip_2/9)
            ## plot L
            loglam5_2 = mod_csim_p[,6] + mod_csim_p[,83:92] %*% row2 + 
              mod_csim_p[,93]*tbf_2 +
              mod_csim_p[,94]*bip_2 + mod_csim_p[,95]*k_2 + mod_csim_p[,96]*hr_2 + 
              mod_csim_p[,97]*outs_2
            lam5_2 = exp(loglam5_2)
            l_2 = rpois(n_sim_2, lam5_2)
            ## plot SV
            loglam6_2 = mod_csim_p[,7] + mod_csim_p[,98:107] %*% row2
            lam6_2 = exp(loglam6_2)
            sv_2 = rpois(n_sim_2, lam6_2)
            
            
            iffipc_2 = 3.34
            iffip_2 = ((13*hr_2)+(3*(bb_2))-(2*(k_2+iffb_2)))/(outs_2/3) + iffipc_2
            k9_2 = k_2/(ip_2/9)
            bb9_2 = bb_2/(ip_2/9)
            whip_2 = (bb_2+h_2)/ip_2
            
            adjust_2 = 4.52 - 4.15
            fipr9_2 = iffip_2 + adjust_2
            row2_2 = data_pred2_p[which(data_pred2_p$Name == name2),]
            row2_2 = row2_2[which(row2_2$Season == 2018),]
            rownum_2 = which(as.character(pf$Team) == as.character(row2_2$Team))
            pf2_2 = pf[rownum_2,]$FIP
            if (length(pf2_2) == 0) {
              pf2_2 = 100
            }
            pfipr9_2 = fipr9_2/(pf2_2/100)
            aliffb_2 = (92439-20425-7666-319-996)*.364*.105
            nliffb_2 = (92700-20782-20782-610-926)*.343*.101
            if (as.character(row2_2$Team) %in% c('Yankees', 'Orioles', 'Red Sox', 'Blue Jays', 'Rays',
                                               'Indians', 'White Sox', 'Twins', 'Tigers', 'Royals',
                                               'Athletics', 'Astros', 'Angels', 'Mariners', 'Rangers')){
              lgfipr9_2 = ((13*2932)+(3*(7666 + 996))-(2*(20425+aliffb_2)))/ 21660.66666 + iffipc_2 
            } else {
              lgfipr9_2 = ((13*2653)+(3*(8020+926))-(2*(20782+nliffb_2)))/ 21828.3333 + iffipc_2 
            }
            
            ipg_2 = (outs_2/3)/32
            drpw_2 = (((((18-ipg_2)*lgfipr9_2)+(((outs_2/3)/row2_2$G)*pfipr9_2)/18) + 2)*1.5)
            
            raap9_2 = lgfipr9_2 - pfipr9_2
            wpgaa_2 = raap9_2/drpw_2
            repl_2 = 0.03*(1-(row2_2$GS/32)) + 0.12*(row2_2$GS/32)
            wpgar_2 = wpgaa_2 + repl_2
            warf_2 = wpgar_2 * (outs_2/27)
            correction_2 = -0.000682902*(outs_2/3)
            war_2 = warf_2 + correction_2
            sharpe_2 = (war_2-1.3)/sd(war_2)
            
            data_p = as.data.frame(cbind(w, l, era, g, sv, ip, h, er, hr,
                                         k, bb, whip, k9, bb9, fip, war, sharpe,
                                         w_2, l_2, era_2, g_2, sv_2, ip_2, h_2, er_2, hr_2,
                                         k_2, bb_2, whip_2, k9_2, bb9_2, fip_2, war_2, sharpe_2))
            colnum1 = which(colnames(data_p) == tolower(stat_p))
            stat2 = paste(tolower(stat_p), '2', sep = '_')
            colnum2 = which(colnames(data_p) == tolower(stat2))
            comp = data_p[,c(colnum1, colnum2)]
            colnames(comp) = c(name_p, name2)
            melt_comp = melt(comp)
            
            text1 = paste('Player 1 Mean:', round(mean(data_p[,colnum1]),2))
            text2 = paste('Player 2 Mean:', round(mean(data_p[,colnum2]),2))
            text3 = paste('Probability Player 1 > Player 2:', 
                          round(mean(data_p[,colnum1] > data_p[,colnum2]),2))
            
            p = ggplot(melt_comp, aes(x = value, fill = variable)) + 
              geom_histogram(aes(y = ..density..),  alpha = 0.4, position = "identity") +
              geom_density(alpha = 0.4) +
              ggtitle(title) +
              xlab(toupper(stat_p)) +
              ylab('Density') +
              theme(plot.title = element_text(hjust = 0.5, size=14, face='bold'),
                    axis.title = element_text(size=14,face="bold")) +
              geom_vline(aes(xintercept=mean(data_p[,colnum1])),
                         color="red", linetype="dashed", size=1) +
              geom_vline(aes(xintercept=mean(data_p[,colnum2])),
                         color="blue", linetype="dashed", size=1) +
              annotate("text", x=Inf, y=Inf, label = text1, vjust = 1, hjust = 1) +
              annotate("text", x=Inf, y=Inf, label = text2, vjust = 3, hjust = 1) +
              annotate("text", x=Inf, y=Inf, label = text3, vjust = 5, hjust = 1)
        } else {
          colnum = which(colnames(data_p) == tolower(stat_p))
          text = paste(('Player Mean:'), round(mean(data_p[,colnum]),2))
          
          p = ggplot(data_p, aes(x = data_p[,colnum])) + 
            geom_histogram(aes(y = ..density..), fill = 'dark blue',  alpha = 0.4, position = "identity") +
            geom_density(fill = 'blue', alpha = 0.4) +
            ggtitle(title) +
            xlab(toupper(stat_p)) +
            ylab('Density') +
            theme(plot.title = element_text(hjust = 0.5, size=14, face='bold'),
                  axis.title = element_text(size=14,face="bold")) +
            geom_vline(aes(xintercept=mean(data_p[,colnum])),
                       color="blue", linetype="dashed", size=1) +
            annotate('text', x=Inf, y=Inf, label = text, vjust = 1, hjust = 1)
      
      }
    
    p
    
    }
  })
  
  output$mytable = DT::renderDataTable({
    if(input$pos == 'Hitters'){
      name = input$name
      stat = input$stat
      header = paste(input$name, "Distribution Data", sep = ' ')
      row = data_pred3[which(data_pred3$Name == name), ]
      x = as.numeric(row[4:13])
      
      loglam = mod_csim[,1] + mod_csim[,9:18] %*% x
      lam = exp(loglam)
      ## num_sims
      n_sim = length(lam)
      if(input$prorated == 'Yes'){
        lam = 600
        n_sim = 3000
      }
      
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
        as.matrix(mod_csim[,53]) * as.matrix(h2)
      
      p3 = exp(logit_p3)/(1+exp(logit_p3))
      x1b = rbinom(n_sim, h2, p3)
      ## plot 2B
      logit_p4 = mod_csim[,5] + mod_csim[,54:63] %*% x + as.matrix(mod_csim[,64]) * as.matrix(pa) +
        as.matrix(mod_csim[,65]) * as.matrix(h2)
      
      p4 = exp(logit_p4)/(1+exp(logit_p4))
      x2b = rbinom(n_sim, h2, p4)
      ## plot 3b
      x3b = rbinom(n_sim, h2, max(1-p3-p4, 0))
      ## plot Ks
      logit_p6 = mod_csim[,6] + mod_csim[,66:75] %*% x + as.matrix(mod_csim[,76]) * as.matrix(pa) +
        as.matrix(mod_csim[,77]) * as.matrix(bip)  + as.matrix(mod_csim[,78]) * as.matrix(h2)
      p6 = exp(logit_p6)/(1+exp(logit_p6))
      k = rbinom(n_sim, (pa-bip), p6)
      ## plot HR
      logit_p7 = mod_csim[,7] + mod_csim[,79:88] %*% x + as.matrix(mod_csim[,89]) * as.matrix(pa) +
        as.matrix(mod_csim[,90]) * as.matrix(bip)  + as.matrix(mod_csim[,91]) * as.matrix(h2) +
        as.matrix(mod_csim[,92]) * as.matrix(k)
      
      p7 = exp(logit_p7)/(1+exp(logit_p7))
      hr = rbinom(n_sim, (pa-bip), p7)
      ## plot H
      h = h2 + hr     
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
      sharpe = (batwar-1.3)/sd(batwar)
      
      data = as.data.frame(cbind(pa, ab, h, x1b, x2b, x3b, hr, bb, k, sb, cs, avg, obp, slg, ops, woba, batwar, sharpe))
      
      round(psych::describe(data), 3)
    } else {
      name_p = input$name_p
      stat_p = input$stat_p
      row = data_pred3_p[which(data_pred3_p$Name == name_p), ]
      x = as.numeric(row[,4:13])
      
      loglam = mod_csim_p[,1] + mod_csim_p[,14:23] %*% x
      lam = exp(loglam)
      n_sim = length(lam)
      if(input$prorated == 'Yes'){
        lam = 600
        n_sim = 3000
      }
      ## num_sims
      ## plot PAs
      tbf = rpois(n_sim, lam)
      ## plot BIP
      logit_p1 = mod_csim_p[,9] + mod_csim_p[,118:127] %*% x + mod_csim_p[,128] * tbf
      p1 = exp(logit_p1)/(1+exp(logit_p1))
      bip = rbinom(n_sim, tbf, p1)
      
      ## plot H - HR
      logit_p2 = mod_csim_p[,10] + mod_csim_p[,129:138] %*% x + mod_csim_p[,139] * bip
      p2 = exp(logit_p2)/(1+exp(logit_p2))
      h2 = rbinom(n_sim, bip, p2)
      ## plot K
      logit_p3 = mod_csim_p[,11] + mod_csim_p[,140:149] %*% x + mod_csim_p[,150] * tbf + 
        mod_csim_p[,151] * bip
      p3 = exp(logit_p3)/(1+exp(logit_p3))
      k = rbinom(n_sim, (tbf-bip), p3)
      ## plot HR
      logit_p4 = mod_csim_p[,12] + mod_csim_p[,152:161] %*% x + mod_csim_p[,162]*tbf +
        mod_csim_p[,163]*bip
      p4 = exp(logit_p4)/(1+exp(logit_p4))
      hr = rbinom(n_sim, (tbf-bip), p4)
      ## plot H
      h = h2 + hr
      ## plot BBs
      bb = (tbf-bip)-(hr+k)
      ## plot IFFB
      logit_p5 = mod_csim_p[,13] + mod_csim_p[,164:173] %*% x + mod_csim_p[,174]*tbf +
        mod_csim_p[,175]*bip + mod_csim_p[,176]*k + mod_csim_p[,177]*hr
      p5 = exp(logit_p5)/(1+exp(logit_p5))
      iffb = rbinom(n_sim, (tbf-bip), p5)
      ## plot ABs
      ab = tbf - bb
      ## plot Outs
      colnames(mod_csim_p)[24:33]
      loglam2 = mod_csim_p[,2] + mod_csim_p[,24:33] %*% x + mod_csim_p[,34]*tbf +
        mod_csim_p[,35]*bip + mod_csim_p[,36]*k + mod_csim_p[,37]*hr
      lam2 = exp(loglam)
      outs = rpois(n_sim, lam2)
      ## plot G
      loglam7 = mod_csim_p[,8] + mod_csim_p[,108:117] %*% x
      lam7 = exp(loglam7)
      g = rpois(n_sim, lam7)
      ip = outs/3
      fip = (13*hr + 3*(bb) - 2*k)/ip + 3.140
      ## plot W
      colnames(mod_csim_p)[38:47]
      logitp10 = mod_csim_p[,3] + mod_csim_p[,38:47] %*% x + mod_csim_p[,48]*tbf +
        mod_csim_p[,49]*bip + mod_csim_p[,50]*k + mod_csim_p[,51]*hr + mod_csim_p[,52]*outs
      p10 = exp(logitp10)/(1+exp(logitp10))
      w = rbinom(n_sim, g, p10)
      ## plot ER
      logitp12 = mod_csim_p[,4] + mod_csim_p[,53:62] %*% x + mod_csim_p[,63]*tbf +
        mod_csim_p[,64]*hr
      p12 = exp(logitp12)/(1+exp(logitp12))
      er = rbinom(n_sim, tbf, p12)
      era = er/(ip/9)
      ## plot L
      loglam5 = mod_csim_p[,6] + mod_csim_p[,83:92] %*% x + mod_csim_p[,93]*tbf +
        mod_csim_p[,94]*bip + mod_csim_p[,95]*k + mod_csim_p[,96]*hr + mod_csim_p[,97]*outs
      lam5 = exp(loglam5)
      l = rpois(n_sim, lam5)
      ## plot SV
      loglam6 = mod_csim_p[,7] + mod_csim_p[,98:107] %*% x
      lam6 = exp(loglam6)
      sv = rpois(n_sim, lam6)
      
      
      iffipc = 3.34
      iffip = ((13*hr)+(3*(bb))-(2*(k+iffb)))/(outs/3) + iffipc
      k9 = k/(ip/9)
      bb9 = bb/(ip/9)
      whip = (bb+h)/ip
      
      adjust = 4.52 - 4.15
      fipr9 = iffip + adjust
      row2 = data_pred2_p[which(data_pred2_p$Name == name_p),]
      row2 = row2[which(row2$Season == 2018),]
      rownum = which(as.character(pf$Team) == as.character(row2$Team))
      pf2 = pf[rownum,]$FIP
      if (length(pf2) == 0) {
        pf2 = 100
      }
      pfipr9 = fipr9/(pf2/100)
      aliffb = (92439-20425-7666-319-996)*.364*.105
      nliffb = (92700-20782-20782-610-926)*.343*.101
      if (as.character(row2$Team) %in% c('Yankees', 'Orioles', 'Red Sox', 'Blue Jays', 'Rays',
                                         'Indians', 'White Sox', 'Twins', 'Tigers', 'Royals',
                                         'Athletics', 'Astros', 'Angels', 'Mariners', 'Rangers')){
        lgfipr9 = ((13*2932)+(3*(7666 + 996))-(2*(20425+aliffb)))/ 21660.66666 + iffipc 
      } else {
        lgfipr9 = ((13*2653)+(3*(8020+926))-(2*(20782+nliffb)))/ 21828.3333 + iffipc 
      }
      
      ipg = (outs/3)/32
      drpw = (((((18-ipg)*lgfipr9)+(((outs/3)/row2$G)*pfipr9)/18) + 2)*1.5)
      
      raap9 = lgfipr9 - pfipr9
      wpgaa = raap9/drpw
      repl = 0.03*(1-(row2$GS/32)) + 0.12*(row2$GS/32)
      wpgar = wpgaa + repl
      warf = wpgar * (outs/27)
      correction = -0.000682902*(outs/3)
      war = warf + correction
      sharpe = (war-1.3)/sd(war)
      data_p = as.data.frame(cbind(w, l, era, g, sv, ip, h, er, hr,
                                   k, bb, whip, k9, bb9, fip, war, sharpe))
      
      round(psych::describe(data_p), 3)
    }
    
  })
  
  output$about = renderUI({
    tags$div(
      tags$h2('About Me:'),
      tags$ul(
        tags$li('I am a Senior at Emory University graduating in May 2019 with a Major in Quantitative Science focusing in Economics.'),
        tags$li('My research is focused on empirical modeling of baseball performance using several methods from bayesian statistics, causal inference, and machine learning.'),
        tags$li('First Place - American Statistical Association - Emory Datafest 2019')
      ),
      tags$h2('More About This Method:'),
      tags$ul(
        tags$li('Forthcoming')
      ),
      tags$h2('Previous Research:'),
      tags$ul(
        tags$li(tags$a(href="https://community.fangraphs.com/we-were-wrong-about-the-home-run-derby-curse/", "Fangraphs Community: We Were Wrong About the Home Run Derby Curse")),
        tags$li(tags$a(href="https://community.fangraphs.com/revisiting-changes-in-spin-rate-and-spin-surgers/", "Fangraphs Community: Revisiting Changes in Spin Rate and Spin-Surgers")),
        tags$li(tags$a(href="https://community.fangraphs.com/gerrit-cole-and-the-pine-tar-controversy/", "Fangraphs Community: Gerrit Cole and the Pine Tar Controversy")),
        tags$li(tags$a(href="https://community.fangraphs.com/revisiting-changes-in-spin-rate-and-spin-surgers/", "Fangraphs Community: Revisiting Changes in Spin Rate and Spin-Surgers")),
        tags$li(tags$a(href="https://community.fangraphs.com/stop-throwing-fastballs-to-the-astros/", "Fangraphs Community: Stop Throwing Fastballs to the Astros")),
        tags$li(tags$a(href="https://community.fangraphs.com/lonnie-chisenhall-finding-his-footing/", "Fangraphs Community: Lonnie Chisenhall: Finding His Footing")),
        tags$li(tags$a(href="https://community.fangraphs.com/brad-peacock-finally-showing-his-feathers/", "Fangraphs Community: Brad Peacock: Finally Showing His Feathers")),
        tags$li(tags$a(href="https://community.fangraphs.com/wikileakes-what-went-wrong-for-mike-leake/", "Fangraphs Community: WikiLeakes: What Went Wrong for Mike Leake?")),
        tags$li(tags$a(href="https://community.fangraphs.com/why-is-nobody-talking-about-adam-duvall/", "Fangraphs Community: Why Is Nobody Talking About Adam Duvall?"))
      ),
      
      tags$h2('Contact Info:'),
      tags$ul(
        tags$li('Email: saul.forman@emory.edu'),
        tags$li(tags$a(href="https://www.linkedin.com/in/saul-forman-43800b142/", "LinkedIn")),
        tags$li(tags$a(href="https://github.com/saulforman", "GitHub"))
      )
    )
    
    })
  
  output$tab1 = DT::renderDataTable({
    if(input$leaderboard == 'Upside') {
      sort = t2[order(-t2$skew),]
      sort
    } else if(input$leaderboard == 'Bust Potential') {
      sort = t2[order(t2$skew),]
      sort
    } else if(input$leaderboard == 'High Variance') {
      sort = t2[order(-t2$sd),]
      sort
    } else {
      sort = t2[order(t2$sd),]
    }
  })
  
  output$method = renderUI({
    tags$div(
      tags$h2('Bayesian Hierarchical Projection in Major League Baseball', align = 'center'),
      tags$h2('1. Introduction'),
      tags$p('Public sabermetric research has largely focused on making precise predictions of future performance but has, for the most part, ignored the question of uncertainty. There is, of course, awareness of the underlying uncertainty, but there has been little effort to describe and quantify the uncertainty in precise terms. Most public projection systems output point estimates and public researchers suggest that the output is at the upper limit of predictive accuracy, and hence should be treated as a near certainty. While these approaches serve as a useful benchmark for predicting the average performance among similar players, they fail to adequately represent the range of possible outcomes for each individual player, along with some representation of how likely each of those outcomes is.'),
      tags$p('In this paper, we propose a method for modeling the uncertainty surround a player’s projected outcomes. We then suggest an approach to valuing the associated projection as a function of both the expected value and the standard deviation. '),
      tags$p('The main technical details are as follows. Our approach is based on Bayesian inference. The mathematical calculations cannot be carried exactly, so variance of a player’s projected performance was estimated using a hierarchical Poisson regression using the “Just Another Gibbs Sampler” (JAGS) library in R. Parameter estimation was performed using Markov Chain Monte Carlo (MCMC) simulation and states were accepted using the Metropolis-Hastings algorithm. The estimated outcomes were then linearly combined using the Wins Above Replacement formula to create an uncertain estimate of WAR. '),
      tags$p('Finally, we borrow a concept from the field of quantitative finance, the Sharpe ratio, which is one approach to analyzing possible investments with the goal of maximizing the expected returns while minimizing the risk. We use the analogue of the Sharpe ratio to weight the distribution of WAR to create a risk-adjusted valuation for each player. For those interested in exploring further, an interactive Shiny web application was developed as a proof-of-concept of the value of “weaponizing risk” in the player market.'),
      tags$h2('2. Research Importance'),
      tags$p('The study of projecting uncertainty, particularly in baseball performance outcomes, has gone without significant treatment in the public sabermetric literature. Specifically, the analysis of uncertainty in baseball performance typically falls short in three areas. '),
      tags$p('First, existing applications will report the standard deviation of a player’s performance. This, however, assumes that a player’s performance is normally distributed around their mean, which is often not an accurate assumption when it comes to estimating “count” statistics (non-negative, discrete events like singles, doubles, homeruns, etc.). More specifically, the variance surrounding a player’s performance is often directly associated with the expected outcome. In other words, the larger your prediction for a given outcome, the more likely it is to vary by a greater amount, suggesting a Poisson process is more appropriate to model spread.'),
      tags$p('Second, as far as we can tell, existing literature only reports uncertainty as it pertains to descriptive statistics, i.e. analyzing the various uncertain elements that resulted in the player’s past performance. While this is important and insightful work, but we will argue that it is also useful to estimate predictive uncertainty for several front-office based use-cases detailed below.'),
      tags$p('Finally, existing projection systems treat each performance statistic as if they were independent of each other, which can result in significantly underestimating the uncertainty in their projections. Nate Silver details the importance of this approach in his article “Why FiveThirtyEight Gave Trump A Better Chance Than Almost Anyone Else”:'),
      tags$blockquote('“The single most important reason that our model gave Trump a better chance than others is because of our assumption that polling errors are correlated. No matter how many polls you have in a state, it’s often the case that all or most of them miss in the same direction. Furthermore, if the polls miss in one direction in one state, they often also miss in the same direction in other states, especially if those states are similar demographically.”'),
      tags$p('Two examples demonstrate this effect in baseball. Suppose there is a low, but non-zero probability that an expected everyday hitter only gets 200 plate appearances in a given year. First (and most apparently), that means there are fewer opportunities for that player to get hits and improve their wOBA. That means lower plate appearances directly impacts all of the player’s counting statistics. Second, the fact that the player in question only gets 200 plate appearances indirectly gives us information about that player. Perhaps they are injured. Maybe their true talent level has declined, and their manager has adjusted their playing time accordingly. Either way, projection systems should demonstrate how error is correlated across statistics.'),
      tags$h2('3. Literature Review'),
      tags$p('Founder of Driveline Baseball, Kyle Boddy, has written specifically about', tags$a('the uncertainty of data itself.', href = 'https://tht.fangraphs.com/pitching-mechanics-uncertainty-of-data-fear/'), 'Sabermetric reporting has done much to further public literacy with regards to important technological advances in baseball, particularly estimating ball flight dynamics. However, there has not been a proportional growth in the amount of skepticism placed in the data which itself relies on a series of arbitrary choices by the researcher and is captured by machines with an uncertain margin of error. The general analysis treats the data as perfect, but we should be carrying out our analysis with an eye towards quantifying the sensitivity to “noise.” While our method reflects the importance of measurement error, our Bayesian estimation procedure also takes into account uncertainty with regards to association of our covariates to predicted performance. In other words, we both estimate the uncertainty of collecting accurate data and the uncertainty of that data being important in our model.'),
      tags$p('Shane Jensen, Associate Professor Department of Statistics the Wharton School at the University of Pennsylvania has written about using Bayesian inference to predict when a hitter has reached “elite” status in their professional career and has used those results to plot aging curves for players of different (elite versus non-elite)', tags$a('tiers at their respective positions.', href='http://www-stat.wharton.upenn.edu/~stjensen/papers/shanejensen.traj09.pdf')),
      tags$p('Jonathan Judge, author at Baseball Prospectus, has employed a method called “Bayesian Bagging” to estimate uncertainty surrounding pitch-receiving. This process revolves around continuously sampling with replacement from the data to train the parameters to a ', tags$a('probability distribution on each sample.', href='https://www.baseballprospectus.com/news/article/38289/bayesian-bagging-generate-uncertainty-intervals-catcher-framing-story/'), 'This article makes a very strong case for quantifying measurement uncertainty in skills as subtle as pitch-receiving. Our methodology seeks to extend the idea of measuring uncertainty proactively to posterior predictive distributions.'),
      tags$p('This paper seeks to add to this existing literature by displaying a hierarchical method for prediction of future seasons performance, describing how one would use this tool to comprehensively value a player, and building an interactive web application for those purposes. We advocate for the use of MCMC methods as opposed to bagging for our projections. First, we find it important to allow for prior information to be incorporated into the model and not purely used for regularization purposes (for example our normal prior on our fixed-effects intercept). In particular, incorporating subjective beliefs through prior probability distributions could provide an answer to the question of how best to fuse qualitative scouting reports and quantitative, empirical performance data. Second, the analysis done in Baseball Prospectus relies on checking the results against preliminary MCMC estimation. Because our posteriors are future predictive distributions, we do not have that luxury and should be willing to sacrifice some computation time for predictive accuracy, especially if this is to be a tool for Front Office decisionmakers spending millions of dollars on players. '),
      tags$h2('4. Variable Selection'),
      tags$h3('Included Variables'),
      tags$p('The variables were based on those tracked on Fangraphs.com, a website dedicated to sabermetric analysis that is highly respected in the baseball analytics community for accuracy of information.'),
      tags$p('Fangraphs separates its seasonal leaderboard page into nine separate categories, several of which are included in our analysis. '),
      tags$p('A statistic was not included in our analysis if it was a perfect linear combination of other, included statistics (K%-BB%).'),
      tags$p('First, “Standard” statistics: This includes counts of G, AB, 1B, 2B, HR, etc.'),
      tags$p('Second, “Advanced” statistics: This includes K%, ISO, wRC, etc.'),
      tags$p('Third, “Batted Ball” statistics: This includes vertical position (GB%, LD%), horizontal position (Pull%, Oppo%), and exit velocity (Soft%, Hard%). '),
      tags$p('Fourth, “Pitch-type” statistics: This includes proportions of a pitch seen in a given year (FB%) along with the average velocity of that pitch (FBv).'),
      tags$p('Fifth, “Pitch-value” statistics: This is a statistic based around the increase in expected runs directly after a game event where a hitter saw a given pitch type. This in combination with our “pitch-type” statistics should give us a good measure of both how a hitter performs against a given pitch-mix, along with how likely it is that they see that pitch mix in a given season.'),
      tags$p('Lastly, “Plate Discipline” statistics: This includes in-zone swing statistics (Z-Swing%, Z-Contact%), out-of-zone swing statistics (O-Swing%, O-Contact%) and overall swing statistics (SwStr%).'),
      tags$p('All of these were collected over the three years preceding the year of prediction.'),
      tags$h3('Non-Included Variables:'),
      tags$p('Playing time (lineup and starting rotation) data was not included in our analysis. Because this application is meant to mimic the tools available in a Major League Front Office environment and because every outcome variable depends on Plate Appearances as the grandparent node, restricting a batter’s projection to lineup position could result in self-fulfilling or self-denying prophesy. Users of the application can easily pro-rate projected plate appearances by clicking the “pro-rate” button. This linearly transforms the plate appearance distribution to be centered around 600 plate appearances. Readers should note that this also increases the estimate of variance as indicated in the display.'),
      tags$p('Injury prediction is beyond the scope of this paper. There is an entirely distinct literature base detailing Bayesian estimation of injury risk as a result of specific movement patterns. While important for fully developing this application into a powerful front office tool, this tool as designed is solely meant to demonstrate a proof-of-concept for uncertainty-based predictions. Also, assessing injury with any accuracy would likely require access proprietary biometric data. Finally, we estimate recurring injury risk through including plate appearances in our covariate matrix pre-PCA. '),
      tags$p('Statcast exit velocity, launch angle, and sprint speed data was not included. Due to its limited availability before 2015, it would have required some arbitrary choices as well as detailed defense of time-varying missing values treatment which is beyond the scope of this paper.'),
      tags$p('Fielding and baserunning runs also were not estimated due to their use of proprietary Baseball Information Solutions data. Instead, “Hitting WAR” is estimated for each position player, as opposed to overall WAR.'),
      tags$p('Pitch receiving, caught stealing percentage, and other attributes of catching WAR were not included in this method.'),
      tags$h2('5. Data Preparation'),
      tags$p('For hitters, data was scraped from Fangraphs and joined into a comprehensive dataframe by Season and Player ID. The dataset was then filtered to exclude hitters who collected fewer than 15 at-bats in a given Season. While it is still important to predict statistics for players with small amounts of playing time, extreme variance in rates (K/9, BB/9) will bias our estimates of probabilities that we use in our hierarchy. Next, we replace missing values using K-Nearest Neighbors (KNN) imputation. The 10 nearest neighbors were used. Next, to increase computational efficiency, principal components analysis (PCA) was used to get a lower-dimensional representation of the data. A front office with more advanced computational resources or the use of Bayesian Deep Learning techniques that approximate posterior distributions may not require dimensionality reduction. The first 10 principal components were used in our model.'),
      tags$p('For pitchers, a similar approach was used to prepare the data. 30 total batters faced (TBF) were used as the cutoff to be included in the training group. However, the overall size of the data would not allow for computationally efficient KNN imputation, so the Expectation Maximization algorithm was used to approximate missing values conditional on observed values. Again, PCA was used and the first 10 principal components were used in the model.'),
      tags$h2('6. Model Formulation'),
      tags$p('To project end-of-season offensive statistics for a given player-year, we posit a Hierarchical Binomial model with a Poisson regression.'),
      tags$p('The hierarchical graph of plate appearance outcomes was adapted from Bauer and Zimbalist (2014). The directed acyclic graph for this hierarchy is illustrated below. For our analysis, we will not be estimating groundball and flyball outs separately. We will simply estimate hits in play (HIP) and subtract them from estimated balls in play (BIP). We will also jointly categorize BB (base on balls) and HBP (hit by pitch) as simply W (walks). Note that walks, home runs and strikeouts together are the at bats designated as TTO, the so-called Three True Outcomes.'),
      img(src='dag.png', height = 150, width = 300, style="display: block; margin-left: auto; margin-right: auto;"),
      tags$p('Our priors are specified below. If multiple variables are children nodes to the same parent node, their model specifications are included on the same line for the reader’s convenience.'),
      img(src='mod1.png', height = 200, width = 600, style="display: block; margin-left: auto; margin-right: auto;"),
      tags$p('The hyper-parameter for the grandparent node “plate-appearances” (PA) was estimated by regressing the log of lambda on the covariate matrix X which is composed of the ten largest principle components of the covariate matrix, a fixed effects term, denoted alpha, which was given an improper, flat prior, and the parent nodes of the performance variable. This is what gives us the ability to correlate errors across variables. '),
      tags$p('Suppose the fixed effect of plate appearances is two standard deviations below the mean (in this case, because plate appearances are a Poisson process, both the mean and variance are simply lambda). Not only does this alter the number of trials used in the Binomial process for the child node balls in play, but it alters the estimate of the probability of a ball in play for each trial.'),
      img(src='mod2.png', height = 80, width = 240, style="display: block; margin-left: auto; margin-right: auto;"),
      tags$p('Additionally, the binomial probabilities connecting each node to its parent are estimated using a logistic regression of p on the ten largest principle components of the covariate matrix. Finally, each coefficient used in each logistic regression was given a double exponential prior (otherwise referred to as the Laplace prior).'),
      img(src='mod3.png', height = 100, width = 300, style="display: block; margin-left: auto; margin-right: auto;"),
      tags$p('The Laplace prior is used as the Bayesian equivalent of L1-regularization (also referred to as “LASSO regression,” or the “Least Absolute Shrinkage and Selection Operator”), to induce feature selection and generalize well to new data, which is crucial in the player projection context.'),
      tags$p('For pitching statistics, a similar approach was used to hierarchically estimate terminal nodes as results from each at-bat. However, instead of separately estimating 1B, 2B, and 3B, we estimate only the components necessary to compute WAR (H, K, HR, and IFFB) from total batters faced. Other counting statistics that are traditionally output by projection models are also included such as wins, losses, saves, and games. '),
      img(src='mod4.png', height = 225, width = 300, style="display: block; margin-left: auto; margin-right: auto;"),
      tags$p('ERA was also estimated outside of the hierarchy using a normal regression.'),
      img(src='mod5.png', height = 120, width = 240, style="display: block; margin-left: auto; margin-right: auto;"),
      tags$p('Estimation was conducted using Markov Chain Monte Carlo (MCMC) simulation with a 500-point burn-in period and 3 chains with 1,000 simulations each. After simulation, the trace plots were examined, and convergence was assessed for each parameter. '),
      tags$h2('7. Use Cases'),
      tags$p('One can imagine several scenarios in which it might be beneficial for a front office to be able to quantify the uncertainty surrounding a player’s projections.'),
      tags$p('First, in some situations, one could make the case that a smart general manager should be risk-averse. For example, the marginal value of a win diminishes after it is already likely a team makes the playoffs. That is, suppose you are the manager of a returning playoff team projected for 90 wins and the second-place team in the division is projected at 85 wins. The value of winning games 85-90 of the year far outweigh the benefits of winning games 91-100. In this case, it may not make sense to play a high-risk, high-reward player because the benefits of a breakout are minimal while the costs of a bust are substantial.'),
      tags$p('Second, there has been increasing emphasis on building a strong team “core” of three or four low-variance players and subsequently adding pieces around that core when it comes time for that team to compete. What allows front offices to go on the offensive and make long-term deals for high-impact players is the certainty that the team will be competitive. Having a high-variance team forfeits the roster flexibility that comes with knowing that your team is likely to still be playing in October.'),
      tags$p('There are, on the other hand, situations in which a general manager may wish to acquire players with a higher variance of outcomes, especially as this includes the possibility of great performance (i.e., players with significant upside potential) along with a greater possibility of a bust. However, the downside performance risk is limited by the expected performance of the best available replacement players, making a high-risk player a potentially beneficial gamble. '),
      tags$p('That is, suppose a general manager has two above-average middle infielders (2B and SS) and wishes to fill an empty bench slot with a third. It is likely that the expected (average) projected performance of those established players is higher than that of an available player on the free agent market. It could be unwise for a general manager to acquire a player who they are almost certain will be worse than the players currently on their roster. However, acquiring a player with a wider variance in outcomes would make sense, as their risk is capped at replacement level. This means that in this particular situation, the worst possible outcome is that this high-variance player serves as a backup or is sent to the Minor Leagues in exchange for a replacement-level player, while the best possible outcome is that this high-variance player becomes even better than the regular middle infielders (a potential outcome that was significantly less likely had they signed a low-variance player).'),
      tags$p('Second, because free agency is a market where prices are implicitly determined by demand from teams throughout the league, if one team values players based off of their risk while others value players based off of their expected value, teams can find potential All-Star level talent at a fraction of the standard cost.'),
      tags$p('Third, due to loss-aversion, general managers may be discounting high-risk high-reward players too much on the free agent market. Being able to rationally diagnose uncertainty in outcomes and assign risk-adjusted values to them would allow for potentially season-altering signings which otherwise would not have happened at a substantial discount.'),
      tags$p('According to this logic, it makes sense for teams to acquire a diversified roster of high and low-variance players, much like an investment manager diversifies the risk in their portfolio. In particular, identifying areas of correlated risk (a sharp increase in the number of league-wide homeruns, changes in the ball causing blisters, disruption of high-tech player development tools, etc.) and building a team with several high-reward players that would only be affected by one or two of those eventualities would minimize team-wide risk while maximizing expected returns.'),
      tags$p('To account for this, we calculate the “Sharpe-ratio” as it relates to Wins Above Replacement for a projected player-year. The formula for the Sharpe ratio is below:'),
      img(src='sharpe.png', height = 120, width = 240, style="display: block; margin-left: auto; margin-right: auto;"),
      tags$p('Here, however, we will be using Wins Above Replacement as the stand-in for the return of our portfolio (our player). We will hold-off on assigning a value to a player’s win total due to recent work on the non-linearity of win value given a team’s context. Instead of a risk-free rate of return, we will be using 1.3 WAR as the “average” WAR of a starting position player. Because we are interested in evaluating risk-adjusted value of on-field performance, we should consider an average player who compiles a similar number of at-bats as “risk free”. Otherwise, we rig the comparison and overvalue players simply due to their context (opportunity for at-bats). Finally, because we are only evaluating the risk-adjusted value of one player and not an entire team (or portfolio) of players, we simply divide by the standard deviation of that player’s WAR projection. More work should be done on diversifying between-player risk on a team as stated above.'),
      tags$h2('8. Results and Interpretation'),
      tags$p('Suppose we are a general manager in need of an outfielder and are interested in comparing a wOBA projection for Melky Cabrera and Bryan Goodwin. We input both players into the system and are shown the following kernel density plot.'),
      img(src='melk.png', height = 400, width = 600, style="display: block; margin-left: auto; margin-right: auto;"),
      tags$p('Upon first glance, it is apparent that both players have a similar expected outcome around .325 wOBA. However, Brian Goodwin’s projection has a much higher variance than Melky Cabrera’s. This means that the probability that Melky Cabrera ends the season with a wOBA of around .325 is higher than the probability of Brian Goodwin achieving a similar result. However, the probability that Brian Goodwin ends the season with a much higher (.400 wOBA) or much lower (.250 wOBA) result is about twice as large as Melky Cabrera’s. As such, a risk-seeking General Manager may prefer acquiring Brian Goodwin and a risk-averse General Manager may prefer acquiring Melky Cabrera.')
    )
  })

})
