# BayesBall
Overview of Project:
This project is aimed at projecting future player performance using Bayesian Hierarchical Poisson regression 
as a probability distribution over outcomes instead of a simple point estimate. We do this by using a 
graphical structure of at-bat outcomes, modeling plate appearances, and then using each simulated parent
node to estimate each child node.

Connections with:
None

Packages used:
projections / projections_p: bnstruct, dplyr, tidyr, rjags, runjags, stringr, missMDA
server / ui: shiny, ggplot2, plotly, psych, reshape2, pracma, robustbase
input data (data_pred3, data_pred3_p, etc.): available upon request

Output:
The final output is a shiny application deployed at https://www.shinyapps.io/admin/#/application/858636

Details:
Extensive Write-Up and Full Model Specification Available Here: 
https://community.fangraphs.com/projecting-risk-in-major-league-baseball-a-bayesian-approach/

The variables used in the projection were all taken from the FanGraphs seasonal leaderboard. 
These include “standard” (G, AB, 1B), “advanced” (K%, ISO, wRC), “batted ball” (GB%, Pull%, Soft%), “pitch type” (FB%, FBv), 
“pitch value” (wFB), and “plate discipline” (O-Swing%, Z-Contact%) statistics. All of these were collected over 
the three years preceding the year of prediction and variables that were perfect linear combinations of others 
were deleted (like K%-BB%). Playing time, injury prediction, and Statcast data were not included in this application. 
In particular, plate appearances were estimated using a combination of previous major league performance and plate 
appearances from the previous three years. As a result, the model does not project accurately for recent call-ups. 
In an attempt to remedy this without manually and arbitrarily altering plate appearances, a “pro-rate” button was 
added which linearly transforms the plate appearance distribution to be centered around 600 plate appearances.

Missing data was imputed using K-Nearest Neighbors (KNN) imputation using the 10 most comparable players from 2010-18. Due to computational resources and the strenuous task of Bayesian inference, principal components analysis (PCA) was used to obtain a lower-dimensional representation of the data and we only used the first 10 principal components for our computation. A similar approach was used to prepare the data for pitchers, however the size of the matrix made KNN imputation infeasible, so the Expectation Maximization (EM) algorithm was used to fill in missing values.

The mathematical calculations cannot be carried out exactly, so variance of a player’s projected performance was estimated using a hierarchical Poisson regression using the “Just Another Gibbs Sampler” (JAGS) library in R. Parameter estimation was performed using Markov Chain Monte Carlo (MCMC) simulation and states were accepted using the Metropolis-Hastings (MH) algorithm. The estimated outcomes were then linearly combined using the Wins Above Replacement formula to create an uncertain estimate of WAR.

For this app in particular, we chose the ex-ante Sharpe ratio as the method by which to evaluate risk, 
due to its interpretability (excess return per unit of risk taken on by the investor / GM).
We use our calculated league average 1.3 Wins Above Replacement 
as the risk-free rate of return. This is not a trivial statement, 
because technically 0 Wins Above Replacement is truly “replacement level”. 
However, we are not using “replacement level” and “risk free” as synonyms. We are 
implicitly assuming that a major league team can, without assuming any risk in outcomes, 
acquire a player capable of producing at 1.3 WAR (and if not, you are comparing production 
to the best available minor league player, who is often times above replacement level).
