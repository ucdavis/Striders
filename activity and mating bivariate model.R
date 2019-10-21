library(lme4) 
library(MCMCglmm) 
library(tidyverse) 
library(broom) 
library(nadiv)

load("dfMale.rdata")

###first assign an uninformative prior; later you should check your model results 
AM_prior = list(R = list(V = diag(2), nu = 0.002),
                     G = list(G1 = list(V = diag(2), nu = 2, alpha.mu = rep(0,2),
                                        alpha.V = diag(25^2,2,2))))
####set up model with bivariate response variable being boldness and exploration bound together using cbind
#scale response variables as well (centered around the mean and standarized to units of 1 phenotypic standard deviation)
mcmc_AM <- MCMCglmm(cbind(scale(activity), scale(mating)) ~ trait-1 + trait:scale(period, scale = FALSE) + 
                          trait:(treatment), #we use the trait keyword to specify that this is a multivariate model, trait-1 tells the model to give us a distinct intercept for each trait; we interact trait with the fixed effects so that we get estimates for the effect of these variables on each of our behaviors
                        random =~ us(trait):id, #tells the model to fit an unstructured covariance matrix for the grouping variable ID, essentially we are calculating the variance in exploration due differences among individuals, the variance in boldness due to differences among individuals and the covariances between these variances
                        rcov =~ us(trait):units, #residual variance = within individual variation, because we have repeated measures for both traits at the individual level we also set aan unstructured covariance matrix; finds the residual variance for each trait and allows these variances to covary (???????)
                        family = c("gaussian","gaussian"), 
                        prior = AM_prior, #include model priors
                        nitt=420000, #total number of iterations
                        burnin=20000, #number of iterations to discard as the model starts
                        thin=100, #number of iterations to discard inbetween successive stored samples, helps to reduce autocorrelation in sampling
                        verbose = TRUE,
                        data = as.data.frame(dfMale))


plot(mcmc_AM$VCV)

                    