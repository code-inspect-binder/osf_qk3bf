####################################################
####################################################
###
###                   SITAR 2018
###
###  The Latent Structure of Interpersonal Problems
###            
###   22th June; Montreal; Talk 10.30; Leon Wendt
###      l.wendt@psychologische-hochschule.de
###
######
########

#   Note: This R code can be used to (1) fit a bifactor
#         CFA model to the Inventory of Interpersonal
#         Problems and to (2) visualize the model.

# iip = inventory of interpersonal problems
#   example data (iip_dat.csv) is freely available:

# read iip data
dat <- read.csv("C:/Users/Sera/Desktop/R Daten/iip_dat.csv", sep = ",") 

#   Huibers MJH, Cohen ZD, Lemmens LHJM, Arntz A, Peeters FPML, Cuijpers P, 
#   DeRubeis RJ (2015) Data from: Predicting optimal outcomes in cognitive 
#   therapy or interpersonal psychotherapy for depressed individuals using 
#   the Personalized Advantage Index approach. Dryad Digital Repository. 
#   https://doi.org/10.5061/dryad.m112v

# note: the octants must be named: pa, bc, de, fg, hi, jk, lm, no

# load/install required packages
if(!require(MASS)){install.packages("MASS")}  
if(!require(lavaan)){install.packages("lavaan")} 
if(!require(ggplot2)){install.packages("ggplot2")} 
if(!require(reshape2)){install.packages("reshape2")} 

# z-transform octant scores
dat[,c("pa", "bc", "de", "fg", "hi", "jk", "lm", "no")] <- 
  scale(dat[,c("pa", "bc", "de", "fg", "hi", "jk", "lm", "no")], center = TRUE, scale = TRUE)

# bifactor model specifiction based on the Interpersonal Circumplex
model <- 'gen =~ l1*pa + l1*bc + l1*de + l1*fg + l1*hi + l1*jk + l1*lm + l1*no
         agency =~ l2*pa + start(1)*pa + l3*bc + l3.n*fg + l2.n*hi + l3.n*jk + l3*no
         commun =~ l3.n*bc + l2.n*de + l3.n*fg + l3*jk + l2*lm +start(1)*lm + l3*no
         agency ~~ 0*commun
         l2 > 0
         l2.n == -1*l2
         l3.n == -1*l3
         l3 == 0.707*l2'

# estimate bifactor model (maximum likelihood)
fit <- cfa(model, dat,estimator = "MLM", std.lv = TRUE) 

# show fit indices & model parameters
summary(fit, standardized = TRUE, fit.measures = TRUE) 

# save model parameters
par <- coef(fit) 


##################################
##################################
###
###     Simulate Random Observations
###     Based on your CFA model
###
##################################
##################################

# specify number of draws
n <- 5 

# model-implied factor correlation matrix 
r1 <-  0        
r2 <-  par["gen~~agency"]
r3 <-  par["gen~~commun"]

sigma <- matrix(c(1, r2, r3, r2, 1, 0 ,r3, 0, 1), nrow = 3)
mu <- c(0,0,0)

# draw random factor scores from a multivariate normal distribution
lat_scores <- mvrnorm(n = n, mu = mu, Sigma = sigma)

# save randomly drawn factor scores
gen <- lat_scores[,1]
agency <- lat_scores[,2]
commun <- lat_scores[,3]

sim_dat_cfa <- data.frame(matrix(ncol = 8, nrow = n))
colnames(sim_dat_cfa) <- c("pa", "bc", "de", "fg", "hi", "jk", "lm", "no")

# compute octant scores (random error is not included)
sim_dat_cfa$pa <- gen* par["l1"] + agency* par["l2"] + commun* 0 
sim_dat_cfa$bc <- gen* par["l1"] + agency* par["l3"] + commun* par["l3.n"] 
sim_dat_cfa$de <- gen* par["l1"] + agency* 0 + commun* par["l2.n"] 
sim_dat_cfa$fg <- gen* par["l1"] + agency* par["l3.n"] + commun* par["l3.n"]
sim_dat_cfa$hi <- gen* par["l1"] + agency* par["l2.n"] + commun* 0 
sim_dat_cfa$jk <- gen* par["l1"] + agency* par["l3.n"] + commun* par["l3"] 
sim_dat_cfa$lm <- gen* par["l1"] + agency* 0 + commun* par["l2"] 
sim_dat_cfa$no <- gen* par["l1"] + agency* par["l3"] + commun* par["l3"]

sim_dat_cfa$id <- 1:n
plot_oct <- melt(sim_dat_cfa,id.vars = "id")
plot_ipc <- data.frame(gen, agency, commun)


##################################
##################################
###
###     Plot Random Observations
###
##################################
##################################

# plot simulated octant scores
octant <- ggplot(plot_oct ,aes(x = variable, y = value, group = id)) + geom_line(color = "black",alpha = 0.6, size = 0.8) +
          coord_cartesian(ylim = c(-3,3)) +
          xlab("Interpersonal Problem Octant") +  ylab("z") +  theme_bw() +
          theme(text=element_text(size=12, family="serif"))+
          theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
                legend.position = "none", panel.border = element_blank()) +
          theme(axis.line = element_line(color = 'black'))
octant

# plot simulated factor scores on the IPC
ipc <- ggplot(plot_ipc ,aes(x = agency, y = commun)) + 
       geom_point(color = "darkgreen",alpha = 0.50) +
       coord_cartesian(ylim = c(-3,3), xlim = c(-3,3)) +
       xlab("Communion") +  ylab("Agency") +  theme_bw() +
       theme(plot.background = element_blank(),panel.grid.major = element_blank(),panel.grid.minor = element_blank(), 
            legend.position = "none", panel.border = element_blank()) +
       theme(axis.line = element_blank()) + 
       geom_hline(yintercept = 0) +
       geom_vline(xintercept = 0) +
       scale_shape(solid = FALSE)
#ipc

