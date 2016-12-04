
# Load Packages
list.of.packages <- c("dplyr","R2jags","R2WinBUGS")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)
library(R2jags)
library(R2WinBUGS)

load("Data/npitch.Rdata")
pitch <- read.csv("Data/all_pitches_pred.csv")

# Change the type of Factor Variables
relief.lookback$year <- as.numeric(as.character(relief.lookback$year))
relief.lookback$pitcher <- as.numeric(as.character(relief.lookback$pitcher))
relief.lookback$date <- as.Date(relief.lookback$date)
relief.lookback$gameday_link <- as.character(relief.lookback$gameday_link)
relief.lookback$pitcher_name <- as.character(relief.lookback$pitcher_name)
relief.lookback[,6:13] <- sapply(relief.lookback[,6:13], function(x){
  as.numeric(as.character(x))
})
pitch$pitcher_name <- as.character(pitch$pitcher_name)
pitch$gameday_link <- as.character(pitch$gameday_link)

# Only Include Regular Season pitches from 2013-2015
reg_gameday <- pitch %>%
  filter(reg_season == "True") %>%
  select(gameday_link)

reg_gameday <- as.character(reg_gameday[,1])

relief.lookback <- relief.lookback %>%
  filter(gameday_link %in% reg_gameday)

pitch <- pitch %>%
  filter(gameday_link %in% reg_gameday)

# Aggregate Stuff (for each pitch)
# Overall Pitches
avg_stuff_all <- pitch %>%
  group_by(gameday_link, pitcher, pitcher_name) %>%
  summarise(mean_stuff = mean(z_stuff), num.pitches = n()) %>%
  filter(num.pitches >= 3) %>%
  select(-num.pitches) %>%
  arrange(desc(mean_stuff))

npitch_all <- inner_join(avg_stuff_all, relief.lookback, by = c("gameday_link","pitcher",
                                                                "pitcher_name"))
#Individual Pitches
avg_stuff_indpitch <- pitch %>%
  group_by(gameday_link, pitcher, pitcher_name, pitch_type) %>%
  summarise(mean_stuff = mean(z_stuff), mean_spin = mean(spin_rate),
            mean_velo = mean(start_speed), mean_vz0 = mean(vz0), 
            num.pitches = n()) %>%
  filter(num.pitches >= 3) %>%
  select(-num.pitches) %>%
  arrange(desc(mean_stuff))
 
# Four-Seam Fastballs
npitch_ff <- avg_stuff_indpitch %>%
  filter(pitch_type == "FF") %>%
  inner_join(relief.lookback)

# Two-Seam Fastballs
npitch_ft <- avg_stuff_indpitch %>%
  filter(pitch_type == "FT") %>%
  inner_join(relief.lookback)

# Curveballs
npitch_cu <- avg_stuff_indpitch %>%
  filter(pitch_type == "CU") %>%
  inner_join(relief.lookback)

# Changeups
npitch_ch <- avg_stuff_indpitch %>%
  filter(pitch_type == "CH") %>%
  inner_join(relief.lookback)

# Cutters
npitch_fc <- avg_stuff_indpitch %>%
  filter(pitch_type == "FC") %>%
  inner_join(relief.lookback)

# Splitters
npitch_fs <- avg_stuff_indpitch %>%
  filter(pitch_type == "FS") %>%
  inner_join(relief.lookback)

# Sinkers
npitch_si <- avg_stuff_indpitch %>%
  filter(pitch_type == "SI") %>%
  inner_join(relief.lookback)

# Sliders
npitch_sl <- avg_stuff_indpitch %>%
  filter(pitch_type == "SL") %>%
  inner_join(relief.lookback)

# All Fastballs
npitch_fast <- avg_stuff_indpitch %>%
  filter(pitch_type %in% c("FF","FT","FS")) %>%
  inner_join(relief.lookback)

# Frequentist Model
library(lme4)
lmer1 <- lmer(mean_stuff ~ (1|pitcher) + npitch1 + npitch2 + npitch3, 
              data = npitch_sl)
summary(lmer1)


# JAGS Model

runJAGSmodel <- function(pitch, y, n.iter = 2000, n.thin = 1){
  mod.data <- list(Y = as.numeric(scale(unlist(pitch[,y]))),
                   X1 = pitch$npitch1,
                   X2 = pitch$npitch2,
                   X3 = pitch$npitch3,
                   X4 = pitch$npitch4,
                   X5 = pitch$npitch5,
                   X6 = pitch$npitch6,
                   X7 = pitch$npitch7,
                   n = nrow(pitch),
                   pitcher = as.factor(pitch$pitcher),
                   n.pitcher = length(unique(pitch$pitcher))
  )
  
  
  pitch_model <- function(){
    for(i in 1:n){
      #mu[i] <- alpha[pitcher[i]] + beta[pitcher[i]]*(gamma[pitcher[i]]*X1[i] + gamma[pitcher[i]]^2*X2[i] + 
       #                                                gamma[pitcher[i]]^3*X3[i])# + gamma[pitcher[i]]^4*X4[i] + gamma[pitcher[i]]^5*X5[i])# + 
      #gamma^6*X6[i] + gamma^7*X7[i]
      mu[i] <- alpha[pitcher[i]] + beta[pitcher[i]]*(X1[i] + gamma*X2[i] + 
                                                      gamma^2*X3[i])# + gamma[pitcher[i]]^4*X4[i] + gamma[pitcher[i]]^5*X5[i])# + 
      #gamma^6*X6[i] + gamma^7*X7[i]
      Y[i] ~ dnorm(mu[i], tau)
    }
    
    tau ~ dgamma(.1,.1)
    
    sd.a ~ dt(0,.1,1)%_%T(0,)
    phi.a <- sd.a^(-2)
    
    sd.b ~dt(0,0.1,1)%_%T(0,)
    phi.b <- sd.b^(-2)
    
    
#     sd.g ~ dt(0,.1,1)%_%T(0,)
#     phi.g <- sd.g^(-2)
    
    #mu.g ~ dnorm(0,4)
    gamma ~ dt(0,.1,1)%_%T(0,)
    #gamma ~ dgamma(0.1,1)
    mu.b ~ dnorm(0,10)
    
    
    for(p in 1:n.pitcher){
      alpha[p] ~ dnorm(0,phi.a)
      beta[p] ~ dnorm(mu.b,phi.b)
      #gamma[p] ~ dnorm(mu.g,phi.g)
      
    }
  }
  
  # Create a function that provides intial values for WinBUGS
  pitch.inits = function() {
    alpha <- rep(0, mod.data$n.pitcher)
    #gamma <- rep(0, mod.data$n.pitcher)
    beta <- rep(0, mod.data$n.pitcher)
    sd.b <- .1
    sd.a <- 1
    sd.g <- 1
    mu.b <- -0.005
    #mu.g <- 0.5
    gamma = 0.2
    return(list(alpha=alpha, gamma=gamma,
                sd.a = sd.a, sd.g = sd.g,mu.b=mu.b))
  }
  
  
  parameters = c("alpha","gamma","sd.a", "sd.g","beta", "sd.b","mu.b")
  
  
  pitch.model.file = paste(getwd(),"Output","pitch-model.txt", sep="/")
  write.model(pitch_model, pitch.model.file)
  
  pitch.sim = jags(mod.data, inits=pitch.inits, parameters, model.file=pitch.model.file,
                   n.iter = n.iter, n.thin = n.thin)
  
  #Runs chain until convergence, returns samples post-burn-in
  pitch.bugs = as.mcmc(pitch.sim$BUGSoutput$sims.matrix)
  pitch.bugs
}


makeCoolPlots <- function(dataset, y){
  velo_model <- runJAGSmodel(dataset, y = y)
  
  (n.pitcher.ff <- length(unique(dataset$pitcher)))
  #Get output into matrices
  alpha.ff.mat <- gamma.ff.mat <- beta.ff.mat <- matrix(numeric(3000*n.pitcher.ff),ncol = n.pitcher.ff)
  for(p in 1:n.pitcher.ff){
    #gamma.ff.mat[,p] <- as.numeric(velo_model[,paste0("gamma[",p,"]")])
    alpha.ff.mat[,p] <- as.numeric(velo_model[,paste0("alpha[",p,"]")])
    beta.ff.mat[,p] <- as.numeric(velo_model[,paste0("beta[",p,"]")])
  }
  
  #Examine posterior means
  library(ggplot2)
  alpha.ff.mean <- apply(alpha.ff.mat,2,mean)
  alpha.ff.df <- data.frame(cbind(1:n.pitcher.ff), alpha.ff.mean)
  colnames(alpha.ff.df) <- c("pitcher", "mean")
  alpha.quantiles <- quantile(alpha.ff.mean, c(0.025, 0.975))
  line.df = data.frame(cbind(factor(c("m","v","v")), c(mean(alpha.ff.mean), alpha.quantiles)))
  colnames(line.df) = c("component", "value")
  rownames(line.df) = NULL
  line.df$component = factor(line.df$component)
  ggplot(alpha.ff.df, mapping = aes(x = pitcher, y = mean)) + 
    geom_point() + 
    geom_hline(data = line.df, aes(yintercept = value, linetype = component, colour = component), 
               lwd = 2) +
    theme(text = element_text(size=20, face = "bold")) +
    xlab("Reliever") +
    ylab(expression(alpha[i])) + 
    ggtitle("Pitch Quality Intercepts") +
    scale_colour_discrete(name="",
                          labels=c("Mean", "95% CI")) + 
    scale_linetype_discrete(name="",
                            labels=c("Mean", "95% CI"))
  
  filename = paste("Output/", paste(deparse(substitute(dataset)), y, "alpha.png", 
                                    sep = "_"), sep = "")
  
  ggsave(filename, width = 8, height = 5)
  
  
  beta.ff.mean <- apply(beta.ff.mat,2,mean)
  beta.ff.df <- data.frame(cbind(1:n.pitcher.ff), beta.ff.mean)
  colnames(beta.ff.df) <- c("pitcher", "mean")
  beta.quantiles <- quantile(beta.ff.mean, c(0.025, 0.975))
  line.df = data.frame(cbind(factor(c("m","v","v")), c(mean(beta.ff.mean), beta.quantiles)))
  colnames(line.df) = c("component", "value")
  rownames(line.df) = NULL
  line.df$component = factor(line.df$component)
  ggplot(beta.ff.df, mapping = aes(x = pitcher, y = mean)) + 
    geom_point() + 
    geom_hline(data = line.df, aes(yintercept = value, linetype = component, colour = component), 
               lwd = 2) +
    theme(text = element_text(size=20, face = "bold")) +
    xlab("Reliever") +
    ylab(expression(beta[i])) + 
    ggtitle("Fatigue Effect Coefficients") +
    scale_colour_discrete(name="",
                          labels=c("Mean", "95% CI")) + 
    scale_linetype_discrete(name="",
                            labels=c("Mean", "95% CI"))
  
  filename = paste("Output/", paste(deparse(substitute(dataset)), y, "beta.png", 
                                    sep = "_"), sep = "")
  
  ggsave(filename, width = 8, height = 5)
  
#   gamma.ff.mean <- apply(gamma.ff.mat,2,mean)
#   gamma.ff.df <- data.frame(cbind(1:n.pitcher.ff), gamma.ff.mean)
#   colnames(gamma.ff.df) <- c("pitcher", "mean")
#   gamma.quantiles <- quantile(gamma.ff.mean, c(0.025, 0.975))
#   line.df = data.frame(cbind(factor(c("m","v","v")), c(mean(gamma.ff.mean), gamma.quantiles)))
#   colnames(line.df) = c("component", "value")
#   rownames(line.df) = NULL
#   line.df$component = factor(line.df$component)
#   ggplot(gamma.ff.df, mapping = aes(x = pitcher, y = mean)) + 
#     geom_point() + 
#     geom_hline(data = line.df, aes(yintercept = value, linetype = component, colour = component), 
#                lwd = 2) +
#     theme(text = element_text(size=20, face = "bold")) +
#     xlab("Reliever") +
#     ylab(expression(phi[i])) + 
#     ggtitle("Recovery Rate Coefficients") +
#     scale_colour_discrete(name="",
#                           labels=c("Mean", "95% CI")) + 
#     scale_linetype_discrete(name="",
#                             labels=c("Mean", "95% CI"))
#   filename = paste("Output/", paste(deparse(substitute(dataset)), y, "phi.png", 
#                                     sep = "_"), sep = "")
#   
#   ggsave(filename, width = 8, height = 5)
  return(velo_model)
}


ff_stuff <- makeCoolPlots(npitch_ff, "mean_stuff")
ft_stuff <- makeCoolPlots(npitch_ft, "mean_stuff")
ff_velo <- makeCoolPlots(npitch_ff, "mean_velo")
ft_velo <- makeCoolPlots(npitch_ft, "mean_velo")
cu_spin <- makeCoolPlots(npitch_cu, "mean_spin")

means_ft_velo <- apply(ft_velo[,117:232], 2, mean)



