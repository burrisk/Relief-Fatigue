setwd("~/Dropbox/GS/Research/Sports/Baseball/Relief")
context <- read.csv("Data/context.csv")
pitch_split <- read.csv("Data/Pitching_Splits.csv")
pitch_stat <- read.csv("Data/Pitching_Statistics.csv")
pfx <- read.csv("Data/Pitchfx.csv")

names <- pitch_stat %>%
  group_by(pitcher_id,Name) %>%
  summarise(num_teams = n())


#Get time difference with player_id and game_id
player_game = pfx %>%
  left_join(names,by = 'pitcher_id') %>%
  group_by(game_date, game_id,pitcher_id, Name) %>%
  arrange(game_date,Name) %>%
  summarise(num_pitches = n(),
            single = sum(event_result == 'single'),
            double = sum(event_result == 'double'),
            triple = sum(event_result == 'triple'),
            hbp = sum(event_result == 'hit_by_pitch'),
            ubb = sum(event_result == 'walk') - sum(event_result == 'intent_walk'),
            ibb = sum(event_result == 'intent_walk'),
            hr = sum(event_result == 'home_run'),
            sf = sum(event_result == 'sac_fly'),
            ab = n_distinct(at_bat_number)
            )%>%
  ungroup() %>%
  arrange(pitcher_id, as.numeric(strptime(game_date, format = "%d-%b-%y"))) %>%
  group_by(pitcher_id) %>%
  mutate(date = as.numeric(strptime(game_date, format = "%d-%b-%y")),
         time_diff = date - lag(date, default = 0),
         time_diff = ifelse(date == time_diff, 0, time_diff),
         time_diff = time_diff/86400)


#Also genius
npitch = matrix(0, nrow = nrow(player_game), ncol = 7)
for(i in 1:nrow(player_game)){
  for(j in 1:7){
    index = ifelse((i-j)>0, i-j, NA)
    if(!is.na(index) &&(player_game$pitcher_id[i] == player_game$pitcher_id[(i-j)]) && (player_game$date[i] - player_game$date[(i-j)] <= 7*86400)){
      d = (player_game$date[i] - player_game$date[i-j])/86400
      npitch[i,d] = player_game$num_pitches[i-j]
    }
  }
}

player_game$npitch1 = npitch[,1]
player_game$npitch2 = npitch[,2]
player_game$npitch3 = npitch[,3]
player_game$npitch4 = npitch[,4]
player_game$npitch5 = npitch[,5]
player_game$npitch6 = npitch[,6]
player_game$npitch7 = npitch[,7]


# Mariano Rivera
rivera = player_game %>%
  filter(Name = "Rivera, Mariano") %>%
  mutate(l4days = npitch1 + npitch2 + npitch3 + npitch4)

####
#####GETTING RELIEVER STUFF DATASET
######
pitchFX = pfx %>%
  select(game_id,game_date,inning,top_inning_sw:pre_outs,pitcher_id,
         initial_speed:init_accel_z) %>%
  group_by(game_id,pitcher_id)  %>% arrange(inning)

firstPitches = pitchFX %>% filter(inning == 1 & pre_balls == 0
                                  & pre_strikes == 0 & pre_outs == 0)

starters = unique(firstPitches$pitcher_id)
pitchFX = pitchFX %>% mutate(starter = pitcher_id %in% starters) 

pitchFX = arrange(pitchFX,inning)
hist((pitchFX %>% filter(starter == 0))$inning)

#filtering
relievers = filter(pitchFX,starter == 0)
starters = filter(pitchFX,starter == 1)

## Reliever Analysis

#Subsetting
fastball.types = c("FA","FC","FF","FT","SI","FS")
offspeed.types = c("SL","KC","CH","CU")
relievers = relievers %>% filter(pitch_type %in% fastball.types 
                                 | pitch_type %in% offspeed.types) %>%
  mutate(fastball = pitch_type %in% fastball.types)


# Calculate Average Velocity of Reliever Fastball
relieverID = unique(relievers$pitcher_id)


relievers.fastballs = relievers %>% filter(fastball == TRUE) %>%
  group_by(pitcher_id) %>%
  summarise(mean_vel = mean(initial_speed)) %>%
  left_join(relievers, by = 'pitcher_id')

relievers = relievers.fastballs
rm(relievers.fastballs)

# Calculate Stuff
relievers = relievers %>%
  mutate(abs_break_x = abs(break_x)) %>%
  mutate(abs_break_z = abs(break_z)) %>%
  mutate(totalMov = sqrt(break_z^2 + break_x^2))

#Fastball Stuff
relievers.fastballs = relievers %>%
  filter(fastball == T) 

relievers.fastballs = relievers.fastballs %>%
  mutate(stuff = scale(relievers.fastballs$initial_speed) + 
           scale(relievers.fastballs$totalMov)) %>%
  mutate(velStuff = scale(relievers.fastballs$initial_speed)
  ) %>%
  mutate(movStuff = scale(relievers.fastballs$totalMov))



relievers.offspeed = relievers %>%
  filter(fastball == FALSE)

relievers.offspeed = relievers.offspeed %>%
  mutate(stuff = scale(relievers.offspeed$initial_speed) + 
           scale(relievers.offspeed$totalMov) +
           scale(relievers.offspeed$mean_vel - relievers.offspeed$initial_speed)
  ) %>%
  mutate(velStuff = scale(relievers.offspeed$initial_speed) + 
           scale(relievers.offspeed$mean_vel - relievers.offspeed$initial_speed)
  ) %>%
  mutate(movStuff = scale(relievers.offspeed$totalMov))

relievers = rbind(relievers.fastballs,relievers.offspeed)


new_dset <- relievers %>%
  left_join(names,by = 'pitcher_id') %>%
  group_by(pitcher_id,Name) %>%
  summarise(mean_stuff = mean(stuff))


new_dset = as.data.frame(new_dset[1:568,1:3])
new_dset = new_dset %>%
  arrange(desc(mean_stuff))

#Final stuff dataset
#stuff = rbind(new_dset,starter_new_dset) #starter_new_dset not found
stuff <- new_dset

#Plots 
pitcherStats = read.csv("Pitching_Statistics.csv")

pitcherStats = pitcherStats %>%
  filter(TBF > 100) %>%
  group_by(pitcher_id) %>%
  summarise(fip = mean(FIP))

stuff = left_join(stuff,pitcherStats, by = 'pitcher_id')

plot(stuff$mean_stuff,stuff$fip, xlab = "Mean Stuff", 
     ylab = "FIP", main = "FIP vs Stuff (All Pitchers)",
     pch = 19, cex = .7, xlim = c(-4,3))
summary(lm(stuff$fip~stuff$mean_stuff))

write.csv(new_dset,file = "new_dataset.csv")

#getting stuff metric
games_r_dset <- relievers %>%
  left_join(names,by = 'pitcher_id') %>%
  group_by(pitcher_id,game_id) %>%
  summarise(mean_stuff_game = mean(stuff))

stuff_dset = as.data.frame(games_r_dset[1:dim(games_r_dset)[1],1:dim(games_r_dset)[2]]) %>%
  rename(stuff = mean_stuff_game)

pitch <- player_game %>%
  select(pitcher_id, game_id, starts_with("npitch")) %>%
  right_join(stuff_dset, by = c("pitcher_id","game_id"))

(pitch$pitcher <- factor(pitch$pitcher))

########JAGS#############
#JAGS Code
library(R2jags)
library(R2WinBUGS)

mod.data <- list(Y = pitch$stuff,
                 X1 = pitch$npitch1,
                 X2 = pitch$npitch2,
                 X3 = pitch$npitch3,
                 X4 = pitch$npitch4,
                 X5 = pitch$npitch5,
                 X6 = pitch$npitch6,
                 X7 = pitch$npitch7,
                 n = nrow(pitch),
                 pitcher = pitch$pitcher,
                 n.pitcher = length(unique(pitch$pitcher))
)


pitch_model <- function(){
  for(i in 1:n){
    mu[i] <- alpha[pitcher[i]] + beta * (gamma*X1[i] + gamma^2*X2[i] + 
      gamma^3*X3[i] + gamma^4*X4[i] + gamma^5*X5[i] + 
      gamma^6*X6[i] + gamma^7*X7[i])
    Y[i] ~ dnorm(mu[i], tau)
  }
  
  tau ~ dgamma(.1,.1)
  gamma ~ dnorm(0,phi.g)
  beta ~ dnorm(0,phi.b)
  
  sd.a ~ dt(0,.1,1)%_%T(0,)
  phi.a <- sd.a^(-2)
  
  sd.b ~ dt(0,.1,1)%_%T(0,)
  phi.b <- sd.b^(-2)
  
  sd.g ~ dt(0,.1,1)%_%T(0,)
  phi.g <- sd.g^(-2)
  for(p in 1:n.pitcher){
    alpha[p] ~ dnorm(0,phi.a)
  }
}

# Create a function that provides intial values for WinBUGS
pitch.inits = function() {
  alpha <- rep(0, mod.data$n.pitcher)
  gamma <- 0
  beta <- 0
  sd.b <- 1
  sd.a <- 1
  sd.g <- 1
  return(list(alpha=alpha, gamma=gamma, beta = beta, sd.b = sd.b,
              sd.a = sd.a, sd.g = sd.g))
}


parameters = c("alpha","gamma","sd.a", "sd.g","beta","sd.b")


pitch.model.file = paste(getwd(),"pitch-model.txt", sep="/")
write.model(pitch_model, pitch.model.file)

pitch.sim = jags(mod.data, inits=pitch.inits, parameters, model.file=pitch.model.file,
                n.iter=5000)

#Runs chain until convergence, returns samples post-burn-in
pitch.bugs = as.mcmc(pitch.sim$BUGSoutput$sims.matrix)  # create an MCMC object
save(pitch.sim,file = "JAGSOutput.Rdata")

#Get output into matrices
alpha_mat <- gamma_mat <- matrix(numeric(3750*568),ncol = mod.data$n.pitcher)
for(p in 1:568){
  gamma_mat[,p] <- as.numeric(pitch.bugs[,paste0("gamma[",p,"]")])
  alpha_mat[,p] <- as.numeric(pitch.bugs[,paste0("alpha[",p,"]")])
}

#Examine posterior means
plot(apply(alpha_mat,2,mean), xlab = "Reliever", 
     ylab = "Alpha Value", main = "Overall Stuff Intercept",
     pch = 19, cex = .7)

plot(apply(gamma_mat,2,mean), xlab = "Reliever", 
     ylab = "Phi Value", main = "Effectiveness Decay",
     pch = 19, cex = .7)


#Look at Frequentist model
pitch2 <- pitch
pitch2$pitcher_id <- as.factor(pitch2$pitcher_id)
rand_mod <- lmer(stuff~ (1|pitcher_id) + npitch1 + npitch2 + npitch3,
                 data = pitch2)
summary(rand_mod)

fix_mod <- lm(stuff~factor(pitcher_id)*npitch1,data = pitch)
