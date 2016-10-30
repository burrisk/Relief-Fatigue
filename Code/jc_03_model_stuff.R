library(dplyr)
setwd("~/Documents/Relief-Fatigue")

load(file ="Data/AllPitches.Rdata")

########
#Extra code to get the right dataset
##########
# Find fastest average pitch for each reliever in a given year
fastest.pitch.dat <- pitch.relief.all %>%
  filter(!is.na(start_speed)) %>%
  filter(!(pitch_type %in% c("PO", "AB", "EP", "SC", "UN", "FO"))) %>%
  group_by(pitcher, year, pitch_type) %>%
  filter(n() >= 3) %>%
  summarise(avg_fast_speed = mean(start_speed)) %>%
  arrange(desc(avg_fast_speed)) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-pitch_type)

pitch.relief.all <- inner_join(pitch.relief.all, fastest.pitch.dat, by = 
                                 c("pitcher", "year"))

# Create Swing and Whiff Indicators
swinging <- c("In play, no out", "Foul", "In play, run(s)", "Swinging Strike", 'Foul Tip',
              "Foul (Runner Going)", "Swinging Strike (Blocked)")
whiff <- c("Swinging Strike", "Swinging Strike (Blocked)")

pitch.relief.all$swinging <- as.numeric(pitch.relief.all$des %in% swinging)
pitch.relief.all$whiff <- as.numeric(pitch.relief.all$des %in% whiff)

# Only include common pitches
keep.pitches <- c("CH", "CU", "FC", "FF", "FS", "FT", "KC", "SI", "SL")
pitches.kept <- pitch.relief.all %>%
  filter(pitch_type %in% keep.pitches)


# This is the dataset where we estimate nastiness of each pitch
save(pitches.kept, file = "Data/AllStandardPitches.Rdata")



pitch.swing <- pitches.kept %>%
  filter(swinging == 1)

# Remove In-Play Bunts
inplay <- c("In play, no out", "In play, run(s)")
bunt.events <- c("Bunt Lineout", "Bunt Popout", "Bunt Groundout", "Sac Bunt",
                 "Sacrifice Bunt DP")
pitch.swing <- pitch.swing %>%
  filter(!(des %in% inplay & event %in% bunt.events))

# Dataset with only full swings- used to model whiff rates
save(pitch.swing, file = "Data/Swings.Rdata")

##########################

####Begin actual coding
load("Data/Swings.Rdata")

pitch.swing <- pitch.swing %>% tbl_df()

##Break pitches into types
##Function to take a model, and test it 
####Should output FP, FN, some sort of model fit
####given glm, assignment rule, dataset, number of folds k

#Split the data into k folds
#Returns a list of length k, each elt is a fold of the dataset
get.folds <- function(ds,k){
  #k is number of folds
  #ds is dataset
  n = dim(ds)[1]
  folds <- vector("list",k)
  left = 1:n
  size_fold = floor(n/k)
  for(i in 1:(k-1)){
    folds[[i]] <- sample(left,size_fold)
    left <- left[-which(left %in% folds[[i]])]
  }
  folds[[k]] = left
  
  return(folds)
}

#Given model to test, assignment prob, calculate avg FP, FN across folds
test.mod <- function(covars,ds,k){
  folds <- get.folds(ds,k)
  results <- lapply(folds,function(x){
    
    #Divide up the dataset
    train.ds = ds[-x,]
    test.ds = ds[x,]
    
    #Get the model
    mod.string = paste0("whiff~",paste(covars,collapse="+"))
    mod <- glm(as.formula(mod.string),data = train.ds)
    
    #Predicted probabilities
    probs = predict.glm(mod,newdata=test.ds)
    
    #Predicted valus
    pred = rbinom(n=length(probs),size=1,prob = probs)
    
    #Comparison vector
    #1 = false positive
    #-1 = false negative
    #0 = true positive & negative
    comp = pred - test.ds$whiff
    out_vec = c("False Pos" = sum(comp==1)/length(comp),
                "False Neg" = sum(comp==-1)/length(comp),
                "True" = sum(comp==0)/length(comp))
    return(out_vec)
  } )
  results.mat <- do.call(rbind,results)
  avg.results <- rbind(apply(results.mat,2,mean),
                       apply(results.mat,2,sd))
  row.names(avg.results)= c('Mean','SD')
  
  return(avg.results)
}
ff <- pitch.swing %>%
  filter(pitch_type =="FF") 

covars <- c("start_speed","pfx_x","pfx_z","spin_rate")

tester = test.mod(covars,ff,5)


