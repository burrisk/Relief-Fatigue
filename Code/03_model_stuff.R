# Load Data
load("Data/Swings.Rdata")
load("Data/AllStandardPitches.Rdata")

# Load Packages
list.of.packages <- c("dplyr", "LaplacesDemon")
new.packages <- list.of.packages[
  !(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, 
                                          repos="http://cran.rstudio.com/")

library(dplyr)
library(LaplacesDemon)

# Covariates to include in the model
covars <- c("poly(start_speed,2)", "spin_rate", "abs(pfx_x)*pfx_z",
            "ax*ay*az")

model.formula <- as.formula(paste0("whiff~", paste(covars, collapse = "+")))

# Four Seam Fastballs (Exploratory)
four.seam.swing <- pitch.swing %>%
  filter(pitch_type == "FF")
four.seam.all <- pitches.kept %>%
  filter(pitch_type == "FF")

glm.four.seam <- glm(model.formula, data = four.seam.swing, 
                     family = "binomial")

summary(glm.four.seam)

four.seam.all$nasty <- as.numeric(scale(logit(predict(glm.four.seam, four.seam.all,
                                                      type = "response"))))

four.seam.stuff <- four.seam.all %>%
  group_by(pitcher_name) %>%
  summarise(avg_stuff = mean(nasty), n_pitch = n()) %>%
  filter(n_pitch > 10) %>%
  arrange(desc(avg_stuff))

# Two-Seam Fastballs

two.seam.swing <- pitch.swing %>%
  filter(pitch_type == "SL")
two.seam.all <- pitches.kept %>%
  filter(pitch_type == "SL")

glm.two.seam <- glm(model.formula, data = two.seam.swing, 
                     family = "binomial")

summary(glm.two.seam)

two.seam.all$nasty <- as.numeric(scale(logit(predict(glm.two.seam, two.seam.all,
                                                      type = "response"))))

two.seam.stuff <- two.seam.all %>%
  group_by(pitcher_name) %>%
  summarise(avg_stuff = mean(nasty), n_pitch = n()) %>%
  filter(n_pitch > 10) %>%
  arrange(desc(avg_stuff))

# Curveballs
curveball.swing <- pitch.swing %>%
  filter(pitch_type == "CU" | pitch_type == "KC")
curveball.all <- pitches.kept %>%
  filter(pitch_type == "KC" | pitch_type == "CU")

glm.curveball <- glm(model.formula, data = curveball.swing,
                     family = "binomial")

summary(glm.curveball)
test.mod(c("spin_rate", "start_speed*abs(pfx_x)*pfx_z",
                     "ax*ay*az"),curveball.swing,5,prob.threshold = .5)

curveball.all$nasty <- as.numeric(scale(logit(predict(glm.curveball, curveball.all,
                                                     type = "response"))))

curveball.stuff <- curveball.all %>%
  group_by(pitcher_name) %>%
  summarise(avg_stuff = mean(nasty), n_pitch = n()) %>%
  filter(n_pitch > 10) %>%
  arrange(desc(avg_stuff))

# Sliders
slider.swing <- pitch.swing %>%
  filter(pitch_type == "SL")
slider.all <- pitches.kept %>%
  filter(pitch_type == "SL")

glm.slider <- glm(whiff ~ start_speed2 + start_speed*pfx_z + abs(pfx_x) +
                       pfx_z + factor(s) + start_speed*abs(pfx_x), data = slider.swing,
                     family = "binomial")

summary(glm.slider)


slider.all$nasty <- as.numeric(scale(logit(predict(glm.slider, slider.all,
                                                      type = "response"))))

slider.stuff <- slider.all %>%
  group_by(pitcher_name) %>%
  summarise(avg_stuff = mean(nasty), n_pitch = n()) %>%
  filter(n_pitch > 10) %>%
  arrange(desc(avg_stuff))


ggplot(curveball.all[sample(1:1000),], aes(x = abs(pfx_x), y = pfx_z, color = pitch_type)) + 
  geom_point()
