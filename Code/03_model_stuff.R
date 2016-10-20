load("Data/Swings.Rdata")


# Fastballs (Exploratory)
four.seam <- pitch.swing %>%
  filter(pitch_type == "FF")
two.seam <- pitch.swing %>%
  filter(pitch_type == "FT")

glm.four.seam <- glm(whiff ~ poly(start_speed, 2) + start_speed*pfx_z + abs(pfx_x) +
                       pfx_z, data = four.seam, 
                     family = "binomial")
