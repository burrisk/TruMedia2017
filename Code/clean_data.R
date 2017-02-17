library(dplyr)
library(ggplot2)
library(LaplacesDemon)
library(pitchRx)
library(mgcv)

ws2016 <- read.csv("Data/2016-WS.csv")
mlb_2014 <- read.csv("Data/2014.csv")
mlb_2015 <- read.csv("Data/2015.csv")
mlb_2016 <- read.csv("Data/2016.csv")

mlb_all <- full_join(mlb_2014, mlb_2015) %>%
  full_join(mlb_2016)

save(mlb_all, file = "Data/mlb_all.Rdata")

load("Data/mlb_all.Rdata")

# Strike Zone Exploration
mlb_all <- mlb_all %>%
  rename(stand = batterHand)

ump_dec <- mlb_all %>%
  filter(pitchResult %in% c("SL", "B")) %>%
  mutate(cs = (pitchResult == "SL")) %>%
  filter(px > -0.708 & px < 0.708) %>%
  filter(pitchType == "CU")

gam1 <- gam(cs ~ s(pz, by = factor(seasonYear), bs = "fs"), family = "binomial", 
            data = ump_dec)
ump_dec$pzStrike = predict.gam(gam1, ump_dec, type = "response")

strikeFX(ump_dec, model = gam1, )

ggplot(ump_dec, aes(x = pz, y = pzStrike, colour = factor(seasonYear))) +
  geom_line(size = 0.25) +
  coord_flip() + 
  xlim(c(1,2.5))

strikeFX(ump_dec, layer = facet_grid(. ~ stand))

ump_dec_horizs <- ump_dec %>%
  filter((px < 0.708) & (px > -0.708))


