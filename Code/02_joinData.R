library(mgcv)

# Get Data into Rdata object
mlb_2014 <- read.csv("Data/2014.csv")
mlb_2015 <- read.csv("Data/2015.csv")
mlb_2016 <- read.csv("Data/2016.csv")

mlb_all <- mlb_2014 %>%
  full_join(mlb_2015) %>%
  full_join(mlb_2016)

save(mlb_all, file = "Data/mlb_all.Rdata")

# Umpire Strike Zone Models
load("Data/mlb_all.Rdata")
load("Data/2008.Rdata")

# Create datasets of only fastballs for 2008 and 2016
ff_2008 <- pitch_2008 %>%
  filter(des %in% c("Ball", "Called Strike", "Ball In Dirt",
                    "Intent Ball", "Pitchout")) %>%
  filter(pitch_type == "FF") %>%
  mutate(cs = factor(des == "Called Strike")) %>%
  rename(batterHand = stand)

ff_2016 <- mlb_2016 %>%
  filter(pitchResult %in% c("B", "SL", "BID", "IB", "PO")) %>%
  filter(pitchType == "FF") %>%
  mutate(cs = factor(pitchResult == "SL"))

sz_mod_08 <- gam(cs ~ s(px, pz, bs = "fs", by = factor(batterHand)), family = "binomial",
                 data = ff_2008)

sz_mod_16 <- gam(cs ~ s(px, pz, bs = "fs", by = factor(batterHand)), family = "binomial",
                 data = ff_2016)

mlb_all$ps2008 = predict(sz_mod_08, mlb_all, type = "response")
mlb_all$ps2016 = predict(sz_mod_16, mlb_all, type = "response")

mlb_all$psdiff = mlb_all$ps2008 - mlb_all$ps2016

save(mlb_all, file = "Data/mlb_all.Rdata")

