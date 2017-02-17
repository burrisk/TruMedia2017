library(dplyr)
library(ggplot2)
library(pitchRx)

load("Data/mlb_all.Rdata")


# Get rid of intentional walk bug and remove outside corner expansion
mlb_all <- mlb_all %>%
  mutate(psdiff = (ps2008 > 0.001)*(psdiff < 0)*psdiff)

# Histogram of Difference
hist(mlb_all$psdiff)

# Get Run Values Data
run_values <- read.csv("Data/Pitch_Run_Values.csv")

run_values <- run_values %>%
  mutate(B = substr(Count, 1, 1), S = substr(Count, 3, 3)) %>%
  dplyr::select(-Count) %>%
  dplyr::select(B, S, wOBA:ValOut)

mlb_all <- mlb_all %>%
  filter(!(pitchResult %in% c("IP") & (paResult %in% c("BI", "CI", "ROE", "SF_ROE",
                                                       "SH","SH_ROE")))) %>%
  mutate(Ball = pitchResult %in% c("B", "BID", "IB", "PO"),
       HBP = pitchResult %in% c("HBP"),
       Strike = pitchResult %in% c("SL", "SS", "MB") |
         (strikes < 2)*(pitchResult %in% c("F", "FT")),
       Single = (pitchResult == "IP")*(paResult == "S"),
       Double = (pitchResult == "IP")*(paResult == "D"),
       Triple = (pitchResult == "IP")*(paResult == "T"),
       HR = (pitchResult == "IP")*(paResult == "HR"),
       Out = (pitchResult == "IP")*(paResult %in% c("DP", "FC", "IP_OUT",
                                                    "SF", "TP")),
       NoC = pitchResult %in% c("F", "FT") & (strikes == 2))

outcome_des = c("Ball", "HBP", "Strike", "Single", "Double", "Triple",
                "HR", "Out", "NoC")

mlb_all <- mlb_all %>%
  mutate(outcomeNum = Ball + 2*HBP + 3*Strike + 4*Single + 5*Double + 6*Triple +
           7*HR + 8*Out + 9*NoC)

mlb_all$outcome <- sapply(mlb_all$outcomeNum, function(i){
  outcome_des[i]
})

run_values$NoC = 0
run_values$HBP = 0.338 - run_values$Runs.PA

colnames(run_values) = c("balls", "strikes", "woba", "runs.pa", "Ball", "Strike",
                         "Single", "Double", "Triple", "HR", "Out", "NoC", "HBP")

run_values <- run_values %>%
  dplyr::select(-woba, -runs.pa)

# Table 2
table2 <- run_values %>%
  mutate(count = paste0(balls,"-",strikes)) %>%
  arrange(strikes, balls) %>%
  dplyr::select(-balls, -strikes) %>%
  dplyr::select(count, Ball, Strike, HBP, Single, Double, Triple, HR, Out, NoC)

library(xtable)
print(xtable(table2, digits = 3), include.rownames = F)

run_values <- reshape(run_values, 
             varying = c("Ball", "Strike", "Single", "Double", "Triple",
                         "HR", "Out", "NoC", "HBP"), 
             v.names = "runs",
             timevar = "outcome",
             times = c("Ball", "Strike", "Single", "Double", "Triple",
                       "HR", "Out", "NoC", "HBP"),
             direction = "long") %>%
  dplyr::select(-id) %>%
  mutate(balls = as.numeric(balls), strikes = as.numeric(strikes))

mlb_all <- left_join(mlb_all, run_values)

# Find Best Batters (in terms of runs generated)
bat_totruns <- mlb_all %>%
  filter(!(is.na(psdiff))) %>%
  group_by(batterId, batter) %>%
  summarise(ARG100 = 100*mean(runs), ARG = sum(runs), pseen = n()) %>%
  arrange(desc(ARG))

# Table 3
table3a <- bat_totruns %>%
  ungroup() %>%
  dplyr::select(batter, ARG, ARG100)

print(xtable(table3a[1:10,], digits = 3), include.rownames = F)

table3b <- bat_totruns %>%
  ungroup() %>%
  dplyr::select(batter, ARG, ARG100) %>%
  arrange(ARG)

print(xtable(table3b[1:10,], digits = 3), include.rownames = F)
  

# Find Most Improved Batters
bat_impruns <- mlb_all %>%
  filter(!(is.na(psdiff))) %>%
  group_by(batterId, batter) %>%
  summarise(w_runs = sum((1+psdiff)*runs), tot_runs = sum(runs), nuw = n(),
            nw = sum(1+psdiff)) %>%
  filter(nuw > 1000) %>%
  mutate(w_runs = w_runs*nuw/nw) %>%
  mutate(run_diff = w_runs - tot_runs)

# Table 4
table4a <- bat_impruns %>%
  ungroup() %>%
  dplyr::select(batter, run_diff) %>%
  arrange(desc(run_diff))

print(xtable(table4a[1:10,], digits = 3), include.rownames = F)

table4b <- bat_impruns %>%
  ungroup() %>%
  dplyr::select(batter, run_diff) %>%
  arrange(run_diff)

print(xtable(table4b[1:10,], digits = 3), include.rownames = F)


# Find Year to Year Correlation
bat_coryr <- mlb_all %>%
  filter(!(is.na(psdiff))) %>%
  group_by(batterId, batter, seasonYear) %>%
  summarise(w_runs = sum((1+psdiff)*runs), tot_runs = sum(runs), nuw = n(),
            nw = sum(1+psdiff)) %>%
  filter(nuw > 500) %>%
  mutate(w_runs = w_runs*nuw/nw) %>%
  mutate(run_diff = w_runs - tot_runs) %>%
  dplyr::select(seasonYear, run_diff)

library(reshape2)
melt.bat_coryr <- melt(bat_coryr, id.vars = c("batterId", "batter", "seasonYear"))
bat_seasons <- dcast(melt.bat_coryr, batterId + batter + variable ~ seasonYear)

names <- c("playerid", "Name", "variable", "x", "y")
season.pairs_bat <- rbind(setNames(bat_seasons[1:5], names),
                      setNames(bat_seasons[c(1:3,5:6)], names))
season.pairs_bat <- season.pairs_bat[which(complete.cases(season.pairs_bat) ==T),]  #Remove NAs
cor(season.pairs_bat$x, season.pairs_bat$y)
plot(season.pairs_bat$x, season.pairs_bat$y)

tot_rundiff = sum(bat_impruns$run_diff)
tot_pitches = sum(bat_impruns$nuw)

#Table 5
table5a <- bat_impruns %>%
  mutate(adj_run_diff = run_diff - tot_rundiff/tot_pitches*nuw) %>%
  ungroup() %>%
  dplyr::select(batter, adj_run_diff) %>%
  arrange(desc(adj_run_diff))

print(xtable(table5a[1:10,], digits = 3), include.rownames = F) 
  

table5b <- bat_impruns %>%
  mutate(adj_run_diff = run_diff - tot_rundiff/tot_pitches*nuw) %>%
  ungroup() %>%
  dplyr::select(batter, adj_run_diff) %>%
  arrange(adj_run_diff)

print(xtable(table5b[1:10,], digits = 3), include.rownames = F)

bat_impruns1415 <- mlb_all %>%
  filter(!(is.na(psdiff))) %>%
  filter(seasonYear %in% c(2014:2015)) %>%
  group_by(batterId, batter) %>%
  summarise(w_runs = sum((1+psdiff)*runs), tot_runs = sum(runs), nuw = n(),
            nw = sum(1+psdiff)) %>%
  filter(nuw > 1000) %>%
  mutate(w_runs = w_runs*nuw/nw) %>%
  mutate(run_diff = w_runs - tot_runs)

tot_rundiff = sum(bat_impruns1415$run_diff)
tot_pitches = sum(bat_impruns1415$nuw)

bat_impruns1415 <- bat_impruns1415 %>%
  mutate(adj_run_diff15 = run_diff - tot_rundiff/tot_pitches*nuw) %>%
  arrange(desc(adj_run_diff15))

bi2 <- full_join(bat_impruns, bat_impruns1415, by = c("batter", "batterId"))

# Find Most Improved Pitchers
pitcher_impruns <- mlb_all %>%
  filter(!(is.na(psdiff))) %>%
  group_by(pitcherId, pitcher) %>%
  summarise(w_runs = sum((1+psdiff)*runs), tot_runs = sum(runs), nuw = n(),
            nw = sum(1+psdiff)) %>%
  mutate(m_runs = tot_runs/nuw*100, mw_runs = w_runs/nw*100) %>%
  filter(nuw > 1000) %>%
  mutate(w_runs = w_runs*nuw/nw) %>%
  mutate(run_diff = w_runs - tot_runs) 

# Find Year to Year Correlation
pitch_coryr <- mlb_all %>%
  filter(!(is.na(psdiff))) %>%
  group_by(pitcherId, pitcher, seasonYear) %>%
  summarise(w_runs = sum((1+psdiff)*runs), tot_runs = sum(runs), nuw = n(),
            nw = sum(1+psdiff)) %>%
  filter(nuw > 500) %>%
  mutate(w_runs = w_runs*nuw/nw) %>%
  mutate(run_diff = w_runs - tot_runs) %>%
  dplyr::select(seasonYear, run_diff)

library(reshape2)
melt.pitch_coryr <- melt(pitch_coryr, id.vars = c("pitcherId", "pitcher", "seasonYear"))
pitch_seasons <- dcast(melt.pitch_coryr, pitcherId + pitcher + variable ~ seasonYear)

pitch_seasons_nona = na.omit(pitch_seasons)
cor(pitch_seasons_nona$`2014` + pitch_seasons_nona$`2015`, pitch_seasons_nona$`2016`)
names <- c("playerid", "Name", "variable", "x", "y")
season.pairs_pitch <- rbind(setNames(pitch_seasons[1:5], names),
                          setNames(pitch_seasons[c(1:3,5:6)], names))
season.pairs_pitch <- season.pairs_pitch[which(complete.cases(season.pairs_pitch) ==T),]  #Remove NAs
cor(season.pairs_pitch$x, season.pairs_pitch$y)
plot(season.pairs_pitch$x, season.pairs_pitch$y)

tot_rundiff = sum(pitcher_impruns$run_diff)
tot_pitches = sum(pitcher_impruns$nuw)

pitcher_impruns <- pitcher_impruns %>%
  mutate(adj_run_diff = run_diff - tot_rundiff/tot_pitches*nuw) %>%
  arrange(desc(adj_run_diff))

# Table 6
table6a <- pitcher_impruns %>%
  ungroup() %>%
  dplyr::select(pitcher, tot_runs, m_runs) %>%
  arrange(tot_runs)

print(xtable(table6a[1:10,], digits = 3), include.rownames = F) 

table6b <- pitcher_impruns %>%
  ungroup() %>%
  dplyr::select(pitcher, tot_runs, m_runs) %>%
  arrange(m_runs)

print(xtable(table6b[1:10,], digits = 3), include.rownames = F) 

# Table 7

# Table 6
table7a <- pitcher_impruns %>%
  ungroup() %>%
  dplyr::select(pitcher, tot_runs, m_runs) %>%
  arrange(tot_runs)

print(xtable(table7a[1:10,], digits = 3), include.rownames = F) 

table7b <- pitcher_impruns %>%
  ungroup() %>%
  dplyr::select(pitcher, tot_runs, m_runs) %>%
  arrange(m_runs)

print(xtable(table7b[1:10,], digits = 3), include.rownames = F) 

# Figure 6

mlb_red <- mlb_all %>%
  filter(pitchType == "FF") %>%
  filter(seasonYear == 2016) %>%
  filter(batterHand == "R") %>%
  filter(pitcherHand == "R")

top = 2.60 + 73.7/12*.136
bot = 0.92 + 73.7/12*.136
left = -1.03
right = 1

sz_coords <- expand.grid(c(bot, top), c(left,right))
sz_coords <- rbind(sz_coords, sz_coords)
colnames(sz_coords) = c("pz", "px")
sz_coords$group <- c(1,1,2,2,3,4,3,4)

ggplot(mlb_red, aes(px, pz, colour = (1+psdiff))) +
  geom_point(size = 0.5) +
  geom_line(data = sz_coords, mapping = aes(x = px, y = pz, group = group), 
            colour = "black") +
  scale_colour_gradient(low = "blue", high = "white", name = "Weight") +
  xlab("Horizontal Location") +
  ylab("Height Above Ground (ft)") +
  xlim(c(-2,2)) +
  ylim(c(1,4)) +
  ggtitle("Weights by Location") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))
  
ggsave("Output/fig6.png")



