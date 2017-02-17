library(dplyr)
library(ggplot2)
library(pitchRx)
library(mgcv)
library(xtable)

load("Data/pitch_raw.Rdata")

# Change date to R Date Object
pitch$date <- as.Date(gsub("_", "-", pitch$date))

# Create MLB Schedule Data Frame
years <- 2008:2016
opening <- as.Date(c("2008-03-31", "2009-04-05", "2010-04-04", "2011-03-31", "2012-03-28", "2013-03-31", "2014-03-31",
                     "2015-04-05", "2016-04-03"))
end.reg.season <- as.Date(c("2008-09-30", "2009-10-06","2010-10-05","2011-09-29", "2012-10-04", "2013-09-30",
                            "2014-09-29", "2015-10-05", "2016-10-03"))
mlb.schedule <- data.frame(opening, end.reg.season)
colnames(mlb.schedule) <- c("start", "end")
rownames(mlb.schedule) <- years

# Define which pitches are in regular season and postseason games
y <- format(pitch$date, "%Y")
pitch$reg_season <- as.numeric(pitch$date >= mlb.schedule[y,"start"]) & 
  (pitch$date <= mlb.schedule[y,"end"])
pitch$post_season <- as.numeric(pitch$date > mlb.schedule[y,"end"])
pitch$year <- y

# Create Balls and Strikes Variables
pitch$b <- substr(pitch$count, 1, 1)
pitch$s <- substr(pitch$count, 3, 3)

# Exclude Spring Training games and Years not in Study
studied.years <- c(2008, 2016)
pitch_study <- pitch %>%
  filter(reg_season + post_season != 0) %>%
  filter(as.numeric(year) %in% studied.years)

pitch_2008 <- pitch_study %>%
  filter(as.numeric(year) == 2008)

save(pitch_2008, file = "Data/2008.Rdata")

pitch_2008_ns <- pitch_study %>%
  filter(as.numeric(year) == 2008) %>%
  filter(des %in% c("Called Strike", "Ball")) %>%
  filter(pitch_type == "FF") %>%
  mutate(cs = (des == "Called Strike")) %>%
  filter(stand == "R")

gam_2008 <- gam(cs ~ s(px,pz), family = "binomial", 
  data = pitch_2008_ns)

pitch_2016_ns <- pitch_study %>%
  filter(as.numeric(year) == 2016) %>%
  filter(des %in% c("Called Strike", "Ball")) %>%
  filter(pitch_type == "FF") %>%
  mutate(cs = (des == "Called Strike")) %>%
  filter(stand == "R")

gam_2016 <- gam(cs ~ s(px,pz), family = "binomial", 
                data = pitch_2016_ns)

top = 2.60 + 73.7/12*.136
bot = 0.92 + 73.7/12*.136
left = -1.03
right = 1

sz_coords <- expand.grid(c(bot, top), c(left,right))
sz_coords <- rbind(sz_coords, sz_coords)
colnames(sz_coords) = c("pz", "px")
sz_coords$group <- c(1,1,2,2,3,4,3,4)



plotStrikeZone <- function(m, title){
  top = 2.60 + 73.7/12*.136
  bot = 0.92 + 73.7/12*.136
  left = -1.03
  right = 1
  
  sz_coords <- expand.grid(c(bot, top), c(left,right))
  sz_coords <- rbind(sz_coords, sz_coords)
  colnames(sz_coords) = c("pz", "px")
  sz_coords$group <- c(1,1,2,2,3,4,3,4)
  
  x_limitz <- seq(-2,2,by = 0.02)
  z_limitz <- seq(1,4,by = 0.02)
  d = expand.grid(x_limitz,z_limitz)
  colnames(d) = c("px", "pz")
  d$pred <- unname(as.numeric(predict.gam(m, d, type = "response")))
  ggplot(d, aes(px, pz)) + 
    geom_tile(aes(fill = pred)) +
    geom_line(data = sz_coords, mapping = aes(x = px, y = pz, group = group), 
              colour = "white") +
    xlab("Horizontal Location") +
    ylab("Height Above Ground (ft)") +
    scale_fill_continuous(name = "probStrike") + # title text +
    ggtitle(title) +
    theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
    theme(axis.text=element_text(size=12),
          axis.title=element_text(size=14))
}

# Figure 1
sz_2008 = plotStrikeZone(gam_2008, "FF/RHB Strike Zone (2008)")
ggsave("Output/fig1a.png", sz_2008)
sz_2016 = plotStrikeZone(gam_2016, "FF/RHB Strike Zone (2016)")
ggsave("Output/fig1b.png", sz_2016)

# Figure 2
ff_middle <- pitch %>%
  filter(reg_season + post_season != 0) %>%
  filter(pitch_type == "FF") %>%
  filter((px > -0.5) & (px < 0.5)) %>%
  filter(des %in% c("Ball", "Called Strike")) %>%
  mutate(cs = (des == "Called Strike"))

gam_ff <- gam(cs ~ s(pz, by = factor(year), bs = "fs"), data = ff_middle)

years = 2008:2016
vertz = seq(1, 2, by = .001)
dat_ele = expand.grid(years, vertz)
colnames(dat_ele) = c("year", "pz")
dat_ele$pred = abs(predict(gam_ff, dat_ele, type = 'response') - 0.5)
dat_ele = dat_ele %>%
  group_by(year) %>%
  arrange(pred) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-pred) %>%
  arrange(year)

ele_current = as.numeric(dat_ele[which(dat_ele$year == 2016),"pz"])
dat_ele$difference = dat_ele$pz - ele_current
dat_ele$change = c(NA, diff(dat_ele$pz, lag = 1))

dat_ele <- dat_ele %>%
  dplyr::select(year, pz, change, difference)

print(xtable(dat_ele, digits= 3), include.rownames = F)

ggplot(dat_ele, aes(x = year, y = pz)) + 
  geom_line(size = 2, color = "red") + 
  geom_point(size = 2, color = "black") +
  xlab("Year") +
  ylab("Height Above Ground (ft)") +
  ggtitle("Empirical Lower Edge over Time") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave("Output/fig2b.png")

# Figure 3 (Pitcher Adjustments)
ff_08 <- pitch_study %>%
  filter(pitch_type == "FF" & year == 2008) %>%
  mutate(take = des %in% c("Ball", "Called Strike", "Ball In Dirt"))

ff_16 <- pitch_study %>%
  filter(pitch_type == "FF" & year == 2016) %>%
  mutate(take = des %in% c("Ball", "Called Strike", "Ball In Dirt"))

 ff_0821 <- ff_08 %>%
   filter(b == 3 & s== 2)

# Find strike zone for batters
gam2108 <- gam(factor(take) ~ s(px, pz), family = "binomial", data = ff_0821)
x_limitz <- seq(-2,2,by = 0.025)
z_limitz <- seq(0,5, by = 0.025)
dbat <- expand.grid(x_limitz, z_limitz)
names(dbat) = c("px", "pz")
dbat$pred08 = predict(gam2108, dbat, type = "response")

ff_1621 <- ff_16  %>%
   filter(b == 3 & s == 2)

gam2116 <- gam(factor(take) ~ s(px, pz), family = "binomial", data = ff_1621)
dbat$pred16 = predict(gam2116, dbat, type = "response")
dbat$diff = dbat$pred08 - dbat$pred16

ggplot(dbat, aes(px, pz)) + 
  geom_tile(aes(fill = diff)) +
  geom_line(data = sz_coords, mapping = aes(x = px, y = pz, group = group), 
            colour = "black") +
  xlab("Horizontal Location") +
  ylab("Height Above Ground (ft)") +
  scale_fill_gradient2(low="blue",mid="white", high="red", midpoint=0,
                       name = "Change") +
  ggtitle("Change in Batter Swing % (3-2 FF)") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave(filename = "Output/fig4a.png")


library(reshape2) # For melt function

# Calculate the common x and y range for plot
xrng = c(-2,2)
yrng = c(0,5)

library(MASS)
# Calculate the 2d density estimate over the common range
d1 = kde2d(ff_08$px, ff_08$pz, lims=c(xrng, yrng), n=100)
d2 = kde2d(ff_16$px, ff_16$pz, lims=c(xrng, yrng), n=100)

# Confirm that the grid points for each density estimate are identical
identical(d1$x, d2$x) # TRUE
identical(d1$y, d2$y) # TRUE

# Calculate the difference between the 2d density estimates
diff12 = d1 
diff12$z = d2$z - d1$z

## Melt data into long format
# First, add row and column names (x and y grid values) to the z-value matrix
rownames(diff12$z) = diff12$x
colnames(diff12$z) = diff12$y

# Now melt it to long format
diff12.m = melt(diff12$z, id.var=rownames(diff12))
names(diff12.m) = c("px","pz","Change")

library(scales)
# Plot difference between two densities
ggplot(diff12.m, aes(px, pz)) +
  geom_tile(aes(fill=Change)) +
  geom_line(data = sz_coords, mapping = aes(x = px, y = pz, group = group), 
            colour = "black") +
  # geom_polygon(data = hulls08, mapping = aes(x = h, y = v, linetype = "2008"), colour = "black",
  #              fill = "NA") +
  # geom_polygon(data = hulls16, mapping = aes(x = h, y = v, linetype = "2016"), colour = "black",
  #              fill = "NA") +
  scale_fill_gradient2(low="blue",mid="white", high="red", midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE) +
  xlab("Horizontal Location") +
  ylab("Height above Ground (ft)") +
  ggtitle("Change in FF Distribution (2008 and 2016)") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave(file = "Output/fig3a.png")



cu_08 <- pitch_study %>%
  filter(pitch_type == "CU" & year == 2008) %>%
  mutate(take = des %in% c("Ball", "Called Strike", "Ball In Dirt"))

cu_16 <- pitch_study %>%
  filter(pitch_type == "CU" & year == 2016) %>%
  mutate(take = des %in% c("Ball", "Called Strike", "Ball In Dirt"))

cu_0821 <- cu_08 %>%
  filter(b == 3 & s== 2)



cu_1621 <- cu_16  %>%
  filter(b == 3 & s == 2)

library(reshape2) # For melt function

# Calculate the common x and y range for plot
xrng = c(-2,2)
yrng = c(0,5)

library(MASS)
# Calculate the 2d density estimate over the common range
d1 = kde2d(cu_08$px, cu_08$pz, lims=c(xrng, yrng), n=100)
d2 = kde2d(cu_16$px, cu_16$pz, lims=c(xrng, yrng), n=100)

# Confirm that the grid points for each density estimate are identical
identical(d1$x, d2$x) # TRUE
identical(d1$y, d2$y) # TRUE

# Calculate the dicuerence between the 2d density estimates
dicu12 = d1 
dicu12$z = d2$z - d1$z

## Melt data into long format
# First, add row and column names (x and y grid values) to the z-value matrix
rownames(dicu12$z) = dicu12$x
colnames(dicu12$z) = dicu12$y

# Now melt it to long format
dicu12.m = melt(dicu12$z, id.var=rownames(dicu12))
names(dicu12.m) = c("px","pz","Change")

library(scales)
# Plot dicuerence between two densities
ggplot(dicu12.m, aes(px, pz)) +
  geom_tile(aes(fill=Change)) +
  geom_line(data = sz_coords, mapping = aes(x = px, y = pz, group = group), 
            colour = "black") +
  # geom_polygon(data = hulls08, mapping = aes(x = h, y = v, linetype = "2008"), colour = "black",
  #              fill = "NA") +
  # geom_polygon(data = hulls16, mapping = aes(x = h, y = v, linetype = "2016"), colour = "black",
  #              fill = "NA") +
  scale_fill_gradient2(low="blue",mid="white", high="red", midpoint=0) +
  coord_cartesian(xlim=xrng, ylim=yrng) +
  guides(colour=FALSE) +
  xlab("Horizontal Location") +
  ylab("Height above Ground (ft)") +
  ggtitle("Change in CU Distribution (2008 and 2016)") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave(file = "Output/fig3b.png")

# Figure 3 (Hitter Adjustments)
ff_32 <- pitch %>%
  filter(reg_season + post_season != 0) %>%
  filter(pitch_type == "FF") %>%
  filter((px > -1) & (px < 1)) %>%
  filter((b == 3) & (s == 2))

meanz_pitch <- ff_32 %>%
  group_by(year) %>%
  summarise(mz = median(pz)) %>%
  ungroup()

ff_32 <- ff_32 %>%
  mutate(take = (des %in% c("Ball", "Called Strike", "Ball In Dirt")))

gam_ff32 <- gam(take ~ s(pz, by = factor(year), bs = "fs"), data = ff_32)

years = 2008:2016
vertz = seq(1, 2, by = .001)
dat_ele = expand.grid(years, vertz)
colnames(dat_ele) = c("year", "pz")
dat_ele$pred = abs(predict(gam_ff32, dat_ele, type = 'response') - 0.5)
dat_ele = dat_ele %>%
  group_by(year) %>%
  arrange(pred) %>%
  filter(row_number() == 1) %>%
  dplyr::select(-pred) %>%
  arrange(year)

ele_current = as.numeric(dat_ele[which(dat_ele$year == 2016),"pz"])
dat_ele$difference = dat_ele$pz - ele_current
dat_ele$change = c(NA, diff(dat_ele$pz, lag = 1))

dat_ele <- dat_ele %>%
  dplyr::select(year, pz, change, difference)

print(xtable(dat_ele, digits= 3), include.rownames = F)

ggplot(dat_ele, aes(x = year, y = pz)) + 
  geom_line(size = 2, color = "red") + 
  geom_point(size = 2, color = "black") +
  xlab("Year") +
  ylab("Height Above Ground (ft)") +
  ggtitle("Empirical Lower Edge over Time") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggplot(meanz_pitch, aes(x = year, y = mz, group = 1)) + 
  geom_line(size = 2, color = "red") + 
  geom_point(size = 2, color = "black") +
  xlab("Year") +
  ylab("Height Above Ground (ft)") +
  ggtitle("Empirical Lower Edge over Time") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave("Output/fig2b.png")


# Slugging Percentage
pitch_slg = pitch_study %>%
  filter(pitch_type == "FF") %>%
  filter(des %in% c("In play, no out", "In play, out(s)", "In play, run(s)")) %>%
  filter(!(event %in% c("Catcher Interference", "Fan interference", "Sac Bunt",
                        "Fielders Choice", "Fielders Choice Out", "Field Error",
                        "Sacrifice Bunt DP")))

pitch_slg$TB = (pitch_slg$event == "Single") + 2*(pitch_slg$event == "Double") + 
  3*(pitch_slg$event == 'Triple') + 4*(pitch_slg$event == "Home Run")

slg_gam = gam(TB ~ s(px, pz), data = pitch_slg)
x_limitz <- seq(-2,2,by = 0.025)
z_limitz <- seq(0,5, by = 0.025)
dbat <- expand.grid(x_limitz, z_limitz)
names(dbat) = c("px", "pz")
dbat$pred = predict(slg_gam, dbat, type = "response")

ggplot(dbat, aes(px, pz)) + 
  geom_tile(aes(fill = pred)) +
  geom_line(data = sz_coords, mapping = aes(x = px, y = pz, group = group), 
            colour = "black") +
  xlab("Horizontal Location") +
  ylab("Height Above Ground (ft)") +
  scale_fill_gradient2(low=("blue"),mid="white", high=("red"), midpoint=0.2,
                       name = "SLG") +
  ggtitle("Slugging Percentage/Balls in Play") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave("Output/fig4b.png")


# Plot what happened over the ten years (Figure 5)
at_bats <- pitch %>%
  filter(reg_season + post_season != 0) %>%
  filter((b == 0) & (s == 0)) %>%
  group_by(gameday_link, num) %>%
  filter(row_number() == 1) %>%
  dplyr::select(year, event) 

walk_years <- at_bats %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(walk_rate = mean(event %in% c("Walk", "Intentional Walk", "Hit By Pitch"))) %>%
  ungroup()


ggplot(walk_years, aes(x = year, y = walk_rate, group = 1)) +
  geom_line(colour = "blue", size = 2) +
  geom_point(size = 2) +
  xlab("Year") +
  ylab("Walk Rate") +
  ggtitle("Walk Rate over Time") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave("Output/fig5a.png")

so_years <- at_bats %>%
  ungroup() %>%
  group_by(year) %>%
  summarise(walk_rate = mean(event %in% c("Strikeout", "Strikeout - DP"))) %>%
  ungroup()


ggplot(so_years, aes(x = year, y = walk_rate, group = 1)) +
  geom_line(colour = "red", size = 2) +
  geom_point(size = 2) +
  xlab("Year") +
  ylab("Strikeout Rate") +
  ggtitle("Strikeout Rate over Time") +
  theme(plot.title = element_text(face = "bold", hjust = 1/2, size = 20)) +
  theme(axis.text=element_text(size=12),
        axis.title=element_text(size=14))

ggsave("Output/fig5b.png")