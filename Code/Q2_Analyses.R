install.packages(c("lme4", "lmerTest", "car", "performance", "FSA", "dplyr"))
library(lme4)
library(lmerTest)
library(car)
library(performance)
library(FSA)
library(dplyr)

data <- read.csv("Data/Cleaned/dissertation_data - main.csv")

data$site <- as.factor(data$site)
data$fg_richness <- as.numeric(data$fg_richness)
data$functional_redundancy <- as.numeric(data$functional_redundancy)
data$functional_dispersion <- as.numeric(data$functional_dispersion)
data$mean_sst <- as.numeric(data$mean_sst)

data <- data %>%
  group_by(site) %>%
  mutate(
    sst_site_mean = mean(mean_sst, na.rm = TRUE),     # Between-site effect
    sst_within = mean_sst - sst_site_mean) %>%       # Within-site effect
  ungroup()

data$sst_site_mean_z <- scale(data$sst_site_mean)
data$sst_within_z <- scale(data$sst_within)

#--------------------Functional Group Richness--------------------#

model_fg <- lmer(fg_richness ~ sst_site_mean_z + sst_within_z + (1 | site), data = data)

summary(model_fg)
anova(model_fg)

res_fg <- resid(model_fg)
qqnorm(res_fg); qqline(res_fg)
plot(fitted(model_fg), res_fg); abline(h = 0, lty = 2)

rich_site <- aggregate(fg_richness ~ site + sst_site_mean, data = data, mean)

plot(rich_site$sst_site_mean,
     rich_site$fg_richness,
     pch = 16,
     col = "steelblue",
     xlab = "Mean Sea Surface Temperature (°C)",
     ylab = "Functional Group Richness")

abline(lm(fg_richness ~ sst_site_mean, data = rich_site), lwd = 2)

model_rich_lm <- lm(fg_richness ~ sst_site_mean, data = rich_site)
new_x <- seq(min(rich_site$sst_site_mean),
             max(rich_site$sst_site_mean), length = 100)

pred <- predict(model_rich_lm,
                newdata = data.frame(sst_site_mean = new_x),
                interval = "confidence")

lines(new_x, pred[,2], lty = 2)
lines(new_x, pred[,3], lty = 2)

#--------------------Functional Redundancy--------------------#

model_fr <- lmer(functional_redundancy ~ sst_site_mean_z + sst_within_z + (1 | site), data = data)

summary(model_fr)
anova(model_fr)

res_fr <- resid(model_fr)
qqnorm(res_fr); qqline(res_fr)
plot(fitted(model_fr), res_fr); abline(h = 0, lty = 2)

fr_site <- aggregate(functional_redundancy ~ site + sst_site_mean, data = data, mean)

plot(fr_site$sst_site_mean,
     fr_site$functional_redundancy,
     pch = 16,
     col = "steelblue",
     xlab = "Mean Sea Surface Temperature (°C)",
     ylab = "Functional Redundancy")

abline(lm(functional_redundancy ~ sst_site_mean, data = fr_site), lwd = 2)

model_fr_lm <- lm(functional_redundancy ~ sst_site_mean, data = fr_site)
new_x <- seq(min(fr_site$sst_site_mean),
             max(fr_site$sst_site_mean), length = 100)

pred <- predict(model_fr_lm,
                newdata = data.frame(sst_site_mean = new_x),
                interval = "confidence")

lines(new_x, pred[,2], lty = 2)
lines(new_x, pred[,3], lty = 2)

#--------------------Functional Dispersion--------------------#

model_fd <- lmer(functional_dispersion ~ sst_site_mean_z + sst_within_z + (1 | site), data = data)

summary(model_fd)
anova(model_fd)

res_fd <- resid(model_fd)
qqnorm(res_fd); qqline(res_fd)
plot(fitted(model_fd), res_fd); abline(h = 0, lty = 2)

fd_site <- aggregate(functional_dispersion ~ site + sst_site_mean, data = data, mean)

plot(fd_site$sst_site_mean,
     fd_site$functional_dispersion,
     pch = 16,
     col = "steelblue",
     xlab = "Mean Sea Surface Temperature (°C)",
     ylab = "Functional Dispersion")

model_fd_lm <- lm(functional_dispersion ~ sst_site_mean, data = fd_site)
abline(model_fd_lm, lwd = 2)

new_x <- seq(min(fd_site$sst_site_mean),
             max(fd_site$sst_site_mean), length = 100)

pred <- predict(model_fd_lm,
                newdata = data.frame(sst_site_mean = new_x),
                interval = "confidence")

lines(new_x, pred[,2], lty = 2)
lines(new_x, pred[,3], lty = 2)
