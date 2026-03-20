library(lme4)
library(lmerTest)
library(car)
library(performance)
library(dplyr)

data <- read.csv("Data/Cleaned/dissertation_data - main.csv")

data$site <- as.factor(data$site)

data$fg_richness <- as.numeric(data$fg_richness)
data$functional_redundancy <- as.numeric(data$functional_redundancy)
data$functional_dispersion <- as.numeric(data$functional_dispersion)

data$mean_sst <- as.numeric(data$mean_sst)
data$wave_exposure <- as.numeric(data$wave_exposure)
data$mean_tidal_range <- as.numeric(data$mean_tidal_range)

data <- data %>%
  mutate(
    sst_z   = as.numeric(scale(mean_sst)),
    wave_z  = as.numeric(scale(wave_exposure)),
    tidal_z = as.numeric(scale(mean_tidal_range)))

#--------------------Functional Group Richness--------------------#

fg_M1 <- lm(fg_richness ~ sst_z, data = data)

fg_M2 <- lm(fg_richness ~ sst_z + wave_z, data = data)

fg_M3 <- lm(fg_richness ~ sst_z + wave_z + tidal_z, data = data)

anova(fg_M1, fg_M2)
anova(fg_M2, fg_M3)

AIC(fg_M1, fg_M2, fg_M3)

coefs_fg <- summary(fg_M3)$coefficients

coefs_fg <- coefs_fg[-1, ]

est_fg  <- coefs_fg[,1]
se_fg   <- coefs_fg[,2]

upper_fg <- est_fg + 1.96 * se_fg
lower_fg <- est_fg - 1.96 * se_fg

plot(est_fg,
     1:length(est_fg),
     xlim = range(c(lower_fg, upper_fg)),
     pch = 16,
     col = "steelblue",
     yaxt = "n",
     main = "(a)",
     font.main = 1,
     cex.main = 0.85,
     xlab = "Standardised Effect Size (β)",
     ylab = "")

axis(2, at = 1:length(est_fg), labels = rownames(coefs_fg), las = 1)

arrows(lower_fg, 1:length(est_fg),
       upper_fg, 1:length(est_fg),
       angle = 90, code = 3, length = 0.05)

abline(v = 0, lty = 2)

#--------------------Functional Redundancy--------------------#

fr_M1 <- lm(functional_redundancy ~ sst_z, data = data)

fr_M2 <- lm(functional_redundancy ~ sst_z + wave_z, data = data)

fr_M3 <- lm(functional_redundancy ~ sst_z + wave_z + tidal_z, data = data)

anova(fr_M1, fr_M2)
anova(fr_M2, fr_M3)

AIC(fr_M1, fr_M2, fr_M3)

coefs_fr <- summary(fr_M3)$coefficients
coefs_fr <- coefs_fr[-1, ]

est_fr  <- coefs_fr[,1]
se_fr   <- coefs_fr[,2]

upper_fr <- est_fr + 1.96 * se_fr
lower_fr <- est_fr - 1.96 * se_fr

plot(est_fr,
     1:length(est_fr),
     xlim = range(c(lower_fr, upper_fr)),
     pch = 16,
     col = "steelblue",
     yaxt = "n",
     main = "(b)",
     font.main = 1,
     cex.main = 0.85,
     xlab = "Standardised Effect Size (β)",
     ylab = "")

axis(2, at = 1:length(est_fr), labels = rownames(coefs_fr), las = 1)

arrows(lower_fr, 1:length(est_fr),
       upper_fr, 1:length(est_fr),
       angle = 90, code = 3, length = 0.05)

abline(v = 0, lty = 2)

#--------------------Functional Dispersion--------------------#

fd_M1 <- lm(functional_dispersion ~ sst_z, data = data)

fd_M2 <- lm(functional_dispersion ~ sst_z + wave_z, data = data)

fd_M3 <- lm(functional_dispersion ~ sst_z + wave_z + tidal_z, data = data)

anova(fd_M1, fd_M2)
anova(fd_M2, fd_M3)

AIC(fd_M1, fd_M2, fd_M3)

coefs_fd <- summary(fd_M3)$coefficients
coefs_fd <- coefs_fd[-1, ]

est_fd  <- coefs_fd[,1]
se_fd   <- coefs_fd[,2]

upper_fd <- est_fd + 1.96 * se_fd
lower_fd <- est_fd - 1.96 * se_fd

plot(est_fd,
     1:length(est_fd),
     xlim = range(c(lower_fd, upper_fd)),
     pch = 16,
     col = "steelblue",
     yaxt = "n",
     main = "(c)",
     font.main = 1,
     cex.main = 0.85,
     xlab = "Standardised Effect Size (β)",
     ylab = "")

axis(2, at = 1:length(est_fd), labels = rownames(coefs_fd), las = 1)

arrows(lower_fd, 1:length(est_fd),
       upper_fd, 1:length(est_fd),
       angle = 90, code = 3, length = 0.05)

abline(v = 0, lty = 2)
