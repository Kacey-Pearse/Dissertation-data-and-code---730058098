install.packages(c("lme4", "lmerTest", "car", "performance", "FSA", "PMCMRplus"))
library(lme4)
library(lmerTest)
library(car)    
library(performance)
library(FSA)
library(PMCMRplus)

data <- read.csv("Data/Cleaned/dissertation_data - main.csv")

data$site <- as.factor(data$site)
data$date <- as.factor(data$date)

data$sst_c <- scale(data$mean_sst, center=TRUE, scale=TRUE)
data$wave_c <- scale(data$wave_exposure, center=TRUE, scale=TRUE)
data$tide_c <- scale(data$mean_tidal_range, center=TRUE, scale=TRUE)

par(mar = c(8,5,2,1))   # more space for site labels
par(cex.lab = 1.2)      # axis label size
par(cex.axis = 0.9)
#--------------------Functional Group Richness--------------------#
anova_fg <- aov(fg_richness ~ site, data = data)
anova_fg

shapiro.test(residuals(anova_fg)) #assumptions violated

qqnorm(residuals(anova_fg))
qqline(residuals(anova_fg)) #assumptions violated

leveneTest(fg_richness~site, data=data) #assumptions satisfied

kruskal.test(fg_richness~site, data=data) #significant

dunnTest(fg_richness ~ site, data = data, method = "bh")
#significant difference in fg richness only between castle beach and menai bridge beach

site_order_rich <- names(sort(tapply(data$fg_richness, data$site, median)))
data$site_rich <- factor(data$site, levels = site_order_rich)


boxplot(fg_richness ~ site_rich,
        data = data,
        col = "steelblue",
        border = "black",
        ylab = "Functional Group Richness",
        xlab = "Site")

stripchart(fg_richness ~ site_rich,
           data = data,
           method = "jitter",
           pch = 16,
           col = rgb(0,0,0,0.6),
           vertical = TRUE,
           add = TRUE)
medians <- tapply(data$fg_richness, data$site_rich, median)
points(1:length(medians), medians, pch = 18, cex = 1.5)

#--------------------Functional Redundancy--------------------#
anova_fr <- aov(functional_redundancy ~ site, data = data)
summary(anova_fr)

shapiro.test(residuals(anova_fr)) #assumptions satisfied

leveneTest(functional_redundancy ~ site, data = data) #assumptions satisfied

TukeyHSD(anova_fr) 
#functional_redundancy significantly higher at Castle Beach

means <- tapply(data$functional_redundancy, data$site, mean)
ses <- tapply(data$functional_redundancy, data$site,
              function(x) sd(x)/sqrt(length(x)))

site_order_fr <- names(sort(tapply(data$functional_redundancy, data$site, mean)))
data$site_fr <- factor(data$site, levels = site_order_fr)

boxplot(functional_redundancy ~ site_fr,
        data = data,
        col = "steelblue",
        border = "black",
        ylab = "Functional Redundancy",
        xlab = "Site")

stripchart(functional_redundancy ~ site_fr,
           data = data,
           method = "jitter",
           pch = 16,
           col = rgb(0,0,0,0.6),
           vertical = TRUE,
           add = TRUE)

#--------------------Functional Dispersion--------------------#
anova_fd <- oneway.test(functional_dispersion ~ site, data = data, var.equal = FALSE)
anova_fd

gh_fd <- gamesHowellTest(functional_dispersion ~ site, data = data)
gh_fd

shapiro.test(residuals(anova_fd)) #Assumptions violated

leveneTest(functional_dispersion ~ site, data = data)

qqnorm(residuals(anova_fd))
qqline(residuals(anova_fd)) #Assumptions violated

kruskal.test(functional_dispersion ~ site, data = data) #Significant

dunnTest(functional_dispersion ~ site, data = data, method = "holm")

site_order_fd <- names(sort(tapply(data$functional_dispersion, data$site, mean)))
data$site_fd <- factor(data$site, levels = site_order_fd)

boxplot(functional_dispersion ~ site_fd,
        data = data,
        col = "steelblue",
        border = "black",
        ylab = "Functional Dispersion",
        xlab = "Site")

stripchart(functional_dispersion ~ site_fd,
           data = data,
           method = "jitter",
           pch = 16,
           col = rgb(0,0,0,0.6),
           vertical = TRUE,
           add = TRUE)
