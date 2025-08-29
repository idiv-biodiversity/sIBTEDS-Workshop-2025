################################################################################
# Alfa & Gamma Rarefaction Curves
# CMAB
################################################################################
################################################################################
getwd()
setwd("C:YOUR/WORKING/DIRECTORY")

################################################################################
################################################################################
################################################################################
library(vegan)

################################################################################
################################################################################
################################################################################
# Alpha-scale rarefaction
#Species richness at various sampling depths for each sample.

# Example community data: rows are samples, columns are species, and values are counts
data(BCI) # Barro Colorado Island dataset in vegan
View(BCI)

# Rarefaction for each sample
rare_alpha <- rarecurve(BCI, step = 5, sample = min(rowSums(BCI)), col = "blue", cex = 0.6, xlab = "Number of individuals", ylab = "Number of Species")
#step: Defines the interval for subsampling.
#sample: Standardizes to the minimum sample size across all samples (min(rowSums(BCI)) ensures all samples are rarefied to the same depth).
#The rarecurve() function plots the curves, but the output object (rare_alpha) contains the rarefied species counts.


################################################################################
################################################################################
################################################################################
# Gamma-scale rarefaction
# Gamma-scale rarefaction estimates species richness across the entire dataset (community) at varying levels of pooled sampling effort.

# Pool all samples together
pooled <- colSums(BCI)
View(pooled)
str(pooled)

# Rarefied richness at various sample sizes
max_samples <- sum(pooled) # Total sample size

gamma_rarefied <- rarefy(pooled, sample = seq(1, max_samples, by = 100))

# Plot gamma-scale rarefaction
plot(seq(1, max_samples, by = 100), gamma_rarefied, type = "l",
     xlab = "Number of individuals", ylab = "Number of Species",
     main = "Gamma-scale Rarefaction Curve", col = "green")


################################################################################
################################################################################
################################################################################
