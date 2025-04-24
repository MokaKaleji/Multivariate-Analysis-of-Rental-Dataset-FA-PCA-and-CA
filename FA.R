################################################################################
#                        Rental Dataset: Factor Analysis                         #
################################################################################
# Author: Moka Kaleji                                                              
# Affiliation: Statistics for high-dimensional dataset, University of Bologna            
#                                                                                 
# Description:
#   This script performs exploratory Factor Analysis (FA) on the rental dataset.
#   Steps include:
#     1. Data import and standardization
#     2. Suitability diagnostics (Bartlett’s test, KMO)
#     3. Parallel analysis to determine factor count
#     4. Maximum Likelihood FA with varimax rotation
#     5. Model fit evaluation and residual diagnostics
#     6. Visualization of factor scores (2D and 3D)
#     7. Extraction and export of loadings and communalities
################################################################################
# Load necessary libraries for factor analysis and visualization
library(psych)           # Core package for psychological statistics, including FA
library(GPArotation)     # Provides various factor rotation methods
library(corrplot)        # For visualizing correlation matrices
library(nFactors)        # For determining the number of factors
library(ggplot2)         # For data visualization
library(plotly)          # For interactive plots
library(scatterplot3d)   # For 3D scatterplots
library(knitr)           # For creating tables
library(kableExtra)      # Enhances table styling
library(officer)         # For exporting Word documents
library(flextable)       # For creating Word-friendly tables

################################################################################

# Load the dataset
data <- read.table("C:/Users/MKK/OneDrive/Desktop/rental.txt", header = TRUE)

# Remove the non-numeric identifier column (e.g., city names)
data1 <- data[, -1]

# Check for missing values (required before proceeding with multivariate analysis)
sum(is.na(data1))  # Should be zero; otherwise, handle imputation or removal

# Plot raw data to inspect trends and scale before standardization
ts.plot(data1, gpars = list(col = rainbow(ncol(data1))))

# Standardize data: transforms variables to have mean 0 and standard deviation 1
data_scaled <- scale(data1)

# Visualize standardized time series
ts.plot(data_scaled, gpars = list(col = rainbow(ncol(data_scaled))))

################################################################################
# Step 1: Assessing Factorability of the Dataset

# Compute correlation matrix (underlying basis for FA)
cor_matrix <- cor(data_scaled)

# Visual inspection of correlation matrix (strong clusters indicate factor structure)
corrplot(cor_matrix, method = "color", tl.cex = 0.7)

# Bartlett’s test of sphericity – tests if the correlation matrix is an identity matrix
# Significant p-value indicates that FA is appropriate
cortest.bartlett(cor(data_scaled), n = nrow(data_scaled))

# Kaiser-Meyer-Olkin (KMO) measure of sampling adequacy
# KMO > 0.6 indicates that FA is suitable
kmo_result <- KMO(data_scaled)

# Convert KMO results into a data frame for documentation
kmo_table <- data.frame(
  Variable = c("Overall MSA", names(kmo_result$MSAi)),  # Variable names
  MSA_Value = c(kmo_result$MSA, kmo_result$MSAi)        # KMO scores
)

# Create a formatted Word table for reporting KMO values
ft <- flextable(kmo_table) %>%
  set_caption("Kaiser-Meyer-Olkin (KMO) Test Results") %>%
  autofit()

# Export the KMO table to a Word document
doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "KMO_result.docx")

################################################################################
# Step 2: Determining the Number of Factors

# Use parallel analysis and scree plot to decide the optimal number of factors
# Parallel analysis compares actual data eigenvalues with simulated random data
fa.parallel(data_scaled, fa = "fa", n.iter = 100)

################################################################################
# Step 3: Factor Extraction and Rotation

# Specify number of factors to extract (based on results from Step 2)
num_factors <- 3

# Perform Maximum Likelihood Factor Analysis (MLFA) with Varimax rotation
# Varimax is an orthogonal rotation that simplifies interpretation
fa_model <- fa(data_scaled, nfactors = num_factors, rotate = "varimax", fm = "ml")

# Display factor loadings (correlations between variables and latent factors)
print(fa_model$loadings)

# Convert loadings to a data frame for documentation
factor_loadings <- as.data.frame(unclass(fa_model$loadings))
factor_loadings <- cbind(Variable = rownames(factor_loadings), factor_loadings)
rownames(factor_loadings) <- NULL

# Create and save loadings table in Word format
ft <- flextable(factor_loadings) %>%
  set_caption("Factor Loadings") %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Factor_Loadings.docx")

################################################################################
# Step 4: Interpretation of Factors

# Visual representation of factor structure (loadings)
fa.diagram(fa_model)

################################################################################
# Step 5: Model Fit Evaluation

# Check chi-square value for goodness-of-fit test
# Non-significant p-value indicates good model fit
fa_model$chi
chi_square_statistic <- fa_model$chi
df <- fa_model$dof
p_value <- 1 - pchisq(chi_square_statistic, df)
p_value

# Inspect residuals between observed and reproduced correlations (should be small)
residuals <- fa_model$residual
print(residuals)

################################################################################
# Step 6: Factor Score Visualization

# Extract estimated factor scores (individuals' position on each factor)
factor_scores = fa_model$scores

# 2D factor score plot for the first two factors
plot(factor_scores[,1], factor_scores[,2],
     main = "Factor Score Plot", xlab = "Factor 1", ylab = "Factor 2", pch = 19, col = "blue")
grid()

# 3D static visualization of factor scores
scatterplot3d(factor_scores[, 1], factor_scores[, 2], factor_scores[, 3], 
              main = "3D Factor Score Plot", xlab = "Factor 1", ylab = "Factor 2", zlab = "Factor 3", color = "blue")

# 3D interactive plot using Plotly
plot_ly(x = factor_scores[, 1], y = factor_scores[, 2], z = factor_scores[, 3], 
        type = "scatter3d", mode = "markers", 
        marker = list(color = "blue", size = 5)) %>%
  layout(title = "3D Factor Score Plot",
         scene = list(xaxis = list(title = "Factor 1"),
                      yaxis = list(title = "Factor 2"),
                      zaxis = list(title = "Factor 3")))

################################################################################
# Step 7: Communality Analysis

# Communality indicates the proportion of variance in each variable explained by the factors
communality_table <- data.frame(
  Variable = names(fa_model[["communality"]]),
  Communality = fa_model[["communality"]]
)

# Export communalities to Word document
ft <- flextable(communality_table) %>%
  set_caption("Communality Values") %>%
  autofit()

doc <- read_docx() %>%
  body_add_flextable(ft)

print(doc, target = "Communality_Table.docx")

################################################################################
# Step 8: Comparison of Rotation Methods

# Plot loadings for first and second factor with Varimax rotation
plot(fa_model$loadings[,1], col = "blue", pch = 18, xlab = "Variables", ylab = "Value", 
     main = "Varimax Rotation: Loadings of the First Factor")
abline(h = 0)

plot(fa_model$loadings[,2], col = "blue", pch = 18, xlab = "Variables", ylab = "Value", 
     main = "Varimax Rotation: Loadings of the Second Factor")
abline(h = 0)

# Apply Promax (oblique) rotation using factanal
out2.p <- factanal(x = data_scaled, factors = 2, rotation = "promax")

# Plot Promax loadings
plot(out2.p$loadings[,1], col = "blue", pch = 18, xlab = "Variables", ylab = "Value", 
     main = "Promax Rotation: Loadings of the First Factor")
abline(h = 0)

plot(out2.p$loadings[,2], col = "blue", pch = 18, xlab = "Variables", ylab = "Value", 
     main = "Promax Rotation: Loadings of the Second Factor")
abline(h = 0)

# Quartimax (alternative orthogonal rotation)
out2.q <- factanal(x = data_scaled, factors = 2, rotation = "quartimax")

# Plot Quartimax loadings
plot(out2.q$loadings[,1], col = "blue", pch = 18, xlab = "Variables", ylab = "Value", 
     main = "Quartimax Rotation: Loadings of the First Factor")
abline(h = 0)

plot(out2.q$loadings[,2], col = "blue", pch = 18, xlab = "Variables", ylab = "Value", 
     main = "Quartimax Rotation: Loadings of the Second Factor")
abline(h = 0)
