# environment
set.seed(123)
library(haven)
library(mice)
library(ggplot2)
theme_set(theme_classic())

# complete data
popular <- haven::read_sav("data/popular2.sav") |> 
  zap_formats()

# rename columns
popular <- popular |> 
  dplyr::rename(
    unit_id = pupil, 
    cluster_id = class,
    popularity_ij =  popular,
    gender_ij = sex,
    extraversion_ij = extrav,
    experience_j = texp,
    assessment_ij = popteach
)

# convert gender to factor
popular$gender_ij <- factor(popular$gender_ij, levels = c(0, 1), labels = c("boy", "girl"))

# remove labels other variables
popular <- zap_label(popular)

# reorder columns, drop unused ones
popular <- popular[, c("unit_id", "cluster_id", "popularity_ij", "gender_ij", "extraversion_ij", "experience_j", "assessment_ij")]

# # create 'data entry error'
# popular[1, "experience_j"] <- 4

# visualize outcome per cluster
ggplot(popular, aes(popularity_ij, color = as.factor(cluster_id))) + 
  geom_density()
ggplot(popular, aes(assessment_ij, popularity_ij, color = as.factor(cluster_id))) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm")

# run analysis models Hox et al chapter 2 https://multilevel-analysis.sites.uu.nl/wp-content/uploads/sites/27/2018/02/02Ch2-Basic3449.pdf
lm(popularity_ij ~ 1, data = popular)
lmer(popularity_ij ~ (1 | cluster_id), data = popular, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + (1 | cluster_id), data = popular, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + (1  + extraversion_ij | cluster_id), data = popular, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + extraversion_ij:experience_j + (1  + extraversion_ij | cluster_id), data = popular, REML = FALSE)

# save as RData
save(popular, file = "data/popular.RData")

# incomplete data
set.seed(234)
names(popular)
patterns <- rbind(
  c(1, 1, 0, 1, 1, 1, 1),
  c(1, 1, 0, 1, 0, 1, 1),
  c(1, 1, 0, 1, 0, 1, 0),
  c(1, 1, 0, 1, 1, 1, 0))
frequency <- c(0.5, 0.15, 0.3, 0.05)
popular_MAR <- split(popular, ~cluster_id) |>
  purrr::map_dfr(~ampute(
    .x, 
    prop = 0.4, 
    patterns = patterns,
    freq = frequency,
    mech = "MAR"
    )$amp)
# evaluate missing data pattern
md.pattern(popular_MAR)

# add case with missing teacher assessment and teacher experience
popular_MAR[2, c("experience_j", "assessment_ij")] <- NA
# add cases with missing gender for kids with low popularity
index <- floor(popular$popularity_ij) <= min(floor(popular$popularity_ij))
popular_MAR[index, "gender_ij"] <- NA

# evaluate missing data pattern
md.pattern(popular_MAR)

# save as RData
save(popular_MAR, file = "data/popular_MAR.RData")

# CCA

# run analysis models Hox et al chapter 2 https://multilevel-analysis.sites.uu.nl/wp-content/uploads/sites/27/2018/02/02Ch2-Basic3449.pdf
lm(popularity_ij ~ 1, data = popular_MAR)
lmer(popularity_ij ~ (1 | cluster_id), data = popular_MAR, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + (1 | cluster_id), data = popular_MAR, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + (1  + extraversion_ij | cluster_id), data = popular_MAR, REML = FALSE)
# does not converge
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + extraversion_ij:experience_j + (1  + extraversion_ij | cluster_id), data = popular_MAR, REML = FALSE)
# is singular

# imputation
pred <- quickpred(popular_MAR)
pred[, "unit_id"] <- 0
pred[, "cluster_id"] <- -2
pred[pred == 1] <- 2
popular_MAR_imp <- popular_MAR |> 
  mice::mice(
    m = 1, 
    maxit = 2, 
    method = "2l.pan", 
    pred = pred,
    seed = 123, 
    printFlag = FALSE
  ) |> 
  complete()
# run analysis models Hox et al chapter 2 https://multilevel-analysis.sites.uu.nl/wp-content/uploads/sites/27/2018/02/02Ch2-Basic3449.pdf
lm(popularity_ij ~ 1, data = popular_MAR_imp)
lmer(popularity_ij ~ (1 | cluster_id), data = popular_MAR, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + (1 | cluster_id), data = popular_MAR_imp, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + (1  + extraversion_ij | cluster_id), data = popular_MAR_imp, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + extraversion_ij:experience_j + (1  + extraversion_ij | cluster_id), data = popular_MAR_imp, REML = FALSE)

