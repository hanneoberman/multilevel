# environment
set.seed(123)
library(haven)
library(mice)
library(ggplot2)
theme_set(theme_classic())

# complete data
popular <- haven::read_sav("popular2.sav") |> zap_formats()
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

# reorder columns, drop unused ones
popular <- popular[, c("unit_id", "cluster_id", "popularity_ij", "gender_ij", "extraversion_ij", "experience_j", "assessment_ij")]

# create 'data entry error'
popular[1, "experience_j"] <- 4

# visualize outcome per cluster
ggplot(popular, aes(popularity_ij, color = as.factor(cluster_id))) + 
  geom_density()
ggplot(popular, aes(teacher_evaluation_ij, popularity_ij, color = as.factor(cluster_id))) + 
  geom_point() + 
  geom_smooth(se = FALSE, method = "lm")

# run analysis models Hox et al chapter 2 https://multilevel-analysis.sites.uu.nl/wp-content/uploads/sites/27/2018/02/02Ch2-Basic3449.pdf
lm(popularity_ij ~ 1, data = popular)
lmer(popularity_ij ~ (1 | cluster_id), data = popular, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + (1 | cluster_id), data = popular, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + (1  + extraversion_ij | cluster_id), data = popular, REML = FALSE)
lmer(popularity_ij ~ gender_ij + extraversion_ij + experience_j + extraversion_ij:experience_j + (1  + extraversion_ij | cluster_id), data = popular, REML = FALSE)

# save as RData
save(popular, file = "popular.RData")

# incomplete data
set.seed(234)
names(popular)
patterns <- rbind(
  c(1, 1, 0, 1, 1, 1, 1),
  c(1, 1, 0, 1, 0, 1, 1),
  c(1, 1, 0, 1, 0, 1, 0),
  c(1, 1, 0, 1, 1, 1, 0))
frequency <- c(0.5, 0.1, 0.3, 0.1)
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

# evaluate missing data pattern
md.pattern(popular_MAR)

# save as RData
save(popular_MAR, file = "popular_MAR.RData")

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


# n_cluster <- 4
# n_unit <- 50
# n_total <- n_cluster * n_unit
# 
# 
# dat <- data.frame(
#   outcome = rnorm(n_total),
#   cluster_id = rep(1:n_cluster, each = n_unit),
#   pred_cat_clust = sample(1:3, n_total, replace = TRUE),
#   pred_cont_clust = rnorm(n_total),
#   pred_cat_unit = sample(1:3, n_total, replace = TRUE),
#   pred_cont_unit = rnorm(n_total),
#   aux_cat_clust = sample(1:3, n_total, replace = TRUE),
#   aux_cont_clust = rnorm(n_total),
#   aux_cat_unit = sample(1:3, n_total, replace = TRUE),
#   aux_cont_unit = rnorm(n_total)
# )
# 
# 
# dat <- data.frame(
#   Y_ij = rnorm(n_total),
#   unit_id = 1:n_total,
#   cluster_id = rep(1:n_cluster, each = n_unit),
#   X1_ij = sample(1:3, n_total, replace = TRUE),
#   X2_ij = rnorm(n_total),
#   Z1_j = sample(1:3, n_total, replace = TRUE),
#   Z2_j = rnorm(n_total),
#   aux_cat_clust = sample(1:3, n_total, replace = TRUE),
#   aux_cont_clust = rnorm(n_total),
#   aux_cat_unit = sample(1:3, n_total, replace = TRUE),
#   aux_cont_unit = rnorm(n_total)
# )
# 
# library(lme4)
# mod <- lmer(outcome ~ . + (1 | cluster_id), data = dat)
# summary(mod)
# 
# 
# # dependencies
# library(simglm)
# library(lme4)
# 
# # set seed
# set.seed(321) 
# 
# # set parameters of model to be simulated
# sim_arguments <- list(
#   formula = y ~ 1 + weight + age + sex + (1 | neighborhood),
#   reg_weights = c(4, -0.03, 0.2, 0.33),
#   fixed = list(weight = list(var_type = 'continuous', mean = 180, sd = 30),
#                age = list(var_type = 'ordinal', levels = 30:60),
#                sex = list(var_type = 'factor', levels = c('male', 'female'))),
#   randomeffect = list(int_neighborhood = list(variance = 8, var_level = 2)),
#   sample_size = list(level1 = 10, level2 = 20)
# )
# 
# # simulate data
# nested_data <- sim_arguments |>
#   simulate_fixed(data = NULL) |>
#   simulate_randomeffect(sim_arguments) |>
#   simulate_error(sim_arguments) |>
#   generate_response(sim_arguments)
# 
# # fit the same model to the data
# fit <- lmer(y ~ 1 + weight + age + sex + (1 | neighborhood),
#             data = nested_data)
# 
# # check that you can recover the same parameters you built the model with
# # e.g., the fixed effect estimates should be close to the reg_weights used above
# summary(fit)


# popularityij = 00+ 10 genderij + 20 extraversionij + 01 experiencej 
# +11 genderij experiencej +21 extraversionijexperiencej
# + u1j genderij + u2j extraversionij + u0j+ eij .
