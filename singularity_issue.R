est_true <- tidy(mod_true_interaction, conf.int = TRUE)
est_pred2 <- pool(mod_imp_interaction) |> tidy(conf.int = TRUE)

# edit pred
pred[, "popularity_ij"] <- 1
imp_pred1 <- mice(
  dat, 
  method = meth, 
  predictorMatrix = pred,
  maxit = 10,
  printFlag = FALSE)
mod_imp_interaction_pred1 <- with(
  imp_pred1,
  lmer(
    popularity_ij ~ gender_ij + extraversion_ij + experience_j + 
      extraversion_ij:experience_j + (1  + extraversion_ij | cluster_id),  
    REML = FALSE, 
    control = lmerControl(optimizer = "bobyqa"))
) 
est_pred1 <- pool(mod_imp_interaction_pred1) |> tidy(conf.int = TRUE)

# edit pred
pred[, "popularity_ij"] <- 4
pred["extraversion_experience", "popularity_ij"] <- 0
imp_pred4 <- mice(
  dat, 
  method = meth, 
  predictorMatrix = pred,
  maxit = 10,
  printFlag = FALSE)
mod_imp_interaction_pred4 <- with(
  imp_pred4,
  lmer(
    popularity_ij ~ gender_ij + extraversion_ij + experience_j + 
      extraversion_ij:experience_j + (1  + extraversion_ij | cluster_id),  
    REML = FALSE, 
    control = lmerControl(optimizer = "bobyqa"))
) 
est_pred4 <- pool(mod_imp_interaction_pred4) |> tidy(conf.int = TRUE)

# bias
est_true$estimate
est_pred4$estimate


# popmis ncr datasets
con <- url("https://www.gerkovink.com/mimp/popular.RData")
load(con)

dat_NCR <- popNCR |> 
  dplyr::rename(
    unit_id = pupil, 
    cluster_id = class,
    popularity_ij =  popular,
    gender_ij = sex,
    extraversion_ij = extrav,
    experience_j = texp,
    assessment_ij = popteach
  ) |>
  mutate(
    cluster_id = as.numeric(cluster_id),
    gender_ij = as.numeric(gender_ij))
dat_NCR <- dat_NCR[, c("unit_id", "cluster_id", "popularity_ij", "gender_ij", "extraversion_ij", "experience_j", "assessment_ij")]
dat_NCR$extraversion_experience <- NA


imp_NCR <- mice(
  dat_NCR, 
  method = meth, 
  predictorMatrix = pred,
  maxit = 10,
  printFlag = FALSE)
mod_imp_NCR <- with(
  imp_NCR,
  lmer(
    popularity_ij ~ gender_ij + extraversion_ij + experience_j + 
      extraversion_ij:experience_j + (1  + extraversion_ij | cluster_id),  
    REML = FALSE, 
    control = lmerControl(optimizer = "bobyqa"))
) 
est_NCR <- pool(mod_imp_NCR) |> tidy(conf.int = TRUE)
