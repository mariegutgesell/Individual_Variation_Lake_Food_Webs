##GAMMs and other needs for MS 

library(mgcv)
library(gratia)
library(visreg)
library(car)
source("marie_code/raw_si_df_formation.R")

rm(list = ls()[!ls() %in% c("raw_si_df")])

##Filter to final list of 53 (52) lakes -- have both cisco and lake trout, and have snails and mussels as baselines
si_sub <- raw_si_df %>%
  filter(Trophic_Group %in% c("LakeTrout", "Cisco", "Snail", "Mussel", "Zoops", "Invert"))

lt_cs_df <- si_sub %>%
  filter(Trophic_Group %in% c("LakeTrout", "Cisco"))

test <- lt_cs_df %>%
  select(Lake_Name) %>%
  unique()


required_taxa <- c("Snail", "Mussel")


sites_with_m_s <- si_sub %>%
  #  distinct(WBY, TAXON) %>%     # remove duplicate taxa per site
  group_by(Lake_Name) %>%
  summarize(has_all = all(required_taxa %in% Trophic_Group)) %>%
  filter(has_all) %>%
  pull(Lake_Name)

# Filter original dataframe to just those sites
sites_with_m_s_df <- si_sub %>%
  filter(Lake_Name %in% sites_with_m_s) 

lakes_with_5_LT <- lt_cs_df %>%
  filter(Trophic_Group == "LakeTrout") %>%
  count(Lake_Name) %>%
  filter(n >= 5) %>%
  filter(Lake_Name %in% sites_with_m_s_df$Lake_Name)

final_si_df <- sites_with_m_s_df %>%
  filter(Lake_Name %in% lakes_with_5_LT$Lake_Name) 


test <- final_si_df %>%
  select(Lake_Name) %>%
  unique()

##Bi-plot 
ggplot(final_si_df, aes(x = d13C, y = d15N, color = Trophic_Group)) +
  geom_point(size = 2) +
  theme_classic() +
  xlab("d13C") + 
  ylab("d15N") +
  facet_wrap(~Lake_Name)


final_si_df <-final_si_df %>%
  mutate(trophic_type = case_when(
    startsWith(Trophic_Group, "Snail") ~ "Littoral Baseline", 
    startsWith(Trophic_Group, "Invert") ~ "Littoral Baseline", 
    startsWith(Trophic_Group, "Mussel") ~ "Pelagic Baseline", 
    startsWith(Trophic_Group, "Zoops") ~ "Pelagic Baseline", 
    startsWith(Trophic_Group, "LakeTrout") ~ "LakeTrout", 
    startsWith(Trophic_Group, "Cisco") ~ "Cisco", 
  )) %>%
  filter(Trophic_Group != "Cisco") %>%
  group_by(Lake_Name) %>%
  filter(all(c("Littoral Baseline", "Pelagic Baseline") %in% trophic_type)) %>%
  ungroup()

test3 <- final_si_df %>%
  select(Lake_Name) %>%
  unique()


##METRICS FOR RESPONSE LETTER
##mean zooplankton and mussel signatures for lakes where both available 
required_taxa_2 <- c("Zoops", "Mussel")

sites_with_m_z <- final_si_df %>%
  #  distinct(WBY, TAXON) %>%     # remove duplicate taxa per site
  group_by(Lake_Name) %>%
  summarize(has_all = all(required_taxa_2 %in% Trophic_Group)) %>%
  filter(has_all) %>%
  pull(Lake_Name)

# Filter original dataframe to just those sites
sites_with_m_z_df <- final_si_df %>%
  filter(Lake_Name %in% sites_with_m_z) 

mean_muss_zoop <- sites_with_m_z_df %>%
  filter(Trophic_Group %in% c("Mussel", "Zoops")) %>%
  group_by(Trophic_Group) %>%
  summarise_at(vars(d13C, d15N), list(mean = mean, sd = sd))

##Calculate mean sample size of mussels and snails 
muss_snail_sample_size <- final_si_df %>%
  filter(Trophic_Group %in% c("Mussel", "Snail")) %>%
  group_by(Lake_Name, Trophic_Group) %>%
  count() 

muss_snail_sample_size <- as.data.frame(muss_snail_sample_size)

str(muss_snail_sample_size)

muss_snail_sample_size_mean <- muss_snail_sample_size %>%
  ungroup() %>%
  group_by(Trophic_Group) %>%
  summarise_at(vars(n), list(mean = mean, sd = sd))



##MEAN TROPHIC RESPONSE ------------
##calculating tp and lc 
BSMsm_mixing_model_pred_bound <- function(x){
  df_prep <- x
  LB <- df_prep %>%
    filter(Trophic_Group == "Snail")
  LB_mean_dC <- mean(LB$d13C)
  LB_mean_dN <- mean(LB$d15N)
  PB <- df_prep %>%
    filter(Trophic_Group == "Mussel")
  PB_mean_dC <- mean(PB$d13C)
  PB_mean_dN <- mean(PB$d15N)
  df_pred <- df_prep %>%
    filter(Trophic_Group == "LakeTrout")
  for(i in 1:nrow(df_pred)){
    LC_pred = ((df_pred$d13C - PB_mean_dC)/(LB_mean_dC - PB_mean_dC))
    LC_pred[LC_pred>1] <- 0.999
    LC_pred[LC_pred<0] <- 0.001
    TP_pred = 2 + (((LC_pred * ((df_pred$d15N - LB_mean_dN)/3.4)))+((1-LC_pred)*((df_pred$d15N - PB_mean_dN)/3.4)))
    dN_pred = df_pred$d15N - (((1-LC_pred)*PB_mean_dN)+(LC_pred*LB_mean_dN))
    LC_TP_df_pred <- cbind(LC_pred, TP_pred, dN_pred)
  }
  LC_TP_df_pred <- as.data.frame(LC_TP_df_pred)
  LC_TP_df_pred$Trophic_Group <- df_pred$Trophic_Group
  LC_TP_df_pred$Lake_Name <- df_pred$Lake_Name
  #  LC_TP_df_pred$SI_ID <- df_pred$SI_ID
  LC_TP_df_pred$Source_Project_for_Data <- df_pred$Source_Project_for_Data
  LC_TP_df_pred
}


##calculating tp and lc off of corrected C 
tp_lc_BSMsm <- split(final_si_df, paste0(final_si_df$Lake_Name)) %>%
  map(BSMsm_mixing_model_pred_bound) %>%
  bind_rows()


##Read in lake trait data
lake_df <- read.csv("data/lake_supp_table.csv") %>%
  rename(Lake_Name = "Lake.Name", Area_ha = "Lake.Area..ha.", summer_air_temp = "Summer.Air.Temperature..C.", LaTro_CUE = "Lake.Trout.CUE", total_pred_CUE = "Total.Predator.CUE", sampling_year = "Sampling.Year")


df <- left_join(tp_lc_BSMsm, lake_df, by = "Lake_Name") 


##calculate % of LT outside of baselines
lt_outside_baselines <- df %>%
  filter(LC_pred == 0.001 | LC_pred == 0.999)

##48 out of 773 are outside of baselines 

48/773

lt_outside_baselines_28 <- df %>%
  filter(Source_Project_for_Data %in% c("BSM_Project", "Tom_Johnston")) %>%
  filter(LC_pred == 0.001 | LC_pred == 0.999)
##19 out of 659
19/659

##calculate mean response per lake 
df_summary <- df %>%
  group_by(Lake_Name, Source_Project_for_Data) %>%
  summarise_at(vars(LC_pred, TP_pred, Area_ha, summer_air_temp, LaTro_CUE, total_pred_CUE, sampling_year), list(mean = mean, sd = sd)) %>%
  mutate(log_area_ha = log(Area_ha_mean),
         log_pred_cue = log(total_pred_CUE_mean),
         log_lt_cue = log(LaTro_CUE_mean),
         logit_lc_pred = logit(LC_pred_mean),
         log_tp_pred = log(TP_pred_mean))


df_summary_28 <- df_summary %>%
  filter(Source_Project_for_Data %in% c("BSM_Project", "Tom_Johnston")) %>%
  filter(!is.na(LaTro_CUE_mean))





##GAMMs for Mean Trophic Metrics

hist(df_summary$LC_pred_mean)
hist(df_summary$logit_lc_pred)
hist(df_summary$TP_pred_mean)
hist(df_summary$log_tp_pred)
##Relationships with PNC 
meanPNC_gamm1 <- gam(logit_lc_pred ~ summer_air_temp_mean +log_area_ha + s(sampling_year_mean, bs = "re"),
             data = df_summary, method = "REML")
summary(meanPNC_gamm1)

v <- visreg(meanPNC_gamm1, "summer_air_temp_mean", partial = TRUE, plot = FALSE)
lc_air_1 <- ggplot() +
  geom_line(data = v$fit, aes(x = summer_air_temp_mean, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = summer_air_temp_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = summer_air_temp_mean, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature", y = "Mean PNC (logit)")
lc_air_1

v <- visreg(meanPNC_gamm1, "log_area_ha", partial = TRUE, plot = FALSE)
lc_area_1 <- ggplot() +
  geom_line(data = v$fit, aes(x = log_area_ha, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_area_ha, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Mean PNC (logit)")
lc_area_1

##w/ interaction term:
meanPNC_gamm1_I <- gam(logit_lc_pred ~ summer_air_temp_mean  *log_area_ha + s(sampling_year_mean, bs = "re"),
                     data = df_summary, method = "REML")
summary(meanPNC_gamm1_I)

meanPNC_gamm1_ml <- gam(logit_lc_pred ~ summer_air_temp_mean +log_area_ha + s(sampling_year_mean, bs = "re"),
                     data = df_summary, method = "ML")
summary(meanPNC_gamm1_ml)
##w/ quadratic on temp 
meanPNC_gamm1_q <- gam(logit_lc_pred  ~ summer_air_temp_mean + I(summer_air_temp_mean^2)+log_area_ha + s(sampling_year_mean, bs = "re"),
                     data = df_summary, method = "ML")
summary(meanPNC_gamm1_q)

meanPNC_gamm1_s <- gam(logit_lc_pred  ~ s(summer_air_temp_mean) +s(log_area_ha) + s(sampling_year_mean, bs = "re"),
                       data = df_summary, method = "ML")
summary(meanPNC_gamm1_s)

AIC(meanPNC_gamm1_ml, meanPNC_gamm1_s, meanPNC_gamm1_q)

##Intraspecific competition
meanPNC_gamm1.1_ml <- gam(logit_lc_pred ~ summer_air_temp_mean + log_area_ha + log_lt_cue + s(sampling_year_mean, bs = "re"),
               data = df_summary_28, method = "ML")
summary(meanPNC_gamm1.1_ml)

##with REML -- this for final model, use ML for model selection
meanPNC_gamm1.1 <- gam(logit_lc_pred ~ summer_air_temp_mean + log_area_ha + log_lt_cue + s(sampling_year_mean, bs = "re"),
                       data = df_summary_28, method = "REML")
summary(meanPNC_gamm1.1)

meanPNC_gamm1.1_q <- gam(logit_lc_pred ~ summer_air_temp_mean +I(summer_air_temp_mean^2)+ log_area_ha + log_lt_cue + s(sampling_year_mean, bs = "re"),
               data = df_summary_28, method = "ML")
summary(meanPNC_gamm1.1_q)

meanPNC_gamm1.1_s <- gam(logit_lc_pred ~ s(summer_air_temp_mean) + s(log_area_ha) + s(log_lt_cue) + s(sampling_year_mean, bs = "re"),
                         data = df_summary_28, method = "ML")
summary(meanPNC_gamm1.1_s)

AIC(meanPNC_gamm1.1_ml, meanPNC_gamm1.1_q, meanPNC_gamm1.1_s)

##plot intraspecific comp results
v <- visreg(meanPNC_gamm1.1, "summer_air_temp_mean", partial = TRUE, plot = FALSE)
lc_air_2 <- ggplot() +
  geom_line(data = v$fit, aes(x = summer_air_temp_mean, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = summer_air_temp_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = summer_air_temp_mean, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature", y = "Mean PNC (logit)")
lc_air_2

v <- visreg(meanPNC_gamm1.1, "log_area_ha", partial = TRUE, plot = FALSE)
lc_area_2 <- ggplot() +
  geom_line(data = v$fit, aes(x = log_area_ha, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_area_ha, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Mean PNC (logit)")
lc_area_2


v <- visreg(meanPNC_gamm1.1, "log_lt_cue", partial = TRUE, plot = FALSE)
lc_lt_2 <- ggplot() +
#  geom_line(data = v$fit, aes(x = log_lt_cue, y = visregFit), color = "red", linewidth = 1) +
#  geom_ribbon(data = v$fit, aes(x = log_lt_cue, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_lt_cue, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (log)", y = "Mean PNC (logit)")
lc_lt_2


##Interspecfic competition
meanPNC_gamm1.2 <- gam(logit_lc_pred ~ summer_air_temp_mean + log_area_ha + log_pred_cue + s(sampling_year_mean, bs = "re"),
               data = df_summary_28, method = "REML")
summary(meanPNC_gamm1.2)

##plot interspecific comp results
v <- visreg(meanPNC_gamm1.2, "summer_air_temp_mean", partial = TRUE, plot = FALSE)
lc_air_3 <- ggplot() +
  geom_line(data = v$fit, aes(x = summer_air_temp_mean, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = summer_air_temp_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = summer_air_temp_mean, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature", y = "Mean PNC (logit)")
lc_air_3

v <- visreg(meanPNC_gamm1.2, "log_area_ha", partial = TRUE, plot = FALSE)
lc_area_3 <- ggplot() +
  geom_line(data = v$fit, aes(x = log_area_ha, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_area_ha, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Mean PNC (logit)")
lc_area_3


v <- visreg(meanPNC_gamm1.2, "log_pred_cue", partial = TRUE, plot = FALSE)
lc_pred_3 <- ggplot() +
  #  geom_line(data = v$fit, aes(x = log_lt_cue, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = log_lt_cue, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_pred_cue, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Other Predator CPUE (log)", y = "Mean PNC (logit)")
lc_pred_3

##Relationships w/ Trophic Position
meanTP_gamm2 <- gam(TP_pred_mean ~ summer_air_temp_mean + log_area_ha + s(sampling_year_mean, bs = "re"),
             data = df_summary, method = "REML")
summary(meanTP_gamm2)

v <- visreg(meanTP_gamm2, "summer_air_temp_mean", partial = TRUE, plot = FALSE)
tp_air_1 <- ggplot() +
  geom_line(data = v$fit, aes(x = summer_air_temp_mean, y = visregFit), color = "red", linewidth = 1) +
  geom_ribbon(data = v$fit, aes(x = summer_air_temp_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = summer_air_temp_mean, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature", y = "Mean TP")
tp_air_1

v <- visreg(meanTP_gamm2, "log_area_ha", partial = TRUE, plot = FALSE)
tp_area_1 <- ggplot() +
#  geom_line(data = v$fit, aes(x = log_area_ha, y = visregFit), color = "red", linewidth = 1) +
#  geom_ribbon(data = v$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_area_ha, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Mean TP")
tp_area_1



meanTP_gamm2_I <- gam(TP_pred_mean ~ summer_air_temp_mean * log_area_ha + s(sampling_year_mean, bs = "re"),
                    data = df_summary, method = "REML")
summary(meanTP_gamm2_I)

#mean_response_plot <- ggarrange(lc_area_1, lc_air_1, tp_area_1, tp_air_1, nrow = 2, ncol = 2)
#mean_response_plot

meanTP_gamm2.1 <- gam(TP_pred_mean ~ summer_air_temp_mean + log_area_ha + log_lt_cue + s(sampling_year_mean, bs = "re"),
               data = df_summary_28, method = "REML")
summary(meanTP_gamm2.1)

##plot intraspecific comp results
v <- visreg(meanTP_gamm2.1, "summer_air_temp_mean", partial = TRUE, plot = FALSE)
tp_air_2 <- ggplot() +
  geom_line(data = v$fit, aes(x = summer_air_temp_mean, y = visregFit), color = "red", linewidth = 1, linetype = "dashed") +
  geom_ribbon(data = v$fit, aes(x = summer_air_temp_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = summer_air_temp_mean, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature", y = "Mean TP")
tp_air_2

v <- visreg(meanTP_gamm2.1, "log_area_ha", partial = TRUE, plot = FALSE)
tp_area_2 <- ggplot() +
 # geom_line(data = v$fit, aes(x = log_area_ha, y = visregFit), color = "red", linewidth = 1) +
#  geom_ribbon(data = v$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_area_ha, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Mean TP")
tp_area_2


v <- visreg(meanTP_gamm2.1, "log_lt_cue", partial = TRUE, plot = FALSE)
tp_lt_2 <- ggplot() +
  #  geom_line(data = v$fit, aes(x = log_lt_cue, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = log_lt_cue, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_lt_cue, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (log)", y = "Mean TP")
tp_lt_2


meanTP_gamm2.1_ml <- gam(TP_pred_mean ~ summer_air_temp_mean + log_area_ha + log_lt_cue + s(sampling_year_mean, bs = "re"),
                      data = df_summary_28, method = "ML")
summary(meanTP_gamm2.1_ml)

meanTP_gamm2.1_q <- gam(TP_pred_mean ~ summer_air_temp_mean + I(summer_air_temp_mean^2)+ log_area_ha + log_lt_cue + s(sampling_year_mean, bs = "re"),
                      data = df_summary_28, method = "ML")
summary(meanTP_gamm2.1_q)

meanTP_gamm2.1_s <- gam(TP_pred_mean ~ s(summer_air_temp_mean) + s(log_area_ha) + s(log_lt_cue) + s(sampling_year_mean, bs = "re"),
                      data = df_summary_28, method = "ML")
summary(meanTP_gamm2.1_s)

AIC(meanTP_gamm2.1_ml, meanTP_gamm2.1_q, meanTP_gamm2.1_s)

##interspecific competition
meanTP_gamm2.2 <- gam(TP_pred_mean ~ summer_air_temp_mean + log_area_ha + log_pred_cue + s(sampling_year_mean, bs = "re"),
               data = df_summary_28, method = "REML")
summary(meanTP_gamm2.2)

v <- visreg(meanTP_gamm2.2, "summer_air_temp_mean", partial = TRUE, plot = FALSE)
tp_air_3 <- ggplot() +
  geom_line(data = v$fit, aes(x = summer_air_temp_mean, y = visregFit), color = "red", linewidth = 1, linetype = "dashed") +
  geom_ribbon(data = v$fit, aes(x = summer_air_temp_mean, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = summer_air_temp_mean, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temperature", y = "Mean TP")
tp_air_3

v <- visreg(meanTP_gamm2.2, "log_area_ha", partial = TRUE, plot = FALSE)
tp_area_3 <- ggplot() +
  # geom_line(data = v$fit, aes(x = log_area_ha, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = log_area_ha, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_area_ha, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Mean TP")
tp_area_3


v <- visreg(meanTP_gamm2.2, "log_pred_cue", partial = TRUE, plot = FALSE)
tp_pred_3 <- ggplot() +
  #  geom_line(data = v$fit, aes(x = log_lt_cue, y = visregFit), color = "red", linewidth = 1) +
  #  geom_ribbon(data = v$fit, aes(x = log_lt_cue, ymin = visregLwr, ymax = visregUpr), alpha = 0.2) +
  geom_point(data = v$res, aes(x = log_pred_cue, y = visregRes), size = 3) +
  theme_classic() +
  labs(x = "Other Predator CPUE (log)", y = "Mean TP")
tp_pred_3


##no evidence of interactions between lake area and temperature for mean TP and mean PNC 

##creating plots for ms 
library(ggpubr)

##mean -- 52 lakes 
mean_52 <- ggarrange(tp_area_1, tp_air_1, lc_air_1, lc_area_2, nrow = 2, ncol = 2, labels = c("a)", "b)", "c)", "d)"))
mean_52

intra_plot <- ggarrange(tp_area_2, tp_air_2, tp_lt_2, lc_area_2, lc_air_2, lc_lt_2, nrow = 2, ncol = 3, labels = c("a)", "b)", "c)", "d)", "e)", "f)"))
intra_plot

inter_plot <- ggarrange(tp_area_3, tp_air_3, tp_pred_3, lc_area_3, lc_air_3, lc_pred_3, nrow = 2, ncol = 3, labels = c("a)", "b)", "c)", "d)", "e)", "f)"))
inter_plot



##INTRASPECIFIC VARIATION ANALYSIS ----------
##siber analysis
#load packages
library(SIBER)



siber_df <- tp_lc_BSMsm %>%
  mutate(log_LC = log(LC_pred),
         log_TP = log(TP_pred))
#check how many lakes are in this df
unique(siber_df$Lake_Name)
#siber_df <- final_si_df %>%
#  filter(Trophic_Group == "LakeTrout")


##set up data for siber - needs to be in the format c("iso1", "iso2", "group", "community")
#community - lake name
siber_df <- rename(siber_df, community = Lake_Name)

#group - species 

siber_df <- siber_df %>%
  mutate(group = case_when(
    startsWith(Trophic_Group, "LakeTrout") ~ "1"))

#iso 1/2 - n15 and c13 isotopes

siber_df1 <- siber_df %>%
  dplyr::select(LC_pred, TP_pred, group, community)

#siber_df1 <- siber_df %>%
#  dplyr::select(d13C, d15N, group, community)

names(siber_df1)[1] <- "iso1" 
names(siber_df1)[2] <- "iso2"
##"iso1" - LC and "iso2" - TP

siber_df1$group <- as.integer(siber_df1$group)

str(siber_df1)
##Testing normal distribution
library(mvnormtest)
shapiro.test(siber_df1$iso1)
shapiro.test(siber_df1$iso2)

hist(siber_df1$iso1)
hist(siber_df1$iso2)

#create siber object
siber.obj <- createSiberObject(siber_df1)

community.hulls.args <- list(col = 1, lty = 1, lwd = 1) 
group.ellipses.args <- list(n = 100, p.interval = NULL, lty = 1, lwd = 2) ## = NULL creates Maximum Liklihood Standard Ellipse (incorporates approx. 40% of data)
group.hulls.args <- list(lty = 2, col = "grey20") 

#plot
par(mfrow = c(1,1))
plotSiberObject(siber.obj,
                ax.pad = 2,
                hulls = F, community.hulls.args = community.hulls.args,
                ellipses = T, group.ellipses.args = group.ellipses.args,
                group.hulls = F, group.hulls.args = group.hulls.args,
                bty = "L",
                iso.order = c(1,2),
                xlab = expression({delta}^13*C~'\u2030'),
                ylab = expression({delta}^15*N~'\u2030')
)

#summary statistics for TA, SEA and SEAc
group.ML <- groupMetricsML(siber.obj)
print(group.ML)

#Make ellipse plot in ggplot
siber_cc_ggplot <- siber_df1 %>% mutate(group = factor(group), 
                                        community = factor(community),
                                        TC_pred = iso1, 
                                        TP_pred = iso2,
                                        .keep = "unused")

first.plot <- ggplot(data = siber_cc_ggplot, 
                     aes(x = TC_pred, 
                         y = TP_pred)) + 
  geom_point(aes(color = community, shape = group), size = 5) +
  ylab("Trophic Position") +
  xlab("% Littoral Coupling") + 
  theme(text = element_text(size=16)) + 
  #  scale_color_manual(values = c("#999999", "#E69F00", "#56B4E9")) + 
  theme_classic()
first.plot

# decide how big an ellipse you want to draw
#p.ell <- pchisq(1,2) ##use this for a standard ellipse
p.ell <- pchisq(1,2)
ellipse.plot <- first.plot + 
  stat_ellipse(aes(group = interaction(group, community), 
                   fill = community,
                   color = community), 
               alpha = 0.25, 
               level = p.ell,
               type = "norm",
               geom = "polygon") +
  #  scale_fill_manual(values = c("#999999", "#E69F00", "#56B4E9")) +
  theme(axis.text.x = element_text(hjust = 1, size = 12),axis.text.y = element_text(size = 10),axis.title.y=element_text(size = 12), axis.title.x = element_text(size = 12), legend.position = "right", text = element_text(family = "Times New Roman")) 

ellipse.plot


#create df of group metrics
group_metrics_df <- as.data.frame(group.ML)

# Add row names and pivot wider
group_metrics_df <- slice(group_metrics_df, 3)

group_metrics_df <- pivot_longer(group_metrics_df, cols = everything(), names_to = "Lake_Name", values_to = "SEAc")

# Remove ".1" from Lake_Name
group_metrics_df <- group_metrics_df %>%
  mutate(Lake_Name = str_replace(Lake_Name, "\\.1$", ""))

##why does pipestone have SEAc of 0? 

##check if TA matches these later for individual lakes


##now calculating the laymans community metrics for lakes individually

#get list of lakes 
ID<- sort(unique(siber_df1$community))
print(ID)

##### loop for calculating laymans metrics for each lake ####


# Get list of lake names
ID <- sort(unique(siber_df1$community))

# Loop over each lake
for (lake in ID) {
  
  # Filter data for current lake
  current_lake <- siber_df1 %>%
    filter(group == "1", community == lake)
  
  # Calculate layman metrics for current lake
  current_layman <- laymanMetrics(current_lake$iso1, current_lake$iso2)
  
  # Convert layman metrics to data frame
  current_layman_df <- as.data.frame(current_layman[["metrics"]]) %>% 
    mutate(metrics = current_layman[["metrics"]]) %>% 
    dplyr::select(metrics) 
  
  # Add row names and pivot wider
  current_df <- current_layman_df %>%
    rownames_to_column(var = "rowname") %>% 
    pivot_wider(names_from = rowname, values_from = metrics)
  
  # Add lake name as column
  current_df$Lake_Name <- lake
  
  # Combine data frames for each lake
  if (lake == ID[1]) {
    combined_df <- current_df
  } else {
    combined_df <- bind_rows(combined_df, current_df)
  }
}

# View combined data frame for all lakes
combined_df

##bind this with the SEAc df 

SIBER_df_final <- merge(group_metrics_df, combined_df, by = "Lake_Name")



##look at relationships w/ temp and lake size 
siber_df_final <- left_join(SIBER_df_final, lake_df, by = "Lake_Name") %>%
  mutate(log_area_ha = log(Area_ha),
         log_lt_cue = log(LaTro_CUE),
         log_pred_cue = log(total_pred_CUE),
         log_dX_range = log(dX_range),
         log_TP_range = log(dY_range),
         log_SEAc = log(SEAc),
         log_SDNND = log(SDNND))

siber_df_28 <- siber_df_final %>%
  filter(Lake_Name %in% df_summary_28$Lake_Name)

library(ggcorrplot)

pred_vars <- siber_df_final %>%
  select(log_area_ha, log_lt_cue, log_pred_cue, summer_air_temp) %>%
  filter(!is.na(log_lt_cue))
pred_vars_cor <- round(cor(pred_vars),1)
ggcorrplot(pred_vars_cor, hc.order = TRUE, type = "lower", lab = TRUE, outline.col = "white")##look at correlation matrix 



dPNC_gamm1 <- gam(log_dX_range ~ summer_air_temp + log_area_ha + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(dPNC_gamm1)
lc_range_air_1 <- visreg(dPNC_gamm1, "summer_air_temp", partial = TRUE, gg = TRUE)
lc_range_air_1 <- lc_range_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "Coupling Range (log)")
lc_range_air_1

lc_range_area_1 <- visreg(dPNC_gamm1, "log_area_ha", partial = TRUE, gg = TRUE)

lc_range_area_1 <- lc_range_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Coupling Range (log)")
lc_range_area_1

dPNC_gamm1_I <- gam(log_dX_range ~ summer_air_temp * log_area_ha + s(sampling_year, bs = "re"),
                  data = siber_df_final, method = "REML")
summary(dPNC_gamm1_I)


dPNC_gamm1.1 <- gam(log_dX_range ~ summer_air_temp + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
               data = siber_df_28, method = "REML")
summary(dPNC_gamm1.1)

dPNC_gamm1.1_ml <- gam(log_dX_range ~ summer_air_temp + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
                    data = siber_df_28, method = "ML")
summary(dPNC_gamm1.1_ml)


dPNC_gamm1.1_q <- gam(log_dX_range ~ summer_air_temp + I(summer_air_temp^2) + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
                    data = siber_df_28, method = "ML")
summary(dPNC_gamm1.1_q)

dPNC_gamm1.1_s <- gam(log_dX_range ~ s(summer_air_temp) + s(log_area_ha) + s(log_lt_cue) + s(sampling_year, bs = "re"),
                    data = siber_df_28, method = "ML")
summary(dPNC_gamm1.1_s)

AIC(dPNC_gamm1.1_ml, dPNC_gamm1.1_q, dPNC_gamm1.1_s)

dPNC_gamm1.2 <- gam(log_dX_range ~ summer_air_temp + log_area_ha + log_pred_cue + s(sampling_year, bs = "re"),
               data = siber_df_28, method = "REML")
summary(dPNC_gamm1.2)

lc_range_air_2 <- visreg(dPNC_gamm1.2, "summer_air_temp", partial = TRUE, gg = TRUE)
lc_range_air_2 <- lc_range_air_2 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "Coupling Range (log)")
lc_range_air_2

lc_range_area_2 <- visreg(dPNC_gamm1.2, "log_area_ha", partial = TRUE, gg = TRUE)

lc_range_area_2 <- lc_range_area_2 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Coupling Range (log)")
lc_range_area_2

lc_range_lt_1 <- visreg(dPNC_gamm1.2, "log_lt_cue", partial = TRUE, gg = TRUE)
lc_range_lt_1 <- lc_range_lt_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (log)", y = "Coupling Range (log)")
lc_range_lt_1

lc_range_pred_1 <- visreg(dPNC_gamm1.2, "log_pred_cue", partial = TRUE, gg = TRUE)
lc_range_pred_1 <- lc_range_pred_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Other Predator CPUE (log)", y = "Coupling Range (log)")
lc_range_pred_1


dTP_gamm2 <- gam(log_TP_range ~ summer_air_temp + log_area_ha + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(dTP_gamm2)
tp_range_air_1 <- visreg(dTP_gamm2, "summer_air_temp", partial = TRUE, gg = TRUE)
tp_range_air_1 <- tp_range_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "Trophic Position Range (log)")
tp_range_air_1

tp_range_area_1 <- visreg(dTP_gamm2, "log_area_ha", partial = TRUE, gg = TRUE)
tp_range_area_1 <- tp_range_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "Trophic Position Range (log)")
tp_range_area_1

tp_range_lt_1 <- visreg(dTP_gamm2, "log_lt_cue", partial = TRUE, gg = TRUE)
tp_range_lt_1 <- tp_range_lt_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (log)", y = "Trophic Position Range (log)")
tp_range_lt_1

dTP_gamm2_I <- gam(log_TP_range ~ summer_air_temp * log_area_ha + s(sampling_year, bs = "re"),
                 data = siber_df_final, method = "REML")
summary(dTP_gamm2_I)

dTP_gamm2.1 <- gam(log_TP_range ~ summer_air_temp + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
               data = siber_df_28, method = "REML")
summary(dTP_gamm2.1)

dTP_gamm2.1_ml <- gam(log_TP_range ~ summer_air_temp + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
                   data = siber_df_28, method = "ML")
summary(dTP_gamm2.1_ml)

dTP_gamm2.1_q <- gam(log_TP_range ~ summer_air_temp + I(summer_air_temp^2) + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
                   data = siber_df_28, method = "ML")
summary(dTP_gamm2.1_q)

dTP_gamm2.1_s <- gam(log_TP_range ~ s(summer_air_temp) + s(log_area_ha) + s(log_lt_cue) + s(sampling_year, bs = "re"),
                   data = siber_df_28, method = "ML")
summary(dTP_gamm2.1_s)


AIC(dTP_gamm2.1_ml, dTP_gamm2.1_q, dTP_gamm2.1_s)

tp_range_air_2 <- visreg(dTP_gamm2.1, "summer_air_temp", partial = TRUE, gg = TRUE)
tp_range_air_2 <- tp_range_air_2 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "TP Range (log)")
tp_range_air_2

tp_range_area_2 <- visreg(dTP_gamm2.1, "log_area_ha", partial = TRUE, gg = TRUE)
tp_range_area_2 <- tp_range_area_2 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "TP Range (log)")
tp_range_area_2

tp_range_lt_1 <- visreg(dTP_gamm2.1, "log_lt_cue", partial = TRUE, gg = TRUE)
tp_range_lt_1 <- tp_range_lt_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Trout CPUE (log)", y = "TP Range (log)")
tp_range_lt_1


dTP_gamm2.2 <- gam(log_TP_range ~ summer_air_temp + log_area_ha + log_pred_cue + s(sampling_year, bs = "re"),
               data = siber_df_28, method = "REML")
summary(dTP_gamm2.2)

#ind_var_comp <- ggarrange(lc_range_area_2, lc_range_air_2, lc_range_lt_1, tp_range_area_2, tp_range_air_2, tp_range_lt_1, nrow = 2, ncol = 3)
#ind_var_comp


SEA_gamm3 <- gam(log_SEAc ~ summer_air_temp + log_area_ha + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(SEA_gamm3)
sea_range_air_1 <- visreg(SEA_gamm3, "summer_air_temp", partial = TRUE, gg = TRUE)
sea_range_air_1 <- sea_range_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "SEAc (log)")
sea_range_air_1

sea_range_area_1 <- visreg(SEA_gamm3, "log_area_ha", partial = TRUE, gg = TRUE)
sea_range_area_1 <- sea_range_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "SEAc (log)")
sea_range_area_1

SEA_gamm3_I <- gam(log_SEAc ~ summer_air_temp * log_area_ha + s(sampling_year, bs = "re"),
                 data = siber_df_final, method = "REML")
summary(SEA_gamm3_I)

SEA_gamm3.1 <- gam(log_SEAc ~ summer_air_temp + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
               data = siber_df_28, method = "REML")
summary(SEA_gamm3.1)


SEA_gamm3.1_ml <- gam(log_SEAc ~ summer_air_temp + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
                   data = siber_df_28, method = "ML")
summary(SEA_gamm3.1_ml)

SEA_gamm3.1_q <- gam(log_SEAc ~ summer_air_temp + I(summer_air_temp^2) + log_area_ha + log_lt_cue + s(sampling_year, bs = "re"),
                   data = siber_df_28, method = "ML")
summary(SEA_gamm3.1_q)

SEA_gamm3.1_s <- gam(log_SEAc ~ s(summer_air_temp) + s(log_area_ha) + s(log_lt_cue) + s(sampling_year, bs = "re"),
                   data = siber_df_28, method = "ML")
summary(SEA_gamm3.1_s)

AIC(SEA_gamm3.1, SEA_gamm3.1_q, SEA_gamm3.1_s)

SEA_gamm3.2 <- gam(log_SEAc ~ summer_air_temp + log_area_ha + log_pred_cue + s(sampling_year, bs = "re"),
               data = siber_df_28, method = "REML")
summary(SEA_gamm3.2)

SDNND_gamm4 <- gam(log_SDNND ~ summer_air_temp + log_area_ha + s(sampling_year, bs = "re"),
             data = siber_df_final, method = "REML")
summary(SDNND_gamm4)


sdnnd_range_air_1 <- visreg(SDNND_gamm4, "summer_air_temp", partial = TRUE, gg = TRUE)
sdnnd_range_air_1 <- sdnnd_range_air_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Mean Summer Air Temp", y = "SDNND (log)")
sdnnd_range_air_1

sdnnd_range_area_1 <- visreg(SDNND_gamm4, "log_area_ha", partial = TRUE, gg = TRUE)
sdnnd_range_area_1 <- sdnnd_range_area_1 +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "SDNND (log)")
sdnnd_range_area_1

SDNND_gamm4_I <- gam(log_SDNND ~ summer_air_temp * log_area_ha + s(sampling_year, bs = "re"),
                   data = siber_df_final, method = "REML")
summary(SDNND_gamm4_I)

SDNND_gamm4.1 <- gam(log_SDNND ~ summer_air_temp + log_area_ha + log_lt_cue +  s(sampling_year, bs = "re"),
               data = siber_df_final, method = "REML")
summary(SDNND_gamm4.1)

SDNND_gamm4.1_ml <- gam(log_SDNND ~ summer_air_temp + log_area_ha + log_lt_cue +  s(sampling_year, bs = "re"),
                     data = siber_df_final, method = "ML")
summary(SDNND_gamm4.1_ml)

SDNND_gamm4.1_q <- gam(log_SDNND ~ summer_air_temp + I(summer_air_temp^2) + log_area_ha + log_lt_cue +  s(sampling_year, bs = "re"),
                     data = siber_df_final, method = "ML")
summary(SDNND_gamm4.1_q)

SDNND_gamm4.1_s <- gam(log_SDNND ~ s(summer_air_temp) + s(log_area_ha) + s(log_lt_cue) +  s(sampling_year, bs = "re"),
                     data = siber_df_final, method = "ML")
summary(SDNND_gamm4.1_s)

sdnnd_range_area_4.1_s <- visreg(SDNND_gamm4.1_s, "log_area_ha", partial = TRUE, gg = TRUE)
sdnnd_range_area_4.1_s <- sdnnd_range_area_4.1_s +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "Lake Area (log)", y = "SDNND (log)")
sdnnd_range_area_4.1_s

sdnnd_lt_4.1_s <- visreg(SDNND_gamm4.1_s, "log_lt_cue", partial = TRUE, gg = TRUE)
sdnnd_lt_4.1_s <- sdnnd_lt_4.1_s +
  geom_point(size = 3) +
  theme_classic() +
  labs(x = "LT CPUE(log)", y = "SDNND (log)")
sdnnd_lt_4.1_s

AIC(SDNND_gamm4.1_ml, SDNND_gamm4.1_q, SDNND_gamm4.1_s)

SDNND_gamm4.2 <- gam(log_SDNND ~ summer_air_temp + log_area_ha + log_pred_cue +  s(sampling_year, bs = "re"),
               data = siber_df_final, method = "REML")
summary(SDNND_gamm4.2)


##Create summary stats table 
library(broom.mixed)
library(dplyr)
library(purrr)
library(tibble)



# ---- Function to extract model summary ----
extract_gam_summary_2 <- function(model, response_name) {
  model_summary <- summary(model)
  
  # Model fit info
  adj_r2 <- round(model_summary$r.sq, 3)
  dev_expl <- paste0(round(model_summary$dev.expl * 100, 1), "%")
  aic_val <- round(AIC(model), 2)
  
  # ---- Fixed (parametric) terms ----
  if (!is.null(model_summary$p.table)) {
    fixed <- as.data.frame(model_summary$p.table)
    fixed$Term <- rownames(fixed)
    rownames(fixed) <- NULL
    fixed <- fixed %>%
      mutate(
        Response = response_name,
        Estimate = round(Estimate, 3),
        Std_Error = round(`Std. Error`, 3),
        Stat_Value = round(`t value`, 3),
        P_Value = round(`Pr(>|t|)`, 3),
        Adj_R2 = "",
        Dev_Expl = "",
        AIC = ""
      )
    fixed$Adj_R2[1] <- adj_r2
    fixed$Dev_Expl[1] <- dev_expl
    fixed$AIC[1] <- aic_val
    
    fixed <- fixed %>%
      mutate(across(c(Estimate, Std_Error, Stat_Value, P_Value, Adj_R2, Dev_Expl, AIC), as.character))
  } else {
    fixed <- tibble(
      Response = response_name,
      Term = "(no parametric terms)",
      Estimate = "",
      Std_Error = "",
      Stat_Value = "",
      P_Value = "",
      Adj_R2 = as.character(adj_r2),
      Dev_Expl = dev_expl,
      AIC = as.character(aic_val)
    )
  }
  
  # ---- Smooth terms ----
  smooth <- as.data.frame(model_summary$s.table)
  smooth$Term <- rownames(smooth)
  rownames(smooth) <- NULL
  
  smooth_terms <- smooth %>%
    mutate(
      Response = response_name,
      Estimate = paste0("edf = ", round(edf, 2)),
      Std_Error = "",
      Stat_Value = paste0("F = ", round(F, 2)),
      P_Value = round(`p-value`, 3),
      Adj_R2 = "",
      Dev_Expl = "",
      AIC = ""
    ) %>%
    select(Response, Term, Estimate, Std_Error, Stat_Value, P_Value, Adj_R2, Dev_Expl, AIC) %>%
    mutate(across(c(Estimate, Std_Error, Stat_Value, P_Value), as.character))
  
  # Combine both
  output <- bind_rows(fixed, smooth_terms)
  
  return(output)
}

##make list of models for models w/ only lake area and summer temp
models_52 <- list(mean_TP = meanTP_gamm2,
                  mean_PNC = meanPNC_gamm1,
                  dTP = dTP_gamm2,
                  dPNC = dPNC_gamm1,
                  SEAc = SEA_gamm3,
                  SDNND = SDNND_gamm4)


str(models_52)
class(models_52[[1]])
# ---- Apply to nested list of models ----
# models_nested is your named list, e.g., list(SDNND = gam1, LC = gam2, ...)
gam_results_52 <- map_dfr(names(models_52), function(name) {
  extract_gam_summary_2(models_52[[name]], response_name = name)
})

# ---- Final touch: make sure NAs are uniform and display ----
gam_results_52 <- gam_results_52 %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))


##write csv
write.csv(gam_results_52, "gamm_tables/gamm_output_52.csv")


##intraspecific comp 
models_intracomp <- list(mean_TP = meanTP_gamm2.1,
                  mean_PNC = meanPNC_gamm1.1,
                  dTP = dTP_gamm2.1,
                  dPNC = dPNC_gamm1.1,
                  SEAc = SEA_gamm3.1,
                  SDNND = SDNND_gamm4.1)


str(models_intracomp)

# ---- Apply to nested list of models ----
# models_nested is your named list, e.g., list(SDNND = gam1, LC = gam2, ...)
gam_results_intracomp <- map_dfr(names(models_intracomp), function(name) {
  extract_gam_summary_2(models_intracomp[[name]], response_name = name)
})

# ---- Final touch: make sure NAs are uniform and display ----
gam_results_intracomp <- gam_results_intracomp %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))


##write csv
write.csv(gam_results_intracomp, "gamm_tables/gamm_output_intracomp.csv")

##interspecific comp 
models_intercomp <- list(mean_TP = meanTP_gamm2.2,
                         mean_PNC = meanPNC_gamm1.2,
                         dTP = dTP_gamm2.2,
                         dPNC = dPNC_gamm1.2,
                         SEAc = SEA_gamm3.2,
                         SDNND = SDNND_gamm4.2)


str(models_intercomp)

# ---- Apply to nested list of models ----
# models_nested is your named list, e.g., list(SDNND = gam1, LC = gam2, ...)
gam_results_intercomp <- map_dfr(names(models_intercomp), function(name) {
  extract_gam_summary_2(models_intercomp[[name]], response_name = name)
})

# ---- Final touch: make sure NAs are uniform and display ----
gam_results_intercomp <- gam_results_intercomp %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))


##write csv
write.csv(gam_results_intercomp, "gamm_tables/gamm_output_intercomp.csv")


##interaction term between lake area and temp  
models_I <- list(mean_TP = meanTP_gamm2_I,
                         mean_PNC = meanPNC_gamm1_I,
                         dTP = dTP_gamm2_I,
                         dPNC = dPNC_gamm1_I,
                         SEAc = SEA_gamm3_I,
                         SDNND = SDNND_gamm4_I)


str(models_I)

# ---- Apply to nested list of models ----
# models_nested is your named list, e.g., list(SDNND = gam1, LC = gam2, ...)
gam_results_I <- map_dfr(names(models_I), function(name) {
  extract_gam_summary_2(models_I[[name]], response_name = name)
})

# ---- Final touch: make sure NAs are uniform and display ----
gam_results_I <- gam_results_I %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))


##write csv
write.csv(gam_results_I, "gamm_tables/gamm_output_52_interaction.csv")


##intraspecific comp - smooth and not smoothed model comparison


models_intracomp_s <- list(mean_TP = meanTP_gamm2.1_ml,
                         mean_TP_s = meanTP_gamm2.1_s,
                         mean_PNC = meanPNC_gamm1.1_ml,
                         mean_PNC_s = meanPNC_gamm1.1_s,
                         dTP = dTP_gamm2.1_ml,
                         dTP_s = dTP_gamm2.1_s,
                         dPNC = dPNC_gamm1.1_ml,
                         dPNC_s = dPNC_gamm1.1_s,
                         SEAc = SEA_gamm3.1_ml,
                         SEAc_s = SEA_gamm3.1_s,
                         SDNND = SDNND_gamm4.1_ml,
                         SDNND_s = SDNND_gamm4.1_s)


str(models_intracomp_s)

# ---- Apply to nested list of models ----
# models_nested is your named list, e.g., list(SDNND = gam1, LC = gam2, ...)
gam_results_intracomp_s <- map_dfr(names(models_intracomp_s), function(name) {
  extract_gam_summary_2(models_intracomp_s[[name]], response_name = name)
})

# ---- Final touch: make sure NAs are uniform and display ----
gam_results_intracomp_s <- gam_results_intracomp_s %>%
  mutate(across(everything(), ~ ifelse(is.na(.), "", as.character(.))))


##write csv
write.csv(gam_results_intracomp_s, "gamm_tables/gamm_output_intracomp_smoothedmodelAIC.csv")

