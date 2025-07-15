##Seasonal Stream Food Webs_SI Analysis --- Lipid Correction Code

source("~/Desktop/Seasonal Stream Food Webs/Code/Stable Isotope Analysis/Seasonal FW_Bi-Plots.R")

##Remove outliers (see Seasonal FW_Baseline Outliers.R code to see how outliers were determined)

SI_cc_al_dt_adj <- SI_cc_al_dt_adj %>%
  filter(Sample_ID != "AT1-18-AL-01") %>%
  filter(Sample_ID != "AT1-18-DT-01")

##Looking at C:N ratio to see if lipid correction necessary ##
SI_cc <- SI_cc_al_dt_adj %>%
  filter(Species == "Creek Chub")

SI_cc_low_cn <- SI_cc %>%
  filter(CN_ratio <= "3.5")

#54/86 === only 62.7% of fish are below 3.5, so need to lipid correct

##Lipid correct creek chub
##equation: dCnormalized = dCraw - 3.32 + 0.99 X C:N ---- see Post et al., 2007

SI_cc <- SI_cc %>%
  mutate(dC_corr = d13C - 3.32 + 0.99 * CN_ratio) %>%
  mutate(dC_corr_2 = d13C + (-3.32+0.99*CN_ratio)) ##this was the way tim did it in his work, just want to check if this gives any different values
##How to lipid correct for plants? ##two different equations, depends if plant contains >40% carbon
##%Carbon from Jan Vanzier elemental analysis:
##Algae: 9-15% carbon
##Detritus: 23 - 36% carbon
##Therefore equation is: Eqn 11 from Post et al., 2007
##dCnormalized = dCraw -3.02 + 0.09 * % carbon

SI_baselines <- SI_cc_al_dt_adj %>%
  filter(Species == "Algae" | Species == "Detritus") %>%
  mutate(dC_corr = d13C - 3.02 + 0.09 * per_C)

##Join datafiles back together
SI_cc_al_dt_adj <- rbind(SI_cc, SI_baselines)

