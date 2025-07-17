##Correlation plots for lake attributes 

library(tidyverse)
library(ggplot2)
library(cowplot)
library(gridExtra)

lake_df <- read.csv("data/lake_supp_table.csv") %>%
  rename(Lake_Name = "Lake.Name", Area_ha = "Lake.Area..ha.", summer_air_temp = "Summer.Air.Temperature..C.", LaTro_CUE = "Lake.Trout.CUE", total_pred_CUE = "Total.Predator.CUE", sampling_year = "Sampling.Year", secchi = "Secchi.Depth..m.", total_p = "Total.Phosphorus..ugL.", SDI = "Development.Index") %>%
  mutate(log_area_ha = log(Area_ha),
         log_lt_cpue = log(LaTro_CUE),
         log_pred_cpue = log(total_pred_CUE),
         log_secchi = log(secchi),
         log_total_phosphorous = log(total_p),
         log_SDI = log(SDI)) %>%
  select(Lake_Name, sampling_year, summer_air_temp, log_area_ha:log_SDI, pLittoral)

sp_rich <- read.csv("data/sp_rich_jul4.csv")

lake_df <- left_join(lake_df, sp_rich, by = "Lake_Name")


vars_to_plot <- lake_df %>%
  select(-Lake_Name, -sampling_year) %>%
  select(where(is.numeric))  # ensures you're only plotting numeric variables


library(GGally)
# Custom correlation function
cor_fun <- function(data, mapping, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  corr <- cor(x, y, use = "complete.obs")
  pval <- cor.test(x, y)$p.value
  
  label <- sprintf("%.2f", corr)
  fill_col <- if (is.na(corr)) "grey80" else scales::col_numeric(
    palette = c("blue", "white", "red"),
    domain = c(-1, 1)
  )(corr)
  
  ggplot() +
    geom_label(aes(x = 0, y = 0, label = label),
               fill = fill_col, color = "black",
               label.r = unit(0.15, "lines")) +
    theme_void()
}


scatter_with_lm_1 <- function(data, mapping, ...) {
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  corr <- cor(x, y, use = "complete.obs")
  fit <- lm(y ~ x)
  pval <- summary(fit)$coefficients[2, 4]
  
  # Only show line if correlation > 0.5
  show_line <- !is.na(corr) && corr > 0.5
  
  linetype <- if (pval < 0.05) "solid" else if (pval < 0.1) "dashed" else "blank"
  
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.6, size = 1.5) +
    {if (show_line && linetype != "blank") 
      geom_smooth(method = "lm", se = FALSE, linetype = linetype, color = "red")} +
    theme_minimal()
}


scatter_with_lm_2 <- function(data, mapping, ...){
  x <- eval_data_col(data, mapping$x)
  y <- eval_data_col(data, mapping$y)
  
  fit <- lm(y ~ x)
  pval <- summary(fit)$coefficients[2, 4]
  
  linetype <- if (pval < 0.05) "solid" else if (pval < 0.1) "dashed" else "blank"
  
  ggplot(data = data, mapping = mapping) +
    geom_point(alpha = 0.6, size = 1.5) +
    {if (linetype != "blank") geom_smooth(method = "lm", se = FALSE, linetype = linetype, color = "red")} +
    theme_minimal()
}


# Create dummy data to simulate correlation range
legend_data <- data.frame(corr = seq(-1, 1, length.out = 100), x = 1)

# Dummy plot with color scale for correlation
cor_legend <- ggplot(legend_data, aes(x = x, y = corr, fill = corr)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", mid = "white", high = "red", 
                       midpoint = 0, limits = c(-1, 1),
                       name = "Correlation") +
  theme_void() +
  theme(legend.position = "right")

# Extract legend only
legend_only <- cowplot::get_legend(cor_legend)



pairplot_1 <- ggpairs(vars_to_plot,
        upper = list(continuous = cor_fun),
        lower = list(continuous = scatter_with_lm_1),
        diag = list(continuous = "blankDiag")) 

pairplot_grob_1 <- GGally::ggmatrix_gtable(pairplot_1)

# Then arrange with legend
grid.arrange(pairplot_grob_1, legend_only, ncol = 2, widths = c(4, 0.5))

pairplot_2 <- ggpairs(vars_to_plot,
        upper = list(continuous = cor_fun),
        lower = list(continuous = scatter_with_lm_2),
        diag = list(continuous = "blankDiag")) 

pairplot_grob_2<- GGally::ggmatrix_gtable(pairplot_2)

# Then arrange with legend
grid.arrange(pairplot_grob_2, legend_only, ncol = 2, widths = c(4, 0.5))


