## Load packages ##

# Package list
pkg_list <- c("tidyverse", "lme4", "emmeans", "lubridate", "knitr", 
              "car", "ggplot2", "patchwork", "kableExtra", "ggsignif",
              "gghalves")

# Load packages
pacman::p_load(pkg_list, character.only = TRUE)

# Not in
`%notin%` <- Negate(`%in%`)

# Load table with trigger and condition information
code_tab <- read.csv("~/Mirror/dissertation/13_eeg/input/code_tab.csv") %>%
  mutate(prime_cond = factor(prime_cond, 
                             levels = c("identity", "competitor", "control"),
                             labels = c("identity", "competitor", "unrelated")))

# Set up geom elements
g_point <- 2
g_line <- 1
font_fam <- "Avenir"
text_size <- 24
alpha_lev <- 0.6
acc_range <- c(0.48, 1.02)
acc_seq <- seq(0.4, 1.1, 0.1)
g_dodge <- 10
sig_size <- 8

# Set up colors
# col_sim_1a <- c("#41ab5d", "#78c679", "#addd8e")
# col_sim <- col_sim_1a[1:2]
# col_var <- c("#fd8d3c", "#feb24c")
col_exp_1a <- c("#762a83", "#1b7837", 
                "#af8dc3", "#7fbf7b", 
                "#e7d4e8", "#d9f0d3")
col_exp <- col_exp_1a[1:4]
col_vot <- c("#2c7fb8", "#41b6c4", "#c7e9b4", "#7fcdbb")
col_test <- c("#7a0177", "#dd3497", "#fa9fb5")
col_corr <- c("#f03b20", "#FFFFFF", "#3182bd")
col_talk <- c("#a6cee3", "#1f78b4", "#b2df8a", "#33a02c")

# Make number formatting function for p values
# val is the p value I want to format
# format_code specifies whether I want to include the symbol or not
# format_code defaults to including the symbol
p_formatting <- function(val, format_code = 1) { 
  
  # If I want to include the symbol (when I'm calling this variable in text)
  if (format_code == 1) {
    
    # If the p value is less than .001, get the less than symbol
    # Otherwise, get the equals symbol
    sign_type <- if_else(val < 0.001, "<", "=")
    
    # If the p value is less than .001, set the value equal to .001
    if (sign_type == "<") {
      val <- ".001"
      
      # Otherwise, get the actual p value,
      # round it to three decimal places, and
      # remove the leading zero
    } else {
      val <- sprintf("%.3f", val)
      val <- substring(val, 2)
    }
    
    # Combine the new/formatted p value with < or =,
    # depending on the p value
    val_string <- paste(sign_type, val, sep = " ")
    val_string
    
    # If I don't want the symbol (as in a table),
    # just round the value to three decimal places
    # and remove the leading zero
  } else if (format_code == 0) {
    
    val <- sprintf("%.3f", val)
    val_string <- substring(val, 2)
    val_string
    
  }
  
}

# Make number formatting function for t and F values
stat_formatting <- function(val) {
  sprintf("%.2f", abs(val))
}

# Make number formatting function for integers (e.g., reaction times in ms)
zero_formatting <- function(val) {
  sprintf("%.0f", val)
}

# Make number formatting function for percentages
pct_formatting <- function(val) {
  val %>% prod(100) %>% sprintf("%.2f", .) %>% paste0("%")
}

# Make number formatting function for degrees of freedom
df_formatting <- function(val) {
  if_else(val%%1 == 0, sprintf("%.0f", val), sprintf("%.2f", val))
}

# General number formatting
# Round to two decimal places, keep trailing zeros
# Include negative sign if less than zero
num_formatting <- function(val) {
  sprintf("%.2f", val)
}

# Make function to output APA formatting for chi-square tests from lmer
# Use with numbers that need to be passed through formatting functions
mod_comp <- function(aov_tab, comp) {
  
  # Pull values
  stat_val <- aov_tab[comp, 1]
  df_val <- aov_tab[comp, 2]
  p_val <- aov_tab[comp, 3]
  n_val <- df_val + 1
  
  # Format values
  stat_val <- stat_formatting(stat_val)
  df_val <- df_formatting(df_val)
  p_val <- p_formatting(p_val, format_code = 1)
  
  # Print
  sprintf("$\\chi^2$(%s, *N* = %s) = %s, *p* %s", df_val, n_val, stat_val, p_val)
  
}

# Make function to output APA formatting for chi-square tests (lm)
# Use with numbers that need to be passed through formatting functions
mod_comp_lm <- function(aov_tab, comp) {
  
  # Pull values
  stat_val <- aov_tab[comp, 1]
  df_val <- aov_tab[comp, 2]
  p_val <- aov_tab[comp, 4]
  n_val <- df_val + 1
  
  # Format values
  stat_val <- stat_formatting(stat_val)
  df_val <- df_formatting(df_val)
  p_val <- p_formatting(p_val, format_code = 1)
  
  # Print
  sprintf("$\\chi^2$(%s, *N* = %s) = %s, *p* %s", df_val, n_val, stat_val, p_val)
  
}


# APA formatting for pairwise comparisons
pair_comp <- function(pairs_tab, comp) {
  
  pairs_tab <- data.frame(pairs_tab)
  ind <- which(pairs_tab$contrast == comp)
  
  df_check <- pairs_tab$df[ind]
  
  if (is.finite(df_check)) {
    
    df <- df_formatting(pairs_tab$df[ind])
    t_val <- stat_formatting(pairs_tab$`t.ratio`[ind])
    p_val <- p_formatting(pairs_tab$`p.value`[ind], format_code = 1)
    
    sprintf("*t*(%s) = %s, *p* %s", df, t_val, p_val)
    
  } else {
    
    z_val <- stat_formatting(pairs_tab$`z.ratio`[ind])
    p_val <- p_formatting(pairs_tab$`p.value`[ind], format_code = 1)
    
    sprintf("*z* = %s, *p* %s", z_val, p_val)
    
  }
  
}

# APA formatting for RTs
apa_rt <- function(rt_tab) {
  
  if (is.data.frame(rt_tab) == FALSE) {
    
    rt_tab <- data.frame(rt_tab %>% summary(type = "response"))
    
  }
  
  tab_cols <- colnames(rt_tab)
  tab_end <- which(tab_cols == "emmean") - 1
  tabs <- tab_cols[1:tab_end]
  
  if ("lower.CL" %in% tabs) {
    
    rt_tab <- rt_tab %>% 
      mutate(rt = -1000/emmean,
             rt_low = -1000/lower.CL,
             rt_high = -1000/upper.CL,
             report = sprintf("*M* = %.0f, 95%% CI [%.0f, %.0f]", rt, rt_low, rt_high))
    
    rt_tab %>%
      select(all_of(tabs), rt, rt_low, rt_high, report)
    
  } else {
    
    rt_tab <- rt_tab %>% 
      mutate(rt = -1000/emmean,
             rt_low = -1000/asymp.LCL,
             rt_high = -1000/asymp.UCL,
             report = sprintf("*M* = %.0f, 95%% CI [%.0f, %.0f]", rt, rt_low, rt_high))
    
    rt_tab %>%
      select(all_of(tabs), rt, rt_low, rt_high, report)
    
  }
  
}

# Pull APA-formatted RTs
apa_rt_pull <- function(rt_tab, col, val) {
  
  no_col <- length(col)
  no_val <- length(val)
  
  if (no_col != no_val) {
    
    stop("The number of columns and values don't match.")
    
  }
  
  tab_cols <- colnames(data.frame(rt_tab))
  
  if (no_col == 1 & no_val == 1) {
    
    tab_ind <- which(tab_cols == col)
    tab_row <- which(rt_tab[tab_ind] == val)
    rt_tab$report[tab_row]
    
  } else {
    
    stop("Sorry I can't deal with multiple variables right now.")
    
  }
  
}

# Make function to output APA formatting for correlations
apa_corr <- function(corr_list) {
  
  corr_r <- corr_list$r %>% 
    as.data.frame() %>% 
    rownames_to_column("Var1") %>% 
    pivot_longer(cols = c(everything(), -Var1), 
                 names_to = "Var2", 
                 values_to = "r") 
  
  corr_p <- corr_list$p  %>% 
    as.data.frame() %>% 
    rownames_to_column("Var1") %>% 
    pivot_longer(cols = c(everything(), -Var1), 
                 names_to = "Var2", 
                 values_to = "p") %>%
    rowwise() %>%
    mutate(pair = paste0(sort(c(Var1, Var2)), collapse = "")) %>%
    ungroup() %>%
    dplyr::filter(!duplicated(pair) & Var1 != Var2) %>%
    select(-pair)
  
  corr_n <- corr_list$n %>% 
    as.data.frame() %>% 
    rownames_to_column("Var1") %>% 
    pivot_longer(cols = c(everything(), -Var1), 
                 names_to = "Var2", 
                 values_to = "n") 
  
  corr_dat <- left_join(corr_p, corr_r, by = c("Var1", "Var2")) %>%
    left_join(corr_n, by = c("Var1", "Var2")) %>%
    rowwise() %>%
    mutate(df = n - 2,
           report = sprintf("*r*(%s) = %s, *p* %s", 
                            df_formatting(df),
                            num_formatting(r), 
                            p_formatting(p))) %>% 
    ungroup() %>% 
    dplyr::filter(abs(r) < 1)
  
  corr_dat
  
}

# Make function for corr plot
labs_func = function(x){
  case_when(x >= 0.05 ~ "",
            x < 0.05 & x >= 0.01 ~ "*",
            x < 0.01 & x >= 0.001 ~ "**",
            x < 0.001 ~ "***")
}
