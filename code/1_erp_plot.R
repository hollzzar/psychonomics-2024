####################
## Set up session ##
####################

# Load packages and variables
source("code/0_global.R", local = TRUE)

# ROI (chans_n1)
roi <- c("C3","C4","CP1","CP2","Cz","F3","F4","FC1","FC2",
         "FC5","FC6","Fz","P3","P4","P7","P8","Pz")

# Microvolts
v_max <- 3
v_min <- -6

###############
## Load data ##
###############

# Load EEG data for correct trials
sub_dat_corr <- read.csv("~/Mirror/dissertation/13_eeg/output/2_sub_eeg/sub_dat_corr.csv")

# Get values in desired channels
viz_dat <- sub_dat_corr %>%
  left_join(code_tab %>% select(prime_cond, T1Label)) %>%
  select(all_of(roi), id, time, prime_cond, trial) %>%
  pivot_longer(cols = all_of(roi), names_to = "chan", values_to = "volt") %>%
  group_by(id, time, prime_cond, trial) %>%
  summarise(erp = mean(volt), .groups = "keep") %>% 
  ungroup() %>% 
  mutate(prime_cond = str_to_title(prime_cond),
         prime_cond = factor(prime_cond, levels = c("Identity", "Competitor", "Unrelated")))

##########
## Plot ##
##########

# Grand mean across participants and trials
viz_plot <- ggplot(viz_dat,
                   aes(x = time, y = erp, color = prime_cond)) +
  annotate(geom = "rect", xmin = 175, xmax = 250, ymin = v_min, ymax = v_max, 
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  annotate(geom = "rect", xmin = 250, xmax = 375, ymin = v_min, ymax = v_max,
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  annotate(geom = "rect", xmin = 450, xmax = 650, ymin = v_min, ymax = v_max,
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "line", size = 1.75) +
  scale_color_manual(values = col_test, name = "Prime type") +
  scale_x_continuous(breaks = seq(-200, 800, 100)) +
  scale_y_continuous(breaks = seq(v_min, v_max, 0.5)) +
  ylab("Amplitude (μV)") +
  xlab("Time (ms)") +
  theme_classic() +
  theme(legend.position = "bottom",
        text = element_text(family = font_fam, size = text_size),
        axis.ticks = element_blank())

# Save plot and crop to remove white space
ggsave("poster/grand_plot.png", viz_plot, 
       width = 16, height = 8, unit = "in", dpi = 300)
knitr::plot_crop("poster/grand_plot.png")

# Add block info
viz_dat <- viz_dat %>%
  mutate(exp_block = case_when(
    between(trial, 1, 96) ~ 1,
    between(trial, 97, 192) ~ 2,
    between(trial, 193, 288) ~ 3,
    between(trial, 289, 384) ~ 4,
    between(trial, 385, 480) ~ 5,
    between(trial, 481, 576) ~ 6
  ),
  block = case_when(
    exp_block %in% c(1, 2) ~ 1,
    exp_block %in% c(3, 4) ~ 2,
    exp_block %in% c(5, 6) ~ 3
  ),
  block = factor(block, levels = c(1, 2, 3), 
                 labels = c("Blocks 1-2", "Blocks 3-4", "Blocks 5-6"))
  )

# Show by block
viz_plot_block <- ggplot(viz_dat, aes(x = time, y = erp, color = prime_cond)) +
  annotate(geom = "rect", xmin = 175, xmax = 250, ymin = v_min, ymax = v_max, 
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  annotate(geom = "rect", xmin = 250, xmax = 375, ymin = v_min, ymax = v_max,
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  annotate(geom = "rect", xmin = 450, xmax = 650, ymin = v_min, ymax = v_max,
           fill = "#bbbbbb", color = "#bbbbbb", alpha = 0.4) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 0, linetype = "dashed") +
  stat_summary(fun = "mean", geom = "line", size = 1.75) +
  facet_wrap(~ block, nrow = 1) +
  scale_color_manual(values = col_test, name = "Prime type") +
  scale_x_continuous(breaks = seq(-200, 800, 200)) +
  scale_y_continuous(breaks = seq(v_min, v_max, 1)) +
  ylab("Amplitude (μV)") +
  xlab("Time (ms)") +
  theme_minimal() +
  theme(legend.position = "bottom",
        text = element_text(family = font_fam, size = text_size),
        axis.ticks = element_blank())

# Save plot and crop to remove white space
ggsave("poster/erp_block_plot.png", viz_plot_block, 
       width = 16, height = 8, unit = "in", dpi = 300)
knitr::plot_crop("poster/erp_block_plot.png")