source("0_import_libraries.R")

# read in results
results.all <- read.csv("Results/Results_I_N_scenarios.csv")
sens.mat.all <- read.csv("Results/sens_mat_I_N_scenarios.csv")
elas.mat.all <- read.csv("Results/elas_mat_I_N_scenarios.csv")
elas.mat.kij.all <- read.csv("Results/elas_mat_kij_I_N_scenarios.csv")


#=============================== Figure 2 ===========================================
#                      R0 values under different scenarios
#====================================================================================
# note that the panels were not ordered by the magnitude of kappa. kappa = 0.15 was set to be panel A because it is the most possible scenario according to the data for other tick species
results.all %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1~5", 
                           I.sce == 2 ~ "5.01~9", 
                           I.sce == 3 ~ "9.01~12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"),
         K.sce = case_when(K.sce == 0.05 ~ "B*')'~kappa~'= 0.05'",
                           K.sce == 0.15 ~ "A*')'~kappa~'= 0.15'",
                           K.sce == 0.5 ~ "C*')'~kappa~'= 0.5'"),
         N.sce = factor(N.sce, levels = unique(N.sce))) %>%
  select(R0, I.sce, N.sce, K.sce) %>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarise(R0.median = median(R0), R0.low = quantile(R0, 0.025), R0.high = quantile(R0, 0.975)) %>%
  ungroup() %>%
  ggplot(aes(x = I.sce, y = N.sce, fill = R0.median)) +
  geom_tile() +
  facet_wrap(~K.sce, labeller = label_parsed) +
  geom_text(aes(label = paste(round(R0.median, 2), "\n", "(", round(R0.low, 2), ", ", round(R0.high, 2), ")", sep = ""))) +
  xlab("Duration of viremia (days)") +
  ylab("Tick abundance (ticks per host)") +
  labs(fill = expression(R[0])) +
  theme_cowplot() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15)) +
  scale_fill_gradient2(midpoint = 1, high = "coral3", low = "cyan3")
ggsave("Figures/Fig2_R0.pdf", width = 10, height = 3.6)




#=============================== Figure 3 ===========================================
#           R0 values without vertical transmission under different scenarios
#====================================================================================
results.all %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1~5", 
                           I.sce == 2 ~ "5.01~9", 
                           I.sce == 3 ~ "9.01~12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"),
         K.sce = case_when(K.sce == 0.05 ~ "B*')'~kappa~'= 0.05'",
                           K.sce == 0.15 ~ "A*')'~kappa~'= 0.15'",
                           K.sce == 0.5 ~ "C*')'~kappa~'= 0.5'"),
         N.sce = factor(N.sce, levels = unique(N.sce))) %>%
  select(R0_novertical, I.sce, N.sce, K.sce) %>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarise(R0.median = median(R0_novertical), R0.low = quantile(R0_novertical, 0.025), R0.high = quantile(R0_novertical, 0.975)) %>%
  ggplot(aes(x = I.sce, y = N.sce, fill = R0.median)) +
  geom_tile() +
  facet_wrap(~K.sce, labeller = label_parsed) +
  geom_text(aes(label = paste(round(R0.median, 2), "\n", "(", round(R0.low, 2), ", ", round(R0.high, 2), ")", sep = ""))) +
  xlab("Duration of viremia (days)") +
  ylab("Tick abundance (ticks per host)") +
  labs(fill = expression(R[0])) +
  theme_cowplot() +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15)) +
  scale_fill_gradient(high = "white", low = "cyan3")
ggsave("Figures/Fig3_R0_novertical.pdf", width = 10, height = 3.6)







#=============================== Figure 4 ===========================================
#       relative contributions of the three pathways under different scenarios
#====================================================================================
# version 1: heatmap by k and transmission pathway
results.all %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1~5", 
                           I.sce == 2 ~ "5.01~9", 
                           I.sce == 3 ~ "9.01~12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"),
         N.sce = factor(N.sce, levels = unique(N.sce)),
         K.sce = factor(K.sce, levels = c(0.15, 0.05, 0.5))) %>%
  arrange(K.sce) %>%
  mutate(panel.title = paste(N.sce, I.sce, sep = ", "),
         panel.title = factor(panel.title, levels = unique(panel.title)))%>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarise(vertical.median = median(vertical), nonsystemic.median = median(nonsystemic), systemic.median = median(systemic),
            vertical.low  = quantile(vertical, 0.025), nonsystemic.low = quantile(nonsystemic, 0.025), systemic.low = quantile(systemic, 0.025),
            vertical.high  = quantile(vertical, 0.975), nonsystemic.high = quantile(nonsystemic, 0.975), systemic.high = quantile(systemic, 0.975)) %>%
  pivot_longer(cols = vertical.median:systemic.high,
               names_to = c("path", ".value"),
               names_pattern = "(.*)\\.(.*)") %>%
  mutate(path = case_when(path == "vertical" ~ "Transovarial", 
                          path == "nonsystemic" ~ "'Non-systemic'", 
                          path == "systemic" ~ "Systemic"),
         path = factor(path, levels = c("Systemic", "'Non-systemic'", "Transovarial"))) %>%
  arrange(K.sce, path ) %>%
  mutate(panel.name = paste(path, "*','~kappa~'='~", K.sce, sep = ""),
         panel.name = factor(panel.name, levels = unique(panel.name)),
         text.color = ifelse(median >= 0.4, "white", "black")) %>%
  ggplot(aes(x = I.sce, y = N.sce, fill = median)) +
  geom_tile() +
  geom_text(aes(label = paste(round(median, 2), "\n", "(", round(low, 2), ", ", round(high, 2), ")", sep = ""), col = text.color), size = 4.2)+
  facet_wrap(~panel.name, labeller = as_labeller(make_labelstring, default = label_parsed))+
  xlab("Duration of viremia (days)") +
  ylab("Tick abundance (ticks per host)") +
  coord_equal() +
  theme_cowplot() +
  scale_fill_distiller(direction = 1, palette = "Blues") +
  scale_color_manual(values = c("black", "white")) +
  labs(fill = "Relative contribution") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15),
        legend.key.width = unit(3, "cm"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  guides(color = FALSE)
ggsave("Figures/Fig4_relative_contribution_heatmap.pdf", width = 10, height = 11)



# version 2: heatmap by k and duration of viremia
results.all %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "I~'='~1*'~'*5", 
                           I.sce == 2 ~ "I~'='~5.01*'~'*9", 
                           I.sce == 3 ~ "I~'='~9.01*'~'*12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"),
         N.sce = factor(N.sce, levels = unique(N.sce)),
         K.sce = factor(K.sce, levels = c(0.15, 0.05, 0.5))) %>%
  arrange(K.sce) %>%
  mutate(panel.title = paste(N.sce, I.sce, sep = ", "),
         panel.title = factor(panel.title, levels = unique(panel.title)))%>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarise(vertical.median = median(vertical), nonsystemic.median = median(nonsystemic), systemic.median = median(systemic),
            vertical.low  = quantile(vertical, 0.025), nonsystemic.low = quantile(nonsystemic, 0.025), systemic.low = quantile(systemic, 0.025),
            vertical.high  = quantile(vertical, 0.975), nonsystemic.high = quantile(nonsystemic, 0.975), systemic.high = quantile(systemic, 0.975)) %>%
  pivot_longer(cols = vertical.median:systemic.high,
               names_to = c("path", ".value"),
               names_pattern = "(.*)\\.(.*)") %>%
  mutate(path = case_when(path == "vertical" ~ "Transovarial", 
                          path == "nonsystemic" ~ "Non-systemic", 
                          path == "systemic" ~ "Systemic"),
         path = factor(path, levels = c("Systemic", "Non-systemic", "Transovarial"))) %>%
  ungroup() %>%
  arrange(K.sce, I.sce) %>%
  mutate(panel.name = paste(I.sce, "*','~kappa~'='~", K.sce, sep = ""),
         panel.name = factor(panel.name, levels = unique(panel.name)),
         text.color = ifelse(median >= 0.4, "white", "black")) %>%
  ggplot(aes(x = path, y = N.sce, fill = median)) +
  geom_tile() +
  geom_text(aes(label = paste(round(median, 2), "\n", "(", round(low, 2), ", ", round(high, 2), ")", sep = ""), col = text.color), size = 4.2)+
  facet_wrap(~panel.name, labeller = as_labeller(make_labelstring, default = label_parsed))+
  xlab("Transmission pathway") +
  ylab("Tick abundance (ticks per host)") +
  coord_equal() +
  theme_cowplot() +
  scale_fill_distiller(direction = 1, palette = "Blues") +
  scale_color_manual(values = c("black", "white")) +
  labs(fill = "Relative contribution") +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15),
        legend.key.width = unit(3, "cm"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15), 
        axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0))+
  guides(color = FALSE)
ggsave("Figures/Fig4_relative_contribution_heatmap_switched.pdf", width = 10, height = 12)





# version 3: barplot
results.all %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "I~'='~1*'~'*5", 
                           I.sce == 2 ~ "I~'='~5.01*'~'*9", 
                           I.sce == 3 ~ "I~'='~9.01*'~'*12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"),
         N.sce = factor(N.sce, levels = unique(N.sce)),
         K.sce = factor(K.sce, levels = c(0.15, 0.05, 0.5))) %>%
  arrange(K.sce) %>%
  mutate(panel.title = paste(N.sce, I.sce, sep = ", "),
         panel.title = factor(panel.title, levels = unique(panel.title)))%>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarise(vertical.median = median(vertical), nonsystemic.median = median(nonsystemic), systemic.median = median(systemic),
            vertical.low  = quantile(vertical, 0.025), nonsystemic.low = quantile(nonsystemic, 0.025), systemic.low = quantile(systemic, 0.025),
            vertical.high  = quantile(vertical, 0.975), nonsystemic.high = quantile(nonsystemic, 0.975), systemic.high = quantile(systemic, 0.975)) %>%
  pivot_longer(cols = vertical.median:systemic.high,
               names_to = c("path", ".value"),
               names_pattern = "(.*)\\.(.*)") %>%
  ungroup() %>%
  mutate(path = case_when(path == "vertical" ~ "Transovarial", 
                          path == "nonsystemic" ~ "Non-systemic", 
                          path == "systemic" ~ "Systemic"),
         path = factor(path, levels = c("Systemic", "Non-systemic", "Transovarial"))) %>%
  arrange(K.sce, I.sce) %>%
  mutate(panel.name = paste("kappa~'='~", K.sce,"*','~", I.sce,  sep = ""),
         panel.name = factor(panel.name, levels = unique(panel.name)),
         text.color = ifelse(median >= 0.4, "white", "black")) %>%
  ggplot(aes(x = N.sce, y = median, fill = path, ymin = low, ymax = high)) +
  geom_col(position = position_dodge(width=0.9)) +
  geom_errorbar(position = position_dodge(width=0.9), width = 0.1) +
  facet_wrap(~panel.name, labeller = as_labeller(make_labelstring, default = label_parsed))+
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15)) +
  coord_flip()+
  scale_fill_aaas() +
  xlab("Tick abundance (ticks per host)") +
  ylab("Relative contribution") +
  labs(fill = "Transmission\npathway")
ggsave("Figures/Fig4_relative_contribution_barplot.pdf", width = 10, height = 11)















#=============================== Figure 5 ===========================================
#                                  kappa = 0.15
#       relative contributions of the three pathways under different scenarios
#====================================================================================
# heatmap for eij     
elas.mat.kij.all %>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarize_all(median) %>%
  filter(K.sce == 0.15) %>%
  gather("element","eij", V1:V25) %>%
  left_join(data.frame(i = rep(c("TIE", "TIL", "TIN", "TIA", "H"), 5), j = rep(c("TIE", "TIL", "TIN", "TIA", "H"), each = 5), element = paste("V", 1:25, sep = ""))) %>%
  mutate(i = factor(i, levels = rev(c("TIE", "TIL", "TIN", "TIA", "H"))),
         j = factor(j, levels = c("TIE", "TIL", "TIN", "TIA", "H"))) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "I = 1~5", 
                           I.sce == 2 ~ "I = 5~9", 
                           I.sce == 3 ~ "I = 9~12"),
         N.sce = case_when(N.sce == 1 ~ "N = 0.01~2",
                           N.sce == 2 ~ "N = 2~5",
                           N.sce == 3 ~ "N = 5~12"),
         panel.title = paste(N.sce, I.sce, sep = ", "),
         eij = ifelse(eij == 0, NA, eij),
         eij.color = ifelse(eij >= 0.15, "white", "black")) %>%
  ggplot(aes(x = j, y = i, fill = eij)) +
  geom_tile() +
  geom_text(aes(label = round(eij, 3), col = eij.color)) +
  facet_wrap(~panel.title,  labeller = as_labeller(make_labelstring_eij)) +
  theme_cowplot() +
  theme(legend.position = "bottom",
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15),
        legend.key.width = unit(4, "cm"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15)) +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_equal() +
  labs(x = "From", y = "To", fill = expression(e[ij])) +
  scale_fill_distiller(direction = 1, na.value = "gray80") +
  scale_color_manual(values = c("black", "white")) +
  guides(col = FALSE)
ggsave("Figures/Fig5_relative_contribution_eij_heatmap_0.15.pdf", width = 10, height = 11)





#=============================== Figure 6 ===========================================
#                                  kappa = 0.15
#                 permutation importance by outcome of interest
#====================================================================================
# plot sensitivity, elasticity and RF
sens.summary <- sens.mat.all %>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarize_all(median, na.rm = TRUE) %>%
  gather("parameter", "value", E:Ra) %>%
  mutate(type = "Sensitivity") %>%
  ungroup()

elas.summary <- elas.mat.all %>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarize_all(median, na.rm = TRUE) %>%
  gather("parameter", "value", E:Ra) %>%
  mutate(type = "Elasticity") %>%
  ungroup()

RF.summary <- read.csv("Results/RF_imp_R0.csv") %>%
  group_by(I.sce, N.sce, K.sce, variable) %>%
  summarise(value = median(importance)) %>%
  rename(parameter = variable)%>%
  mutate(type = "Random forest") %>%
  ungroup()

import.order <- sens.summary %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter, type) %>%
  summarise(average = mean(value)) %>%
  ungroup() %>%
  mutate(order = rank(-average)) %>%
  rbind(elas.summary %>%
          filter(K.sce == 0.15) %>%
          group_by(parameter, type) %>%
          summarise(average = mean(value)) %>%
          ungroup() %>%
          mutate(order = rank(-average))) %>%
  rbind(RF.summary %>%
          filter(K.sce == 0.15) %>%
          group_by(parameter, type) %>%
          summarise(average = mean(value)) %>%
          ungroup() %>%
          mutate(order = rank(-average))) %>%
  select(-average) %>%
  spread(type, order) %>% 
  rowwise() %>%
  mutate(min.rank = min(Elasticity, `Random forest`,Sensitivity)) %>%
  arrange(min.rank)


plot.a <-  RF.summary %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1~5", 
                           I.sce == 2 ~ "5~9", 
                           I.sce == 3 ~ "9~12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4", size = 3.5) +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        legend.justification = "left",
        plot.tag.position = c(-0.14, 0.075),
        plot.tag = element_text(size = 11),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 2.5), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(0.8, "cm"),
        legend.box.spacing = unit(2, "pt"),
        legend.text = element_text(size = 9),
        panel.border = element_rect(colour = "black")) + 
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  labs(fill = "", y = "", x= "", tag = "Viremia\n(days)\n\nAbundance\n(per host)") +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))


plot.b <- read.csv("Results/RF_imp_systemic.csv") %>%
  #  filter(variable %in% important.par$parameter) %>%
  group_by(I.sce, N.sce, K.sce, variable) %>%
  summarise(value = median(importance)) %>%
  rename(parameter = variable)%>%
  mutate(type = "Random forest") %>%
  ungroup() %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1~5", 
                           I.sce == 2 ~ "5~9", 
                           I.sce == 3 ~ "9~12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4", size = 3.5) +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        legend.justification = "left",
        plot.tag.position = c(-0.14, 0.075),
        plot.tag = element_text(size = 11),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(0.8, "cm"),
        legend.box.spacing = unit(2, "pt"),
        legend.text = element_text(size = 9),
        panel.border = element_rect(colour = "black")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))


plot.c <- read.csv("Results/RF_imp_nonsystemic.csv") %>%
  group_by(I.sce, N.sce, K.sce, variable) %>%
  summarise(value = median(importance)) %>%
  rename(parameter = variable)%>%
  mutate(type = "Random forest") %>%
  ungroup() %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1~5", 
                           I.sce == 2 ~ "5~9", 
                           I.sce == 3 ~ "9~12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4", size = 3.5) +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        legend.justification = "left",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(0.8, "cm"),
        legend.box.spacing = unit(2, "pt"),
        legend.text = element_text(size = 9),
        panel.border = element_rect(colour = "black")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))




plot.d <- read.csv("Results/RF_imp_vertical.csv") %>%
  group_by(I.sce, N.sce, K.sce, variable) %>%
  summarise(value = median(importance)) %>%
  rename(parameter = variable)%>%
  mutate(type = "Random forest") %>%
  ungroup() %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels =rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1~5", 
                           I.sce == 2 ~ "5~9", 
                           I.sce == 3 ~ "9~12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4", size = 3.5) +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        legend.justification = "left",
        # plot.tag.position = c(-0.1, 0.12),
        # plot.tag = element_text(size = 11),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(0.8, "cm"),
        legend.box.spacing = unit(2, "pt"),
        legend.text = element_text(size = 9),
        panel.border = element_rect(colour = "black")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))




plot.e <- read.csv("Results/RF_imp_pattern.csv") %>%
  group_by(I.sce, N.sce, K.sce, variable) %>%
  summarise(value = median(importance)) %>%
  rename(parameter = variable)%>%
  mutate(type = "Random forest") %>%
  ungroup() %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1~5", 
                           I.sce == 2 ~ "5~9", 
                           I.sce == 3 ~ "9~12"),
         N.sce = case_when(N.sce == 1 ~ "0.01~2",
                           N.sce == 2 ~ "2~5",
                           N.sce == 3 ~ "5~12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4", size = 3.5) +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        legend.justification = "left",
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(0.8, "cm"),
        legend.box.spacing = unit(2, "pt"),
        legend.text = element_text(size = 9),
        panel.border = element_rect(colour = "black")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))

save_plot("Figures/Fig6_importance_ranking.pdf", plot_grid(plot.a, plot.b, plot.c, plot.d, plot.e, ncol = 5, rel_widths = c(1.2, 1, 1, 1, 1)) + draw_plot_label(c('A*")"*~R[0]', 'B*")"*~Systemic', 'C*")"*~Non-systemic', 'D*")"*~Transovarial', 'E*")"*~Pattern'), x = c(0, 0.2, 0.4, 0.6, 0.8), y = c(1, 1, 1, 1, 1), parse = TRUE), base_height = 10, base_width = 13)
