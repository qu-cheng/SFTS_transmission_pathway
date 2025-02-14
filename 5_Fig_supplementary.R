source("0_import_libraries.R")

# read in results
results.all <- read.csv("Results/Results_I_N_scenarios.csv")
sens.mat.all <- read.csv("Results/sens_mat_I_N_scenarios.csv")
elas.mat.all <- read.csv("Results/elas_mat_I_N_scenarios.csv")
elas.mat.kij.all <- read.csv("Results/elas_mat_kij_I_N_scenarios.csv")

# read parameter sets
par1 <- read.csv("Parameters/LHS_pars_scaled_k0.05.csv") %>%
  mutate(k = 0.05)
par2 <- read.csv("Parameters/LHS_pars_scaled_k0.15.csv")%>%
  mutate(k = 0.15)
par3 <- read.csv("Parameters/LHS_pars_scaled_k0.5.csv")%>%
  mutate(k = 0.5)

par.all <- rbind(par1, par2, par3)


#=============================== Figure ST1.1 =======================================
#            histograms for the parameters that do not vary with scenarios
#====================================================================================
par1 %>%
  gather("par", "value", E:Caa1) %>%
  filter(par %in% c("E", "Sl", "Sn", "Sa", "Cs", "Dl", "Dn", "Da", "Hcn", "Hcs", "theta", "Pl", "Pn", "Pa", "Ql", "Qn", "Qa", "Ra")) %>%
  mutate(par = factor(par, levels = unique(par), labels = c("E", "S[l]", "S[n]", "S[a]", "C[s]", "D[l]", "D[n]", "D[a]", "H[cn]", "H[cs]", "theta", "P[l]", "P[n]", "P[a]", "Q[l]", "Q[n]", "Q[a]", "R[a]"))) %>%
  ggplot(aes(x = value)) +
  geom_histogram(fill = "gray80") +
  facet_wrap(~par, scales = "free", labeller = label_parsed) +
  xlab("Value") +
  ylab("Count") +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14),
        axis.text = element_text(size = 11)) 
ggsave("Figures/FigST1.1.pdf", width = 10, height = 8)




#=============================== Figure ST1.2 =======================================
#                        Histograms of I, Nlh, Nnh, and Nah
#====================================================================================
par.all %>%
  gather("par", "value", E:Caa1) %>%
  filter(par %in% c("I1", "I2", "I3", "Nlh1", "Nlh2", "Nlh3", "Nnh1", "Nnh2", "Nnh3", "Nah1", "Nah2", "Nah3")) %>%
  mutate(par.letter = gsub("[^a-zA-Z]", "", par),
         par.scenario = parse_number(par)) %>%
  mutate(par.letter = factor(par.letter, levels = unique(par.letter), labels = c("I", "N[lh]", "N[nh]", "N[ah]"))) %>%
  ggplot(aes(x = value, fill = as.factor(par.scenario))) +
  geom_histogram(alpha = 0.4, position = "identity") +
  facet_wrap(~par.letter, scales = "free", labeller = label_parsed, nrow = 1)+
  xlab("Value") +
  ylab("Count") +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14),
        axis.text = element_text(size = 11)) +
  scale_fill_aaas() +
  guides(fill = FALSE)
ggsave("Figures/FigST1.2.pdf", width = 10, height = 3)


#=============================== Figure ST1.3 =======================================
#                         Histograms of Cxy when k = 0.15
#====================================================================================
par.all %>%
  filter(k == 0.15) %>%
  gather("par", "value", E:Caa1) %>%
  filter(par %in% c("k", "Cll3","Cll2" ,"Cll1","Cnl3","Cnl2" , "Cnl1" , "Cal3" , "Cal2"  ,"Cal1"  ,"Cln3" , "Cln2" , "Cln1" , "Cnn3" , "Cnn2"  ,"Cnn1" , "Can3" , "Can2" , "Can1" , "Cla3" , "Cla2" , "Cla1" , "Cna3" , "Cna2" , "Cna1" , "Caa3"  ,"Caa2" , "Caa1")) %>%
  mutate(par.letter = gsub("[^a-zA-Z]", "", par),
         par.scenario = parse_number(par)) %>%
  mutate(par.letter = factor(par.letter, levels = unique(par.letter), labels = c("C[ll]", "C[nl]", "C[al]", "C[ln]", "C[nn]", "C[an]", "C[la]", "C[na]", "C[aa]"))) %>%
  ggplot(aes(x = value, fill = as.factor(par.scenario))) +
  geom_histogram(alpha = 0.4, position = "identity") +
  facet_wrap(~par.letter, scales = "free", labeller = label_parsed)+
  xlab("Value") +
  ylab("Count") +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14),
        axis.text = element_text(size = 11)) +
  scale_fill_aaas() +
  guides(fill = FALSE) +
  ggtitle(expression(kappa*" = 0.15"))
ggsave("Figures/FigST1.3.pdf", width = 10, height = 8)




#=============================== Figure ST1.4 =======================================
#                         Histograms of Cxy when k = 0.05
#====================================================================================
par.all %>%
  filter(k == 0.05) %>%
  gather("par", "value", E:Caa1) %>%
  filter(par %in% c("k", "Cll3","Cll2" ,"Cll1","Cnl3","Cnl2" , "Cnl1" , "Cal3" , "Cal2"  ,"Cal1"  ,"Cln3" , "Cln2" , "Cln1" , "Cnn3" , "Cnn2"  ,"Cnn1" , "Can3" , "Can2" , "Can1" , "Cla3" , "Cla2" , "Cla1" , "Cna3" , "Cna2" , "Cna1" , "Caa3"  ,"Caa2" , "Caa1")) %>%
  mutate(par.letter = gsub("[^a-zA-Z]", "", par),
         par.scenario = parse_number(par)) %>%
  mutate(par.letter = factor(par.letter, levels = unique(par.letter), labels = c("C[ll]", "C[nl]", "C[al]", "C[ln]", "C[nn]", "C[an]", "C[la]", "C[na]", "C[aa]"))) %>%
  ggplot(aes(x = value, fill = as.factor(par.scenario))) +
  geom_histogram(alpha = 0.4, position = "identity") +
  facet_wrap(~par.letter, scales = "free", labeller = label_parsed)+
  xlab("Value") +
  ylab("Count") +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14),
        axis.text = element_text(size = 11)) +
  scale_fill_aaas() +
  guides(fill = FALSE) +
  ggtitle(expression(kappa*" = 0.05"))
ggsave("Figures/FigST1.4.pdf", width = 10, height = 8)






#=============================== Figure ST1.5 =======================================
#                         Histograms of Cxy when k = 0.5
#====================================================================================
par.all %>%
  filter(k == 0.5) %>%
  gather("par", "value", E:Caa1) %>%
  filter(par %in% c("k", "Cll3","Cll2" ,"Cll1","Cnl3","Cnl2" , "Cnl1" , "Cal3" , "Cal2"  ,"Cal1"  ,"Cln3" , "Cln2" , "Cln1" , "Cnn3" , "Cnn2"  ,"Cnn1" , "Can3" , "Can2" , "Can1" , "Cla3" , "Cla2" , "Cla1" , "Cna3" , "Cna2" , "Cna1" , "Caa3"  ,"Caa2" , "Caa1")) %>%
  mutate(par.letter = gsub("[^a-zA-Z]", "", par),
         par.scenario = parse_number(par)) %>%
  mutate(par.letter = factor(par.letter, levels = unique(par.letter), labels = c("C[ll]", "C[nl]", "C[al]", "C[ln]", "C[nn]", "C[an]", "C[la]", "C[na]", "C[aa]"))) %>%
  ggplot(aes(x = value, fill = as.factor(par.scenario))) +
  geom_histogram(alpha = 0.4, position = "identity") +
  facet_wrap(~par.letter, scales = "free", labeller = label_parsed)+
  xlab("Value") +
  ylab("Count") +
  theme_cowplot() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 14),
        axis.text = element_text(size = 11)) +
  scale_fill_aaas() +
  guides(fill = FALSE) +
  ggtitle(expression(kappa*" = 0.5"))
ggsave("Figures/FigST1.5.pdf", width = 10, height = 8)















#=============================== Figure ST2.1 =======================================
#                        Ternary plot interpretation
#====================================================================================
dat.set <- data.frame(vertical = c(60,60,20),
                      nonsystemic = c(20,20,60),
                      systemic = c(20,20,20),
                      vertical.end = c(20,20,20),
                      nonsystemic.end = c(20,60,20),
                      systemic.end = c(60,20,60))

data.frame(vertical = 0.15, nonsystemic = 0.35, systemic = 0.5) %>%
  ggtern(aes(x = systemic, y = nonsystemic, z = vertical)) + 
  geom_segment(data = dat.set, aes(x = systemic, y = nonsystemic, z = vertical,
                                   xend = systemic.end, yend = nonsystemic.end, zend = vertical.end),
               col = "red")+
  theme_bw() +
  labs(x = "Systemic",
       y = "Non-systemic",
       z = "Transovarial") +
  theme_showarrows() +
  geom_Tline(Tintercept = 0.6, col = "red") +
  geom_Lline(Lintercept = 0.6, col = "red") +
  geom_Rline(Rintercept = 0.6, col = "red") +
  geom_Lline(Lintercept = 0.5, col = "black", linetype = "dashed") +
  geom_point(shape = 17, size = 3) +
  annotate("text", x = 72, y = 18,z = 10, label = "Systemic", col = "blue") +
  annotate("text", x = 10, y = 18,z = 72, label = "Transovarial", col = "blue") +
  annotate("text", x = 13, y = 74,z = 13, label = "Non-systemic", col = "blue") +
  annotate("text", x = 45, y = 10,z = 45, label = "S + T", col = "blue") +
  annotate("text", x = 45, y = 45,z = 10, label = "S + N", col = "blue") +
  annotate("text", x = 10, y = 45,z = 45, label = "N + T", col = "blue") +
  annotate("text", x = 33, y = 33,z = 33, label = "All", col = "blue") +
  theme(tern.panel.expand = 0.28)
ggsave("Figures/FigST2.1.pdf", width = 7, height = 6)










#=============================== Figure S1 ===========================================
#                       ternary plots by scenarios
#====================================================================================
results.all %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "I = 1–5", 
                           I.sce == 2 ~ "I = 5.01–9", 
                           I.sce == 3 ~ "I = 9.01–12"),
         N.sce = case_when(N.sce == 1 ~ "N = 0.01–2",
                           N.sce == 2 ~ "N = 2–5",
                           N.sce == 3 ~ "N = 5–12"),
         K.sce = case_when(K.sce == 0.05 ~ "k = 0.05",
                           K.sce == 0.15 ~ "k = 0.15",
                           K.sce == 0.5 ~ "k = 0.5"),
         N.sce = factor(N.sce, levels = unique(N.sce)),
         panel.title = paste(N.sce, I.sce, sep = ", "),
         panel.title = factor(panel.title, levels = unique(panel.title))) %>%
  ggtern(aes(x = systemic, y = nonsystemic, z = vertical, col = as.factor(K.sce))) + 
  geom_point(stroke = 0, size = 0.8) +
  theme_bw() +
  labs(xarrow = "Systemic",
       yarrow = "Non-systemic",
       zarrow = "Transovarial",
       x = "Sys.",
       y = "Non-sys.",
       z = "Trans.") +
  theme_showarrows() +
  geom_confidence_tern(breaks = 0.95, alpha = 1) +
  # geom_mean_ellipse() +
  geom_Tline(Tintercept = 0.6, col = "gray30") +
  geom_Lline(Lintercept = 0.6, col = "gray30") +
  geom_Rline(Rintercept = 0.6, col = "gray30") +
  geom_segment(data = dat.set, aes(x = vertical, y = nonsystemic, z = systemic,
                                   xend = vertical.end, yend = nonsystemic.end, zend = systemic.end),
               col = "gray30")+
  annotate("text", x = 72, y = 18,z = 10, label = "S", col = "blue", size = 4, fontface = "bold") +
  annotate("text", x = 10, y = 18,z = 72, label = "T", col = "blue", size = 4, fontface = "bold") +
  annotate("text", x = 13, y = 74,z = 13, label = "NS", col = "blue", size = 4, fontface = "bold") +
  annotate("text", x = 45, y = 10,z = 45, label = "S + T", col = "blue", size = 4, fontface = "bold") +
  annotate("text", x = 45, y = 45,z = 10, label = "S + N", col = "blue", size = 4, fontface = "bold") +
  annotate("text", x = 10, y = 45,z = 45, label = "N + T", col = "blue", size = 4, fontface = "bold") +
  annotate("text", x = 33, y = 33,z = 33, label = "All", col = "blue", size = 4, fontface = "bold") +
  # facet_grid(rows = vars(I.sce), cols = vars(N.sce)) +
  facet_wrap(~ panel.title, labeller = as_labeller(make_labelstring_eij)) +
  scale_color_locuszoom() +
  theme(strip.background = element_blank(),
        strip.text = element_text(hjust = 0, size = 15),
        legend.position = "top",
        axis.text = element_text(colour = "gray50"),
        tern.axis.arrow.start = 0.2,
        tern.axis.arrow.finish = 0.8,
        tern.panel.expand = 0.23,
        legend.text = element_text(size = 14)) +
  labs(col = "") +
  custom_percent("%")
ggsave("Figures/FigS1_Ternary.pdf", width = 10, height = 10)







#=============================== Figure S2 ===========================================
#                                  kappa = 0.05
#       relative contributions of the three pathways under different scenarios
#====================================================================================
# heatmap for eij     
elas.mat.kij.all %>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarize_all(median) %>%
  filter(K.sce == 0.05) %>%
  gather("element","eij", V1:V25) %>%
  left_join(data.frame(i = rep(c("TIE", "TIL", "TIN", "TIA", "H"), 5), j = rep(c("TIE", "TIL", "TIN", "TIA", "H"), each = 5), element = paste("V", 1:25, sep = ""))) %>%
  mutate(i = factor(i, levels = rev(c("TIE", "TIL", "TIN", "TIA", "H"))),
         j = factor(j, levels = c("TIE", "TIL", "TIN", "TIA", "H"))) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "I = 1-5", 
                           I.sce == 2 ~ "I = 5-9", 
                           I.sce == 3 ~ "I = 9-12"),
         N.sce = case_when(N.sce == 1 ~ "N = 0.01-2",
                           N.sce == 2 ~ "N = 2-5",
                           N.sce == 3 ~ "N = 5-12"),
         panel.title = paste(N.sce, I.sce, sep = ", "),
         eij = ifelse(eij == 0, NA, eij),
         eij.color = ifelse(eij >= 0.15, "white", "black")) %>%
  ggplot(aes(x = j, y = i, fill = eij)) +
  geom_tile() +
  geom_text(aes(label = round(eij, 3), col = eij.color)) +
  facet_grid(cols = vars(I.sce), rows = vars(N.sce),
             switch = "y") +
  theme_cowplot() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 15),
        legend.key.width = unit(4, "cm"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        strip.placement = "outside") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_equal() +
  labs(x = "From", y = "To", fill = expression(e[ij])) +
  scale_fill_distiller(direction = 1, na.value = "gray95") +
  scale_color_manual(values = c("black", "white")) +
  guides(col = FALSE)
ggsave("Figures/FigS2_relative_contribution_eij_heatmap_0.05.pdf", width = 10, height = 11)








#=============================== Figure S3 ===========================================
#                                  kappa = 0.5
#       relative contributions of the three pathways under different scenarios
#====================================================================================
# heatmap for eij     
elas.mat.kij.all %>%
  group_by(I.sce, N.sce, K.sce) %>%
  summarize_all(median) %>%
  filter(K.sce == 0.5) %>%
  gather("element","eij", V1:V25) %>%
  left_join(data.frame(i = rep(c("TIE", "TIL", "TIN", "TIA", "H"), 5), j = rep(c("TIE", "TIL", "TIN", "TIA", "H"), each = 5), element = paste("V", 1:25, sep = ""))) %>%
  mutate(i = factor(i, levels = rev(c("TIE", "TIL", "TIN", "TIA", "H"))),
         j = factor(j, levels = c("TIE", "TIL", "TIN", "TIA", "H"))) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "I = 1-5", 
                           I.sce == 2 ~ "I = 5-9", 
                           I.sce == 3 ~ "I = 9-12"),
         N.sce = case_when(N.sce == 1 ~ "N = 0.01-2",
                           N.sce == 2 ~ "N = 2-5",
                           N.sce == 3 ~ "N = 5-12"),
         panel.title = paste(N.sce, I.sce, sep = ", "),
         eij = ifelse(eij == 0, NA, eij),
         eij.color = ifelse(eij >= 0.15, "white", "black")) %>%
  ggplot(aes(x = j, y = i, fill = eij)) +
  geom_tile() +
  geom_text(aes(label = round(eij, 3), col = eij.color)) +
  facet_grid(cols = vars(I.sce), rows = vars(N.sce),
             switch = "y") +
  theme_cowplot() +
  theme(legend.position = "bottom",
        strip.text = element_text(size = 15),
        legend.key.width = unit(4, "cm"),
        axis.title = element_text(size = 15),
        axis.text = element_text(size = 14),
        legend.title = element_text(size = 15),
        strip.placement = "outside") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0)) +
  coord_equal() +
  labs(x = "From", y = "To", fill = expression(e[ij])) +
  scale_fill_distiller(direction = 1, na.value = "gray95") +
  scale_color_manual(values = c("black", "white")) +
  guides(col = FALSE)
ggsave("Figures/FigS3_relative_contribution_eij_heatmap_0.5.pdf", width = 10, height = 11)






#=============================== Figure S4 ==========================================
#                                kappa = 0.15
#       Contribution of each parameter to R0 measured by 3 indices
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


plot.a <- sens.summary %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        plot.tag.position = c(-0.1, 0.075),
        plot.tag = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 2.5), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "", tag = "Viremia\n(days)\n\nAbundance\n(per host)") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))

plot.b <- elas.summary %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = round(rank(-value))) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))

plot.c <- RF.summary %>%
  filter(K.sce == 0.15) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30))) 

title <- ggdraw() + 
  draw_label(
    expression(paste(kappa, " = 0.15", sep = "")),
    fontface = 'bold',
    x = 0.05,
    hjust = 0,
    size = 18
  ) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 7))

save_plot("Figures/FigS4_R0_sens_elas_RF_0.15.pdf", plot_grid(title, plot_grid(plot.a, plot.b, plot.c, ncol = 3, labels = c("A) Sensitivity", "B) Elasticity", "C) RF"), rel_widths = c(1.15, 1, 1)), ncol = 1, rel_heights = c(0.04, 1)), base_height = 10, base_width = 10)






#=============================== Figure S5 ==========================================
#                                kappa = 0.05
#       Contribution of each parameter to R0 measured by 3 indices
#====================================================================================
plot.a <- sens.summary %>%
  filter(K.sce == 0.05) %>%
  group_by(parameter) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        plot.tag.position = c(-0.1, 0.075),
        plot.tag = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 2.5), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "", tag = "Viremia\n(days)\n\nAbundance\n(per host)") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))

plot.b <- elas.summary %>%
  filter(K.sce == 0.05) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = round(rank(-value))) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))

plot.c <- RF.summary %>%
  filter(K.sce == 0.05) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30))) 

title <- ggdraw() + 
  draw_label(
    expression(paste(kappa, " = 0.05", sep = "")),
    fontface = 'bold',
    x = 0.05,
    hjust = 0,
    size = 18
  ) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 7))

save_plot("Figures/FigS5_R0_sens_elas_RF_0.05.pdf", plot_grid(title, plot_grid(plot.a, plot.b, plot.c, ncol = 3, labels = c("A) Sensitivity", "B) Elasticity", "C) RF"), rel_widths = c(1.15, 1, 1)), ncol = 1, rel_heights = c(0.04, 1)), base_height = 10, base_width = 10)








#=============================== Figure S6 ==========================================
#                                kappa = 0.5
#       Contribution of each parameter to R0 measured by 3 indices
#====================================================================================
plot.a <- sens.summary %>%
  filter(K.sce == 0.5) %>%
  group_by(parameter) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        plot.tag.position = c(-0.1, 0.075),
        plot.tag = element_text(size = 12),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 2.5), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "", tag = "Viremia\n(days)\n\nAbundance\n(per host)") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))

plot.b <- elas.summary %>%
  filter(K.sce == 0.5) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = round(rank(-value))) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30)))

plot.c <- RF.summary %>%
  filter(K.sce == 0.5) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  group_by(N.sce, I.sce) %>%
  mutate(value.order = rank(-value)) %>%
  ggplot(aes(x = I.sce, y = parameter, fill = value)) +
  geom_tile() +
  geom_text(aes(label = value.order), col = "red4") +
  theme_cowplot() +
  facet_grid(~N.sce, scales = "free_x", switch = "x") +
  theme(strip.background = element_blank(),
        strip.placement = "outside",
        legend.position = "top",
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.spacing = unit(0.1, "lines"),
        plot.margin = unit(c(1.5, 0.2, 0.5, 0), "lines"),     # margin(t, r, b, l)
        legend.key.width = unit(1, "cm"),
        legend.box.spacing = unit(2, "pt")) + 
  labs(fill = "", y = "", x= "") +
  scale_x_discrete(expand = c(0,0)) +
  scale_y_discrete(expand = c(0,0), labels = lab) +
  scale_fill_gradientn(colors = rev(paletteer_c("grDevices::Blues 3", 30))) 

title <- ggdraw() + 
  draw_label(
    expression(paste(kappa, " = 0.5", sep = "")),
    fontface = 'bold',
    x = 0.05,
    hjust = 0,
    size = 18
  ) +
  theme(plot.margin = ggplot2::margin(0, 0, 0, 7))

save_plot("Figures/FigS6_R0_sens_elas_RF_0.5.pdf", plot_grid(title, plot_grid(plot.a, plot.b, plot.c, ncol = 3, labels = c("A) Sensitivity", "B) Elasticity", "C) RF"), rel_widths = c(1.15, 1, 1)), ncol = 1, rel_heights = c(0.04, 1)), base_height = 10, base_width = 10)







#=============================== Figure S7 ===========================================
#                                   pdp
#======================================================================================
pdp_R0 <- read.csv("Results/RF_pdp_R0.csv") %>%
  mutate(par.name = rep(rep(par.names, each = 51), 27))

pdp_R0 %>%
  filter(K.sce ==0.15) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  filter(par.name %in% c("Ra", "E", "Sl", "Sn", "Sa", "Hcn", "Cs")) %>%
  group_by(K.sce, I.sce, N.sce, outcome, par.name) %>%
  mutate(par.std = (par - min(par))/(max(par) - min(par)),
         par.name = factor(par.name, levels = c("Ra", "E", "Sl", "Sn", "Sa", "Hcn", "Cs"))) %>%
  ggplot(aes(x = par.std, y = yhat, col = par.name)) +
  geom_line() +
  facet_grid(rows = vars(N.sce), cols = vars(I.sce),
             labeller = labeller(I.sce = function(x) paste("I = ", x),
                                 N.sce = function(x) paste("N = ", x)),
             switch = "y") +
  theme_cowplot() +
  theme(legend.position = "top") +
  scale_color_npg(labels = c(expression(R[a]), "E", expression(S[l]), expression(S[n]), expression(S[a]), expression(H[cn]), expression(C[s]))) +
  geom_hline(yintercept = 1, linetype = "dashed", col = "gray30") +
  labs(col = "", x = "Model parameters scaled to 0~1", y = expression(Predicted~R[0]~when~other~parameters~set~to~their~mean~values)) +
  guides(color = guide_legend(nrow = 1))
ggsave("Figures/FigS7_pdp_R0.pdf", width = 9, height = 9)









#=============================== Figure S8 ===========================================
#                              kappa = 0.05
#                 permutation importance by outcome of interest
#====================================================================================
plot.a <-  RF.summary %>%
  filter(K.sce == 0.05) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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
  filter(K.sce == 0.05) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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
  filter(K.sce == 0.05) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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
  filter(K.sce == 0.05) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels =rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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
  filter(K.sce == 0.05) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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

save_plot("Figures/FigS8_importance_ranking_k0.05.pdf", plot_grid(plot.a, plot.b, plot.c, plot.d, plot.e, ncol = 5, rel_widths = c(1.2, 1, 1, 1, 1)) + draw_plot_label(c('A*")"*~R[0]', 'B*")"*~Systemic', 'C*")"*~Non-systemic', 'D*")"*~Transovarial', 'E*")"*~Pattern'), x = c(0, 0.2, 0.4, 0.6, 0.8), y = c(1, 1, 1, 1, 1), parse = TRUE), base_height = 10, base_width = 13)









#=============================== Figure S9 ===========================================
#                              kappa = 0.5
#                 permutation importance by outcome of interest
#====================================================================================
plot.a <-  RF.summary %>%
  filter(K.sce == 0.5) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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
  filter(K.sce == 0.5) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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
  filter(K.sce == 0.5) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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
  filter(K.sce == 0.5) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels =rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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
  filter(K.sce == 0.5) %>%
  group_by(parameter) %>%
  mutate(average = mean(value)) %>%
  ungroup() %>%
  arrange(average) %>%
  mutate(parameter = factor(parameter, levels = rev(import.order$parameter)),
         scenarios = paste(N.sce, I.sce, sep = ", ")) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
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

save_plot("Figures/FigS9_importance_ranking_k0.5.pdf", plot_grid(plot.a, plot.b, plot.c, plot.d, plot.e, ncol = 5, rel_widths = c(1.2, 1, 1, 1, 1)) + draw_plot_label(c('A*")"*~R[0]', 'B*")"*~Systemic', 'C*")"*~Non-systemic', 'D*")"*~Transovarial', 'E*")"*~Pattern'), x = c(0, 0.2, 0.4, 0.6, 0.8), y = c(1, 1, 1, 1, 1), parse = TRUE), base_height = 10, base_width = 13)










#=============================== Figure S10 ===========================================
#                                   pdp
#======================================================================================
pdp_sys <- read.csv("Results/RF_pdp_systemic.csv") %>%
  mutate(par.name = rep(rep(par.names, each = 51), 27))

pdp_sys %>%
  filter(K.sce ==0.15) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  filter(par.name %in% c("Hcn", "Cs", "Hcs", "Ra", "Ql", "Sn", "E")) %>%
  group_by(K.sce, I.sce, N.sce, outcome, par.name) %>%
  mutate(par.std = (par - min(par))/(max(par) - min(par)),
         par.name = factor(par.name, levels = c("Hcn", "Cs", "Hcs", "Ra", "Ql", "Sn", "E"))) %>%
  ggplot(aes(x = par.std, y = yhat, col = par.name)) +
  geom_line() +
  facet_grid(rows = vars(N.sce), cols = vars(I.sce),
             labeller = labeller(I.sce = function(x) paste("I = ", x),
                                 N.sce = function(x) paste("N = ", x)),
             switch = "y") +
  theme_cowplot() +
  theme(legend.position = "top") +
  scale_color_d3(labels = c(expression(H[cn]), expression(C[s]), expression(H[cs]), expression(R[a]), expression(Q[l]), expression(S[n]), expression("E"))) +
  labs(col = "", x = "Model parameters scaled to 0~1", y = "Relative contribution of systemic transmission when other parameters set to their mean values") +
  guides(color = guide_legend(nrow = 1))
ggsave("Figures/FigS10_pdp_systemic.pdf", width = 9, height = 9)











#=============================== Figure S11 ===========================================
#                                   pdp
#======================================================================================
pdp_nonsys <- read.csv("Results/RF_pdp_nonsystemic.csv") %>%
  mutate(par.name = rep(rep(par.names, each = 51), 27))

pdp_nonsys %>%
  filter(K.sce ==0.15) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  filter(par.name %in% c("Hcn", "Cs", "Ql", "Hcs", "Pa", "I", "Ra")) %>%
  group_by(K.sce, I.sce, N.sce, outcome, par.name) %>%
  mutate(par.std = (par - min(par))/(max(par) - min(par)),
         par.name = factor(par.name, levels = c("Hcn", "Cs", "Ql", "Hcs", "Pa", "I", "Ra"))) %>%
  ggplot(aes(x = par.std, y = yhat, col = par.name)) +
  geom_line() +
  facet_grid(rows = vars(N.sce), cols = vars(I.sce),
             labeller = labeller(I.sce = function(x) paste("I = ", x),
                                 N.sce = function(x) paste("N = ", x)),
             switch = "y") +
  theme_cowplot() +
  theme(legend.position = "top") +
  scale_color_d3(labels = c(expression(H[cn]), expression(C[s]), expression(Q[l]), expression(H[cs]), expression(P[a]), expression("I"), expression(R[a]))) +
  labs(col = "", x = "Model parameters scaled to 0~1", y = "Relative contribution of non-systemic transmission when other parameters set to their mean values") +
  guides(color = guide_legend(nrow = 1))
ggsave("Figures/FigS11_pdp_nonsystemic.pdf", width = 9, height = 9)











#=============================== Figure S12 ===========================================
#                                   pdp
#======================================================================================
pdp_ver <- read.csv("Results/RF_pdp_vertical.csv") %>%
  mutate(par.name = rep(rep(par.names, each = 51), 27))

pdp_ver %>%
  filter(K.sce ==0.15) %>%
  mutate(I.sce = case_when(I.sce == 1 ~ "1-5", 
                           I.sce == 2 ~ "5-9", 
                           I.sce == 3 ~ "9-12"),
         N.sce = case_when(N.sce == 1 ~ "0.01-2",
                           N.sce == 2 ~ "2-5",
                           N.sce == 3 ~ "5-12"))  %>%
  filter(par.name %in% c("Ra", "E", "Sn", "Sl", "Sa", "Hcs", "Ql")) %>%
  group_by(K.sce, I.sce, N.sce, outcome, par.name) %>%
  mutate(par.std = (par - min(par))/(max(par) - min(par)),
         par.name = factor(par.name, levels = c("Ra", "E", "Sn", "Sl", "Sa", "Hcs", "Ql"))) %>%
  ggplot(aes(x = par.std, y = yhat, col = par.name)) +
  geom_line() +
  facet_grid(rows = vars(N.sce), cols = vars(I.sce),
             labeller = labeller(I.sce = function(x) paste("I = ", x),
                                 N.sce = function(x) paste("N = ", x)),
             switch = "y") +
  theme_cowplot() +
  theme(legend.position = "top") +
  scale_color_npg(labels = c(expression(R[a]), "E", expression(S[n]), expression(S[l]), expression(S[a]), expression(H[cs]), expression(Q[l]))) +
  labs(col = "", x = "Model parameters scaled to 0~1", y = "Relative contribution of transovarial transmission when other parameters set to their mean values") +
  guides(color = guide_legend(nrow = 1))
ggsave("Figures/FigS12_pdp_transovarial.pdf", width = 9, height = 9)

