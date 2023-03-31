# source("documentation.qmd")

plastic_degradation <- rbind(PP_deg_summary,PE_deg_summary) |> 
  mutate(across(where(is_character),as_factor))


# colors ------------------------------------------------------------------

col_pol <- c("#5D8F5F", "#8F5D72")


# degradation amount ------------------------------------------------------

umol<- ggplot(data = plastic_degradation, 
       aes(x = time, y = average_Net_excess_13C_umol, group= incubation, color = incubation)) +
  geom_point(aes(color= incubation), size = 3)+
  scale_color_manual(values = col_pol, name = "Polymer type",labels = c("¹³C-PP", "¹³C-PE"))+
  geom_errorbar(aes(ymin=average_Net_excess_13C_umol-std_Net_excess_13C_umol, 
                    ymax=average_Net_excess_13C_umol+std_Net_excess_13C_umol),
                width=.2, position=position_dodge(0.05))+
  geom_smooth(method = "lm",formula = y ~ x , se = F, color = "black", size = 1, linetype = 5) +
  stat_poly_eq(formula = y ~ x ,
               method = "lm",
               use_label(c("eq", "r2")),
               coef.digits = 10,
               size= 4,
               geom = "text_npc",
               label.x = 1,
               label.y = c(0.45,0.25)) +
  theme_minimal()+
  # ylim(0,2)+
  ylab("net ¹³CO₂ production (µmol)")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        legend.position = "top")

umol



# percentage degradation plots --------------------------------------------

perct <- ggplot(data = plastic_degradation, 
       aes(x = time, y = perct_degradation, group= incubation, color = incubation)) +
  geom_point(aes(color= incubation), size = 3)+
  scale_color_manual(values = col_pol, name = "Polymer type",labels = c("¹³C-PP", "¹³C-PE"))+
  geom_errorbar(aes(ymin=perct_degradation-sd_perct_deg, 
                    ymax=perct_degradation+sd_perct_deg),
                width=.2, position=position_dodge(0.05))+
  geom_smooth(method = "lm",formula = y ~ x , se = F,aes( color = incubation), size = 1, linetype = 5) +
  stat_poly_eq(formula = y ~ x ,
               method = "lm",
               use_label(c("eq", "r2")),
               coef.digits = 10,
               size= 4,
               geom = "text_npc",
               label.y = c(0.9,0.8)) +
  theme_minimal()+
  ylim(0,100)+
  ylab("Percentage of degradation")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(size = 9, face = "bold"),
        axis.text.y = element_text(size = 9, face = "bold"),
        legend.position = "top",
        legend.text = element_text(size = 12, face = "bold"),
        legend.title = element_text(size = 12, face = "bold"))

ggpubr::ggarrange(perct,umol,  common.legend = T)
