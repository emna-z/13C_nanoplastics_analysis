DIC_tab <- read_delim("../data_used/DIC_measures.txt")

liquid_L <- 0.15

DIC_tab <- DIC_tab |> 
  mutate(DIC_L_Phase = DIC_uM*liquid_L) |> 
  co2_liquid
  

model_DIC <- ggplot(data = tab, aes(x = time, y = DIC_uM)) +
  facet_grid(~incubation)+
  geom_point(color = "cadetblue", size = 3)+
  geom_smooth(method = "lm",formula = y ~ x , se = F, color = "black", size = 1, linetype = 5) +
  # stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x , aes(label = after_stat(eq.label)), coef.digits = 5, size= 3) +
  theme_minimal()+
  ggtitle("DIC linear regressions", subtitle = "") +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"))
model_DIC  

DIC_kill_PE <- function(x){y= 2026.4+x*} 