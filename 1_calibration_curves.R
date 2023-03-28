library(tidyverse)
install.packages("ggpmisc")
library("ggpmisc")

# regression calibration curve experiment ---------------------------------

calib_1 <- read_delim("../data_used/calibration_curve_GC-FID_Mishal.txt")
calib_curve_1 <- ggplot(data = calib_1, aes(x = area, y = ppm)) +
  geom_smooth(method = "lm",formula = y ~ x + 0, se = F, color = "black", size = 1, linetype = 5) +
  # stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0, aes(label = after_stat(eq.label)), coef.digits = 5, size= 6) +
  geom_point(color = "cadetblue", size = 3)+
  theme_minimal()+
  ggtitle("calibration curve for CO2 PP, PE and background", subtitle = "Mishal - June 2022") +
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"))
# calib_curve_1

linear_reg_1 <- lm(ppm ~ 0+area, data = calib_1 )
slope_1 <- linear_reg_1$coefficients


# regression kill controls ------------------------------------------------

calib_2 <- read_delim("../data_used/calibration_curve_kill_controls.txt")
calib_curve_2 <- ggplot(data = calib_2, aes(x = area, y = ppm)) +
  geom_smooth(method = "lm",formula = y ~ x + 0, se = F, color = "black", size = 1, linetype = 5) +
  # stat_poly_line(formula = y ~ x + 0) +
  stat_poly_eq(formula = y ~ x + 0, aes(label = after_stat(eq.label)), coef.digits = 5, size= 6) +
  geom_point(color = "cadetblue", size = 3)+
  theme_minimal()+
  ggtitle("calibration curve for CO2 repeat kill controls", subtitle = "Rachel - Nov 2022")+
  theme(axis.title.y = element_text(face = "bold"),
        axis.title.x = element_text(face = "bold"),
        axis.text.x = element_text(size = 11, face = "bold"),
        axis.text.y = element_text(size = 11, face = "bold"))
# calib_curve_2

linear_reg_2 <- lm(ppm ~ 0+area, data = calib_2 )
slope_2 <- linear_reg_2$coefficients

