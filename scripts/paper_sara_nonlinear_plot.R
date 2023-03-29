library(tidyverse)
library(ggplot2)
library(magrittr)
library(gamlss)
library(drc)
library(lemon)
library(glue)
library(broom)
library(ggh4x)
library(showtext)
library(nlstools)

# Modelling 
df_up <- read_excel("data/data_updated.xlsx")
names(df_up) <- c("doy","day","temp","min","per")
df_up$temp <- as.factor(df_up$temp)
df_up$min <- as.factor(df_up$min)

mod_20_15 <- df_up %>% 
  filter(temp == -20, min == 15) %>% 
  nls(per ~ a + b*exp(c*day), 
      start = list(a= 1, b = 15, c = -0.1),
      data = .)

mod_20_30 <- df_up %>% 
  filter(temp == -20, min == 30) %>% 
  nls(per ~ a + b*exp(c*day), 
      start = list(a= 1, b = 15, c = -0.1),
      data = .)

mod_15_15 <- df_up %>% 
  filter(temp == -15, min == 15) %>% 
  nls(per ~ a + b*exp(c*day), 
      start = list(a= 1, b = 15, c = -0.1),
      data = .)

mod_15_30 <- df_up %>% 
  filter(temp == -15, min == 30) %>% 
  nls(per ~ a + b*exp(c*day), 
      start = list(a= 1, b = 15, c = -0.1),
      data = .)

mod_10_15 <- df_up %>% 
  filter(temp == -10, min == 15) %>% 
  nls(per ~ a + b*exp(c*day), 
      start = list(a= 1, b = 15, c = -0.1),
      data = .)

mod_10_30 <- df_up %>% 
  filter(temp == -10, min == 30) %>% 
  nls(per ~ a + b*exp(c*day), 
      start = list(a= 1, b = 15, c = -0.1),
      data = .)

mod_5_15 <- df_up %>% 
  filter(temp == 5, min == 15) %>% 
  nls(per ~ a + b*exp(c*day), 
      start = list(a= 1, b = 15, c = -0.1),
      data = .)

mod_5_30 <- df_up %>% 
  filter(!(temp == 5 & min == 30 & doy > 250)) %>% 
  filter(temp == 5, min == 30) %>% 
  nls(per ~ a + b*exp(c*day), 
      start = list(a= 1, b = 15, c = -0.1),
      data = .)


# Summary of each model 
models <- bind_rows(data.frame(temp = 5, min = 15, glance(mod_5_15)),
                    data.frame(temp = 5, min = 30, glance(mod_5_30)),
                    data.frame(temp = -10, min = 15, glance(mod_10_15)),
                    data.frame(temp = -10, min = 30, glance(mod_10_30)),
                    data.frame(temp = -15, min = 15, glance(mod_15_15)),
                    data.frame(temp = -15, min = 30, glance(mod_15_30)),
                    data.frame(temp = -20, min = 15, glance(mod_20_15)),
                    data.frame(temp = -20, min = 30, glance(mod_20_30)))

coefff <- bind_rows(coef(mod_5_15),coef(mod_5_30),
                    coef(mod_10_15),coef(mod_10_30),
                    coef(mod_15_15),coef(mod_15_30),
                    coef(mod_20_15),coef(mod_20_30))

tab_temp <- c("5","5","-10","-10","-15","-15","-20","-20")
tab_interval <- c(15,30,15,30,15,30,15,30)

coef_tab <- bind_cols(temp = tab_temp, inter = tab_interval, 
                      coefff, AIC = round(models$AIC, digits = 2))
coef_tab <- coef_tab %>% 
  dplyr::mutate_if(is.numeric, round, digits = 2)

names(coef_tab) <- c("temperature","logging interval", "a","b","c","AIC")

z <- rbind(round(tidy(mod_5_15)$std.error, dig =  2),
           round(tidy(mod_5_30)$std.error, dig =  2),
           round(tidy(mod_10_15)$std.error, dig = 2),
           round(tidy(mod_10_30)$std.error, dig = 2),
           round(tidy(mod_15_15)$std.error, dig = 2),
           round(tidy(mod_15_30)$std.error, dig = 2),
           round(tidy(mod_20_15)$std.error, dig = 2),
           round(tidy(mod_20_30)$std.error, dig = 2))
z <- data.frame(z)
names(z) <- c("st_a","st_b","st_c")

coef_tab <- coef_tab %>% 
  mutate(a = glue("{a} ± {z$st_a}")) %>% 
  mutate(b = glue("{b} ± {z$st_b}")) %>% 
  mutate(c = glue("{c} ± {z$st_c}"))
coef_tab$AIC <- as.character(coef_tab$AIC)

# Saving an excel file

library("openxlsx")
write.xlsx(coef_tab, 'data/tab_coeff.xlsx')
path <- glue('cmd /c ','"',getwd(),'/data/tab_coeff.xlsx"')
system(path)

# residuals plots separately 

plot_res_separately <- function(model){
  zz <- nlsResiduals(model)
  
  augment(model) %>% 
    mutate(stand_res = zz$resi2[,2]) %>% 
    as.data.frame() %>% 
    ggplot(aes(x = day, y = stand_res))+
    geom_point(size = 2) +
    theme_bw() + 
    ylim(-3,3) +
    labs(x = "Day of experiment", y = "SR") +
    scale_x_continuous(breaks = seq(0,110,by=20),
                       minor_breaks = seq(0,110,by=10),
                       guide = "axis_minor") +
    theme(panel.grid = element_blank()) +
    geom_hline(yintercept=0, color = "black", size=1)+
    # numbers outside in bold
    theme(axis.text =element_text(face="plain",size=24, colour="black")) +
    # variable names
    theme(axis.title =element_text(face="plain",size=24, colour="black")) +
    # border
    theme(panel.border = element_rect(size = 2, colour = NA, fill = NA)) +
    theme(axis.line = element_line(color = "black", size = 1)) +
    theme(axis.ticks.length=unit(0.2, "cm")) +
    theme(axis.ticks = element_line(size = 1))
}

res_plot_5_15 <- plot_res_separately(mod_5_15);res_plot_5_15
res_plot_5_30 <- plot_res_separately(mod_5_30)
res_plot_10_15 <- plot_res_separately(mod_10_15)
res_plot_10_30 <- plot_res_separately(mod_10_30)
res_plot_15_15 <- plot_res_separately(mod_15_15)
res_plot_15_30 <- plot_res_separately(mod_15_30)
res_plot_20_15 <- plot_res_separately(mod_20_15)
res_plot_20_30 <- plot_res_separately(mod_20_30)


# pdf --------------------------------------------------
pdf("plots/battery/5_15.pdf", width = 4.81, height = 3.42)
res_plot_5_15
dev.off()

# pdf --------------------------------------------------
pdf("plots/battery/5_30.pdf", width = 4.81, height = 3.42)
res_plot_5_30
dev.off()

# pdf --------------------------------------------------
pdf("plots/battery/10_15.pdf", width = 4.81, height = 3.42)
res_plot_10_15
dev.off()

# pdf --------------------------------------------------
pdf("plots/battery/10_30.pdf", width = 4.81, height = 3.42)
res_plot_10_30
dev.off()

# pdf --------------------------------------------------
pdf("plots/battery/15_15.pdf", width = 4.81, height = 3.42)
res_plot_15_15
dev.off()

# pdf --------------------------------------------------
pdf("plots/battery/15_30.pdf", width = 4.81, height = 3.42)
res_plot_15_30
dev.off()

# pdf --------------------------------------------------
pdf("plots/battery/20_15.pdf", width = 4.81, height = 3.42)
res_plot_20_15
dev.off()

# pdf --------------------------------------------------
pdf("plots/battery/20_30.pdf", width = 4.81, height = 3.42)
res_plot_20_30
dev.off()

# curve's plot

log.labs <- c("Logging interval: 15 min", "Logging interval: 30 min")
names(log.labs) <- c("15", "30")

temp.labs <- c("+5°C", "-10°C", "-15°C", "-20°C")
names(temp.labs) <- c("5", "-10","-15","-20")

all_curves <- df_up %>% 
  filter(!(temp == 5 & min == 30 & doy > 250)) %>% 
  mutate(temp = fct_relevel(temp, "5","-10","-15","-20")) %>% 
  ggplot(aes(x = day, y = per)) + 
  geom_point(color = "gray48") +
  facet_grid2(temp~min, labeller = labeller(min = log.labs, temp = temp.labs),
              axes = "all", remove_labels = "all") +
  geom_smooth(method = "nls", 
              formula = y ~ a + b*exp(c*x), se = F,
              method.args = list(start = c(a= 1, b = 15, c = -0.1)), size = 0.5, color = "black") +
  labs(y = "Battery status (%)", x = "Day of experiment") +
  theme(axis.line.x = element_blank())+
  theme(panel.border = element_rect(size = 1, colour = "black", fill = NA))+
  theme(axis.ticks.length=unit(0.2, "cm")) + # you want your ticks to be major 
  theme(axis.title = element_text(face="plain", size=12))+
  theme(axis.text =element_text(face="plain",size=12, colour="black"))+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_x_continuous(breaks = seq(0,110,by=20),
                     minor_breaks = seq(0,110,by=10),
                     guide = "axis_minor") +
  scale_y_continuous(breaks = seq(0,100,by=20),
                     minor_breaks = seq(0,100,by=10),
                     guide = "axis_minor") +
  theme(strip.text = element_text(size = 12, color = "black", face = "plain")) +
  theme(strip.text.y = element_text(angle = 90)) +
  theme(axis.ticks.length.x = unit(0.3, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5)) +
  theme(strip.background = element_blank())+
  theme(axis.text.x=element_text(face="plain",size=12,colour="black"));all_curves

ggsave("plots/battery/all_curves.pdf", 
       plot = all_curves, width = 20, 
       height = 28, units = "cm")












