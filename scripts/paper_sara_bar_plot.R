library(readxl)
library(tidyverse)
library(ggplot2)
library(gtsummary)
library(patchwork)
library(lemon)
library(ggh4x)
library(latex2exp)
library(ggpubr)


load("data/data_sap.RData")

names <- c(RIV = "Tree 2                Tree 1",
           ANS = "Tree 2                Tree 1",
           LAT = "Tree 2                Tree 1",
           VAL = "Tree 2                Tree 1")

RIV <- filter_df_day %>% 
  filter(place == "RIV") %>% 
  mutate(ml = if_else(site == 2, -ml, ml)) %>% 
  ggplot(aes(x = doy, y = ml/1000, fill = site)) +
  geom_bar(stat="identity") + 
  facet_wrap2(.~place, axes = "all", remove_labels= "x", ncol = 1, 
              strip.position = "right", scales = "free_y",
              labeller = labeller(place = names)) +
  geom_hline(yintercept=0) +
  theme(axis.line.x = element_blank())+
  theme(panel.border = element_rect(size = 1, colour = "black", fill = NA))+
  theme(axis.ticks.length=unit(0.2, "cm")) + # you want your ticks to be major 
  theme(axis.title = element_text(face="plain", size=12))+
  theme(axis.text =element_text(face="plain",size=12, colour="black"))+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_x_continuous(breaks = seq(80,130,by=10),
                     minor_breaks = seq(80,130,by=2),
                     guide = "axis_minor",
                     limits = c(80,130),
                     expand = c(0.008,0.008)) +
  scale_y_continuous(breaks = seq(-3,3,by =1),
                     guide = "axis_minor",
                     minor_breaks = seq(-3,3,by=0.2),
                     labels = abs(seq(-3,3,by=1)), 
                     lim = c(-3,3), expand = c(0.02,0.02))+
  theme(strip.text = element_text(size = 12, color = "black", face = "plain")) +
  theme(strip.text.y = element_text(angle = 90)) +
  theme(strip.background = element_blank()) +
  labs(y = "",
       x = "",
       fill = "Tree") + 
  guides(fill="none") +
  theme(axis.ticks.length.x = unit(0.3, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5))+
  scale_fill_manual(values=c("black", "#787878")) +
  theme(axis.text.x = element_blank()) + 
  theme(plot.margin=unit(c(0.2,0.2,-0.5,0.2),"cm"));RIV

# ------------------------------------------------------------------------

ANS <- filter_df_day %>% 
  filter(place == "ANS") %>% 
  mutate(ml = if_else(site == 2, -ml, ml)) %>% 
  ggplot(aes(x = doy, y = ml/1000, fill = site)) +
  geom_bar(stat="identity") + 
  facet_wrap2(.~place, axes = "all", remove_labels= "x", ncol = 1, 
              strip.position = "right", scales = "free_y",
              labeller = labeller(place = names)) +
  geom_hline(yintercept=0) +
  theme(axis.line.x = element_blank())+
  theme(panel.border = element_rect(size = 1, colour = "black", fill = NA))+
  theme(axis.ticks.length=unit(0.2, "cm")) + # you want your ticks to be major 
  theme(axis.title = element_text(face="plain", size=12))+
  theme(axis.text =element_text(face="plain",size=12, colour="black"))+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_x_continuous(breaks = seq(80,130,by=10),
                     minor_breaks = seq(80,130,by=2),
                     guide = "axis_minor",
                     limits = c(80,130),
                     expand = c(0.008,0.008)) +
  scale_y_continuous(breaks = seq(-5,5,by =2),
                     guide = "axis_minor",
                     minor_breaks = seq(-5,5,by=0.5),
                     labels = abs(seq(-5,5,by=2)), lim = c(-5,5),
                     expand = c(0.02,0.02))+
  theme(strip.text = element_text(size = 12, color = "black", face = "plain")) +
  theme(strip.text.y = element_text(angle = 90)) +
  theme(strip.background = element_blank()) +
  labs(y = "",
       x = "",
       fill = "Tree") + 
  guides(fill="none") +
  theme(axis.ticks.length.x = unit(0.3, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5))+
  scale_fill_manual(values=c("black", "#787878")) +
  theme(axis.text.x = element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,-0.5,0.2),"cm"));ANS


# -------------------------------------------------------------------------

LAT <- filter_df_day %>% 
  filter(place == "LAT") %>% 
  mutate(ml = if_else(site == 2, -ml, ml)) %>% 
  ggplot(aes(x = doy, y = ml/1000, fill = site)) +
  geom_bar(stat="identity") + 
  facet_wrap2(.~place, axes = "all", remove_labels= "x", ncol = 1, 
              strip.position = "right", scales = "free_y",
              labeller = labeller(place = names)) +
  geom_hline(yintercept=0) +
  theme(axis.line.x = element_blank())+
  theme(panel.border = element_rect(size = 1, colour = "black", fill = NA))+
  theme(axis.ticks.length=unit(0.2, "cm")) + # you want your ticks to be major 
  theme(axis.title = element_text(face="plain", size=12))+
  theme(axis.text =element_text(face="plain",size=12, colour="black"))+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_x_continuous(breaks = seq(80,130,by=10),
                     minor_breaks = seq(80,130,by=2),
                     guide = "axis_minor",
                     limits = c(80,130),
                     expand = c(0.008,0.008)) +
  scale_y_continuous(breaks = seq(-3,3,by =1),
                     guide = "axis_minor",
                     minor_breaks = seq(-3,3,by=0.2),
                     labels = abs(seq(-3,3,by=1)), lim = c(-3,3),
                     expand = c(0.02,0.02))+
  theme(strip.text = element_text(size = 12, color = "black", face = "plain")) +
  theme(strip.text.y = element_text(angle = 90)) +
  theme(strip.background = element_blank()) +
  labs(y = "",
       x = "",
       fill = "Tree") + 
  guides(fill="none") +
  theme(axis.ticks.length.x = unit(0.3, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5))+
  scale_fill_manual(values=c("black", "#787878"))+
  theme(axis.text.x = element_blank())+
  theme(plot.margin=unit(c(0.2,0.2,-0.5,0.2),"cm"));LAT

# -------------------------------------------------------------------------

VAL <- filter_df_day %>% 
  filter(place == "VAL") %>% 
  mutate(ml = if_else(site == 2, -ml, ml)) %>% 
  ggplot(aes(x = doy, y = ml/1000, fill = site)) +
  geom_bar(stat="identity") + 
  facet_wrap2(.~place, axes = "all", remove_labels= "x", ncol = 1, 
              strip.position = "right", scales = "free_y",
              labeller = labeller(place = names)) +
  geom_hline(yintercept=0) +
  theme(axis.line.x = element_blank())+
  theme(panel.border = element_rect(size = 1, colour = "black", fill = NA))+
  theme(axis.ticks.length=unit(0.2, "cm")) + # you want your ticks to be major 
  theme(axis.title = element_text(face="plain", size=12))+
  theme(axis.text =element_text(face="plain",size=12, colour="black"))+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_x_continuous(breaks = seq(80,130,by=10),
                     minor_breaks = seq(80,130,by=2),
                     guide = "axis_minor",
                     limits = c(80,130),
                     expand = c(0.008,0.008)) +
  scale_y_continuous(breaks = seq(-8,8,by = 2),
                     guide = "axis_minor",
                     minor_breaks = seq(-8,8,by=0.5),
                     labels = abs(seq(-8,8,by=2)), lim = c(-8,8),
                     expand = c(0.02,0.02))+
  theme(strip.text = element_text(size = 12, color = "black", face = "plain")) +
  theme(strip.text.y = element_text(angle = 90)) +
  theme(strip.background = element_blank()) +
  labs(y = "Volume of sap production (l)",
       x = "Day of the year (DOY)",
       fill = "Tree") + 
  guides(fill="none") +
  theme(axis.ticks.length.x = unit(0.3, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5))+
  scale_fill_manual(values=c("black", "#787878"));VAL

hey <- ggarrange(RIV, ANS, LAT, VAL, ncol = 1)

ggsave("plots/sap/bar_plot.pdf", plot = hey, dpi = 300, 
       width = 8.27, height = 11.69, units = "in", device = "pdf")

ggsave("plots/sap/bar_plot.png", plot = hey, dpi = 300, 
       width = 8.27, height = 11.69, units = "in")


