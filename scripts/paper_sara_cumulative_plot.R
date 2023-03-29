library(readxl)
library(tidyverse)
library(patchwork)
library(ggh4x)

# organizing
df <- read_excel("data/sap_prod.xlsx")
names(df) <- c("place","tree","site","year","doy","hour",
               "ml","max_temp","min_temp","avg_temp")

df$tree <- as.factor(df$tree)
df$site <- as.factor(df$site)
df$place <- as.factor(df$place)

# fix names ---------------------------------------------------
df <- df %>% 
  mutate(place = case_when(tree == 1 ~ "RIV",
                           tree == 2 ~ "RIV",
                           tree == 3 ~ "ANS",
                           tree == 4 ~ "ANS",
                           tree == 5 ~ "LAT",
                           tree == 6 ~ "LAT",
                           tree == 7 ~ "VAL",
                           tree == 8 ~ "VAL", 
                           TRUE ~ as.character(place)))

df <- df %>% 
  mutate(place = fct_relevel(place, "RIV", "ANS", "LAT","VAL"))


# clean first and last zeros ----------------------------------
df <- df %>% 
  filter(!(tree == 1 & doy < 80)) %>% 
  filter(!(tree == 2 & doy < 80)) %>% 
  filter(!(tree == 3 & doy < 95)) %>% 
  filter(!(tree == 4 & doy < 91)) %>% 
  filter(!(tree == 5 & doy < 82)) %>% 
  filter(!(tree == 6 & doy < 93)) %>% 
  filter(!(tree == 7 & doy < 79)) %>% 
  filter(!(tree == 8 & doy < 85)) %>% 
  # delete doy after last day
  filter(!(tree == 1 & doy > 115)) %>% 
  filter(!(tree == 2 & doy > 122)) %>% 
  filter(!(tree == 3 & doy > 129)) %>%   
  filter(!(tree == 4 & doy > 129)) %>% 
  filter(!(tree == 5 & doy > 130)) %>% 
  filter(!(tree == 6 & doy > 127)) %>% 
  filter(!(tree == 7 & doy > 128)) %>% 
  filter(!(tree == 8 & doy > 129))

filter_df_day <-  df %>% 
  group_by(place, tree, site, doy) %>% 
  summarise(ml = sum(ml),
            mean_max = mean(max_temp),
            mean_min = mean(min_temp),
            mean_avg = mean(avg_temp))

cumm_plot <- filter_df_day %>% 
  dplyr::arrange(place, tree, site, desc(ml)) %>% 
  group_by(tree) %>% 
  mutate(cum_sum = cumsum(ml),
         per_ml = (cum_sum/sum(ml)) *100,
         day = 1:length(doy),
         per_day = (day/length(doy)) * 100) %>% 
  ggplot(aes(x = per_day, per_ml, color = site)) +
  geom_step() + 
  facet_wrap2(~place, ncol = 1, strip.position = "right", 
              axes = "all", remove_labels= "x") +
  # geom_abline(intercept = 100, slope = -1) +
  theme(axis.line.x = element_blank())+
  theme(panel.border = element_rect(size = 1, colour = "black", fill = NA))+
  theme(axis.ticks.length=unit(0.2, "cm")) + 
  theme(axis.title = element_text(face="plain", size=12))+
  theme(axis.text =element_text(face="plain",size=12, colour="black"))+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill = "white", color = NA)) +
  scale_x_continuous(breaks = seq(0,100,by=20),
                     minor_breaks = seq(0,100,by=5),
                     guide = "axis_minor") +
  scale_y_continuous(breaks = seq(0,100,by=20),
                     minor_breaks = seq(0,100,by=5),
                     guide = "axis_minor",
                     limits = c(0,100)) +
  theme(strip.text = element_text(size = 12, color = "black", face = "plain")) +
  theme(strip.text.y = element_text(angle = 90)) +
  theme(strip.background = element_blank()) +
  labs(y = "Cumulative sap production (%)",
       x = "Cumulative days of sap production (%)",
       color = "Tree") + 
  guides(fill="none") +
  theme(axis.ticks.length.x = unit(0.3, "cm"),
        ggh4x.axis.ticks.length.minor = rel(0.5))+
  scale_color_manual(values=c("#595959", "#B4B4B4")) +
  theme(legend.position = c(0.4, 0.05), legend.direction="vertical") +
  theme(legend.key=element_blank()) +
  geom_abline(intercept = 100, slope = -1)+
  coord_equal();cumm_plot

ggsave("plots/sap/cumm_plot.png", plot = cumm_plot, dpi = 300, 
       width = 10, height = 25, units = "cm")








