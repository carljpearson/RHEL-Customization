library(tidyverse)
library(googlesheets)
library(lmerTest)


df_long %>%
  ggplot(aes(x=alert_name,y=conf ,fill=alert_name)) +
  #geom_violin(alpha=.5,color="white")+
  stat_summary(fun=mean,size=1) +
  ggthemes::theme_tufte(base_family = "sans",base_size = 15) +
  theme(
    legend.position = "none",
    axis.title.y = element_blank()
  ) + 
  scale_y_continuous(breaks = c(1,4,7)) +
  coord_flip(ylim=c(1,7)) +
  labs(y="Confidence",
       subtitle = "Point = mean, line = SD, color = response distributions")

ggsave("/Users/carlpearson/Documents/r_github/RHEL-Customization/plots/conf_viol2_alt.png",width = 12,height = 8)