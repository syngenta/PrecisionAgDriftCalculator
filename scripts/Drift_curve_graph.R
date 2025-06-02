library(tidyverse)

drift_dat <-
  tibble(
    z=seq(0.1,10,by=0.01),
    drift=perc_drift(z,get_params())
  )

minimal_curve <-
drift_dat %>%
  ggplot(aes(x=z,y=drift))+
  geom_line()+
  scale_y_continuous(trans = "log10",limits = c(0.1,100))+
  theme_bw()+
  theme(panel.background = element_rect(fill = "transparent"),
        plot.background = element_rect(fill='transparent', color=NA),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        legend.background = element_rect(fill='transparent'),
        legend.box.background = element_rect(fill='transparent'))+
  labs(x="Distance from Crop [m]",
       y="Drift [% of App. Rate]")

ggsave(minimal_curve,filename = "field_images/drift_curve.png",bg = "transparent",width = 4,height = 3)
