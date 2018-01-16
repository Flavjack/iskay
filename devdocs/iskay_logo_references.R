
# Script para el logo de Iskay --------------------------------------------


library(ggplot2)
library(ggthemes)
set.seed(1234)
lo <- -rexp(100000,12)
dt <- tibble::tibble(id= 1:length(lo), lo)
#pl <- "#E69F00"
#pl <- "#56B4E9"
pl <- "#f1c40f"
#pl <- "#e74c3c"
#pl <- "#005073"
#cl <- "#34495e"
cl <- "#2980b9"
#hisogram without fill
as <- ggplot(dt, aes(lo)) +
  geom_histogram(binwidth = 0.040,colour= cl, fill=pl, size = 1.4) +
  scale_x_continuous(limits = c(-0.75, -0.15))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x = element_blank())+
  theme_tufte()        
as
ggsave('D://OMAR_2016/Github_Repos/iskay/inst/iskay_logo.tiff', units="in", width=5, height=4, dpi=300)

