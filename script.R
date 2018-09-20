#install.packages("tidyverse")
#install.packages("plotly")

require(plotly)
require(tidyverse)
require(RColorBrewer)

dta <- read_csv("data/FRS HBAI - tables v1.csv")

dta %>% 
  mutate(prop = N_ten4s / N_all2) %>% 
  filter(regname == "UK") %>% 
  ggplot(aes(x = yearcode, y = age2, fill = prop)) + 
  geom_tile() +
  facet_wrap(~tenurename) + 
  scale_fill_distiller(palette = "Paired") + 
  coord_fixed() #%>%  
#  ggplotly()


read_csv("data/FRS HBAI - tables v1.csv") %>% 
  select(
    region = regname, year = yearcode, age = age2, tenure = tenurename, n = N_ten4s, N = N_all2
  ) -> dta 


dta %>% 
  plot_ly(x = ~year, y = ~age, fill = ~I(n/N)) %>% 
  


  
## read in data set (tolerance data from the ALDA book)
library(plotly)
tolerance <- read.table("http://www.ats.ucla.edu/stat/r/examples/alda/data/tolerance1_pp.txt",
                          sep = ",", header = TRUE)

## change id and male to factor variables
tolerance <- within(tolerance, {
  id <- factor(id)
  male <- factor(male, levels = 0:1, labels = c("female", "male"))
})


p <- ggplot(data = tolerance, aes(x = time, y = tolerance)) + geom_point() +
  stat_smooth(method = "lm", se = FALSE) + facet_wrap(~id)

p <- ggplotly(p)


library(plotly)

p <- ggplot(mpg, aes(displ, hwy))+
  geom_point()+
  stat_smooth()+
  facet_wrap(~year) +
  coord_fixed()

p <- ggplotly(p)
print(p)


########################

read_csv("data/FRS HBAI - tables v1.csv") %>% 
  select(
    region = regname, year = yearcode, age = age2, tenure = tenurename, n = N_ten4s, N = N_all2
  ) %>% 
  mutate(proportion = n / N) -> dta 

dta %>% 
  ggplot(aes(x = year, y = age, fill = proportion)) + 
  geom_tile() + 
  facet_grid(region ~ tenure) -> p

p %>% ggplotly() -> p2

pal <- colorRampPalette(brewer.pal(11, name = "Paired"))(100)
p2
plotly_json(p2)
p2 %>% 
  style(
      colorscale = "Rainbow",
      showscale = F,
      zauto = F,
      zmin = 0, zmax = 1,
      traces = 1:28,
      hoverinfo = "x+y+z"
) -> p3


plotly_json(p3)

p3
p3 %>% 
  style(
    traces = 29,
    marker = list(
      colorscale = "Rainbow",
      colorbar = list(title = "Proportion"), 
      color = list(0, 1)
    )
  ) -> p4

p4
plotly_json(p4)


