# Datamanagement: Eurostat Example


# Setup----
rm(list = ls())
library(tidyverse)


# Read and explore the data ----

#' Read the data (eurostat_data.csv) and 
#' store them as eurost.



#' We only want to use the year (denoted by the variable 
#' time in the dataset) 2014 (time==2014). 
#' Please, filter accordingly.

eurost <- ______



# Reproduce Plots ----


#' Reproduce these three plots. 
#' Below the graphs you can find some information.



## geom_point----



#' * x = unemp_youth_t
#' * y = gdp_gr
#' * color = emigration_t / immigration_t
#' * size = inv_per_empl
#' * Use theme_classic()
#' * You might realize that the label symbols of size are a bit 
#'    weird as soon as you add the regression line. This is because 
#'    the function to create the regression line interacts 
#'    with the size argument within the ggplot() function. 
#'    Therefore, as soon as you add the regression function, 
#'    move the size argument to geom_point. 
#'    But don't forget that the size argument has to be put within aes()!
#'   + Here is an important learning: aes() can be passed to either ggplot()
#'      or to a specific layer (e.g. geom_). Aesthetics specified to ggplot()
#'      are used as defaults for every layer, while aes() passed to a 
#'      specific layer a used as default for that layer only. 
#' * Furthermore, add this line 
#'    + scale_color_manual(values = c("red"), labels = c(" "))`. 
#'      Do you understand what it does?
#' 



## geom_col ----


#' * use theme_economist()
#' * x = geo_code
#' * y = unemp_youth_t
#' * fill = unemp_youth_t
#' * Play a bit with the widht of the bars.
#' * Crucially, we add points which relate to unemp_workagepop_t. 
#'    You need to write a new aes() in the point function and 
#'    assign the color "red". Outside of the aes() argument, a
#'    djust the size of the points to 3. 
#' 
#' * Rename the axes accordingly. Note that although 
#'   the y axis is actually just related to youth unemployment, 
#'   we simply rename it as Unemployment because the total 
#'   unemployment (e.g. the points) are within the same scale. 



## geom_hist ----


#' Here, we first do some some data management. 

eurost2 <- read_csv2("data/eurostat_data.csv") %>%
  filter(geo_code %in% c("DE", "IT", "EL", "ES", "UK"),
         time>=1990,
         time<=2015) %>%
  mutate(unemp_tod = if_else(time==2015, unemp_workagepop_t, NA_real_),
         unemp_youth_tod = if_else(time==2015, unemp_youth_t, NA_real_))

#' * As aesthetic in the principle `ggplot()` function, 
#' just use x = geo_code
#' * What we are doing then is using two seperate `geom_violin` 
#'   and two seperate `geom_point` functions
#'   + Both violin functions use the argument `alpha = 0.5` 
#'     to inrease the transparency of the violin plots
#'   + One violin function has`y=unemp_youth_t` and 
#'     the other`y = unemp_workagepop_t`. Use the appropriate colors
#'   + One point function has `y = unemp_tod` and the other 
#'     `y = unemp_youth_tod`. Use `color = "black", size = 3` 
#'      outside of the aesthetic. 
#' * Use `theme_minimal()`
#' * Add this line: 
#'   `scale_fill_manual(values = c("red", "blue"), labels = c("Total Unemployment", "Youth Unemployment"))`



# New Stuff ----

#' All the exercises will use the first plot. 
#' To make our lifes easier, we safe this plot as main_plot

main_plot <- ggplot(
  data = eurost,
  mapping = aes(
    x = unemp_youth_t,
    y = gdp_gr,
    color = emigration_t / immigration_t)
) +
  geom_point(
    aes(size = inv_per_empl)
  ) +
  labs(x = "Share of Unemployed Youth (15-24) in Pct.",
       y = "Real GDP growth rate (YOY)",
       title = "GDP growth and youth unemployment in 2014",
       subtitle = "Correlation between lower growth rate and higher youth unemployment",
       caption = "Source: Eurostat",
       size = "Investment p. person\n employed (in Mill. €)",
       color= "Ratio of Emigration \n to Immigration")+
  geom_smooth(method="lm", se=FALSE, color="black")+
  theme_classic()


main_plot



## Adjusting colors ----

#' * Remember, we just saved the first plot as main_plot. 
#'   So you do not need to rewrite everything from this plot but 
#'   only ....
#' * In this case, use the function `scale_color_gradient2()` 
#'   to get the colors. 
#'   + Use `?scale_color_gradient2()` to understand the 
#'     arguments you need to use to replicate the plot



## Adding labels to points ----



#' * Again, use main_plot as the base
#' * The function from ggrepel we want to use is `geom_text_repel`.
#'   Use geo_code as the label within the `aes()` argument of `geom_text_repel`.



## Facets and more ----


#' * You can add a facets (e.g. the same plotting relationship 
#'  in many windows representing different variables such as 
#'  different years) with the function 
#'  `+facet_wrap(~FACETS_VARIABLE_NAME)`. Make facets using 
#'  our main graph, using löcation as a facets variable. 
#' * Use location as the facet variable.



# Interactive Graph ----
 
#' With the plotly package, we can actually build interactive graphs.
#' The easiest way is to simply use the `ggplotly()` function and 
#' parse a ggplot object to the `p` argument:

library(plotly)

ggplotly(p = main_plot)

