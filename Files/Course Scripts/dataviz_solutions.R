# Packages and script set-up ----

rm(list = ls())
library(tidyverse) # We will now be using ggplot2 which is part of the tidyverse

# Base R Plotting ----
#' Base R has several functions for data visualization.
#' Crucially, you need separate functions for each type of plot.
#' Some examples:

hist(x = mpg$displ)


boxplot(displ ~ year,
        data = mpg)

plot(x = mpg$displ,
     y = mpg$hwy)

## Disadvantages of Base R ----

# The grammar of graphics in ggplot2 ----

# Replicating the graph ----

## Data and Relationship ----
ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy,
                     color = class))

## Geometric objects ----
ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy,
                     color = class)) +
  geom_point()

### Practice with different geoms ----

#' Now, it is up to you: Reproduce the following plots!
#'
#' Follow the instructions about the mapping and the type of
#' geom below each plot.
#'
#### Boxplot

ggplot(data = mpg,
       mapping = aes(x = class,
                     y = hwy)) +
  geom_boxplot()

#' * Change the Axis as displayed in the plot. Use no color variable.
#' * Use the `geom_boxplot()` function

#### Violin Plot

ggplot(data = mpg,
       mapping = aes(x = class,
                     y = hwy)) +
  geom_violin()

#' * Change the Axis as displayed in the plot. Use no color variable.
#' * Use the `geom_violin()` function

#### Density Plot

ggplot(data = mpg,
       mapping = aes(x = hwy,)) +
  geom_density()

#' * Change the Axis as displayed in the plot (keep only x axis). Use no color variable.
#' * Use the `geom_density()` function

#### Histogram

ggplot(data = mpg,
       mapping = aes(x = hwy,)) +
  geom_histogram()

#' * Can you imagine how to plot the histogram?

#### Bar Plot

ggplot(data = mpg,
       mapping = aes(x = class)) +
  geom_bar()

#' * Change the Axis as displayed in the plot (keep only x axis). Use no color variable.
#' * Use the `geom_bar()` function

#### Number of observations per location

ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy)) +
  geom_count()

#' * Change the Axis as displayed in the plot (keep only x axis). Use no color variable.
#' * Use the `geom_count()` function
#' * Can you tell the difference to `geom_point()`?



### Smoothed regression line ----

ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy,
                     color = class)) +
  geom_point() +
  geom_smooth(color = "blue",
              method = "lm")


## Additional Stuff ----

### Labels ----

ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy,
                     color = class)) +
  geom_point() +
  geom_smooth(col = "blue",
              method = "lm") +
  labs(
    x = "Engine Displacement in Liters",
    y = "Highway miles per gallon",
    title = "MPG data",
    subtitle = "Cars with higher engine displacement tend to have lower highway mpg",
    caption = "Source: mpg data in ggplot2 \n Credits for the tutorial: TheRBootcamp",
    color = "Car Classes"
  )

### Themes ----

#### Serious themes ----

ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy,
                     color = class)) +
  geom_point() +
  geom_smooth(col = "blue",
              method = "lm") +
  labs(
    x = "Engine Displacement in Liters",
    y = "Highway miles per gallon",
    title = "MPG data",
    subtitle = "Cars with higher engine displacement tend to have lower highway mpg",
    caption = "Source: mpg data in ggplot2 \n Credits for the tutorial: TheRBootcamp",
    color = "Car Classes"
  ) +
  theme_classic()

#### Themes from ggthemes ----

library(ggthemes)

ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy,
                     color = class)) +
  geom_point() +
  geom_smooth(col = "blue",
              method = "lm") +
  labs(
    x = "Engine Displacement in Liters",
    y = "Highway miles per gallon",
    title = "MPG data",
    subtitle = "Cars with higher engine displacement tend to have lower highway mpg",
    caption = "Source: mpg data in ggplot2 \n Credits for the tutorial: TheRBootcamp",
    color = "Car Classes"
  ) +
  theme_fivethirtyeight()


## Finish ----

ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy,
                     color = class)) +
  geom_point() +
  geom_smooth(col = "blue",
              method = "lm") +
  labs(
    x = "Engine Displacement in Liters",
    y = "Highway miles per gallon",
    title = "MPG data",
    subtitle = "Cars with higher engine displacement tend to have lower highway mpg",
    caption = "Source: mpg data in ggplot2 \n Credits for the tutorial: TheRBootcamp",
    color = "Car Classes"
  ) +
  theme_bw()

# Minor Addenda ----

## Saving a ggplot object in the environment ----

#' Storing our final plot from above as final_plot

final_plot <- ggplot(data = mpg,
                     mapping = aes(x = displ,
                                   y = hwy,
                                   color = class)) +
  geom_point() +
  geom_smooth(col = "blue",
              method = "lm") +
  labs(
    x = "Engine Displacement in Liters",
    y = "Highway miles per gallon",
    title = "MPG data",
    subtitle = "Cars with higher engine displacement tend to have lower highway mpg",
    caption = "Source: mpg data in ggplot2 \n Credits for the tutorial: TheRBootcamp",
    color = "Car Classes"
  ) +
  theme_bw()


#' We can then add stuff to the ggplot object in the usual way.
#' Suppose, we want to add a fat vertical line (x=3) again:

final_plot +
  geom_vline(xintercept = 3, size = 5)

## Integrating ggplot with other tidyverse functions ----
mpg %>%
  filter(hwy >= 25) %>%
  ggplot(# do not call data=mpg again here!
    mapping = aes(x = displ,
                  y = hwy,
                  color = class)) +
  geom_point() +
  geom_smooth(col = "blue",
              method = "lm") +
  labs(
    x = "Engine Displacement in Liters",
    y = "Highway miles per gallon",
    title = "MPG data",
    subtitle = "Cars with higher engine displacement tend to have lower highway mpg",
    caption = "Source: mpg data in ggplot2 \n Credits for the tutorial: TheRBootcamp",
    color = "Car Classes"
  ) +
  theme_bw()



## ggsave ----


# Addenda II ----

## Likert scale ----

mpg2 <- mpg %>%
  group_by(class) %>%
  mutate(length.class = n()) %>%
  group_by(trans, add = TRUE) %>%
  mutate(
    length.trans = n(),
    percentage.trans = 100 * length.trans / length.class
  ) %>%
  distinct(trans, class, .keep_all = TRUE)

ggplot(data = mpg2,
       mapping = aes(x = class,
                     y = percentage.trans,
                     fill = trans)) +
  geom_bar(stat = "identity", width = 0.7) +
  coord_flip()


## Playing with themes ----
ggplot(data = mpg,
       mapping = aes(x = displ,
                     y = hwy,
                     color = class)) +
  geom_point() +
  geom_smooth(col = "blue",
              method = "lm") +
  labs(
    x = "Engine Displacement in Liters",
    y = "Highway miles per gallon",
    title = "MPG data",
    subtitle = "Cars with higher engine displacement tend to have lower highway mpg",
    caption = "Source: mpg data in ggplot2 \n Credits for the tutorial: TheRBootcamp",
    color = "Car Classes"
  ) +
  theme(
    panel.background = element_rect(fill = "pink"),
    panel.grid.major.y = element_line(colour = "green"),
    panel.grid.minor.y = element_line(colour = "green"),
    legend.position = "bottom",
    panel.border = element_rect(color = "orange", fill = NA, size =
                                  5)
  ) +
  scale_x_continuous(breaks = seq(1, 8, by = 0.5)) +
  geom_vline(xintercept = 3, size = 5)
