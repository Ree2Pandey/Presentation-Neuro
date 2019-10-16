
# load packages -----------------------------------------------------------

# check if tidyverse is installed and if not then install it
if (!require("tidyverse")) install.packages("tidyverse")

# run every time you restart RStudio
library(readxl)     # load readxl, for reading Excel files
library(tidyverse)  # load tidyverse, for working with datasets


# read data ---------------------------------------------------------------

# read the Presentation data
d <- 
  read_excel(
    "SFN Excel.xlsx", 
    col_names = c("genotype", "visual", "cortex", "ratiovc", "posmed", "ratiopm", "area", "ratioarea", "motor", "ratiom"),
    skip = 3
  ) %>% 
  select(genotype, visual, ratiovc, motor, ratiom) %>% 
  filter(!str_detect(genotype, "3 allele")) %>% 
  mutate(genotype = as_factor(genotype)) %>% 
  print()

# take a quick look at all the variables in the dataset
glimpse(d)

distinct(d, genotype)


# summarize data ----------------------------------------------------------

d_mean <-
  d %>% 
  group_by(genotype) %>% 
  summarize(
    visual = mean(visual, na.rm = TRUE),
    ratiovc = mean(ratiovc, na.rm = TRUE),
    motor = mean(motor, na.rm = TRUE),
    ratiom = mean(ratiom, na.rm = TRUE)
  )

# Compare V1 area between the treatments --------------------------------

ggplot(d, aes(x= genotype, y=visual, color = genotype)) +
  geom_point(size = 4) +
  labs(
    x="Area (mm2) of the Primary Visual Area",
    y=NULL
  ) +
  guides(color = "none") +
  theme_gray(base_size = 16)                            # make text bigger; default was 11

ggsave("figs/Compare V1 area between the treatments.png", width = 11.5, height = 4.76, units = "in")





# Compare Visual area by Cortex ratio between the treatments -----------------

ggplot(d, aes(x= genotype, y= ratiovc, color = genotype)) +
  geom_errorbar(mapping = aes(ymin = ratiovc, ymax = ratiovc), 
                data = d_mean, color = "black", width = .5, size = 2) +
  geom_point(size = 4) +
  labs(
    title="Proportion of the Neocortex occupied by the Primary Visual Area",
   y = "Proportion",
    x=NULL
  ) +
  guides(color = "none") +
  theme_gray(base_size = 16)                            # make text bigger; default was 11

ggsave("figs/Compare Visual Area by Cortex ratio between the treatments.png", width = 11.5, height = 4.76, units = "in")




# Compare Motor Area between the treatments-------------------------------------------------------------

ggplot(d, aes(x= genotype, y= motor, color = genotype)) +
  geom_point(size = 4) +
  labs(
    x="Area (mm2) of the Primary Motor Area",
    y=NULL
  ) +
  guides(color = "none") +
  theme_gray(base_size = 16)                            # make text bigger; default was 11

ggsave("figs/Compare Motor Area between the treatments.png", width = 11.5, height = 4.76, units = "in")





# Compare Motor area by Cortex ratio between the treatments -------------------------------------------------------------

ggplot(d, aes(x= genotype, y= ratiom, color = genotype)) +
  geom_point(size = 4) +
  labs(
    x="Proportion of the Neocortex occupied by the Primary Motor Area",
    y=NULL
  ) +
  guides(color = "none") +
  theme_gray(base_size = 16)                            # make text bigger; default was 11

ggsave("figs/Compare Motor Area by Cortex ratio between the treatmentss.png", width = 11.5, height = 4.76, units = "in")











