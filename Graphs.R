
# load packages -----------------------------------------------------------

# check if tidyverse is installed and if not then install it
if (!require("tidyverse")) install.packages("tidyverse")

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
    "Double-Emx-KO_DATASET.xlsx", 
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


# Compare V1 area between the treatments --------------------------------

ggplot(d, aes(x=visual, y=genotype, color = genotype)) +
  geom_point(size = 4) +
  labs(
    x="Area (mm2) of the Primary Visual Area",
    y=NULL
  ) +
  scale_y_discrete(limits = rev(levels(d$genotype))) +  # put genotype in reverse order
  guides(color = "none") +
  theme_gray(base_size = 16)                            # make text bigger; default was 11

ggsave("figs/compare Visual Area between the treatments.png", width = 11.5, height = 4.76, units = "in")





# Compare Visual area by Cortex ratio between the treatments -----------------

ggplot(d, aes(x=ratiovc, y=genotype, color = genotype)) +
  geom_point(size = 4) +
  labs(
    x="Proportion of the Neocortex occupied by the Primary Visual Area",
    y=NULL
  ) +
  scale_y_discrete(limits = rev(levels(d$genotype))) +  # put genotype in reverse order
  guides(color = "none") +
  theme_gray(base_size = 16)                            # make text bigger; default was 11

ggsave("figs/Compare Visual Area by Cortex ratio between the treatments.png", width = 11.5, height = 4.76, units = "in")




# Compare Motor Area between the treatments-------------------------------------------------------------

ggplot(d, aes(x=motor, y=genotype, color = genotype)) +
  geom_point(size = 4) +
  labs(
    x="Area (mm2) of the Primary Motor Area",
    y=NULL
  ) +
  scale_y_discrete(limits = rev(levels(d$genotype))) +  # put genotype in reverse order
  guides(color = "none") +
  theme_gray(base_size = 16)                            # make text bigger; default was 11

ggsave("figs/Compare Motor Area between the treatments.png", width = 11.5, height = 4.76, units = "in")





# Compare Motor area by Cortex ratio between the treatments -------------------------------------------------------------

ggplot(d, aes(x=ratiom, y=genotype, color = genotype)) +
  geom_point(size = 4) +
  labs(
    x="Proportion of the Neocortex occupied by the Primary Motor Area",
    y=NULL
  ) +
  scale_y_discrete(limits = rev(levels(d$genotype))) +  # put genotype in reverse order
  guides(color = "none") +
  theme_gray(base_size = 16)                            # make text bigger; default was 11

ggsave("figs/Compare Motor Area by Cortex ratio between the treatmentss.png", width = 11.5, height = 4.76, units = "in")











