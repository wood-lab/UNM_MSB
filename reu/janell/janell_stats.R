# Janell's data exploration, plotting, and analysis
# by Janell Goldsmith and Chelsea Wood
# janellngoldsmith@gmail.com and chelwood@uw.edu
# Started 20 July 2025


# Load up the packagaes you need

library(ggplot2)
library(tidyverse)
library(lme4)
library(car)
library(ggeffects)
library(reshape2)


# R is really useful for doing quick tallies and plots.
# Let's do some together!

hyb_ama_data<-read.csv("data/processed/Hybognathus_amarus_processed_machine_readable_2025.07.06.csv")


# Let's do some quick data tallies - we can see how many values we have for estimates of each parasite species in each
# host across the different treatment-decade combinations

tally_up <- hyb_ama_data %>%
  group_by(combo) %>%
  summarize(n = n())
tally_up


# First, we need to limit our dataset to just the diplostomatids. Let's see which parasite categories exist, and which
# we need to prune out.

levels(as.factor(hyb_ama_data$psite_spp))


# Looks like we want trem.diplo, trem.dlum, and trem.em. Let's trim the dataset so that it includes only those taxa.

diplo_data_separate <- hyb_ama_data %>%
  filter(psite_spp == "trem.diplo" | psite_spp == "trem.dlum" | psite_spp == "trem.em")

levels(as.factor(diplo_data_separate$psite_spp))


# Now we want to sum up all three categories to get a total count of all diplostomatids (trem.diplo + trem.dlum +
# trem.em). Let's do that with a pipe:

diplo_data <- diplo_data_separate %>%
  group_by(IndividualFishID) %>%
  summarize(diplo = sum(psite_count))
diplo_data


# Cool. Now we just need to merge in the bird data.

bird_data<-read.csv("reu/janell/Bird Data - Sheet2.csv")

diplo_plus_birds <- merge(diplo_data, bird_data, by.x = "YearCollected", by.y = "Year", all.x = TRUE)



### PRELIMINARY ANALYSIS - CHELSEA, 20 JULY 2025


