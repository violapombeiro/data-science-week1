# Week 1: Initial Data Exploration ====
# Author: Viola
# Date: 20/01/2026

# Load packages ====
library(tidyverse)
library(here)
library(naniar)
library(janitor)
library(skimr)
# Load data ====
mosquito_egg_raw <- read_csv(here("data", "mosquito_egg_data.csv"),
                             name_repair = janitor::make_clean_names)

# Basic overview ====
glimpse(mosquito_egg_raw)
summary(mosquito_egg_raw)
skim(mosquito_egg_raw)

# React table====
# view interactive table of data
view(mosquito_egg_raw)


# Counts by site and treatment====

mosquito_egg_raw |> 
  group_by(site, treatment) |> 
  summarise(n = n())

# Observations ====
# Your observations (add as comments below):
# - What biological system is this?
#   A mosquito. 
# - What's being measured?
#   The amount of eggs laid and hatched, when under different doses of a drug
# - How many observations?
#   
# - Anything surprising?
# -   Site collection numbers stem from syntax issues.
#
# - Any obvious problems?
# -   Sites have issues with syntax - Site-A/Site A, HIGH_DOSE / Dose
# -   body_mass_mg has a lowest value of -93.0 - this is impossible
# -   Some values are N/A - check entry fields

# FIX 1: Incorrect string spelling ====

# Show the problem:
# [Code to demonstrate issue exists]
mosquito_egg_raw |> distinct(site)
mosquito_egg_raw |> distinct(treatment)
# Fix it:
mosquito_egg_data_step1 <- mosquito_egg_raw |>
  mutate(site = case_when(
    site == "Site_C" ~ "Site C",
    site == "Site-C" ~ "Site C",
    site == "site_c" ~ "Site C",
    site == "Site_A" ~ "Site A",
    site == "site_a" ~ "Site A",
    site == "Site-A" ~ "Site A",
    site == "Site_B" ~ "Site B",
    site == "site_b" ~ "Site B",
    site == "Site-B" ~ "Site B",
    .default = as.character(site)
   )
           )
mosquito_egg_data_step1 <- mosquito_egg_data_step1 |>
  mutate(treatment = case_when(
    treatment == "CONTROL" ~ "Control",
    treatment == "control" ~ "Control",
    treatment == "Low_dose" ~ "Low Dose",
    treatment == "low_dose" ~ "Low Dose",
    treatment == "LOW_DOSE" ~ "Low Dose",
    treatment == "Medium_dose" ~ "Medium Dose",
    treatment == "MEDIUM_DOSE" ~ "Medium Dose",
    treatment == "medium_dose" ~ "Medium Dose",
    treatment == "High_dose" ~ "High Dose",
    treatment == "high_dose" ~ "High Dose",
    treatment == "HIGH_DOSE" ~ "High Dose",
    .default = as.character(treatment)
  ))
mosquito_egg_data_step1 <- mosquito_egg_data_step1 |>
  mutate(collector = case_when(
    collector == "Garci" ~ "Garcia",
    collector == "Smyth" ~ "Smith",
    .default = as.character(collector)
  ))
  # Verify it worked:
  mosquito_egg_data_step1 |> distinct(site)
  mosquito_egg_data_step1 |> distinct(treatment)
  mosquito_egg_data_step1 |> distinct(collector)
  # What changed and why it matters:
  # distinct() allows visualization of each unique string for site/treatment
  # Mutate() then allows us to change each string to a string of our choice,
  # standardising the answers
  
  
  # FIX 2: [Issue description]  ====

# Show the problem:
# [Code]

# Fix it:
mosquito_egg_data_step2 <- mosquito_egg_data_step1 |>
  # YOUR CODE
  
  
  # Verify it worked:
  # [Code]
  
  # What changed and why it matters:
  # [2-3 sentences]
  #