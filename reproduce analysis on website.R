
library(tidyverse)
library(haven)

# This script reproduces the statistics published at trygghetsundersokelsen.no

# data import av runde 1 NSD-filer

df <- read_sav("Norwegian Crime Survey NSD v1.sav")

# Please confer the following site for how to access 
# data from the National Crime Survey:
# https://trygghetsundersokelsen.no/data.html#en

# For data documentation: https://data.trygghetsundersokelsen.no

# Content: 
#       Frequencies for the whole population is calculated for all variables.
#       But all variables is not plotted against all demographic variables. 
#       For each variable, the script applies three demographic variables. 
#       But all demographic variables are used at some point. 
#       The demographic variables are as follows: 
#         - Age
#         - Gender
#         - Education
#         - Immigration
#         - Married
#         - Children
#         - County

# Weighting: 
#       All frequencies are weighted using {{weight_edu}}.
#       The weight is based on demographic variables 
#       (age, gender and geography) and education level.


# DISCLAIMER:
#       OsloMet, ideas2evidence, Frischsenteret and the Ministry of Justice 
#       and Public Security do not take any responsibility for the use and 
#       interpretation of data from the National Crime Survey. The above 
#       mentioned further take no responsibility for any negative consequences
#       that may arise as a result of the use of this data.

# ---------------------------------------------------- #
# General functions for recoding demographic variables #
# ---------------------------------------------------- #

# Recode {{immigration}}
recode_immigration <- 
  function(x){
    factor(case_when(
      x == 1 ~ 1,
      x == 2 ~ 2,
      x %in% c(3, 4, 5) ~ 3,
      TRUE ~  NA_real_
    ),
    labels = c("No, have not immigrated to Norway",
               "Have immigrated to Norway",
               "Born in Norway, but both or one of the parents immigrated")
    )}

# Recode {{gift}}
recode_married <-
  function(x) {
    factor(
      case_when(
        x %in% c(1, 2) ~ 1,
        x == 3 ~ 2,
        TRUE ~ NA_real_
      ),
      labels = c("Married or cohabitant", "Living alone")
    )
  }
    

# ------------------------------------------------------------------------------------------------ #
# urtrygg1: Percentage that feels unsafe if they go out alone at night in the area where they live #
# ------------------------------------------------------------------------------------------------ #

# all
df |> 
  count(urtrygg1, wt = weight_edu) |> 
  filter(urtrygg1 < 5) |> 
  mutate(
    pct = n/sum(n)
  )

# education
df |> 
  group_by(utdanning_kort) |> 
  count(urtrygg1, wt = weight_edu) |> 
  filter(urtrygg1 < 5 & utdanning_kort <97) |> 
  mutate(
    pct = n/sum(n)
  )

# age
df |> 
  group_by(aldersgruppe_vuttrekk_dsf) |> 
  count(urtrygg1, wt = weight_edu) |> 
  filter(urtrygg1 < 5) |> 
  mutate(
    pct = n/sum(n)
  )

# map/county
df |> 
  group_by(fylkenr_dsf) |> 
  count(urtrygg1, wt = weight_edu) |> 
  filter(urtrygg1 < 5) |> 
  mutate(
    pct = n/sum(n)
  )

# ------------------------------------------------------------------- #
# urtrygg2: Percentage that always plan ahead to avoid crime exposure #
# ------------------------------------------------------------------- #

# all
df |> 
  count(urtrygg2, wt = weight_edu) |> 
  filter(urtrygg2 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# Children
df |> 
  group_by(barn) |> 
  count(urtrygg2, wt = weight_edu) |> 
  filter(urtrygg2 < 5 & barn < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# gender
df |> 
  group_by(kjonn_dsf) |> 
  count(urtrygg2, wt = weight_edu) |> 
  filter(urtrygg2 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# map/county
df |> 
  group_by(fylkenr_dsf) |> 
  count(urtrygg2, wt = weight_edu) |> 
  filter(urtrygg2 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# ----------------------------------------------------------------- #
# urtrygg3: Percentage that is worried about being exposed to crime #
# ----------------------------------------------------------------- #

# all
df |> 
  count(urtrygg3, wt = weight_edu) |> 
  filter(urtrygg3 < 5) |> 
  mutate(
    pct = n/sum(n)
  )

# immigration
df |> 
 mutate(
   innvandret = recode_immigration(innvandret)
 ) |> 
  group_by(innvandret) |> 
  count(urtrygg3, wt = weight_edu) |> 
  filter(urtrygg3 < 5 & !is.na(innvandret)) |> 
  mutate(
    pct = n/sum(n)
  ) 

# married
df |> 
  mutate(
    gift = recode_married(gift)
  ) |> 
  group_by(gift) |> 
  count(urtrygg3, wt = weight_edu) |> 
  filter(urtrygg3 < 5 & !is.na(gift)) |> 
  mutate(
    pct = n/sum(n)
  )

# ------------------------------------------------------------ #
# urkrim_1: Percentage that worries about exposure to burglary #
# ------------------------------------------------------------ #

# all 
df |> 
  count(urkrim_1, wt = weight_edu) |> 
  filter(urkrim_1 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# children
df |> 
  group_by(barn) |> 
  count(urkrim_1, wt = weight_edu) |> 
  filter(urkrim_1 < 6 & barn < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# county
df |> 
  group_by(fylkenr_dsf) |> 
  count(urkrim_1, wt = weight_edu) |> 
  filter(urkrim_1 < 6) |> 
  mutate(
    pct = n/sum(n)
  )


# --------------------------------------------------------- #
# urkrim_2: Percentage that worries about exposure to theft #
# --------------------------------------------------------- #

# all
df |> 
  count(urkrim_2, wt = weight_edu) |> 
  filter(urkrim_2 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# education
df |> 
  group_by(utdanning_kort) |> 
  count(urkrim_2, wt = weight_edu) |> 
  filter(urkrim_2 < 6 & utdanning_kort < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# gender
df |> 
  group_by(kjonn_dsf) |> 
  count(urkrim_2, wt = weight_edu) |> 
  filter(urkrim_2 < 6) |> 
  mutate(
    pct = n/sum(n)
  )


# ------------------------------------------------------------------------------------------------- #
# urkrim_5: Percentage that worries about exposure for robbery with violence or threats of violence #
# ------------------------------------------------------------------------------------------------- #

# all
df |> 
  count(urkrim_5, wt = weight_edu) |> 
  filter(urkrim_5 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# immigration
df |> 
  mutate(
    innvandret = recode_immigration(innvandret)
  ) |> 
  group_by(innvandret) |> 
  count(urkrim_5, wt = weight_edu) |> 
  filter(urkrim_5 < 6 & !is.na(innvandret)) |> 
  mutate(
    pct = n/sum(n)
  )

# age
df |> 
  group_by(aldersgruppe_vuttrekk_dsf) |> 
  count(urkrim_5, wt = weight_edu) |> 
  filter(urkrim_5 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# --------------------------------------------------------------------- #
# urkrim_7: Percentage that worries about exposure to physical violence #
# --------------------------------------------------------------------- #

# all
df |> 
  count(urkrim_7, wt = weight_edu) |> 
  filter(urkrim_7 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# married
df |> 
  mutate(
    gift = recode_married(gift)
  ) |> 
  group_by(gift) |> 
  count(urkrim_7, wt = weight_edu) |> 
  filter(urkrim_7 < 6 & !is.na(gift)) |> 
  mutate(
    pct = n/sum(n)
  )

# children
df |> 
  group_by(barn) |> 
  count(urkrim_7, wt = weight_edu) |> 
  filter(urkrim_7 < 6 & barn < 97) |> 
  mutate(
    pct = n/sum(n)
  )


# -------------------------------------------------------------- #
# urkrim_8: Percentage that worries about exposure to hate crime #
# -------------------------------------------------------------- #

# all
df |> 
  count(urkrim_8, wt = weight_edu) |> 
  filter(urkrim_8 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# age
df |> 
  group_by(aldersgruppe_vuttrekk_dsf) |> 
  count(urkrim_8, wt = weight_edu) |> 
  filter(urkrim_8 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# children
df |> 
  group_by(barn) |> 
  count(urkrim_8, wt = weight_edu) |> 
  filter(urkrim_8 < 6 & barn < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# ------------------------------------------------------------------------- #
# urkrim_9: Percentage that worries about exposure to sexual abuse or abuse #
# ------------------------------------------------------------------------- #

# all
df |> 
  count(urkrim_9, wt = weight_edu) |> 
  filter(urkrim_9 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# immigration
df |> 
  mutate(
    innvandret = recode_immigration(innvandret)
  ) |> 
  group_by(innvandret) |> 
  count(urkrim_9, wt = weight_edu) |> 
  filter(urkrim_9 < 6 & !is.na(innvandret)) |> 
  mutate(
    pct = n/sum(n)
  )

# education
df |> 
  group_by(utdanning_kort) |> 
  count(urkrim_9, wt = weight_edu) |> 
  filter(urkrim_9 < 6 & utdanning_kort < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# ----------------------------------------------------------------------------------------------------------- #
# urkrim_10: Percentage that worries about dissemination of photos, videos and information against their will #
# ----------------------------------------------------------------------------------------------------------- #

# all
df |> 
  count(urkrim_10, wt = weight_edu) |> 
  filter(urkrim_10 < 6) |> 
  mutate(
    pct = n/sum(n)
  )

# married
df |> 
  mutate(
    gift = recode_married(gift)
  ) |> 
  group_by(gift) |> 
  count(urkrim_10, wt = weight_edu) |> 
  filter(urkrim_10 < 6 & !is.na(gift)) |> 
  mutate(
    pct = n/sum(n)
  )

# immigration
df |> 
  mutate(
    innvandret = recode_immigration(innvandret)
  ) |> 
  group_by(innvandret) |> 
  count(urkrim_10, wt = weight_edu) |> 
  filter(urkrim_10 < 6 & !is.na(innvandret)) |> 
  mutate(
    pct = n/sum(n)
  )


# ----------------------------------------------------------------------------------------------- #
# utsibilde1: Percentage that is exposed to sharing/spread of images or videos against their will #
# ----------------------------------------------------------------------------------------------- #

# all
df |> 
  count(utsibilde1, wt = weight_edu) |> 
  filter(utsibilde1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# education
df |> 
  group_by(utdanning_kort) |> 
  count(utsibilde1, wt = weight_edu) |> 
  filter(utsibilde1 < 97 & utdanning_kort < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# children
df |> 
  group_by(barn) |> 
  count(utsibilde1, wt = weight_edu) |> 
  filter(utsibilde1 < 97 & barn < 97) |> 
  mutate(
    pct = n/sum(n)
  )


# ---------------------------------------------------------------------------------------- #
# utsiident1: Percentage that is exposed to misuse of personal information on the internet #
# ---------------------------------------------------------------------------------------- #

# all
df |> 
  count(utsiident1, wt = weight_edu) |> 
  filter(utsiident1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# gender
df |> 
  group_by(kjonn_dsf) |> 
  count(utsiident1, wt = weight_edu) |> 
  filter(utsiident1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# age
df |> 
  group_by(aldersgruppe_vuttrekk_dsf) |> 
  count(utsiident1, wt = weight_edu) |> 
  filter(utsiident1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# ----------------------------------------------------------- #
# uttbol1: Percentage that is exposed to residential burglary #
# ----------------------------------------------------------- #

# all
df |> 
  count(uttbol1, wt = weight_edu) |> 
  filter(uttbol1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# gender
df |> 
  group_by(kjonn_dsf) |> 
  count(uttbol1, wt = weight_edu) |> 
  filter(uttbol1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# education
df |> 
  group_by(utdanning_kort) |> 
  count(uttbol1, wt = weight_edu) |> 
  filter(uttbol1 < 97 & utdanning_kort < 97) |> 
  mutate(
    pct = n/sum(n)
  )


# ------------------------------------------------------------------ #
# uttlom1: Percentage that is exposed to theft of money or valuables #
# ------------------------------------------------------------------ #

# all
df |> 
  count(uttlom1, wt = weight_edu) |> 
  filter(uttlom1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# children
df |> 
  group_by(barn) |> 
  count(uttlom1, wt = weight_edu) |> 
  filter(uttlom1 < 97 & barn < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# gender
df |> 
  group_by(kjonn_dsf) |> 
  count(uttlom1, wt = weight_edu) |> 
  filter(uttlom1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# ---------------------------------------------------------------------------------------- #
# uttran1: Percentage that is exposed to robbery with threats of violence or with violence #
# ---------------------------------------------------------------------------------------- #

# all
df |> 
  count(uttran1, wt = weight_edu) |> 
  filter(uttran1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# education
df |> 
  group_by(utdanning_kort) |> 
  count(uttran1, wt = weight_edu) |> 
  filter(uttran1 < 97 & utdanning_kort < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# immigration
df |> 
  mutate(
    innvandret = recode_immigration(innvandret)
  ) |> 
  group_by(innvandret) |> 
  count(uttran1, wt = weight_edu) |> 
  filter(uttran1 < 97 & !is.na(innvandret)) |> 
  mutate(
    pct = n/sum(n)
  )

# ----------------------------------------------- #
# uttruss1: Percentage that is exposed to threats #
# ----------------------------------------------- #

# all
df |> 
  count(uttruss1, wt = weight_edu) |> 
  filter(uttruss1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# married
df |> 
  mutate(
    gift = recode_married(gift)
  ) |> 
  group_by(gift) |> 
  count(uttruss1, wt = weight_edu) |> 
  filter(uttruss1 < 97 & !is.na(gift)) |> 
  mutate(
    pct = n/sum(n)
  )

# gender
df |> 
  group_by(kjonn_dsf) |> 
  count(uttruss1, wt = weight_edu) |> 
  filter(uttruss1 < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# ------------------------------------------------- #
# uttsyk1: Percentage that is exposed to bike theft #
# ------------------------------------------------- #

# all
df |> 
  count(uttsyk1, wt = weight_edu) |> 
  filter(uttsyk1 < 3) |> 
  mutate(
    pct = n/sum(n)
  )


# age
df |> 
  group_by(aldersgruppe_vuttrekk_dsf) |> 
  count(uttsyk1, wt = weight_edu) |> 
  filter(uttsyk1 < 3) |> 
  mutate(
    pct = n/sum(n)
  )


# education
df |> 
  group_by(utdanning_kort) |> 
  count(uttsyk1, wt = weight_edu) |> 
  filter(uttsyk1 < 3 & utdanning_kort < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# --------------------------------------------------------------- #
# utvrist: Percentage that is exposed to violent shakes or pushes #
# --------------------------------------------------------------- #

# all
df |> 
  count(utvrist, wt = weight_edu) |> 
  filter(utvrist < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# married
df |> 
  mutate(
    gift = recode_married(gift)
  ) |> 
  group_by(gift) |> 
  count(utvrist, wt = weight_edu) |> 
  filter(utvrist < 97 & !is.na(gift)) |> 
  mutate(
    pct = n/sum(n)
  )

# immigration
df |> 
  mutate(
    innvandret = recode_immigration(innvandret)
  ) |> 
  group_by(innvandret) |> 
  count(utvrist, wt = weight_edu) |> 
  filter(utvrist < 97 & !is.na(innvandret)) |> 
  mutate(
    pct = n/sum(n)
  )

# --------------------------------------------------------------------- #
# utvslag: Percentage that is exposed to hits with fist or hard objects #
# --------------------------------------------------------------------- #

# all
df |> 
  count(utvslag, wt = weight_edu) |> 
  filter(utvslag < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# age
df |> 
  group_by(aldersgruppe_vuttrekk_dsf) |> 
  count(utvslag, wt = weight_edu) |> 
  filter(utvslag < 97) |> 
  mutate(
    pct = n/sum(n)
  )

# gender
df |> 
  group_by(kjonn_dsf) |> 
  count(utvslag, wt = weight_edu) |> 
  filter(utvslag < 97) |> 
  mutate(
    pct = n/sum(n)
  )

