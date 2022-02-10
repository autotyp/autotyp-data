# Alienability enhancements
#
# This file is an AUTOTYP aggregation
#
# For questions, open an issue
#
# Copyright 2022 Taras Zakharko (CC BY 4.0).


#  ███████╗███████╗████████╗██╗   ██╗██████╗
#  ██╔════╝██╔════╝╚══██╔══╝██║   ██║██╔══██╗
#  ███████╗█████╗     ██║   ██║   ██║██████╔╝
#  ╚════██║██╔══╝     ██║   ██║   ██║██╔═══╝
#  ███████║███████╗   ██║   ╚██████╔╝██║
#  ╚══════╝╚══════╝   ╚═╝    ╚═════╝ ╚═╝
#

source("R/plugin-support.R")
source("R/expand_na.R")



Clusivity <- Clusivity %>%
  # Clusivity distinction type
  mutate(
    ClusivityType = case_when(
      HasClusivityWithMinimalNumberSystem ~ "min/aug type",
      HasClusivityAsPerson ~ "excl as person type",
      HasClusivity ~ "plain i/e type",
      !HasClusivity ~ "no i/e"
    ) %>%
    factor(levels = c("no i/e", "plain i/e type", "excl as person type", "min/aug type")),
    .after = HasClusivity
  )

#  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
# ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
# ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
# ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
# ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
#  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝


descriptor <- .metadata$Clusivity
descriptor$fields$ClusivityType <- describe_data(
  computed = "enhancements/Clusivity.R",
  ptype = factor(),
  levels = tribble(
    ~ level, ~ description,
    "no i/e", "no clusivity distinction",
    "plain i/e type", "there is clusivity distinction",
    "excl as person type", "clusivity is a person category",
    "min/aug type", "clusivity is present with a minimal/augmented number system"
  ),
  description = glue::trim("
    Type of inclusive/exclusive distinction, as defined in Bickel & Nichols
    2005 in Filimonova ed. Clusivity, Amsterdam: Benjamins, 47-70"
  )
)

export_dataset(
  "Clusivity",
  Clusivity,
  descriptor,
  "Categories"
)
