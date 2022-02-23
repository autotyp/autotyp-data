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

Alienability <- Alienability %>%
  # Presence of bound nouns
  mutate(
    NPHasBoundNouns = NPBoundNounsCount > 0L,
    .before = NPBoundNounsCount
  ) %>%
  # Presence of alienable classes
  mutate(
    NPHasAlienableClasses = NPAlienableClassesCount > 0L,
    .before = NPAlienableClassesCount
  ) %>%
  # Presence of inalienable classes
  mutate(
    NPHasInalienableClasses = NPInalienableClassesCount > 0L,
    .before = NPInalienableClassesCount
  ) %>%
  # Presence of possesive classes
  mutate(
    NPHasPossessiveClasses = NPPossessiveClassesCount > 0L,
    NPHasMultiplePossessiveClasses = NPPossessiveClassesCount > 1L,
    .before = NPPossessiveClassesCount
  ) %>%
  # Presence of posessive classivication
  mutate(
    NPHasPossessiveClassification = NPHasBoundNouns | NPHasPossessiveClasses,
    .before = NPPossessiveClassificationType
  )


#  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
# ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
# ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
# ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
# ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
#  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝


descriptor <- .metadata$Alienability
descriptor$fields <- c(descriptor$fields, list(
  NPHasPossessiveClassification = describe_data(
    ptype = logical(),
    computed = "enhancements/Alienability.R",
    description = glue::trim("
      Presence of any possessive classification, either in the form overtly
      marked classes or in the form of bound nouns"
    )
  ),
  NPHasBoundNouns = describe_data(
    ptype = logical(),
    computed = "enhancements/Alienability.R",
    description = "Presence of bound (obligatorily possessed) nouns"
  ),
  NPHasAlienableClasses = describe_data(
    ptype = logical(),
    computed = "enhancements/Alienability.R",
    description = "Presence of any alienable classes"
  ),
  NPHasInalienableClasses = describe_data(
    ptype = logical(),
    computed = "enhancements/Alienability.R",
    description = "Presence of any inalienable class"
  ),
  NPHasPossessiveClasses = describe_data(
    ptype = logical(),
    computed = "enhancements/Alienability.R",
    description = glue::trim("
      Presence of any possessive class distinction, distinguished by overt
      markers or differences in the form of the possessor or the possessed or both
    ")
  ),
  NPHasMultiplePossessiveClasses = describe_data(
    ptype = logical(),
    computed = "enhancements/Alienability.R",
    description = "Presence of multiple (more then one) overt possessive classes"
  )
))

export_dataset(
  "Alienability",
  Alienability,
  descriptor,
  "Categories"
)
