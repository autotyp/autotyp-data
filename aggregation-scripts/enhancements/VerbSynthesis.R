# VerbSynthesis enhancements
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





VerbSynthesis <- VerbSynthesis %>%
  # Number of categories expressed in maximally inflected verb form
  mutate(
    VerbInflectionMaxCategoryCount = if_else(
      IsVerbInflectionSurveyComplete & IsVerbAgreementSurveyComplete,
      list_sizes(VerbInflectionCategories) + list_sizes(VerbAgreement),
      NA_integer_
    ),
    VerbInflectionMaxCategorySansAgreementCount = if_else(
      IsVerbInflectionSurveyComplete,
      list_sizes(VerbInflectionCategories),
      NA_integer_
    ),
    .before = VerbInflectionMaxFormativeCount
  ) %>%
  # Exponense type classification (Bickel & Nichols 2007)
  mutate(
    VerbInflectionExponenceType = case_when(
      VerbInflectionMaxCategoryCount > VerbInflectionMaxFormativeCount  ~ "cumulative",
      VerbInflectionMaxCategoryCount < VerbInflectionMaxFormativeCount  ~ "distributive",
      VerbInflectionMaxCategoryCount == VerbInflectionMaxFormativeCount ~ "separative",
    ) %>% factor(levels = c("cumulative", "distributive", "separative")),
    .before = VerbInflectionMaxCategoryCount
  ) %>%
  # Various incorporation aggregates
  mutate(
    VerbHasAnyIncorporation = list_sizes(VerbIncorporation)>0L,
    VerbHasNounIncorporation = map_lgl(VerbIncorporation, ~ "N" %in% .),
    VerbHasVerbIncorporation = map_lgl(VerbIncorporation, ~ "V" %in% .),
    VerbHasNounOrVerbIncorporation = VerbHasNounIncorporation | VerbHasVerbIncorporation,
    .after = VerbIncorporation
  ) %>%
  ungroup()


#  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
# ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
# ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
# ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
# ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
#  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝


descriptor <- .metadata$VerbSynthesis
descriptor$fields <- c(descriptor$fields, list(
  VerbInflectionExponenceType = describe_data(
    computed = "enhancements/VerbSynthesis.R",
    ptype = factor(),
    description = "
      Overall type of exponence in verbal inflection, determined by comparing
      the number of formatives and the number of categories they express (inflection
      defined as in Bickel & Nichols 2007 in T. Shopen [ed.], Language Typology and
      Syntactic Description. CUP)
    ",
    levels = tribble(
      ~ level, ~ description,
      "cumulative", "
        there are more categories than marker slots expressing these
        categories (at least some feature cumulation)
      ",
      "distributive", "
        there are fewer categories than marker slots expressing these
        categories (at least some feature distribution)
      ",
      "separative", "the number of categories and marker slots expressing these categories match"
    )
  ),
  VerbInflectionMaxCategoryCount = describe_data(
    computed = "enhancements/VerbSynthesis.R",
    ptype = integer(),
    description = glue::trim(
      "Number of categories (including agreement) expressed in maximally
      inflected verb form ('inflection' as defined in Bickel & Nichols 2007
      in T. Shopen [ed.], Language Typology and Syntactic Description. CUP)"
    )
  ),
  VerbInflectionMaxCategorySansAgreementCount = describe_data(
    computed = "enhancements/VerbSynthesis.R",
    ptype = integer(),
    description = glue::trim("
      Number of categories (excluding agreement) expressed in maximally
      inflected verb form ('inflection' as defined in Bickel & Nichols 2007 in
      T. Shopen [ed.], Language Typology and Syntactic Description. CUP)"
    )
  ),
  VerbHasAnyIncorporation = describe_data(
    computed = "enhancements/VerbSynthesis.R",
    ptype = logical(),
    description = "Presence of any type of incorporation in the verb form"
  ),
  VerbHasNounIncorporation = describe_data(
    computed = "enhancements/VerbSynthesis.R",
    ptype = logical(),
    description = "Presence of noun incorporation in the verb form"
  ),
  VerbHasVerbIncorporation = describe_data(
    computed = "enhancements/VerbSynthesis.R",
    ptype = logical(),
    description = "Presence of verb incorporation in the verb form"
  ),
  VerbHasNounOrVerbIncorporation = describe_data(
    computed = "enhancements/VerbSynthesis.R",
    ptype = logical(),
    description = "Presence of verb or noun incorporation in the verb form"
  )
))

export_dataset(
  "VerbSynthesis",
  VerbSynthesis,
  descriptor,
  "Morphology"
)
