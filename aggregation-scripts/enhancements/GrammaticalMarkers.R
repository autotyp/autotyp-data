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

GrammaticalMarkers <- GrammaticalMarkers %>%
  # Flexivity
  mutate(
    MarkerIsFlexive = {
      MarkerStemFlexivity %in% c("cat_based", "lex-based", "both") |
      MarkerFormativeFlexivity %in% c("cat_based", "lex-based", "both")
    },
    MarkerHasLexicalFlexivity = {
      MarkerStemFlexivity %in% c("lex-based", "both") |
      MarkerFormativeFlexivity %in% c("lex-based", "both")
    },
    .before = MarkerStemFlexivity
  ) %>%
  # Number of expressed categories
  mutate(
    MarkerExpressedCategoriesCount = list_sizes(MarkerExpressedCategories),
    MarkerExpressesMultipleCategories = MarkerExpressedCategoriesCount > 1L,
    .before = MarkerExpressedCategories
  )



#  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
# ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
# ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
# ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
# ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
#  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝


descriptor <- .metadata$GrammaticalMarkers
descriptor$fields <- c(descriptor$fields, list(
  MarkerIsFlexive = describe_data(
    computed = "enhancements/GrammaticalMarkers.R",
    ptype = logical(),
    description = glue::trim("
      Some kind of flexivity in stem and/or affixes, either lexically or
      grammatically conditioned (see Bickel & Nichols 2007 in Language
      typology and syntactic description, ed. T. Shopen, Cambridge:
      Cambridge University Press)"
    )
  ),
  MarkerHasLexicalFlexivity = describe_data(
    computed = "enhancements/GrammaticalMarkers.R",
    ptype = logical(),
    description = glue::trim("
      Lexical flexivity (i.e. lexically-conditioned allomorphy, also
      known as inflectional classes) in the shape of stems and/or affixes
      (see Bickel & Nichols 2007 in Language typology and syntactic description,
      ed. T. Shopen, Cambridge: Cambridge University Press)"
    )
  ),
  MarkerExpressedCategoriesCount = describe_data(
    computed = "enhancements/GrammaticalMarkers.R",
    ptype = integer(),
    description = "Number of categories expressed by the marker"
  ),
  MarkerExpressesMultipleCategories = describe_data(
    computed = "enhancements/GrammaticalMarkers.R",
    ptype = logical(),
    description = "Does the marker express more than one category?"
  )
))

export_dataset(
  "GrammaticalMarkers",
  GrammaticalMarkers,
  descriptor,
  "Morphology"
)
