# GrammaticalMarkers aggregations
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


library(magrittr)
source("R/plugin-support.R")

# convert snake case to camel case
to_camel_case <- function(x) {
  str_replace(x, fixed("+"),  "Plus") %>%
  str_split("[- _]+") %>%
  map_chr(~ {
    # capitalize each word
    . <- map_chr(., ~ { substr(., 1L, 1L) <- toupper(substr(., 1L, 1L)); . })
    # collapse them together
    str_flatten(., "")
  })
}

# collect relevant properties for exemplars per language
GrammaticalMarkersPerLanguage <- GrammaticalMarkers %>%
  select(
    LID,
    Language,
    MarkerExemplar,
    MarkerMorphemeType,
    # MarkerIsGrammaticalWord,
    MarkerIsPersonPortmanteau,
    MarkerHostRestriction,
    MarkerFusion,
    MarkerFusionBinned6,
    MarkerFusionIsIsolating,
    MarkerFusionIsNonlinear,
    MarkerFusionIsReduplicative,
    MarkerFusionIsTonal,
    MarkerStemFlexivity,
    MarkerFormativeFlexivity,
    MarkerPosition,
    MarkerPositionBinned4,
    MarkerPositionBinned5,
    MarkerHasPreposedExponent,
    MarkerHasPreposedDefaultExponent,
    MarkerHasPostposedExponent,
    MarkerHasMultipleExponents,
    MarkerHasMultipleDefaultExponents,
    MarkerBehavior,
    MarkerBehaviorBinned4,
    MarkerBehaviorIsSpreading,
    MarkerExpressedCategoriesCount,
    MarkerExpressesMultipleCategories
  ) %>%
  # remove all non-exemplars
  filter(!is.na(MarkerExemplar)) %>%
  # collapse unique entries
  distinct() %T>% {
    # save the variables to be transposed for generating field metadata
    new_variables <<- expand_grid(
      MarkerExemplar = unique(.$MarkerExemplar),
      Variable = setdiff(names(.), c("LID", "Language", "MarkerExemplar"))
    ) %>%
    mutate(
      NewVariable = str_c(to_camel_case(MarkerExemplar), Variable)
    )
  } %>%
  mutate(MarkerExemplar = to_camel_case(MarkerExemplar)) %>%
  # pivot to wide
  pivot_wider(
    names_from = MarkerExemplar,
    id_cols = c(LID, Language),
    values_from = c(-LID, -Language, -MarkerExemplar),
    names_glue = "{MarkerExemplar}{.value}",
    # return NA on conflicts
    values_fn = function(x) {
      x <- x[!is.na(x)]
      if(length(x) == 0 || length(x) > 1) x <- vec_cast(NA, x)
      x
    },
    values_fill = NA
  ) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything())




descriptor <- describe_data(
  ptype = tibble(),
  description = "
    Grammatical marker exemplars properties (based on the data from `GrammaticalMarkers`),
    aggregated per language
  ",
  computed = "GrammaticalMarkers.R",
  fields = c(
    .metadata$Register$fields["LID"],
    .metadata$Register$fields["Language"],
    .metadata$Register$fields["Glottocode"],
    new_variables %>%
    rowwise() %>%
    group_map(~ {
      # build the descriptor
      descriptor <- .metadata$GrammaticalMarkers$fields[[.$Variable]]
      descriptor$description <- format_inline(
        "Value of `GrammaticalMarkers::{.$Variable}` for exemplar {.q {.$MarkerExemplar}}"
      )
      descriptor$computed <- "GrammaticalMarkers.R"
      descriptor


      # fix factors
      if(is.factor(descriptor$ptype)) {
        descriptor <- fix_metadata_levels(
          descriptor,
          GrammaticalMarkersPerLanguage[[.$NewVariable]]
        )
        GrammaticalMarkersPerLanguage[[.$NewVariable]] <<- factor(
          as.character(GrammaticalMarkersPerLanguage[[.$NewVariable]]),
          levels = levels(descriptor$ptype)
        )
      }

      descriptor
    }) %>% set_names(new_variables$NewVariable)
  )
)

export_dataset(
  "GrammaticalMarkersPerLanguage",
  GrammaticalMarkersPerLanguage,
  descriptor,
  c("PerLanguageSummaries", "Morphology")
)
