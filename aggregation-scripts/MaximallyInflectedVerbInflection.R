# MaximallyInflectedVerbSynthesis aggregations
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

# convert snake case to camel case
to_camel_case <- function(x) {
  str_replace(x, fixed("+"),  "Plus") %>%
  str_split("[- _,/]+") %>%
  map_chr(~ {
    # capitalize each word
    . <- map_chr(., ~ { substr(., 1L, 1L) <- toupper(substr(., 1L, 1L)); . })
    # collapse them together
    str_flatten(., "")
  })
}


# ██████╗ ██████╗ ███████╗███████╗███████╗███╗   ██╗ ██████╗███████╗
# ██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝████╗  ██║██╔════╝██╔════╝
# ██████╔╝██████╔╝█████╗  ███████╗█████╗  ██╔██╗ ██║██║     █████╗
# ██╔═══╝ ██╔══██╗██╔══╝  ╚════██║██╔══╝  ██║╚██╗██║██║     ██╔══╝
# ██║     ██║  ██║███████╗███████║███████╗██║ ╚████║╚██████╗███████╗
# ╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═══╝ ╚═════╝╚══════╝

# Wide aggregation of inflected categoy presence
MaximallyInflectedVerbInflectionCategoriesAggregatedPresence <-
  MaximallyInflectedVerbSynthesis %>%
  filter(IsVerbInflectionSurveyComplete) %>%
  unnest(VerbInflectionCategories) %>%
  # collect all category and metacategoryin a single value column
  # while pivoting ot to camel case so that we can have nice looking
  # variable names
  group_by(LID, Language) %>%
  summarize(value = unique(c(
    str_c("Category", to_camel_case(VerbInflectionCategory)),
    str_c("Macrocategory", to_camel_case(VerbInflectionMacrocategory))
  )), .groups = "drop") %>%
  filter(!is.na(value)) %>%
  pivot_wider(
    names_from = value,
    names_glue = "IsInflectedFor{value}",
    values_from = value,
    values_fn = function(.) TRUE,
    values_fill = FALSE
  ) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything()) %>%
  arrange(LID)


#  ██████╗ ██████╗ ██╗   ██╗███╗   ██╗████████╗███████╗
# ██╔════╝██╔═══██╗██║   ██║████╗  ██║╚══██╔══╝██╔════╝
# ██║     ██║   ██║██║   ██║██╔██╗ ██║   ██║   ███████╗
# ██║     ██║   ██║██║   ██║██║╚██╗██║   ██║   ╚════██║
# ╚██████╗╚██████╔╝╚██████╔╝██║ ╚████║   ██║   ███████║
#  ╚═════╝ ╚═════╝  ╚═════╝ ╚═╝  ╚═══╝   ╚═╝   ╚══════╝


# Count the marked categories and roles for each position
MaximallyInflectedVerbInflectionAndAgreementCountsByPosition <- bind_rows(
    # inflectional categories
    filter(MaximallyInflectedVerbSynthesis, IsVerbInflectionSurveyComplete) %>%
    unnest(VerbInflectionCategories) %>%
    mutate(InflectionType = "Category") %>%
    select(
      LID,
      Language,
      InflectionType,
      PositionBinned5 = VerbInflectionMarkerPositionBinned5
    ),
    # agreement
    filter(MaximallyInflectedVerbSynthesis, IsVerbAgreementSurveyComplete) %>%
    unnest(VerbAgreement) %>%
    mutate(InflectionType = "Agreement") %>%
    select(
      LID,
      Language,
      InflectionType,
      PositionBinned5 = VerbAgreementMarkerPositionBinned5
    )
  ) %>%
  drop_na() %>%
  count(LID, Language, PositionBinned5) %>%
  mutate(PositionBinned5= to_camel_case(PositionBinned5)) %>%
  pivot_wider(
    names_from = PositionBinned5,
    names_glue = "InflectionMarkersAt{PositionBinned5}Count",
    values_from = n,
    values_fill = 0L
  ) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything()) %>%
  arrange(LID)



# ██████╗  ██████╗ ███████╗██╗████████╗██╗ ██████╗ ███╗   ██╗
# ██╔══██╗██╔═══██╗██╔════╝██║╚══██╔══╝██║██╔═══██╗████╗  ██║
# ██████╔╝██║   ██║███████╗██║   ██║   ██║██║   ██║██╔██╗ ██║
# ██╔═══╝ ██║   ██║╚════██║██║   ██║   ██║██║   ██║██║╚██╗██║
# ██║     ╚██████╔╝███████║██║   ██║   ██║╚██████╔╝██║ ╚████║
# ╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝

aggregate_by_position <- function(data, out_names_prefix) {
  value_fields <- c(
    "MarkerPosition",
    "MarkerPositionBinned4",
    "MarkerPositionBinned5",
    "MarkerHasPreposedExponent",
    "MarkerHasPostposedExponent",
    "MarkerHasMultipleExponents"
  )

  # pivot by every variable
  map(value_fields, function(value_field) {
    select(data, LID, Glottocode, Language, Category, all_of(value_field)) %>%
    mutate(Category= to_camel_case(Category)) %>%
    pivot_wider(
      names_from = Category,
      names_glue = str_c(value_field, "For", "{Category}"),
      values_from = all_of(value_field),
      values_fn = function(.) {
        . <- unique(na.omit(.))
        if(length(.) == 0L) return(NA)

        if(is_character(.) || is.factor(.)) {
          if(length(.)>1) "multiple" else .
        } else
        if(is_logical(.)) {
          any(.)
        } else {
          abort(format_inline("cannot aggregate position value {as_label(.)}"))
        }
      }
    )
  }) %>%
  set_names(str_c(out_names_prefix, value_fields))
}

# combine all aggregates
aggregates_by_position <- c(
  # aggregate inflection category by position
  MaximallyInflectedVerbSynthesis %>%
  filter(IsVerbInflectionSurveyComplete) %>%
  select(-IsVerbInflectionSurveyComplete) %>%
  unnest(VerbInflectionCategories) %>%
  filter(!is.na(VerbInflectionCategory)) %>%
  select(
    LID,
    Glottocode,
    Language,
    Category = VerbInflectionCategory,
    MarkerPosition = VerbInflectionMarkerPosition,
    MarkerPositionBinned4 = VerbInflectionMarkerPositionBinned4,
    MarkerPositionBinned5 = VerbInflectionMarkerPositionBinned5,
    MarkerHasPreposedExponent = VerbInflectionMarkerHasPreposedExponent,
    MarkerHasPostposedExponent  = VerbInflectionMarkerHasPostposedExponent,
    MarkerHasMultipleExponents = VerbInflectionMarkerHasMultipleExponents
  ) %>%
  aggregate_by_position("MaximallyInflectedVerbInflectionCategoriesAggregatedBy"),
  # aggregate macrocategory by position
  MaximallyInflectedVerbSynthesis %>%
  filter(IsVerbInflectionSurveyComplete) %>%
  select(-IsVerbInflectionSurveyComplete) %>%
  unnest(VerbInflectionCategories) %>%
  filter(!is.na(VerbInflectionMacrocategory)) %>%
  select(
    LID,
    Glottocode,
    Language,
    Category = VerbInflectionMacrocategory,
    MarkerPosition = VerbInflectionMarkerPosition,
    MarkerPositionBinned4 = VerbInflectionMarkerPositionBinned4,
    MarkerPositionBinned5 = VerbInflectionMarkerPositionBinned5,
    MarkerHasPreposedExponent = VerbInflectionMarkerHasPreposedExponent,
    MarkerHasPostposedExponent  = VerbInflectionMarkerHasPostposedExponent,
    MarkerHasMultipleExponents = VerbInflectionMarkerHasMultipleExponents
  ) %>%
  filter(!is.na(Category)) %>%
  aggregate_by_position("MaximallyInflectedVerbInflectionMacrocategoriesAggregatedBy"),
  # aggregate aggreement by position
  MaximallyInflectedVerbSynthesis %>%
  filter(IsVerbAgreementSurveyComplete) %>%
  select(-IsVerbAgreementSurveyComplete) %>%
  unnest(VerbAgreement) %>%
  filter(!is.na(VerbAgreementMicrorelation), VerbAgreementMicrorelation != "<any>") %>%
  select(
    LID,
    Glottocode,
    Language,
    Category = VerbAgreementMicrorelation,
    MarkerPosition = VerbAgreementMarkerPosition,
    MarkerPositionBinned4 = VerbAgreementMarkerPositionBinned4,
    MarkerPositionBinned5 = VerbAgreementMarkerPositionBinned5,
    MarkerHasPreposedExponent = VerbAgreementMarkerHasPreposedExponent,
    MarkerHasPostposedExponent  = VerbAgreementMarkerHasPostposedExponent,
    MarkerHasMultipleExponents = VerbAgreementMarkerHasMultipleExponents
  ) %>%
  filter(!is.na(Category)) %>%
  aggregate_by_position("MaximallyInflectedVerbAgreementAggregatedBy")
)


#  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
# ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
# ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
# ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
# ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
#  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝

descriptor <- describe_data(
  ptype = tibble(),
  description = "Per-language summaries of presence of inflectional categories",
  fields = c(
    .metadata$Register$fields[c("LID", "Glottocode", "Language")],
    {
      nms <- str_subset(names(MaximallyInflectedVerbInflectionCategoriesAggregatedPresence), "^IsInflectedFor.+ategory")
      map(nms, ~ {
        cat <- str_remove(., "^IsInflectedFor.+ategory")

        describe_data(
          ptype = logical(),
          computed = "MaximallyInflectedVerbInflection.R",
          description = format_inline(
            "Is the maximally inflected verb form inflected for {.q {cat}}?"
          )
        )
      }) %>% set_names(nms)
    }
  )
)

export_dataset(
  "MaximallyInflectedVerbInflectionCategoriesAggregatedPresence",
  MaximallyInflectedVerbInflectionCategoriesAggregatedPresence,
  descriptor,
  c("PerLanguageSummaries", "InflectionForMaximallyInflectedVerbs")
)



descriptor <- describe_data(
  ptype = tibble(),
  description = "Per-language number of inflection and agreement markers on the verb",
  computed = "MaximallyInflectedVerbInflection.R",
  fields = list(
    LID = .metadata$Register$fields$LID,
    Glottocode = .metadata$Register$fields$Glottocode,
    Language = .metadata$Register$fields$Language,
    InflectionMarkersAtPostCount = describe_data(
      ptype = integer(),
      computed = "MaximallyInflectedVerbInflection.R",
      description = "
        Number of inflection and agreement markers with a formative
        that follows the host in maximally inflected verb forms
      "
    ),
    InflectionMarkersAtPraeCount = describe_data(
      ptype = integer(),
      computed = "MaximallyInflectedVerbInflection.R",
      description = "
        Number of verbal inflection and agreement markers with a formative
        that precedes the host in maximally inflected verb forms
      "
    ),
    InflectionMarkersAtSplitCount = describe_data(
      ptype = integer(),
      computed = "MaximallyInflectedVerbInflection.R",
      description = "
        Number of verbal inflection and agreement markers with a formative
        that can occur at different positions in maximally inflected verb forms
      "
    ),
    InflectionMarkersAtSimulCount = describe_data(
      ptype = integer(),
      computed = "MaximallyInflectedVerbInflection.R",
      description = "
        Number of verbal inflection and agreement markers with multiple exponence
        in maximally inflected verb forms
      "
    ),
    InflectionMarkersAtInCount = describe_data(
      ptype = integer(),
      description = "
        Number of verbal inflection and agreement markers with formatives
        inside the phological host in maximally inflected verb forms
      "
    )
  )
)

export_dataset(
  "MaximallyInflectedVerbInflectionAndAgreementCountsByPosition",
  MaximallyInflectedVerbInflectionAndAgreementCountsByPosition,
  descriptor,
  c("PerLanguageSummaries")
)

for(i in seq_along(aggregates_by_position)) {
  name <- names(aggregates_by_position)[[i]]
  type <- switch(str_match(name, "^MaximallyInflectedVerb(.*)AggregatedBy.*$")[, 2L],
    "InflectionCategories" = "inflection category on maximally inflected verbs",
    "InflectionMacrocategories" = "inflection category (binned into broad types) on maximally inflected verbs",
    "Agreement" = "agreement microrelation on maximally inflected verbs"
  )
  variable <- str_match(name, "^.+AggregatedBy(.*)$")[, 2L]
  description = format_inline(
    "Marker position (from `GrammaticalMarkers::{variable}`),\nreshaped and aggregated per language and {type}"
  )

  descriptor <- describe_data(
    ptype = tibble(),
    description = description,
    computed = "MaximallyInflectedVerbInflection.R",
    fields = c(
      .metadata$Register$fields[c("LID", "Glottocode", "Language")],
      {
        nms <- str_subset(names(aggregates_by_position[[i]]), "^Marker")
        map(nms, ~ {

          cat <- str_remove(., "^Marker.*For")

          descriptor <- describe_data(
            ptype = if(is.logical(aggregates_by_position[[i]][[.]])) logical() else factor(),
            computed = "MaximallyInflectedVerbInflection.R",
            description = format_inline(
              "GrammaticalMarkers::{variable} value for {cat}"
            )
          )

          # fix factors
          if(is.factor(descriptor$ptype)) {
            dd <- switch(var <- str_remove(., "For.*$"),
              MarkerPosition = .metadata$Position$fields$Position,
              MarkerPositionBinned4 = .metadata$Position$fields$PositionBinned4,
              MarkerPositionBinned5 = .metadata$Position$fields$PositionBinned5,
              abort("Unknown field {var}")
            )

            descriptor$levels <- add_row(dd$levels,
              level = "multiple", description = "multiple markers at different positions"
            )
            descriptor <- fix_metadata_levels(descriptor, aggregates_by_position[[i]][[.]])

            aggregates_by_position[[i]][[.]] <<- factor(
              as.character(aggregates_by_position[[i]][[.]]),
              levels = levels(descriptor$ptype)
            )
          }

          descriptor
        }) %>% set_names(nms)
      }
    )
  )

  export_dataset(
    names(aggregates_by_position)[[i]],
    aggregates_by_position[[i]],
    descriptor,
    c("PerLanguageSummaries")
  )
}

writeLines(names(aggregates_by_position))
