# Predicate classes per language aggregation
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
snake_to_camel <- function(x) {
  tolower(x) %>%
  str_split(fixed("_")) %>%
  map_chr(~ {
    # capitalize each word
    map_chr(., ~ { substr(., 1L, 1L) <- toupper(substr(., 1L, 1L)); . }) %>%
    # collapse them together
    str_flatten("")
  })
}

# ███████╗███████╗███╗   ███╗ █████╗ ███╗   ██╗████████╗██╗ ██████╗███████╗
# ██╔════╝██╔════╝████╗ ████║██╔══██╗████╗  ██║╚══██╔══╝██║██╔════╝██╔════╝
# ███████╗█████╗  ██╔████╔██║███████║██╔██╗ ██║   ██║   ██║██║     ███████╗
# ╚════██║██╔══╝  ██║╚██╔╝██║██╔══██║██║╚██╗██║   ██║   ██║██║     ╚════██║
# ███████║███████╗██║ ╚═╝ ██║██║  ██║██║ ╚████║   ██║   ██║╚██████╗███████║
# ╚══════╝╚══════╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝   ╚═╝   ╚═╝ ╚═════╝╚══════╝
#
# Build a summary table where presence of a prediate class with specific
# semantic property (according to verb semantic class) is coded as TRUE or
# FALSE

# Build the summary table of valence class semantic classes across languages
PredicateClassesSemanticsPerLanguage <- PredicateClasses %>%
  # unstack the invididual predicates
  unnest(IndividualPredicates) %>%
  # collect all semantic classes in a single semantic class column
  # (we lose distinction between malchukov class and internal class)
  group_by(LID) %>%
  summarize(SemanticClass = unique(c(
    IndividualPredicateSemanticClass,
    IndividualPredicateMalchukovSemanticClass
  )), .groups = "drop") %>%
  # remove NA entries
  filter(!is.na(SemanticClass)) %>%
  # transform to camel case
  mutate(
    SemanticClass = snake_to_camel(SemanticClass)
  ) %>%
  # pivot to wide
  pivot_wider(
    names_from = SemanticClass,
    names_glue = "HasPredicateClassWithVerbsOfType{SemanticClass}",
    values_from = SemanticClass,
    values_fn = function(.) TRUE,
    values_fill = FALSE
  )

#  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
# ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
# ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
# ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
# ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
#  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝


# build the predicate classes per language table
PredicateClassesSemanticsPerLanguage <- PredicateClasses %>%
  group_by(LID, Glottocode, Language) %>%
  summarize(PredicateClassesCount = n_distinct(PredicateClassID), .groups = "drop") %>%
  left_join(PredicateClassesSemanticsPerLanguage, by = "LID") %>%
  # only keep real languages
  filter(!is.na(LID)) %>%
  arrange(LID)


descriptor <- describe_data(
  ptype = tibble(),
  description =
    "Per-language summaries of predicate classes with distinct morphosyntactic behavior"
  ,
  computed = "PredicateClass.R",
  fields = c(list(
    LID = .metadata$Register$fields$LID,
    Glottocode = .metadata$Register$fields$Glottocode,
    Language = .metadata$Register$fields$Language,
    PredicateClassesCount = describe_data(
      ptype = integer(),
      computed = "PredicateClass.R",
      description = "Number of non-default predicate classes with distinct morphosyntactic behavior"
    )),
    {
      nms <- str_subset(names(PredicateClassesSemanticsPerLanguage), "^HasPredicateClassWithVerbsOfType")
      map(nms, ~ {
        sem <- str_remove(., "^HasPredicateClassWithVerbsOfType")

        describe_data(
          ptype = logical(),
          computed = "PredicateClass.R",
          description = format_inline(
            "Does at least one non-default predicate class contain verbs of type {.q {sem}}"
          )
        )
      }) %>% set_names(nms)
    }
  )
)

export_dataset(
  "PredicateClassesSemanticsPerLanguage",
  PredicateClassesSemanticsPerLanguage,
  descriptor,
  c("PerLanguageSummaries", "GrammaticalRelations")
)
