# LocusOfMarking aggregates
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
  str_split(x, "[- _]+") %>%
  map_chr(~ {
    # capitalize each word
    . <- map_chr(., ~ { substr(., 1L, 1L) <- toupper(substr(., 1L, 1L)); . })
    # collapse them together
    str_flatten(., "")
  })
}


# ███╗   ███╗ █████╗  ██████╗██████╗  ██████╗ ██████╗  ██████╗ ██╗     ███████╗
# ████╗ ████║██╔══██╗██╔════╝██╔══██╗██╔═══██╗██╔══██╗██╔═══██╗██║     ██╔════╝
# ██╔████╔██║███████║██║     ██████╔╝██║   ██║██████╔╝██║   ██║██║     █████╗
# ██║╚██╔╝██║██╔══██║██║     ██╔══██╗██║   ██║██╔══██╗██║   ██║██║     ██╔══╝
# ██║ ╚═╝ ██║██║  ██║╚██████╗██║  ██║╚██████╔╝██║  ██║╚██████╔╝███████╗███████╗
# ╚═╝     ╚═╝╚═╝  ╚═╝ ╚═════╝╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚══════╝
#
# Aggregate over default locus of marking for syntactic macrorelations
# If we have multiple loci of marking (via multipe microrelatinons), we choose
# the most representative relation (see code for procedure)
DefaultLocusOfMarkingPerMacrorelation <- LocusOfMarkingPerMicrorelation %>%
  # unnnest the data and take only the default locus
  unnest(LocusOfMarking) %>%
  filter(IsDefaultLocusOfMarking) %>%
  # remove all unnessesary columns
  select(
    LID,
    Language,
    Microrelation,
    Macrorelation,
    LocusOfMarking,
    LocusOfMarkingBinned5,
    LocusOfMarkingBinned6
  ) %>%
  distinct() %>%
  # determine the locus of markign for each macrorelation
  group_by(LID, Language, Macrorelation) %>%
  summarize(
    # no ambiguity, take the first row
    if(n_distinct(LocusOfMarking) == 1L) {
      select(cur_data(), -Microrelation)[1L, ]
    } else
    # disambiguate A (A-default comes first)
    if(Macrorelation[[1L]] == "A" && "A-default" %in% Microrelation) {
      filter(cur_data(), Microrelation == "A-default") %>%
      select(-Microrelation)
    } else
    # disambiguate A (Act comes next)
    if(Macrorelation[[1L]] == "A" && "Act" %in% Microrelation) {
      filter(cur_data(), Microrelation == "Act") %>%
      select(-Microrelation)
    } else
    # disambiguate P (U-default comes first)
    if(Macrorelation[[1L]] == "P" && "U-default" %in% Microrelation) {
      filter(cur_data(), Microrelation == "U-default") %>%
      select(-Microrelation)
    } else
    # disambiguate P (Pat comes next)
    if(Macrorelation[[1L]] == "P" && "Pat" %in% Microrelation) {
      filter(cur_data(), Microrelation == "Pat") %>%
      select(-Microrelation)
    }
    # no default marking can be established, return NA
    else {
      NA
    },
    .groups = "drop"
  ) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything())


# ███████╗██╗   ██╗███╗   ███╗███╗   ███╗ █████╗ ██████╗ ██╗   ██╗
# ██╔════╝██║   ██║████╗ ████║████╗ ████║██╔══██╗██╔══██╗╚██╗ ██╔╝
# ███████╗██║   ██║██╔████╔██║██╔████╔██║███████║██████╔╝ ╚████╔╝
# ╚════██║██║   ██║██║╚██╔╝██║██║╚██╔╝██║██╔══██║██╔══██╗  ╚██╔╝
# ███████║╚██████╔╝██║ ╚═╝ ██║██║ ╚═╝ ██║██║  ██║██║  ██║   ██║
# ╚══════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝
#
# Agregate various aspects of microrelation marking in a per-language
# summary table
#

# Presense of head and dependent marking for various macrorelations
MarkingPresence <- LocusOfMarkingPerMicrorelation %>%
  unnest(LocusOfMarking) %>%
  filter(IsDefaultLocusOfMarking) %>%
  filter(!is.na(Macrorelation)) %>%
  group_by(LID, Language, Macrorelation) %>%
  summarize(
    HasHeadMarking = any(LocusOfMarkingBinned5 %in% c("2", "H")),
    HasDependentMarking = any(LocusOfMarkingBinned5 %in% c("2", "D")),
    .groups = "drop"
  ) %>%
  mutate(Macrorelation = to_camel_case(Macrorelation)) %>%
  pivot_wider(
    names_from = Macrorelation,
    values_from = c(HasHeadMarking, HasDependentMarking),
    names_glue = "{.value}For{Macrorelation}",
    values_fill = FALSE
  ) %>%
  # combinations of role pairs
  mutate(
    HasHeadMarkingForSAndA = HasHeadMarkingForS & HasHeadMarkingForA,
    HasHeadMarkingForSOrA  = HasHeadMarkingForS | HasHeadMarkingForA,
    HasHeadMarkingForAAndP = HasHeadMarkingForA & HasHeadMarkingForP,
    HasHeadMarkingForAOrP  = HasHeadMarkingForA | HasHeadMarkingForP,
    HasDependentMarkingForSAndA = HasDependentMarkingForS & HasDependentMarkingForA,
    HasDependentMarkingForSOrA  = HasDependentMarkingForS | HasDependentMarkingForA,
    HasDependentMarkingForAAndP = HasDependentMarkingForA & HasDependentMarkingForP,
    HasDependentMarkingForAOrP  = HasDependentMarkingForA | HasDependentMarkingForP
  )

# Locus of marking for various microrelations conditioned by morphosyntactic categories
MarkingPerMicrorelation <- LocusOfMarkingPerMicrorelation %>%
  unnest(LocusOfMarking) %>%
  mutate(RoleCatLabel =
    str_c(
      to_camel_case(Microrelation),
      if_else(
        IsDefaultLocusOfMarking,
        "",
        str_c("If", to_camel_case(as.character(LocusOfMarkingCategoryCondition) %|% "Other"))
      )
    )
  ) %>%
  select(
    LID,
    Language,
    RoleCatLabel,
    LocusOfMarking,
    LocusOfMarkingBinned5,
    LocusOfMarkingBinned6
  ) %>%
  pivot_wider(
    names_from=RoleCatLabel,
    values_from=c(LocusOfMarking, LocusOfMarkingBinned5, LocusOfMarkingBinned6),
    names_glue = "{.value}For{RoleCatLabel}",
    values_fn = function(x) {
      x <- unique(x)
      if(length(x) > 1) "multiple" else x
    },
    values_fill = NA
  )

# combine both into one very large wide table
LocusOfMarkingPerLanguage <- inner_join(
    MarkingPresence,
    MarkingPerMicrorelation,
    by = c("LID", "Language")
  ) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything()) %>%
  arrange(LID, Language)


# TODO: improve this
descriptor <- describe_data(
  ptype = tibble(),
  description = "Locus of marking, aggregated per language",
  computed = "LocusOfMarking.R",
  fields = c(
    .metadata$Register$fields[c("LID", "Language", "Glottocode")],
    map(setdiff(names(LocusOfMarkingPerLanguage), c("LID", "Language", "Glottocode")), ~ {
      descriptor <- describe_data(
        ptype = if(is.logical(LocusOfMarkingPerLanguage[[.]])) logical() else factor(),
        computed = "LocusOfMarking.R",
        description = "<pending>"
      )

      # fix factors
      if(is.factor(descriptor$ptype)) {
        # variable name
        var <- gsub("For.+$", "", .)

        dd <- .metadata$LocusOfMarkingPerMicrorelation$fields$LocusOfMarking$element$fields[[var]]
        !is_null(dd) || abort("Unknown variable {var}")

        descriptor$levels <- add_row(dd$levels,
          level = "multiple", description = "multiple different loci"
        )
        descriptor <- fix_metadata_levels(descriptor, LocusOfMarkingPerLanguage[[.]])

        LocusOfMarkingPerLanguage[[.]] <<- factor(
          as.character(LocusOfMarkingPerLanguage[[.]]),
          levels = levels(descriptor$ptype)
        )
      }

      descriptor
    }) %>% set_names(setdiff(names(LocusOfMarkingPerLanguage), c("LID", "Language", "Glottocode")))
  )
)

export_dataset("LocusOfMarkingPerLanguage", LocusOfMarkingPerLanguage, descriptor, c("PerLanguageSummaries", "Morphology"))

descriptor <- describe_data(
  ptype = tibble(),
  description = "Default locus of marking, aggregated per language and macrorelation",
  computed = "LocusOfMarking.R",
  fields = list(
    LID = .metadata$Register$fields$LID,
    Language = .metadata$Register$fields$Language,
    Glottocode = .metadata$Register$fields$Glottocode,
    Macrorelation = .metadata$LocusOfMarkingPerMicrorelation$fields$Macrorelation,
    LocusOfMarking = .metadata$LocusOfMarking$fields$LocusOfMarking,
    LocusOfMarkingBinned5 = .metadata$LocusOfMarking$fields$LocusOfMarkingBinned5,
    LocusOfMarkingBinned6 = .metadata$LocusOfMarking$fields$LocusOfMarkingBinned6
  )
)

export_dataset("DefaultLocusOfMarkingPerMacrorelation", DefaultLocusOfMarkingPerMacrorelation, descriptor, "Morphology")
