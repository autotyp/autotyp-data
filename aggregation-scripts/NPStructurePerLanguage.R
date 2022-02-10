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


# ███████╗██╗   ██╗███╗   ███╗███╗   ███╗ █████╗ ██████╗ ██╗   ██╗
# ██╔════╝██║   ██║████╗ ████║████╗ ████║██╔══██╗██╔══██╗╚██╗ ██╔╝
# ███████╗██║   ██║██╔████╔██║██╔████╔██║███████║██████╔╝ ╚████╔╝
# ╚════██║██║   ██║██║╚██╔╝██║██║╚██╔╝██║██╔══██║██╔══██╗  ╚██╔╝
# ███████║╚██████╔╝██║ ╚═╝ ██║██║ ╚═╝ ██║██║  ██║██║  ██║   ██║
# ╚══════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝
#
# Agregate various aspects of NP structre in a per-language summary table


NPStructurePerLanguage <- NPStructure %>%
  # unnest the dependent PoS
  unnest(NPStructureDependentPoSConstraints) %>%
  mutate(isAdj = NPStructureDependentPoSConstraints == "Adj") %>%
  # aggregate per language
  group_by(LID, Language) %>%
  summarize(
    # summaries for all NP
    NPHasAgreement = any(NPStructureMarkingAssignmentType == "agreement/class marker"),
    NPHasConstructMarkers = any(NPStructureMarkingAssignmentType == "construct state"),
    NPHasGovernment = any(NPStructureMarkingAssignmentType == "government"),
    NPHasOvertMarking = any(NPStructureIsOvertlyMarked),
    # summaries for adjectives
    NPHasAdjAgreement = any(isAdj & NPStructureMarkingAssignmentType == "agreement/class marker"),
    NPHasAdjConstructMarkers = any(isAdj & NPStructureMarkingAssignmentType == "construct state"),
    NPHasAdjGovernment = any(isAdj & NPStructureMarkingAssignmentType == "government"),
    NPHasAdjOvertMarking = any(isAdj & NPStructureIsOvertlyMarked),
    .groups = "drop"
  ) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything()) %>%
  arrange(LID, Language)



# ██████╗ ██████╗ ███████╗███████╗███████╗███╗   ██╗ ██████╗███████╗
# ██╔══██╗██╔══██╗██╔════╝██╔════╝██╔════╝████╗  ██║██╔════╝██╔════╝
# ██████╔╝██████╔╝█████╗  ███████╗█████╗  ██╔██╗ ██║██║     █████╗
# ██╔═══╝ ██╔══██╗██╔══╝  ╚════██║██╔══╝  ██║╚██╗██║██║     ██╔══╝
# ██║     ██║  ██║███████╗███████║███████╗██║ ╚████║╚██████╗███████╗
# ╚═╝     ╚═╝  ╚═╝╚══════╝╚══════╝╚══════╝╚═╝  ╚═══╝ ╚═════╝╚══════╝
#
# Aggregate various aspects of NP structures as binary variables in a per structure summary

# Agreement
agreement_presence <- NPStructure %>%
  # unnest the dependent PoS
  unnest(NPStructureAgreement, keep_empty = TRUE) %>%
  select(LID, NPStructureID, NPStructureAgreementCategory) %>%
  distinct() %>%
  # mark presence
  mutate(value = TRUE) %>%
  # compelte for all factor levels
  complete(LID, NPStructureID, NPStructureAgreementCategory, fill  = list(value = FALSE)) %>%
  filter(!is.na(NPStructureAgreementCategory)) %>%
  mutate(NPStructureAgreementCategory=to_camel_case(NPStructureAgreementCategory)) %>%
  pivot_wider(
    names_from = NPStructureAgreementCategory,
    names_glue = "NPHas{NPStructureAgreementCategory}Agreement",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)

# Alienability
alienability_presence <- NPStructure %>%
  # only limit to certain alignability types
  mutate(
    Alienability = case_when(
      NPStructureAlienabilityType %in% c("alienable", "marked alienable") ~ "Alienable",
      NPStructureAlienabilityType %in% "fluid" ~ "Fluid",
      NPStructureAlienabilityType %in% c("inalienable", "unmarked inalienable") ~ "Inalienable",
    )
  ) %>%
  filter(!is.na(Alienability)) %>%
  # pivot to wider
  select(LID, NPStructureID, Alienability) %>%
  distinct() %>%
  mutate(value = TRUE) %>%
  pivot_wider(
    names_from = Alienability,
    names_glue = "NPAlienabilityConstraintIs{Alienability}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)

# Locus (binned)
locus_binned_presence <- NPStructure %>%
  # join with the formative locus data
  select(LID, NPStructureID, NPStructureMarkerID) %>%
  unnest(NPStructureMarkerID) %>%
  left_join(
    select(GrammaticalMarkers, MarkerID, MarkerLocusBinned6),
    by = c("NPStructureMarkerID" = "MarkerID")
  ) %>%
  select(-NPStructureMarkerID) %>%
  # mark presence
  mutate(value = TRUE) %>%
  # compelte for all factor levels
  complete(LID, NPStructureID, MarkerLocusBinned6, fill  = list(value = FALSE)) %>%
  # remove zero marking
  filter(!is.na(MarkerLocusBinned6), MarkerLocusBinned6 != "Ø") %>%
  distinct() %>%
  # pivot to wider
  pivot_wider(
    names_from = MarkerLocusBinned6,
    names_glue = "NPLocusOfMarkingBinned6Is{str_remove_all(MarkerLocusBinned6, ' ')}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)

# Locus
locus_presence <- NPStructure %>%
  # join with the formative locus data
  select(LID, NPStructureID, NPStructureMarkerID) %>%
  unnest(NPStructureMarkerID) %>%
  left_join(
    select(GrammaticalMarkers, MarkerID, MarkerLocus),
    by = c("NPStructureMarkerID" = "MarkerID")
  ) %>%
  select(-NPStructureMarkerID) %>%
  # mark presence
  mutate(value = TRUE) %>%
  # compelte for all factor levels
  complete(LID, NPStructureID, MarkerLocus, fill  = list(value = FALSE)) %>%
  distinct() %>%
  # remove zero marking
  filter(!is.na(MarkerLocus)) %>%
  pivot_wider(
    names_from = MarkerLocus,
    names_glue = "NPLocusOfMarkingIs{str_remove_all(MarkerLocus, ' ')}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)

# marker source
marker_source <- NPStructure %>%
  # join with the formative locus data
  select(LID, NPStructureID, NPStructureMarkerID) %>%
  unnest(NPStructureMarkerID) %>%
  left_join(
    select(GrammaticalMarkers, MarkerID, MarkerSource),
    by = c("NPStructureMarkerID" = "MarkerID")
  ) %>%
  select(-NPStructureMarkerID) %>%
  distinct() %>%
  # mark presence
  mutate(value = TRUE) %>%
  # compelte for all factor levels
  complete(NPStructureID, MarkerSource, fill  = list(value = FALSE)) %>%
  # remove zero marking
  filter(!is.na(MarkerSource)) %>%
  mutate(MarkerSource=to_camel_case(MarkerSource)) %>%
  pivot_wider(
    names_from = MarkerSource,
    names_glue = "NPMarkerSourceIs{MarkerSource}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)

# marker morpheme type
marker_type <- NPStructure %>%
  # join with the formative locus data
  select(LID, NPStructureID, NPStructureMarkerID) %>%
  unnest(NPStructureMarkerID) %>%
  left_join(
    select(GrammaticalMarkers, MarkerID, MarkerMorphemeType),
    by = c("NPStructureMarkerID" = "MarkerID")
  ) %>%
  select(-NPStructureMarkerID) %>%
  distinct() %>%
  # mark presence
  mutate(value = TRUE) %>%
  # compelte for all factor levels
  complete(NPStructureID, MarkerMorphemeType, fill  = list(value = FALSE)) %>%
  # remove zero marking
  filter(!is.na(MarkerMorphemeType)) %>%
  mutate(MarkerMorphemeType=to_camel_case(MarkerMorphemeType)) %>%
  pivot_wider(
    names_from = MarkerMorphemeType,
    names_glue = "NPMarkerTypeIsConstrainedTo{MarkerMorphemeType}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)


# Fusion
fusion_binned_presence <- NPStructure %>%
  # join with the formative locus data
  select(LID, NPStructureID, NPStructureMarkerID) %>%
  unnest(NPStructureMarkerID) %>%
  left_join(
    select(GrammaticalMarkers, MarkerID, MarkerFusionBinned6),
    by = c("NPStructureMarkerID" = "MarkerID")
  ) %>%
  select(-NPStructureMarkerID) %>%
  filter(!is.na(MarkerFusionBinned6)) %>%
  distinct() %>%
  # pivot to wider
  mutate(value = TRUE) %>%
  mutate(MarkerFusionBinned6=to_camel_case(MarkerFusionBinned6)) %>%
  pivot_wider(
    names_from = MarkerFusionBinned6,
    names_glue = "NPMarkerFusionBinned6Is{MarkerFusionBinned6}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)

# Fusion
fusion_presence <- NPStructure %>%
  # join with the formative locus data
  select(LID, NPStructureID, NPStructureMarkerID) %>%
  unnest(NPStructureMarkerID) %>%
  left_join(
    select(GrammaticalMarkers, MarkerID, MarkerFusion),
    by = c("NPStructureMarkerID" = "MarkerID")
  ) %>%
  select(-NPStructureMarkerID) %>%
  filter(!is.na(MarkerFusion)) %>%
  distinct() %>%
  # pivot to wider
  mutate(value = TRUE) %>%
  mutate(MarkerFusion=to_camel_case(MarkerFusion)) %>%
  pivot_wider(
    names_from = MarkerFusion,
    names_glue = "NPMarkerFusionIs{MarkerFusion}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)

# Dependent semantics constraints
dep_sem_constraints_presence <- NPStructure %>%
  # take dependent semantics constraints
  select(LID, NPStructureID, sem_constraint = NPStructureDependentSemConstraints) %>%
  filter(!is.na(sem_constraint), sem_constraint != "neutral", sem_constraint != "default") %>%
  # make nice lookign labels
  mutate(sem_constraint = to_camel_case(sem_constraint)) %>%
  # pivot to wider
  mutate(value = TRUE) %>%
  pivot_wider(
    names_from = sem_constraint,
    names_glue = "NPDependentIsConstrainedTo{sem_constraint}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)



# Dependent PoS constraints
dep_PoS_constraints_presence <- NPStructure %>%
  # take dependent PoS constraints
  select(LID, NPStructureID, pos_constraint = NPStructureDependentPoSConstraints) %>%
  unnest(pos_constraint) %>%
  # make nice lookign labels
  mutate(pos_constraint = to_camel_case(pos_constraint)) %>%
  # pivot to wider
  mutate(value = TRUE) %>%
  pivot_wider(
    names_from = pos_constraint,
    names_glue = "NPDependentIsConstrainedTo{pos_constraint}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)



# Head semantics constraints
head_sem_constraints_presence <- NPStructure %>%
  # take dependent semantics constraints
  select(LID, NPStructureID, NPStructureHeadSemConstraints) %>%
  unnest(NPStructureHeadSemConstraints) %>%
  select(LID, NPStructureID, sem_constraint = NPStructureHeadSemConstraints) %>%
  # clean up the constraints
  filter(!is.na(sem_constraint), sem_constraint != "neutral", sem_constraint != "default") %>%
  # make nice lookign labels
  mutate(sem_constraint = to_camel_case(sem_constraint)) %>%
  # pivot to wider
  mutate(value = TRUE) %>%
  pivot_wider(
    names_from = sem_constraint,
    names_glue = "NPHeadIsConstrainedTo{sem_constraint}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)


# Head semantics constraints (macrocategory)
head_macrosem_constraints_presence <- NPStructure %>%
  # take dependent semantics constraints
  select(LID, NPStructureID, NPStructureHeadSemConstraints) %>%
  unnest(NPStructureHeadSemConstraints) %>%
  select(LID, NPStructureID, sem_constraint = NPStructureHeadSemConstraintsBinned) %>%
  # clean up the constraints
  filter(!is.na(sem_constraint)) %>%
  distinct() %>%
  # make nice lookign labels
  mutate(sem_constraint = to_camel_case(sem_constraint)) %>%
  # pivot to wider
  mutate(value = TRUE) %>%
  pivot_wider(
    names_from = sem_constraint,
    names_glue = "NPHeadIsConstrainedTo{sem_constraint}",
    values_fill = FALSE
  ) %>%
  arrange(LID, NPStructureID)

# assemble the wide table
NPStructurePresence <- NPStructure %>%
  select(LID, Language, NPStructureID) %>%
  left_join(agreement_presence, by = c("LID", "NPStructureID")) %>%
  left_join(alienability_presence, by = c("LID", "NPStructureID")) %>%
  left_join(marker_source, by = c("LID", "NPStructureID")) %>%
  left_join(marker_type, by = c("LID", "NPStructureID")) %>%
  left_join(locus_presence, by = c("LID", "NPStructureID")) %>%
  left_join(locus_binned_presence, by = c("LID", "NPStructureID")) %>%
  left_join(fusion_presence, by = c("LID", "NPStructureID")) %>%
  left_join(fusion_binned_presence, by = c("LID", "NPStructureID")) %>%
  left_join(dep_sem_constraints_presence, by = c("LID", "NPStructureID")) %>%
  left_join(dep_PoS_constraints_presence, by = c("LID", "NPStructureID")) %>%
  left_join(head_sem_constraints_presence, by = c("LID", "NPStructureID")) %>%
  left_join(head_macrosem_constraints_presence, by = c("LID", "NPStructureID")) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything()) %>%
  arrange(LID, Language)



descriptor <- describe_data(
  ptype = tibble(),
  description = "Per-language aggregations of NP properties",
  computed = "NPStructurePerLanguage.R",
  fields = list(
    LID = .metadata$Register$fields$LID,
    Language = .metadata$Register$fields$Language,
    Glottocode = .metadata$Register$fields$Glottocode,
    NPHasAgreement = describe_data(
      ptype = logical(),
      computed = "NPStructurePerLanguage.R",
      description = "
        NPs with some kind of agreement (on the head or dependent or both)
        (see Riessler 2016, Adjective Attribution, Language Sciences Press)
      "
    ),
    NPHasConstructMarkers = describe_data(
      ptype = logical(),
      computed = "NPStructurePerLanguage.R",
      description = "
        NPs with some kind of (anti-)construct marker on the head or dependent,
        i.e. a marker which signals the dependency of an embedded element and
        the head but results neither from agreement nor from case assignment
        (e.g. part of speech markers, attributive particles, linkers, construct
        markers etc.) (see Riessler 2016, Adjective Attribution, Language Sciences Press)
      "
    ),
    NPHasGovernment = describe_data(
      ptype = integer(),
      computed = "NPStructurePerLanguage.R",
      description = "
        NPs with some kind of marker which is governed/assigned by the head
        (see Riessler 2016, Adjective Attribution, Language Sciences Press)
      "
    ),
    NPHasOvertMarking = describe_data(
      ptype = logical(),
      computed = "NPStructurePerLanguage.R",
      description = "NPs with some overt marking"
    ),
    NPHasAdjAgreement = describe_data(
      ptype = logical(),
      computed = "NPStructurePerLanguage.R",
      description = "
        Adjective attribution with some kind of agreement (on the head or
        dependent or both) (see Riessler 2016, Adjective Attribution,
        Language Sciences Press)
      "
    ),
    NPHasAdjConstructMarkers = describe_data(
      ptype = logical(),
      computed = "NPStructurePerLanguage.R",
      description = "
        Adjective attribution with some kind of (anti-)construct marker on
        the head or dependent, i.e. a marker which signals the dependency of
        an embedded element and the head but results neither from agreement
        nor from case assignment (e.g. part of speech markers, attributive
        particles, linkers, construct markers etc.) (see Riessler 2016, Adjective
        Attribution, Language Sciences Press)
      "
    ),
    NPHasAdjGovernment = describe_data(
      ptype = integer(),
      computed = "NPStructurePerLanguage.R",
      description = "
        Adjective attribution with some kind of marker which is governed/assigned
        by the head (see Riessler 2016, Adjective Attribution, Language Sciences Press)
      "
    ),
    NPHasAdjOvertMarking = describe_data(
      ptype = logical(),
      computed = "NPStructurePerLanguage.R",
      description = "Adjective attribution with some overt marking"
    )
  )
)

export_dataset("NPStructurePerLanguage", NPStructurePerLanguage, descriptor, c("PerLanguageSummaries", "NP"))



descriptor <- describe_data(
  ptype = tibble(),
  description = "Per-language presence of NP properties",
  computed = "NPStructurePerLanguage.R",
  fields = c(
    .metadata$Register$fields[c("LID", "Language", "Glottocode")],
    map(names(NPStructurePresence)[-(1:3)], ~ {
      describe_data(
        ptype = logical(),
        computed = "NPStructurePerLanguage.R",
        description = "<pending>"
      )
    }) %>% set_names(names(NPStructurePresence)[-(1:3)])
  )
)

export_dataset("NPStructurePresence", NPStructurePresence, descriptor, c("PerLanguageSummaries", "NP"))
