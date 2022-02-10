# Alignment computations
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


#  █████╗  ██████╗ ██████╗ ███████╗███████╗███╗   ███╗███████╗███╗   ██╗████████╗
# ██╔══██╗██╔════╝ ██╔══██╗██╔════╝██╔════╝████╗ ████║██╔════╝████╗  ██║╚══██╔══╝
# ███████║██║  ███╗██████╔╝█████╗  █████╗  ██╔████╔██║█████╗  ██╔██╗ ██║   ██║
# ██╔══██║██║   ██║██╔══██╗██╔══╝  ██╔══╝  ██║╚██╔╝██║██╔══╝  ██║╚██╗██║   ██║
# ██║  ██║╚██████╔╝██║  ██║███████╗███████╗██║ ╚═╝ ██║███████╗██║ ╚████║   ██║
# ╚═╝  ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝   ╚═╝
#
# We have agreement if either
#
#  a) syntactic roles are marked in VerbSyntehsis
#  b) agreement is marked as present in GrammaticalRelationCoverage
#  c) we have head-marking locus for AGR in LocusOfMarking
#
# In case of a conflict we report an NA
MorphologyPerLanguage_Agreement <- list(
    # presence of agreement in VerbSynthesis
    filter(VerbSynthesis, IsVerbInflectionSurveyComplete) %>%
    transmute(LID = LID, HasAgreement_VerbSynthesis = list_sizes(VerbAgreement) > 0),
    # presence of agreement in GrammaticalRelationCoverage
    unnest(GrammaticalRelationCoverage, Agreement) %>%
    transmute(LID = LID, HasAgreement_GR = HasAgreement),
    # presence of agreement in LocusOfMarking (head marking of syntactic roles)
    unnest(LocusOfMarkingPerMicrorelation, LocusOfMarking) %>%
    filter(IsDefaultLocusOfMarking) %>%
    select(LID, Macrorelation, LocusOfMarkingBinned5) %>%
    group_by(LID) %>%
    summarize(
      HasAgreement_Locus = any(
        Macrorelation %in% c("S", "A", "P") & LocusOfMarkingBinned5 %in% c("2", "H"),
        na.rm = TRUE
      ),
      .groups = "drop"
    )
  ) %>%
  reduce(full_join, by = "LID") %>%
  # combine the data
  transmute(
    LID = LID,
    HasAnyVerbAgreement = pmap_lgl(cur_data(), function(LID, ...) {
      # collect the unique values (drop the NAs)
      values <- c(...)
      value <- unique(values[!is.na(values)])

      # report NA if we have contradictions
      if (length(value) != 1L) NA else value
    })
  )


# ███╗   ███╗ ██████╗ ██████╗ ██████╗ ██╗  ██╗███████╗███╗   ███╗███████╗███████╗
# ████╗ ████║██╔═══██╗██╔══██╗██╔══██╗██║  ██║██╔════╝████╗ ████║██╔════╝██╔════╝
# ██╔████╔██║██║   ██║██████╔╝██████╔╝███████║█████╗  ██╔████╔██║█████╗  ███████╗
# ██║╚██╔╝██║██║   ██║██╔══██╗██╔═══╝ ██╔══██║██╔══╝  ██║╚██╔╝██║██╔══╝  ╚════██║
# ██║ ╚═╝ ██║╚██████╔╝██║  ██║██║     ██║  ██║███████╗██║ ╚═╝ ██║███████╗███████║
# ╚═╝     ╚═╝ ╚═════╝ ╚═╝  ╚═╝╚═╝     ╚═╝  ╚═╝╚══════╝╚═╝     ╚═╝╚══════╝╚══════╝
#
# Morpheme type summary (e.g. presence of suffixes) comes from MorphemeClasses, or,
# if the former has no data, from GrammaticalMarkers
MorphologyPerLanguage_Morphemes <- rows_upsert(
    # MorphemeClasses morpheme summaries per language
    MorphemeClasses %>%
    group_by(LID) %>%
    summarize(
      # number of morpheme classes
      MorphemeClassesCount = n(),
      # number of morpheme classes that occur before the respective phonological host
      PreposedMorphemeClassesCount = sum(MorphemePosition %in% "prae"),
      # number of morpheme classes that occur after the respective phonological host
      PostposedMorphemeClassesCount = sum(MorphemePosition %in% "post"),
      # are there any prefixes
      HasAnyPrefixes = any(
        MorphemePosition == "prae" &
        MorphemeType == "formative" &
        MorphemeHostRestriction == "restricted",
        na.rm = TRUE
      ),
      # are there any suffixes
      HasAnySuffixes = any(
        MorphemePosition == "post" &
        MorphemeType == "formative" &
        MorphemeHostRestriction == "restricted",
        na.rm = TRUE
      ),
      # are there any infixes
      HasAnyInfixes = any(
        MorphemePosition == "in" &
        MorphemeType == "formative" &
        MorphemeHostRestriction == "restricted",
        na.rm = TRUE
      ),
      # are there any proclitics
      HasAnyProclitics = any(
        MorphemePosition == "prae" &
        MorphemeType == "formative" &
        MorphemeHostRestriction != "restricted",
        na.rm = TRUE
      ),
      # are there any enclitics
      HasAnyEnclitics = any(
        MorphemePosition == "post" &
        MorphemeType == "formative" &
        MorphemeHostRestriction != "restricted",
        na.rm = TRUE
      ),
      # are there any enclitics
      HasAnyEndoclitics = any(
        MorphemePosition == "in" &
        MorphemeType == "formative" &
        MorphemeHostRestriction != "restricted",
        na.rm = TRUE
      ),
      # are there any preposed formatives (in general)
      HasAnyPreposedFormatives = any(
        MorphemePosition == "prae" &
        MorphemeType == "formative",
        na.rm = TRUE
      ),
      # are there any postposed formatives (in general)
      HasAnyPostposedFormatives = any(
        MorphemePosition == "post" &
        MorphemeType == "formative",
        na.rm = TRUE
      ),
      # are there any infix-like formatives (in general)
      HasAnyInterposedFormatives = any(
        MorphemePosition == "in" &
        MorphemeType == "formative",
        na.rm = TRUE
      ),
      .groups = "drop"
    ),
    # GrammaticalMarkers morpheme summaries per language
    GrammaticalMarkers %>%
    group_by(LID) %>%
    summarize(
      # are there any prefixes
      HasAnyPrefixes = any(
        MarkerPosition == "prae" &
        MarkerMorphemeType == "formative" &
        MarkerHostRestriction == "restricted",
        na.rm = TRUE
      ),
      # are there any suffixes
      HasAnySuffixes = any(
        MarkerPosition == "post" &
        MarkerMorphemeType == "formative" &
        MarkerHostRestriction == "restricted",
        na.rm = TRUE
      ),
      # are there any infixes
      HasAnyInfixes = any(
        MarkerPosition == "in" &
        MarkerMorphemeType == "formative" &
        MarkerHostRestriction == "restricted",
        na.rm = TRUE
      ),
      # are there any proclitics
      HasAnyProclitics = any(
        MarkerPosition == "prae" &
        MarkerMorphemeType == "formative" &
        MarkerHostRestriction != "restricted",
        na.rm = TRUE
      ),
      # are there any enclitics
      HasAnyEnclitics = any(
        MarkerPosition == "post" &
        MarkerMorphemeType == "formative" &
        MarkerHostRestriction != "restricted",
        na.rm = TRUE
      ),
      # are there any enclitics
      HasAnyEndoclitics = any(
        MarkerPosition == "in" &
        MarkerMorphemeType == "formative" &
        MarkerHostRestriction != "restricted",
        na.rm = TRUE
      ),
      # are there any preposed formatives (in general)
      HasAnyPreposedFormatives = any(
        MarkerPosition == "prae" &
        MarkerMorphemeType == "formative",
        na.rm = TRUE
      ),
      # are there any postposed formatives (in general)
      HasAnyPostposedFormatives = any(
        MarkerPosition == "post" &
        MarkerMorphemeType == "formative",
        na.rm = TRUE
      ),
      # are there any infix-like formatives (in general)
      HasAnyInterposedFormatives = any(
        MarkerPosition == "in" &
        MarkerMorphemeType == "formative",
        na.rm = TRUE
      ),
      .groups = "drop"
    ),
    by = "LID"
  )


# ███████╗██╗     ███████╗██╗  ██╗██╗██╗   ██╗██╗████████╗██╗   ██╗
# ██╔════╝██║     ██╔════╝╚██╗██╔╝██║██║   ██║██║╚══██╔══╝╚██╗ ██╔╝
# █████╗  ██║     █████╗   ╚███╔╝ ██║██║   ██║██║   ██║    ╚████╔╝
# ██╔══╝  ██║     ██╔══╝   ██╔██╗ ██║╚██╗ ██╔╝██║   ██║     ╚██╔╝
# ██║     ███████╗███████╗██╔╝ ██╗██║ ╚████╔╝ ██║   ██║      ██║
# ╚═╝     ╚══════╝╚══════╝╚═╝  ╚═╝╚═╝  ╚═══╝  ╚═╝   ╚═╝      ╚═╝
#

# Information on marker flexivity comes from GrammaticalMarkers
MorphologyPerLanguage_Flexivity <- GrammaticalMarkers %>%
  group_by(LID) %>%
  summarize(
    # any flexivity
    HasAnyFlexivity = any(
      MarkerIsFlexive,
      na.rm = TRUE
    ),
    # any flexivity in the nominal domain
    HasAnyFlexivityForNouns = any(
      MarkerIsFlexive & MarkerClausalDomain %in% "argument",
      na.rm = TRUE
    ),
    # any flexivity in the verbal domain
    HasAnyFlexivityForVerbs = any(
      MarkerIsFlexive & MarkerClausalDomain %in% "predicate",
      na.rm = TRUE
    ),
    # any lexical flexivity
    HasAnyLexicalFlexivity = any(
      MarkerHasLexicalFlexivity,
      na.rm = TRUE
    ),
    # any lexical flexivity in the nominal domain
    HasAnyLexicalFlexivityForNouns = any(
      MarkerHasLexicalFlexivity & MarkerClausalDomain %in% "argument",
      na.rm = TRUE
    ),
    # any lexical flexivity in the verbal domain
    HasAnyLexicalFlexivityForVerbs = any(
      MarkerHasLexicalFlexivity & MarkerClausalDomain %in% "predicate",
      na.rm = TRUE
    ),
    .groups = "drop"
  )

# ██████╗  ██████╗ ██╗  ██╗   ██╗███████╗██╗  ██╗██████╗
# ██╔══██╗██╔═══██╗██║  ╚██╗ ██╔╝██╔════╝╚██╗██╔╝██╔══██╗
# ██████╔╝██║   ██║██║   ╚████╔╝ █████╗   ╚███╔╝ ██████╔╝
# ██╔═══╝ ██║   ██║██║    ╚██╔╝  ██╔══╝   ██╔██╗ ██╔═══╝
# ██║     ╚██████╔╝███████╗██║   ███████╗██╔╝ ██╗██║
# ╚═╝      ╚═════╝ ╚══════╝╚═╝   ╚══════╝╚═╝  ╚═╝╚═╝
#
# Information on marker polyexponence comes from GrammaticalMarkers

MorphologyPerLanguage_Polyexponence <- GrammaticalMarkers %>%
  group_by(LID) %>%
  summarize(
    HasAnyPolyexponence = any(
      MarkerExpressesMultipleCategories,
      na.rm = TRUE
    ),
    # any polyxeponence in the nominal domain
    HasAnyPolyexponenceForNouns = any(
      MarkerExpressesMultipleCategories & MarkerClausalDomain %in% "argument",
      na.rm = TRUE
    ),
    # any polyxeponence in the verbal domain
    HasAnyPolyexponenceForVerbs = any(
      MarkerExpressesMultipleCategories & MarkerClausalDomain %in% "predicate",
      na.rm = TRUE
    ),
    .groups = "drop"
  )



# ███████╗███████╗██████╗  ██████╗ ███████╗
# ╚══███╔╝██╔════╝██╔══██╗██╔═══██╗██╔════╝
#   ███╔╝ █████╗  ██████╔╝██║   ██║███████╗
#  ███╔╝  ██╔══╝  ██╔══██╗██║   ██║╚════██║
# ███████╗███████╗██║  ██║╚██████╔╝███████║
# ╚══════╝╚══════╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝
#
# Information on zero opposition in verbal domain comes from GrammaticalMarkers
MorphologyPerLanguage_Zeros <- GrammaticalMarkers %>%
  # only verbal domain
  filter(MarkerClausalDomain %in% "predicate") %>%
  group_by(LID) %>%
  summarize(
    # number of slots with zero opposition
    VerbSlotsInOppositionToZeroCount = sum(
      MarkerIsInOppositionToZero
    ),
    # number of preposed slots with zero opposition
    PreposedVerbSlotsInOppositionToZeroCount = sum(
      MarkerIsInOppositionToZero & MarkerHasPreposedExponent
    ),
    # number of postposed slots with zero opposition
    PostposedVerbSlotsInOppositionToZeroCount = sum(
      MarkerIsInOppositionToZero & MarkerHasPostposedExponent
    ),
    .groups = "drop"
  )


#  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
# ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
# ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
# ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
# ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
#  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝
#
# Combine all the parts

MorphologyPerLanguage <- list(
    MorphologyPerLanguage_Agreement,
    MorphologyPerLanguage_Morphemes,
    MorphologyPerLanguage_Flexivity,
    MorphologyPerLanguage_Polyexponence,
    MorphologyPerLanguage_Zeros
  ) %>%
  reduce(full_join, by = "LID")

# add language info
MorphologyPerLanguage <- right_join(
  select(Register, LID, Glottocode, Language),
  MorphologyPerLanguage,
  by = "LID"
)



descriptor <- describe_data(
  ptype = tibble(),
  description = "Per-language summary of morphological properties",
  computed = "MorphologyPerLanguage.R",
  fields = list(
    LID = .metadata$Register$fields$LID,
    Glottocode = .metadata$Register$fields$Glottocode,
    Language = .metadata$Register$fields$Language,
    HasAnyVerbAgreement = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Does the language have some kind of verb agreement (including pronominal/anaphoric
        agreement), based on our records in the Synthesis, Locus and Grammatical Relations
        modules"
    ),
    MorphemeClassesCount = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Total number of distinct morpheme classes in the language, i.e. morphological elements
        that are distinct in position, host restrictions or formative vs. word status
      "
    ),
    PreposedMorphemeClassesCount = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Number of morpheme classes that appear before their phonological host
        (with distinct host restrictions or word vs formative status)
      "
    ),
    PostposedMorphemeClassesCount = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Number of morpheme classes that appear after their phonological host
        (with distinct host restrictions or word vs formative status)
      "
    ),
    HasAnyPrefixes = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "Are prefixes (restricted preposed formatives) present in the language"
    ),
    HasAnySuffixes = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "Are suffixes (restricted postposed formatives) present in the language"
    ),
    HasAnyInfixes = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "Are infixes (restricted interposed formatives) present in the language"
    ),
    HasAnyProclitics = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Are proclitics (unrestricted or semirestricted preposed formatives) present
        in the language
      "
    ),
    HasAnyEnclitics = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Are enclitics (unrestricted or semirestricted postposed formatives) present
        in the language
      "
    ),
    HasAnyEndoclitics = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Are endoclitics (unrestricted or semirestricted interposed formatives) present
        in the language
      "
    ),
    HasAnyPreposedFormatives = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "Are any preposed formatives present in the language"
    ),
    HasAnyPostposedFormatives = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "Are any postposed formatives present in the language"
    ),
    HasAnyInterposedFormatives = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "Are any interposed formatives present in the language"
    ),
    HasAnyFlexivity = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of flexivity in any part of morphology, i.e. some kind of allomorphy in stems
         and/or affixes, lexically or grammatically conditioned
      "
    ),
    HasAnyFlexivityForNouns = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of flexivity in nominal morphology (case, possession, number etc.), i.e.
        some kind of allomorphy in stems and/or affixes, lexically or grammatically conditioned
      "
    ),
    HasAnyFlexivityForVerbs = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of flexivity in verbal morphology (tense, agreement etc.), i.e. some kind of
        allomorphy in stems and/or affixes, lexically or grammatically conditioned
      "
    ),
    HasAnyLexicalFlexivity = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of lexical flexivity in any morphology, i.e. some kind of lexically conditioned
        allomorphy in stems and/or affixes anywhere nominal or verbal morphology
      "
    ),
    HasAnyLexicalFlexivityForNouns = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of lexical flexivity in nominal morphology (case, possession, number etc.), i.e.
        some kind of lexically conditioned allomorphy in stems and/or affixes anywhere nominal
        morphology
      "
    ),
    HasAnyLexicalFlexivityForVerbs = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of lexical flexivity in verbal morphology (tense, agreement etc.), i.e.
        some kind of lexically conditioned allomorphy in stems and/or affixes anywhere in
        verbal morphology
      "
    ),
    HasAnyPolyexponence = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of polyexponence somewhere in any morphology: whether or not markers express
        more than one category
      "
    ),
    HasAnyPolyexponenceForNouns = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of polyexponence somewhere in nominal morphology (case, possession, number etc.):
        whether or not markers express more than one category
      "
    ),
    HasAnyPolyexponenceForVerbs = describe_data(
      ptype = logical(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Some kind of polyexponence somewhere verbal morphology (tense, agreement etc.):
        whether or not markers express more than one category
      "
    ),
    VerbSlotsInOppositionToZeroCount = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Number of slots in verbal morphology where one or more overt markers are in
        opposition to markers with zero exponence (data from VerbSynthesis)"
    ),
    PreposedVerbSlotsInOppositionToZeroCount = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Number of slots in pre-stem morphology where one or more overt markers are in
        opposition to markers with zero exponence (data from VerbSynthesis)"
    ),
    PostposedVerbSlotsInOppositionToZeroCount = describe_data(
      ptype = integer(),
      computed = "MorphologyPerLanguage.R",
      description = "
        Number of slots in post-stem morphology where one or more overt markers are in
        opposition to markers with zero exponence (data from VerbSynthesis)
      "
    )
  )
)

export_dataset("MorphologyPerLanguage", MorphologyPerLanguage, descriptor, c("PerLanguageSummaries", "Morphology"))
