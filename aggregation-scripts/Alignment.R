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

# we are only interested in a subset of relevant roles...
# this constant also gives us a sorting order
RELEVANT_ROLES <- c("S", "Atr", "Aditr", "P", "T", "G")

#   ██████╗ ██████╗     ██████╗  █████╗ ████████╗ █████╗
#  ██╔════╝ ██╔══██╗    ██╔══██╗██╔══██╗╚══██╔══╝██╔══██╗
#  ██║  ███╗██████╔╝    ██║  ██║███████║   ██║   ███████║
#  ██║   ██║██╔══██╗    ██║  ██║██╔══██║   ██║   ██╔══██║
#  ╚██████╔╝██║  ██║    ██████╔╝██║  ██║   ██║   ██║  ██║
#   ╚═════╝ ╚═╝  ╚═╝    ╚═════╝ ╚═╝  ╚═╝   ╚═╝   ╚═╝  ╚═╝
#
# We are only interested in a subset of GR that code case marking.
#
# Of those, we only want to look at entries that have been fully coded
# for the relevant set of predicates. We can use the GR coverage data to
# build lists of fully coded GRs and select only those from the full dataset.
#
# The GR coverage table only selects languages that have fully coded case for
# default transitive predicates. In addition, the colums
# DitransitivePredicatesCoded and OtherPredicatesCoded indicate whether other
# predicate types are coded as well

# case coverage per language
GR_coverage_case <- GrammaticalRelationCoverage %>%
  # extract the nested case coverage information into regular columns
  unnest(Case) %>%
  # only take languages with coded case
  filter(
    # case marking is present in the language
    HasCase,
    # default predicates are fully coded
    IsCaseCodedForDefaultPredicates
  ) %>%
  transmute(
    LID = LID,
    # GR type is case
    SelectorType = "case marker or adposition",
    # check if ditransitives are coded
    DitransitivePredicatesCoded = IsCaseCodedForDitransitivePredicates,
    # check if non-default predicates are coded
    OtherPredicatesCoded = IsCaseCodedForOtherPredicates
  )

# agreement coverage per language
GR_coverage_agreement <- GrammaticalRelationCoverage %>%
  # extract the nested case coverage information into regular columns
  unnest(Agreement) %>%
  # only take languages with coded case
  filter(
    # case marking is present in the language
    HasAgreement,
    # default predicates are fully coded
    IsAgreementCodedForDefaultPredicates
  ) %>%
  transmute(
    LID = LID,
    # GR type is case
    SelectorType = "agreement (grammatical)",
    # check if ditransitives are coded
    DitransitivePredicatesCoded = IsAgreementCodedForDitransitivePredicates,
    # check if non-default predicates are coded
    OtherPredicatesCoded = IsAgreementCodedForOtherPredicates
  )

# agreement coverage per language
GR_coverage_per_marker_agreement <- GrammaticalRelationCoverage %>%
  # extract the nested case coverage information into regular columns
  unnest(Agreement) %>%
  # only take languages with coded case
  filter(
    # agreement marking is present in the language
    HasAgreement,
    # agreement per marker is coded
    IsAgreementMarkersCodedForDefaultPredicates
  ) %>%
  transmute(
    LID = LID,
    # GR type is case
    SelectorType = "agreement (marker)",
    # ditransitives are never coded
    DitransitivePredicatesCoded = FALSE,
    # check if non-default predicates are coded
    OtherPredicatesCoded = IsAgreementMarkersCodedForOtherPredicates
  )

# select the relevant GRs
GR <- GrammaticalRelationsRaw %>%
  # only take GRs wher the survey is complete
  # !QUESTION: shold not all covered GR be "done" anyway?
  filter(IsSelectorSurveyComplete) %>%
  select(-IsSelectorSurveyComplete) %>%
  # only take the fully coded GRs as indicated by coverage
  inner_join(
    bind_rows(
      GR_coverage_case,
      GR_coverage_agreement,
      GR_coverage_per_marker_agreement
    ),
    by = c("LID", "SelectorType")
  ) %>%
  # SelectedArguments is a nested table, unnest it
  unnest(SelectedArguments, keep_empty = TRUE) %>%
  # only active diathesis
  filter(is.na(DiathesisCondition) | DiathesisCondition %in% "ACT") %>%
  select(-DiathesisCondition) %>%
  # no syntactic domain
  filter(is.na(SyntacticDomainCondition)) %>%
  # only default semantics
  filter(SemanticCondition %in% "default") %>%
  select(-SemanticCondition) %>%
  # we are only interested in a subset of roles
  filter(
    is.na(SelectedRole) | SelectedRole %in% RELEVANT_ROLES,
    is.na(CoargumentRoleCondition) | CoargumentRoleCondition %in% RELEVANT_ROLES
  ) %>%
  # recode the roles
  mutate(
    SelectedRole = factor(SelectedRole, RELEVANT_ROLES),
    CoargumentRoleCondition = factor(CoargumentRoleCondition, RELEVANT_ROLES),
  ) %>%
  # only take predicate classes that are fully coded
  filter(
    # default transitive preducates are always selected
    PredicateClassID %in% c(1L, 2L) |
    # ditransitives are selected if they are coded
    (PredicateClassID %in% c(3L) & DitransitivePredicatesCoded) |
    # others are selected if they are coded
    OtherPredicatesCoded
  ) %>%
  # remove the auxiliary fields
  select(
    -DitransitivePredicatesCoded,
    -OtherPredicatesCoded
  ) %>%
  # link with the marker slot (only for agreement per morpheme!)
  left_join(
    GrammaticalMarkers %>%
    select(MarkerID, MarkerSlot = MarkerSlot) %>%
    mutate(SelectorType = "agreement (marker)"),
    by = c("MarkerID", "SelectorType")
  )

# save prototypes of roles and references — will be needed later
ROLES <- vec_unique(GR$SelectedRole)
REFERENCES <- vec_unique(GR$ReferentialCondition)

#  ██╗   ██╗ █████╗ ██╗     ███████╗███╗   ██╗ ██████╗███████╗
#  ██║   ██║██╔══██╗██║     ██╔════╝████╗  ██║██╔════╝██╔════╝
#  ██║   ██║███████║██║     █████╗  ██╔██╗ ██║██║     █████╗
#  ╚██╗ ██╔╝██╔══██║██║     ██╔══╝  ██║╚██╗██║██║     ██╔══╝
#   ╚████╔╝ ██║  ██║███████╗███████╗██║ ╚████║╚██████╗███████╗
#    ╚═══╝  ╚═╝  ╚═╝╚══════╝╚══════╝╚═╝  ╚═══╝ ╚═════╝╚══════╝
#
# Compute predicate valence from the described arguments
PredClasses <- GR %>%
  select(
    LID,
    SelectorType,
    PredicateClassID,
    SelectedRole
  ) %>%
  distinct() %>%
  # get the expected roles given an observed role
  left_join(
    tribble(
      ~ SelectedRole,   ~ ExpectedRole,
      "S",      vec_cast("S", ROLES),
      "Atr",    vec_cast("Atr", ROLES),
      "Atr",    vec_cast("P", ROLES),
      "P",      vec_cast("Atr", ROLES),
      "P",      vec_cast("P", ROLES),
      "Aditr",  vec_cast("Aditr", ROLES),
      "Aditr",  vec_cast("T", ROLES),
      "Aditr",  vec_cast("G", ROLES),
      "T",      vec_cast("Aditr", ROLES),
      "T",      vec_cast("T", ROLES),
      "T",      vec_cast("G", ROLES),
      "G",      vec_cast("Aditr", ROLES),
      "G",      vec_cast("T", ROLES),
      "G",      vec_cast("G", ROLES)
    ),
    by = "SelectedRole"
  ) %>%
  # summary per predicate class
  group_by(LID, SelectorType, PredicateClassID) %>%
  summarize(
    Valence = case_when(
      all(SelectedRole %in% "S") ~ 1L,
      all(SelectedRole %in% c("Atr", "P")) ~ 2L,
      all(SelectedRole %in% c("Aditr", "T", "G")) ~ 3L,
    ),
    IsValid = !is.na(Valence),
    Roles = list(sort(unique(SelectedRole))),
    ExpectedRoles = list(sort(unique(ExpectedRole))),
    .groups = "drop"
  )

# we only want to work with valid predicate classes
InvalidPredClasses <- filter(PredClasses, !IsValid)
PredClasses <- filter(PredClasses, IsValid)



#   ██████╗ ██████╗ ███╗   ██╗██████╗ ██╗████████╗██╗ ██████╗ ███╗   ██╗███████╗
#  ██╔════╝██╔═══██╗████╗  ██║██╔══██╗██║╚══██╔══╝██║██╔═══██╗████╗  ██║██╔════╝
#  ██║     ██║   ██║██╔██╗ ██║██║  ██║██║   ██║   ██║██║   ██║██╔██╗ ██║███████╗
#  ██║     ██║   ██║██║╚██╗██║██║  ██║██║   ██║   ██║██║   ██║██║╚██╗██║╚════██║
#  ╚██████╗╚██████╔╝██║ ╚████║██████╔╝██║   ██║   ██║╚██████╔╝██║ ╚████║███████║
#   ╚═════╝ ╚═════╝ ╚═╝  ╚═══╝╚═════╝ ╚═╝   ╚═╝   ╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚══════╝
#
# AUTOTYP uses a rich variable system to specify conditions under which
# a GR selects an argument (e.g. clause type, TAM, polarity). Alignment
# derivation must take these conditions under account. We say that each
# combination of conditions defines a *subsystem*.
#
# One challenge is that in order to simplify encoding, NA in a condition
# variable has a special meanign <any> which again means that the respective
# entry applies to every subsystem. In practice this means that we have to
# replace NAs in conditioning variables by corresponsing attested values, and
# we have to replicate the data while doing so
#
# To deal with the NA issue, we need a custom function, which will
# replace NA value by all other accounted values within a group
# for brevity, it is defined in a separate file expand_na.R
#
# Coargument conditions are a special case since they are dependent on the
# role of the argument. They also almost never need expansion since they tend
# to be exhautively coded. To further complicate matters, they are subject to
# reflexivity constraints (e.g. context 1sg > 1sg is generally banned). Luckily,
# for our purpose it is sufficient to expand coarguments depending on role
# and reference of the coded arguments — this takes care of all the implicit
# constraints and if somethign went wrong we will catch it during the check
# phase (next).


# using expand_na(), dealing with <any> in condition variables is simple
GR_expanded <- GR %>%
  # group everything by language
  group_by(LID, Language, SelectorType) %>%
  # expand all NAs in the condition variables
  # note: MarkerSlot does not need to be expanded
  expand_na(
    ReferentialCondition,
    ClauseRankCondition,
    CategoryCondition,
    PolarityCondition,
    SyntacticDomainCondition
  ) %>%
  # expand the coarguments for each (Role, Reference) pair separately
  group_by(LID, Language, SelectorType, SelectedRole, ReferentialCondition) %>%
  expand_na(CoargumentRoleCondition, CoargumentReferentialCondition) %>%
  # drop the groups
  ungroup()

#   ██████╗██╗  ██╗███████╗ ██████╗██╗  ██╗███████╗
#  ██╔════╝██║  ██║██╔════╝██╔════╝██║ ██╔╝██╔════╝
#  ██║     ███████║█████╗  ██║     █████╔╝ ███████╗
#  ██║     ██╔══██║██╔══╝  ██║     ██╔═██╗ ╚════██║
#  ╚██████╗██║  ██║███████╗╚██████╗██║  ██╗███████║
#   ╚═════╝╚═╝  ╚═╝╚══════╝ ╚═════╝╚═╝  ╚═╝╚══════╝
#
# Note: this is only done for case marking currently
#
# Now that we have expanded the GR conditions, we can check if they contain
# all the expected entries. The idea is that a valid coding should result in
# a certain symmetry: e.g. if there is a split in tense for one case marker,
# this split should be coded (either explicitly or implicitly) for all
# case markers
#
# What we do here is we build all combinations of relevant variables that
# are accounted in the data (conditioned by language ID), and check
# whether those combinations are actually coded in the data. If not, we flag
# it as a potential data input error. For example, if Reference = N appears
# in a GR type for a specific language, we build a table that contains
#
#  Role Ref  PredicateClassID              <other conditions...>
#  S    N    <all attested intransivives>  <all attested systems>
#  S    N    ...                           ...
#  Atr  N    <all attested transitives>    <all attested systems>
#  Atr  N    ...                           ...
#  P    N    <all attested transitives>    <all attested systems>
#  P    N    ...                           ...

GR_expected_to_be_coded <- GR_expanded %>%
  # Prase 1: collect all data on referential categories and coarguments
  #
  # we start by collecting all known entries per LID
  # if there is not a single non-NA entry, we just take an appropriate NA
  group_by(LID, Language, SelectorType) %>%
  summarize(
    # attested argument referential categories
    ReferentialCondition = list(
      vec_unique(na.omit(ReferentialCondition)) %0% vec_cast(NA, REFERENCES)
    ),
    # coargument sensitivity check
    CoargumentSensitivity = {
      any(!is.na(CoargumentRoleCondition)) ||
      any(!is.na(CoargumentReferentialCondition))
    },
    # attested coargument roles
    CoargumentRoleCondition = list(
      vec_unique(vec_c(NA, CoargumentRoleCondition))
    ),
    # attested coargument referential categories
    CoargumentReferentialCondition = list(
      vec_unique(na.omit(CoargumentReferentialCondition)) %0%
      vec_cast(NA, GR$CoargumentReferentialCondition)
    ),
    # drop the groups
    .groups = "drop"
  ) %>%
  # Prase 2: all roles must be coded
  #
  # we simply expand the table to add all the roles
  expand_grid(SelectedRole = ROLES) %>%
  relocate(SelectedRole, .before = ReferentialCondition) %>%
  # Prase 3: all referential categories and coarguments must be coded
  #
  # we simply expand coargument roles and filter out all the invalid contexts
  # using a filterign join
  unnest(CoargumentRoleCondition) %>%
  semi_join(
    # note: only A and P care about coargument sensitivity!
    tribble(
      ~ SelectedRole,  ~ CoargumentRoleCondition,  ~ CoargumentSensitivity,
         "S",                NA,                    FALSE,
       "Atr",                NA,                    FALSE,
         "P",                NA,                    FALSE,
      "Aditr",               NA,                    FALSE,
         "T",                NA,                    FALSE,
         "G",                NA,                    FALSE,
         "S",                NA,                    TRUE,
       "Atr",                "P",                   TRUE,
         "P",              "Atr",                   TRUE,
      "Aditr",               NA,                    TRUE,
         "T",                NA,                    TRUE,
         "G",                NA,                    TRUE
    ),
    by = c("SelectedRole", "CoargumentRoleCondition", "CoargumentSensitivity")
  ) %>%
  # expand the coargument references for non-NA coarguments
  mutate(
    # coargument reference list is NA if there is no coargument
    CoargumentReferentialCondition = if_else(
      is.na(CoargumentRoleCondition),
      list(vec_cast(NA, REFERENCES)),
      CoargumentReferentialCondition
    )
  ) %>%
  unnest(CoargumentReferentialCondition) %>%
  # expand the argument referential categories
  unnest(ReferentialCondition) %>%
  # remove all reflexive contexts (only for agreement)
  filter(
    (SelectorType == "case marker or adposition") | {
      r1 <- {
        str_detect(ReferentialCondition, "1") &
        str_detect(CoargumentReferentialCondition, "1")
      }
      r2 <- {
        str_detect(ReferentialCondition, "2") &
        str_detect(CoargumentReferentialCondition, "2")
      }

      # only take this entry of both contexts are false
      # NAs are same as FALSE
      !(r1 %|% FALSE) & !(r2 %|% FALSE)
    }
  ) %>%
  # clean up auxiliary columns needed in phase 3
  select(
    LID,
    Language,
    SelectorType,
    SelectedRole,
    ReferentialCondition,
    CoargumentRoleCondition,
    CoargumentReferentialCondition
  ) %>%
  # Prase 4: all predicate classes must be coded
  #
  # we join in all known predicate classes using roles as a key (note: inner
  # join is used to remove ditransitives if they are not coded)
  inner_join(
    PredClasses %>%
      select(
        LID, SelectorType,
        PredicateClassID,
        SelectedRole = ExpectedRoles
      ) %>%
      # ExpectedRoles is a list, need to expand this
      unnest(SelectedRole),
    by = c("LID", "SelectorType", "SelectedRole")
  ) %>%
  # Prase 5: all systems must be coded
  #
  # a system is a unique combination of conditions, so we simply take all
  # condition value we find per GR type and generate all possible combinations
  left_join(
    # nesting builds all combinations of values
    expand(GR_expanded, nesting(
      LID,
      SelectorType,
      ClauseRankCondition,
      CategoryCondition,
      PolarityCondition,
      SyntacticDomainCondition,
      MarkerSlot
    )),
    by = c("LID", "SelectorType")
  )
  # we are done here, PHEW!


# ID of the dummy "no agreement" GR
no_agreement_ID <- min(GR_expanded$SelectorID, 0L, na.rm = TRUE) - 1L

# to run the coding check, we will simply join the coded and the expected
# GR sets
GR_expanded <- GR_expanded %>%
  # mark these records as coded
  mutate(.Coded = TRUE) %>%
  # join with expanded set, using all variables
  full_join(
    # mark these records as expected
    mutate(GR_expected_to_be_coded, .Expected = TRUE),
    by = setdiff(names(GR_expected_to_be_coded), ".Expected")
  ) %>%
  # compute the coding status
  mutate(
    .Status = case_when(
      .Coded & .Expected ~ "coded",
      .Expected ~ "missing",
      .Coded ~ "unexpected"
    ),
    .before = 1L
  ) %>%
  select(-.Coded, - .Expected) %>%
  # check that every role is coded exactly once
  group_by(
    LID,
    SelectorType,
    SelectedRole,
    ReferentialCondition,
    CoargumentRoleCondition,
    CoargumentReferentialCondition,
    PredicateClassID,
    ClauseRankCondition,
    CategoryCondition,
    PolarityCondition,
    SyntacticDomainCondition,
    MarkerSlot
  ) %>%
  group_map(.keep= TRUE, ~ {
    if(nrow(.) > 1L && all(.$.Status == "coded")) {
      .$.Status <- format_inline("dup {unique(.$SelectedRole)}")
    }
    .
  }) %>%
  bind_rows() %>%
  ungroup() %>%
  # missing entires are expected with agreement GRs, as they
  # refer to argument that do not trigger agreement
  #
  # we have no good way of validating it, so we are assumign the
  # coding is correct, by adding a dummy "does not trigger agreement"
  # GR that is marked as coded
  mutate(
    # this is a dummy "does not trigger agreement" GR if this is not case
    # marking and status is missing
    .no_agreement = {
      !(SelectorType %in% "case marker or adposition") &
      .Status %in% "missing"
    },
    SelectorID = ifelse(
      .no_agreement,
      no_agreement_ID,
      SelectorID
    ),
    .Status = ifelse(
      .no_agreement,
      "coded",
      .Status
    ),
    SelectorLabel = ifelse(
      .no_agreement,
      "does not trigger agreement",
      SelectorLabel
    )
  ) %>%
  select(-.no_agreement)


# gather all invalid or missing GRs and write the report
GR_expanded %>%
  filter(.Status != "coded") %>%
  group_by(LID, Language, SelectorType) %>%
  group_map(function(data, group) {
    LID <- group$LID
    Language <- group$Language
    SelType <- group$SelectorType

    # remove all NA columns to clean up output
    data <- select(data, !where(~ all(is.na(.))))

    # format the entry
    c(
      format_inline("# {Language} (LID = {LID}, SelectorType = {SelType})"),
      "",
      str_c("  ", knitr::kable(data, "simple"))
    ) %>%
    str_flatten("\n")
  }) %>%
  str_flatten("\n\n\n") %>%
  writeLines("reports/GR_issues.md")

if(any(GR_expanded$.Status != "coded")) {
  n <- n_distinct(filter(GR_expanded, .Status != "coded")$LID)
  cli_alert_warning("There were issues with GR coding for {n} language{?s}")
  cli_alert_info("report written to {.uri reports/GR_issues.md}")
}



# only keep data with fully coded GRs
GR_expanded <- GR_expanded %>%
  group_by(LID, SelectorType) %>%
  filter(all(.Status == "coded")) %>%
  ungroup() %>%
  select(-.Status)



#   ██████╗ ██████╗  █████╗ ██████╗  ██████╗ ███████╗
#  ██╔════╝██╔═══██╗██╔══██╗██╔══██╗██╔════╝ ██╔════╝
#  ██║     ██║   ██║███████║██████╔╝██║  ███╗███████╗
#  ██║     ██║   ██║██╔══██║██╔══██╗██║   ██║╚════██║
#  ╚██████╗╚██████╔╝██║  ██║██║  ██║╚██████╔╝███████║
#   ╚═════╝ ╚═════╝ ╚═╝  ╚═╝╚═╝  ╚═╝ ╚═════╝ ╚══════╝
#
# For languages with coargument sensitivity, coargument is specified in the
# pair of fields CoargumentRole and CoargumentReference
#
# These fields are special since they are scoped by the Role (e.g. A
# coargument cannot be specified for S role). As such, they are not conditions
# like the one's above
#
# However, we can turn them into conditions by exracting each coargument into
# it's own column and replicating the data to account for each coargument
# combination.
#
# E.g. we go from this:
#
#   Role Ref CoargRole CoargRef
#   ---- --- --------- --------
#   S    1sg        NA       NA
#   A    1sg        P        2sg
#   A    1sg        P        3sg
#   A    1sg        ...
#   P    1sg        A        2sg
#   P    1sg        A        2sg
#   P    1sg        A        ...
#
# to this:
#
#   Role Ref CoargA CoargP
#   ---- --- ------ ------
#      S 1sg    2sg    2sg
#      S 1sg    2sg    3sg
#      S 1sg    2sg    ...
#      S 1sg    ...    2sg
#      S 1sg    ...    3sg
#      S 1sg    ...    ...
#      A 1sg    2sg    2sg
#      A 1sg    2sg    3sg
#      A 1sg    2sg    ...
#      A 1sg    ...    2sg
#      A 1sg    ...    3sg
#      A 1sg    ...    ...
#      P 1sg    2sg    2sg
#      P 1sg    2sg    3sg
#      P 1sg    2sg    ...
#      P 1sg    ...    2sg
#      P 1sg    ...    3sg
#      P 1sg    ...    ...
#
#
# where (CoargA, CoargP) defines a system


# the pivot spec for the coarguments (we only care about Atr and P)
coarg_pivot_spec <- tribble(
  ~ .name,           ~ .value,                      ~ CoargumentRoleCondition,
    "CoargumentAtr",   "CoargumentReferentialCondition",   "Atr",
    "CoargumentP",     "CoargumentReferentialCondition",   "P"
)

GR_expanded <- GR_expanded %>%
  # the trasnformation is conditioned by argument reference since not all
  # reference/coreference combinations are valid
  group_by(LID, SelectorType, ReferentialCondition) %>%
  group_modify(function(data, group) {
    # collect all available coargument references for this group
    coarg_refs <- vec_unique(na.omit(data$CoargumentReferentialCondition))
    if(length(coarg_refs) == 0) coarg_refs <- vec_cast(NA, REFERENCES)

    # pivot using the spec
    pivot_wider_spec(
      data,
      coarg_pivot_spec,
      # we are building liss of references initially
      values_fn = list,
      values_fill = list(coarg_refs)
    )
  }) %>%
  # expand the coargument references
  unnest(CoargumentAtr) %>%
  unnest(CoargumentP)



#   █████╗ ██████╗ ██████╗  █████╗ ███╗   ██╗ ██████╗ ███████╗
#  ██╔══██╗██╔══██╗██╔══██╗██╔══██╗████╗  ██║██╔════╝ ██╔════╝
#  ███████║██████╔╝██████╔╝███████║██╔██╗ ██║██║  ███╗█████╗
#  ██╔══██║██╔══██╗██╔══██╗██╔══██║██║╚██╗██║██║   ██║██╔══╝
#  ██║  ██║██║  ██║██║  ██║██║  ██║██║ ╚████║╚██████╔╝███████╗
#  ╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝╚═╝  ╚═══╝ ╚═════╝ ╚══════╝
#
# Arrange the GRs neatly
GR_expanded <- GR_expanded %>%
  select(
    LID, Language,
    SelectorID,
    MarkerID,
    SelectorType,
    SelectorLabel,
    SelectedRole,
    ReferentialCondition,
    PredicateClassID,
    CoargumentAtr,
    CoargumentP,
    MarkerSlot,
    ClauseRankCondition,
    CategoryCondition,
    SyntacticDomainCondition,
    PolarityCondition
  ) %>%
  arrange(
    LID,
    SelectorType,
    SelectedRole,
    ReferentialCondition,
    PredicateClassID,
    CoargumentAtr,
    CoargumentP,
    MarkerSlot,
    ClauseRankCondition,
    CategoryCondition,
    SyntacticDomainCondition,
    PolarityCondition
  )

#  ██████╗ ██████╗     ██████╗  ██████╗ ██╗     ███████╗███████╗
# ██╔════╝ ██╔══██╗    ██╔══██╗██╔═══██╗██║     ██╔════╝██╔════╝
# ██║  ███╗██████╔╝    ██████╔╝██║   ██║██║     █████╗  ███████╗
# ██║   ██║██╔══██╗    ██╔══██╗██║   ██║██║     ██╔══╝  ╚════██║
# ╚██████╔╝██║  ██║    ██║  ██║╚██████╔╝███████╗███████╗███████║
#  ╚═════╝ ╚═╝  ╚═╝    ╚═╝  ╚═╝ ╚═════╝ ╚══════╝╚══════╝╚══════╝
#
# Save aggregated roles per GR
#

# compute roles per GR
GR_roles <- GR_expanded %>%
  # group by all alignment contexts
  group_by(
    # language
    LID, Language,
    # GR
    SelectorID, MarkerID,
    # GR Type
    SelectorType, SelectorLabel,
    # argument referential type
    ReferentialCondition,
    # coarguments referential type
    CoargumentAtr, CoargumentP,
    # predicate class
    PredicateClassID,
    # all the other system conditions
    MarkerSlot,
    ClauseRankCondition,
    CategoryCondition,
    SyntacticDomainCondition,
    PolarityCondition
  ) %>%
  # now aggregate by SynpatternID to find which roles are selected by same GR
  group_by(SelectorID, .add = TRUE) %>%
  # sort the roles so that we get them in canonical order
  arrange(SelectedRole) %>%
  # collect the roles
  summarize(
    SelectedRoles = new_list_of(list(SelectedRole), ptype = vec_ptype(SelectedRole)),
    .groups = "drop"
  ) %>%
  # fix the coargument contexts: previous step generates all the
  # possible coargument combinations, which is nessesary for
  # agreement computation, but we only care about a relevant
  # subset when listing GRs
  mutate(
    # we don't care about CoargumentAtr if the roles do not include P
    CoargumentAtr = if_else(
      map_lgl(SelectedRoles, ~ "P" %in% .),
      CoargumentAtr,
      vec_cast(NA, CoargumentAtr)
    ),
    # we don't care about CoargumentA if the roles do not include Atr
    CoargumentP = if_else(
      map_lgl(SelectedRoles, ~ "Atr" %in% .),
      CoargumentP,
      vec_cast(NA, CoargumentP)
    ),
  ) %>%
  # and compact the results
  vec_unique() %>%
  # sort everything
  arrange(
    LID,
    SelectorType,
    SelectorID,
    ReferentialCondition,
    PredicateClassID,
    CoargumentAtr,
    CoargumentP,
    MarkerSlot,
    ClauseRankCondition,
    CategoryCondition,
    SyntacticDomainCondition,
    PolarityCondition
  )

#   █████╗ ██╗     ██╗ ██████╗ ███╗   ██╗███╗   ███╗███████╗███╗   ██╗████████╗
#  ██╔══██╗██║     ██║██╔════╝ ████╗  ██║████╗ ████║██╔════╝████╗  ██║╚══██╔══╝
#  ███████║██║     ██║██║  ███╗██╔██╗ ██║██╔████╔██║█████╗  ██╔██╗ ██║   ██║
#  ██╔══██║██║     ██║██║   ██║██║╚██╗██║██║╚██╔╝██║██╔══╝  ██║╚██╗██║   ██║
#  ██║  ██║███████╗██║╚██████╔╝██║ ╚████║██║ ╚═╝ ██║███████╗██║ ╚████║   ██║
#  ╚═╝  ╚═╝╚══════╝╚═╝ ╚═════╝ ╚═╝  ╚═══╝╚═╝     ╚═╝╚══════╝╚═╝  ╚═══╝   ╚═╝
#
# To compute alignments, we need to build combinations of (S, A, P, T, G) roles
# to compare. We do this by looking at all combinations of intransitive and
# transitive predicates.
#
# Once we have these combinations, we can easily create grouped data using
# the predicate combination as a key (and further splitting by all other
# relevant conditions)


PredClasses <- PredClasses %>%
  # take only predicate classes that are in the data
  semi_join(
    GR_expanded,
    by = c("LID", "SelectorType", "PredicateClassID")
  )

# generate all predicate class combinations for valid predicate classes
predicate_class_combinations <- PredClasses %>%
  # collect all predicate types by valence
  select(LID, SelectorType, Valence, PredicateClassID) %>%
  pivot_wider(
    names_from = Valence,
    values_from = PredicateClassID,
    names_glue = "P{Valence}",
    values_fn = list
  ) %>%
  # generate all combinations
  unnest(P1, keep_empty = TRUE) %>%
  unnest(P2, keep_empty = TRUE) %>%
  unnest(P3, keep_empty = TRUE) %>%
  # prepare for joining with selectors
  transmute(
    LID = LID,
    SelectorType = SelectorType,
    # predicate combination id
    CombinedPredicateClassID = list(P1, P2, P3) %>%
      pmap(function(...) {
        ids <- c(...)
        ids[!is.na(ids)]
      }),
    PredicateClassID = CombinedPredicateClassID,
  )


# compute roles per GR
GR_expanded <- GR_expanded %>%
  # join by expanded predicate combination PredicateClassID
  inner_join(
    unnest(predicate_class_combinations, PredicateClassID) %>%
    # make sure to clean up any missing ditransitives
    filter(!is.na(PredicateClassID)),
    by = c("LID", "SelectorType", "PredicateClassID")
  ) %>%
  # group by all alignment contexts
  group_by(
    # language
    LID, Language,
    # GR
    SelectorID, MarkerID,
    # GR Type
    SelectorType, SelectorLabel,
    # argument referential type
    ReferentialCondition,
    # coarguments referential type
    CoargumentAtr, CoargumentP,
    # predicate combination
    CombinedPredicateClassID,
    # all the other system conditions
    MarkerSlot,
    ClauseRankCondition,
    CategoryCondition,
    SyntacticDomainCondition,
    PolarityCondition
  ) %>%
  # now aggregate by SynpatternID to find which roles are selected by same GR
  group_by(SelectorID, .add = TRUE) %>%
  # sort the roles so that we get them in canonical order
  arrange(SelectedRole) %>%
  # collect the roles
  summarize(
    SelectedRoles = new_list_of(list(SelectedRole), ptype = vec_ptype(SelectedRole)),
    .groups = "drop"
  )

# now compute the alignemnts
alignments <- GR_expanded %>%
  # group by all alignment contexts
  group_by(
    # language
    LID, Language,
    # GR Type
    SelectorType,
    # argument referential type
    ReferentialCondition,
    # coarguments referential type
    CoargumentAtr, CoargumentP,
    # predicate combination
    CombinedPredicateClassID,
    # all the other system conditions
    MarkerSlot,
    ClauseRankCondition,
    CategoryCondition,
    SyntacticDomainCondition,
    PolarityCondition
  ) %>%
  # sort the role groups so that we get them in canonical order
  # (S=P must precede Atr etc.)
  arrange(map_int(SelectedRoles, ~ min(as.integer(.)))) %>%
  # compute the alignments proper
  summarize(
    Alignment = list(SelectedRoles),
    .groups = "drop"
  ) %>%
  mutate(
    # alignment of S, Atr and P
    Alignment2 = map_chr(Alignment, ~ {
      map(., intersect, c("S", "Atr", "P")) %>%
      compact() %>%
      map_chr(str_flatten, "=") %>%
      str_flatten("≠")
    }),
    # alignment of S, A, Aditr, P, T and G
    Alignment3 = map_chr(Alignment, ~ {
      map_chr(., str_flatten, "=") %>%
      str_flatten("≠")
    }) %>%
    # set it to NA if there are no ditransitives
    vec_assign(list_sizes(CombinedPredicateClassID) < 3L, NA),
    # alignment of  P, T and G
    AlignmentPTG = map_chr(Alignment, ~ {
      # sort the roles
      roles <- map(., ~ factor(intersect(., c("P", "T", "G")), RELEVANT_ROLES)) %>% compact()
      roles <- roles[order(map_int(roles, ~ min(as.integer(.))))]

      roles %>% map_chr(str_flatten, "=") %>% str_flatten("≠")
    }) %>%
    # set it to NA when there are no ditransitives
    vec_assign(list_sizes(CombinedPredicateClassID) < 3L, NA)
  )


# ███████╗██╗   ██╗███╗   ███╗███╗   ███╗ █████╗ ██████╗ ██╗   ██╗
# ██╔════╝██║   ██║████╗ ████║████╗ ████║██╔══██╗██╔══██╗╚██╗ ██╔╝
# ███████╗██║   ██║██╔████╔██║██╔████╔██║███████║██████╔╝ ╚████╔╝
# ╚════██║██║   ██║██║╚██╔╝██║██║╚██╔╝██║██╔══██║██╔══██╗  ╚██╔╝
# ███████║╚██████╔╝██║ ╚═╝ ██║██║ ╚═╝ ██║██║  ██║██║  ██║   ██║
# ╚══════╝ ╚═════╝ ╚═╝     ╚═╝╚═╝     ╚═╝╚═╝  ╚═╝╚═╝  ╚═╝   ╚═╝
#
# Compute the per language summary of the following variables:
#
# - Degree of accusativity (proportion of S≠P alignments)
# - Degree of ergativity (proportion of S≠A alignments)
# - Dominant Alignment2
# - Dominant Alignment3
# - Dominant AlignmentPTG

compute_dominant_alignment <- function(x) {
  # remove NAs
  x <- x[!is.na(x)]
  if(length(x) == 0L) return(NA_character_)

  # find the most frequent item
  tt <- table(x)
  m <- which(max(tt) == tt)
  if(length(m) > 1L) "mixed" else names(tt)[m]
}

AlignmentForDefaultPredicatesPerLanguage <- alignments %>%
  # only default predicates
  filter(map_lgl(CombinedPredicateClassID, ~ all(. <= 3L))) %>%
  group_by(LID, Language, SelectorType) %>%
  summarize(
    AccDegree = sum(str_detect(Alignment2, "S.*≠.*P"))/n(),
    ErgDegree = sum(str_detect(Alignment2, "S.*≠.*Atr"))/n(),
    DominantAlignment2 = compute_dominant_alignment(Alignment2),
    DominantAlignment3 = compute_dominant_alignment(Alignment3),
    DominantAlignmentPTG = compute_dominant_alignment(AlignmentPTG),
    # splits presence
    HasReferenceConditionedSplits = n_distinct(ReferentialCondition) > 1L,
    HasCategoryConditionedSplits = n_distinct(CategoryCondition) > 1L,
    HasClauseRankConditionedSplits = n_distinct(ClauseRankCondition) > 1L,
    HasPolarityConditionedSplits = n_distinct(PolarityCondition) > 1L,
    .groups = "drop"
  ) %>%
  # add the binned degrees
  mutate(
    AccDegreeBinned = cut(AccDegree, 3L, labels = c("low", "medium", "high")),
    ErgDegreeBinned = cut(ErgDegree, 3L, labels = c("low", "medium", "high")),
    .after = ErgDegree
  ) %>%
  # build column labels for selector types
  mutate(
    SelectorType = case_when(
      SelectorType == "case marker or adposition" ~ "Case",
      SelectorType == "agreement (grammatical)" ~ "Agreement",
      SelectorType == "agreement (marker)" ~ "MarkerAgreement"
     )
  ) %>%
  filter(!is.na(SelectorType)) %>%
  # pivot to wide
  pivot_wider(
    names_from = SelectorType,
    values_from = c(-LID, -Language, -SelectorType),
    names_glue = "{.value}For{SelectorType}"
  ) %>%
  arrange(LID)

#  ██████╗ ██╗   ██╗████████╗██████╗ ██╗   ██╗████████╗
# ██╔═══██╗██║   ██║╚══██╔══╝██╔══██╗██║   ██║╚══██╔══╝
# ██║   ██║██║   ██║   ██║   ██████╔╝██║   ██║   ██║
# ██║   ██║██║   ██║   ██║   ██╔═══╝ ██║   ██║   ██║
# ╚██████╔╝╚██████╔╝   ██║   ██║     ╚██████╔╝   ██║
#  ╚═════╝  ╚═════╝    ╚═╝   ╚═╝      ╚═════╝    ╚═╝


# final cleanup
GR_roles <- GR_roles %>%
  # remove dummy "no agreement" entries
  filter(!SelectorID %in% no_agreement_ID) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  # add selector properties
  left_join(select(GrammaticalRelationsRaw,
    SelectorID,
    SelectorTypeBinned,
    SelectorTypeBinned4,
    IsOvertlyCoded,
    SelectorLocusOfMarking,
    SelectorClauseScope,
    CoreferenceControllerOrControllee,
    CoreferenceArgumentTreatment
  ), by = "SelectorID") %>%
  select(
    LID,
    Glottocode,
    Language,
    SelectorID,
    SelectorLabel,
    SelectorType,
    SelectorTypeBinned,
    SelectorTypeBinned4,
    MarkerID,
    IsOvertlyCoded,
    SelectorLocusOfMarking,
    SelectorClauseScope,
    CoreferenceControllerOrControllee,
    CoreferenceArgumentTreatment,
    everything()) %>%
  # drop unused factor levels
  mutate(
    ReferentialCondition = fct_drop(ReferentialCondition),
    CoargumentAtr = fct_drop(CoargumentAtr),
    CoargumentP = fct_drop(CoargumentP),
    ClauseRankCondition =fct_drop(ClauseRankCondition),
    CategoryCondition = fct_drop(CategoryCondition),
    SyntacticDomainCondition = fct_drop(SyntacticDomainCondition),
    PolarityCondition = fct_drop(PolarityCondition)
  )


alignments <- alignments %>%
  # remove the list alignment
  select(-Alignment) %>%
  # collapse the combined predicate class ID into a single string
  mutate(
    CombinedPredicateClassID = map_chr(
      CombinedPredicateClassID,
      str_flatten,
      "."
    )
  ) %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything()) %>%
  # drop unused factor levels
  mutate(
    ReferentialCondition = fct_drop(ReferentialCondition),
    CoargumentAtr = fct_drop(CoargumentAtr),
    CoargumentP = fct_drop(CoargumentP),
    ClauseRankCondition =fct_drop(ClauseRankCondition),
    CategoryCondition = fct_drop(CategoryCondition),
    SyntacticDomainCondition = fct_drop(SyntacticDomainCondition),
    PolarityCondition = fct_drop(PolarityCondition)
  )


AlignmentForDefaultPredicatesPerLanguage <- AlignmentForDefaultPredicatesPerLanguage %>%
  # add glottocodes
  left_join(select(Register, LID, Glottocode), by = "LID") %>%
  select(LID, Glottocode, Language, everything())

descriptor <- describe_data(
  ptype = tibble(),
  description = "
    Detailed coding of grammatical relations as selectors of generalized semantic roles,
    conditioned by various parameters (https://www.autotyp.uzh.ch/projects/grhandbook/GR_Quest.pdf).

    Note: this is a processed and validated dataset, generated from complete data in
    `GrammaticalRelationsRaw` (`IsSelectorSurveyComplete==TRUE`). We currently include GR data on
    case marking and agreement only, with default diathesis and semantic/syntactic domain condition.
  ",
  computed = "Alignment.R",
  fields = list(
    LID = .metadata$Register$fields$LID,
    Glottocode = .metadata$Register$fields$Glottocode,
    Language = .metadata$Register$fields$Language,
    SelectorID = .metadata$GrammaticalRelations$fields$SelectorID,
    SelectorLabel = .metadata$GrammaticalRelationsRaw$fields$SelectorLabel,
    SelectorType = .metadata$GrammaticalRelationsRaw$fields$SelectorType,
    SelectorTypeBinned = .metadata$GrammaticalRelationsRaw$fields$SelectorTypeBinned,
    SelectorTypeBinned4 = .metadata$GrammaticalRelationsRaw$fields$SelectorTypeBinned4,
    MarkerID = .metadata$GrammaticalRelations$fields$MarkerID,
    IsOvertlyCoded = .metadata$GrammaticalRelations$fields$IsOvertlyCoded,
    SelectorLocusOfMarking = .metadata$GrammaticalRelations$fields$SelectorLocusOfMarking,
    SelectorClauseScope = .metadata$GrammaticalRelations$fields$SelectorClauseScope,
    CoreferenceControllerOrControllee = .metadata$GrammaticalRelations$fields$CoreferenceControllerOrControllee,
    CoreferenceArgumentTreatment = .metadata$GrammaticalRelations$fields$CoreferenceArgumentTreatment,
    ReferentialCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$ReferentialCondition,
      GR_roles$ReferentialCondition
    ),
    CoargumentAtr = {
      desc <- .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$ReferentialCondition
      desc <- fix_metadata_levels(desc, GR_roles$CoargumentAtr)
      desc$description <- "Refrential condition on the Atr coargument"
      desc$computed <- "Alignment.R"
      desc
    },
    CoargumentP = {
      desc <- .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$ReferentialCondition
      desc <- fix_metadata_levels(desc, GR_roles$CoargumentP)
      desc$description <- "Refrential condition on the P coargument"
      desc$computed <- "Alignment.R"
      desc
    },
    PredicateClassID = .metadata$PredicateClasses$fields$PredicateClassID,
    MarkerSlot = .metadata$GrammaticalMarkers$fields$MarkerSlot,
    ClauseRankCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$ClauseRankCondition,
      GR_roles$ClauseRankCondition
    ),
    CategoryCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$CategoryCondition,
      GR_roles$CategoryCondition
    ),
    SyntacticDomainCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$SyntacticDomainCondition,
      GR_roles$SyntacticDomainCondition
    ),
    PolarityCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$PolarityCondition,
      GR_roles$PolarityCondition
    ),
    SelectedRoles = describe_data(
      ptype = new_list_of(),
      element = {
        desc <- .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$SelectedRole
        desc$levels <- filter(desc$levels, level %in% RELEVANT_ROLES)
        desc
      },
      description = "Selected generalized semantic roles",
      computed = "Alignment.R"
    )
  )
)

export_dataset("GrammaticalRelations", GR_roles, descriptor, "GrammaticalRelations")

descriptor <- describe_data(
  ptype = tibble(),
  description = "
    Computed alignments for case and agreement (syntactic and per-marker)
  ",
  computed = "Alignment.R",
  fields = list(
    LID = .metadata$Register$fields$LID,
    Glottocode = .metadata$Register$fields$Glottocode,
    Language = .metadata$Register$fields$Language,
    SelectorType = .metadata$GrammaticalRelationsRaw$fields$SelectorType,
    ReferentialCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$ReferentialCondition,
      alignments$ReferentialCondition
    ),
    CoargumentAtr = {
      desc <- .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$ReferentialCondition
      desc <- fix_metadata_levels(desc, alignments$CoargumentAtr)
      desc$description <- "Refrential condition on the Atr coargument"
      desc$computed <- "Alignment.R"
      desc
    },
    CoargumentP = {
      desc <- .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$ReferentialCondition
      desc <- fix_metadata_levels(desc, alignments$CoargumentP)
      desc$description <- "Refrential condition on the P coargument"
      desc$computed <- "Alignment.R"
      desc
    },
    CombinedPredicateClassID = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Combination of predicate classes (intransitive.transitive.ditransitive) for which
        the roles are considered
      ",
      computed = "Alignment.R",
    ),
    MarkerSlot = .metadata$GrammaticalMarkers$fields$MarkerSlot,
    ClauseRankCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$ClauseRankCondition,
      alignments$ClauseRankCondition
    ),
    CategoryCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$CategoryCondition,
      alignments$CategoryCondition
    ),
    SyntacticDomainCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$SyntacticDomainCondition,
      alignments$SyntacticDomainCondition
    ),
    PolarityCondition = fix_metadata_levels(
      .metadata$GrammaticalRelationsRaw$fields$SelectedArguments$element$fields$PolarityCondition,
      alignments$PolarityCondition
    ),
    Alignment2 = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "Alignment of generalized semantic roles S, Atr and P",
      computed = "Alignment.R",
    ),
    Alignment3 = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Full ditransitive alignment of generalized semantic roles S, Atr, Aditr, P, T and G
      ",
      computed = "Alignment.R",
    ),
    AlignmentPTG = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "Alignment of generalized patient semantic roles P, T and G",
      computed = "Alignment.R",
    )
  )
)

export_dataset("Alignment", alignments, descriptor, "GrammaticalRelations")

descriptor <- describe_data(
  ptype = tibble(),
  description = "Per-language summary of morphosyntactic alignments (default predicate class only)",
  computed = "Alignment.R",
  fields = list(
    LID = .metadata$Register$fields$LID,
    Glottocode = .metadata$Register$fields$Glottocode,
    Language = .metadata$Register$fields$Language,
    AccDegreeForAgreement = describe_data(
      ptype = double(),
      description = "
        Degree of accusativity for syntactic agreement: proportion of S≠P alignment
        among all alignments (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    AccDegreeForMarkerAgreement = describe_data(
      ptype = double(),
      description = "
        Degree of accusativity for per-marker agreement: proportion of S≠P alignment
        among all alignments (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    AccDegreeForCase = describe_data(
      ptype = double(),
      description = "
        Degree of accusativity for case marking: proportion of S≠P alignment among all
        alignments (default predicates only)
      ",
      computed = "Alignment.R",
    ),
    ErgDegreeForAgreement = describe_data(
      ptype = double(),
      description = "
        Degree of ergativity for syntactic agreement: proportion of S≠Atr alignment
        among all alignments (default predicate class only)
      ",
      computed = "Alignment.R"
    ),
    ErgDegreeForMarkerAgreement = describe_data(
      ptype = double(),
      description = "
        Degree of ergativity for per-marker agreement: proportion of S≠Atr alignment
        among all alignments (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    ErgDegreeForCase = describe_data(
      ptype = double(),
      description = "
        Degree of ergativity for case marking: proportion of S≠Atr alignment among
        all alignments (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    AccDegreeBinnedForAgreement = describe_data(
      ptype = factor(),
      levels = tibble(
        level = c("low", "medium", "high"),
        description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
      ),
      description = "
        Degree of accusativity for syntactic agreement: proportion of S≠P alignment
        among all alignments, binned into three categories (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    AccDegreeBinnedForMarkerAgreement = describe_data(
      ptype = factor(),
      levels = tibble(
        level = c("low", "medium", "high"),
        description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
      ),
      description = "
        Degree of accusativity for per-marker agreement: proportion of S≠P alignment among
        all alignments, binned into three categories (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    AccDegreeBinnedForCase = describe_data(
      ptype = factor(),
      levels = tibble(
        level = c("low", "medium", "high"),
        description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
      ),
      description = "
        Degree of accusativity for case marking: proportion of S≠P alignment among all
        alignments, binned into three categories (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    ErgDegreeBinnedForAgreement = describe_data(
      ptype = factor(),
      levels = tibble(
        level = c("low", "medium", "high"),
        description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
      ),
      description = "
        Degree of ergativity for syntactic agreement: proportion of S≠Atr alignment among
        all alignments, binned into three categories (default predicate class only)
      ",
      computed = "Alignment.R"
    ),
    ErgDegreeBinnedForMarkerAgreement = describe_data(
      ptype = factor(),
      levels = tibble(
        level = c("low", "medium", "high"),
        description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
      ),
      description = "
        Degree of ergativity for per-marker agreement: proportion of S≠Atr alignment among
        all alignments, binned into three categories (default predicate class only)
      ",
      computed = "Alignment.R"
    ),
    ErgDegreeBinnedForCase = describe_data(
      ptype = factor(),
      levels = tibble(
        level = c("low", "medium", "high"),
        description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
      ),
      description = "
        Degree of ergativity for case marking: proportion of S≠Atr alignment among all
        alignments, binned into three categories (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignment2ForAgreement = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Syntactic agreement: dominant (most frequent) alignment of generalized
        semantic roles S, Atr and P (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignment2ForMarkerAgreement = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Per-marker agreement: dominant (most frequent) alignment of generalized
        semantic roles S, Atr and P (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignment2ForCase = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Case marking: dominant (most frequent) alignment of generalized
        semantic roles S, Atr and P (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignment3ForAgreement = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Syntactic agreement: dominant (most frequent) full ditransitive alignment of
        generalized semantic roles S, Atr, Aditr, P, T and G (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignment3ForMarkerAgreement = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Per-marker agreement: dominant (most frequent) full ditransitive alignment
        of generalized semantic roles S, Atr, Aditr, P, T and G (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignment3ForCase = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Case marking: dominant (most frequent) full ditransitive alignment of
        generalized semantic roles S, Atr, Aditr, P, T and G (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignmentPTGForAgreement = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Syntactic agreement: dominant (most frequent) alignment of generalized patient
        semantic roles P, T and G (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignmentPTGForMarkerAgreement = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Per-marker agreement: dominant (most frequent) alignment of generalized patient
        semantic roles P, T and G (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    DominantAlignmentPTGForCase = describe_data(
      ptype = .metadata$Register$fields$Language$ptype,
      description = "
        Case marking: dominant (most frequent) alignment of generalized patient
        semantic roles P, T and G (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    HasReferenceConditionedSplitsForAgreement = describe_data(
      ptype = logical(),
      description = "
        Syntactic agreement: split alignment conditioned by argument reference
        (default predicate class only)
      ",
        computed = "Alignment.R",
    ),
    HasReferenceConditionedSplitsForCase = describe_data(
      ptype = logical(),
      description = "
        Case marking: split alignment conditioned by argument reference
        (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    HasReferenceConditionedSplitsForMarkerAgreement = describe_data(
      ptype = logical(),
      description = "
        Per-marker agreement: split alignment conditioned by argument reference
        (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    HasCategoryConditionedSplitsForAgreement = describe_data(
      ptype = logical(),
      description = "
        Syntactic agreement: split alignment conditioned by verbal category such as TAM etc.
        (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    HasCategoryConditionedSplitsForCase = describe_data(
      ptype = logical(),
      description = "
        Case marking: split alignment conditioned by verbal category such as TAM etc.
        (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    HasCategoryConditionedSplitsForMarkerAgreement = describe_data(
      ptype = logical(),
      description =
        "
          Per-marker agreement: split alignment conditioned by verbal category such as TAM etc.
          (default predicate class only)
        ",
        computed = "Alignment.R",
    ),
    HasClauseRankConditionedSplitsForAgreement = describe_data(
      ptype = logical(),
      description =
        "
          Syntactic agreement: split alignment conditioned by clause rank such as main,
          dependent etc. (default predicate class only)
        ",
        computed = "Alignment.R",
    ),
    HasClauseRankConditionedSplitsForCase = describe_data(
      ptype = logical(),
      description =
        "
          Syntactic agreement: split alignment conditioned by clause rank such as main,
          dependent etc. (default predicate class only)
        ",
        computed = "Alignment.R",
    ),
    HasClauseRankConditionedSplitsForMarkerAgreement = describe_data(
      ptype = logical(),
      description =
        "
          Syntactic agreement: split alignment conditioned by clause rank such as main,
          dependent etc. (default predicate class only)
        ",
        computed = "Alignment.R",
    ),
    HasPolarityConditionedSplitsForAgreement = describe_data(
      ptype = logical(),
      description = "
        Syntactic agreement: split alignment conditioned by clause polarity
        (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    HasPolarityConditionedSplitsForCase = describe_data(
      ptype = logical(),
      description = "
        Syntactic agreement: split alignment conditioned by clause polarity
        (default predicate class only)
      ",
      computed = "Alignment.R",
    ),
    HasPolarityConditionedSplitsForMarkerAgreement = describe_data(
      ptype = logical(),
      description = "
        Syntactic agreement: split alignment conditioned by clause polarity
        (default predicate class only)
      ",
      computed = "Alignment.R",
    )
  )
)

export_dataset("AlignmentForDefaultPredicatesPerLanguage", AlignmentForDefaultPredicatesPerLanguage, descriptor, c("PerLanguageSummaries", "GrammaticalRelations"))
