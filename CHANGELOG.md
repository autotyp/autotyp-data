# (unreleased)

- (bugfix) ditransitive object alignment (`Alignment$AlignmentPTG`) computation
  did not properly order the roles, resulting in ambigous alignment specification
  (e.g. "P=T≠G" vs. "G≠P=T"). This is now fixed, the roles are always in canonical 
  order. 

# AUTOTYP 1.1.1

This is a minor maintenance release with improvements to the CLDF export

 - Improved CLDF export for the dataset `MaximallyInflectedVerbSynthesis`

Many thanks to Robert Forkel for implementing this change. 

# AUTOTYP 1.1.0

The main changes in this release are:

 - A new [CLDF](https://cldf.clld.org) export, kindly contributed by Robert Forkel
 - Revamp of the verb synthesis datasets which has been renamed to 
 `MaximallyInflectedVerbSynthesis` (see below)
 - Enhancements to `GrammaticalRelations`

Important: this release introduces breaking changes to the Morphology module.

### CLDF export

A CLDF export of AUTOTYP database was one of the most requested features since the new release. 
We are very thankful to Robert Forkel for writing the code that generates a CLDF dataset of
AUTOTYP data. Starting from AUTOTYP 1.1.0 there is a CLDF dataset in `data`. The accompanying
Python scripts used to generate this export can be found in the repository 
[autotyp/autotyp-cldf-scripts](https://github.com/autotyp/autotyp-cldf-scripts)

### Revamp of verb synthesis datasets in the Morphology module 

We wanted to clarify that the verb synthesis dataset in the Morphology module describes
maximally inflected verb forms only rather than all attested verb forms. To make this more
obvious we have renamed the affected datasets to include the phrase `MaximallyInflected` as well as 
updated the variable metadata to clarify this point. Thus: 

  - `VerbSynthesis` becomes `MaximallyInflectedVerbSynthesis`
  - `VerbInflectionAndAgreementCountsByPosition` becomes  `MaximallyInflectedVerbInflectionAndAgreementCountsByPosition`
  - `VerbAgreementAggregatedByMarkerPosition` becomes `MaximallyInflectedVerbAgreementAggregatedByMarkerPosition`
  - `VerbInflectionCategoriesAggregatedByMarkerPosition` becomes  `MaximallyInflectedVerbInflectionCategoriesAggregatedByMarkerPosition`
  - etc.

This is a breaking change.

### Enhancements to `GrammaticalRelations`

Multiple new variables that describe properties of grammatical relations have been added to 
`GrammaticalRelations` dataset. Please refer to the list of changes below. 

### Detailed list of changes

  - CLDF export is now available in `data/cldf`
  - `VerbSynthesis` has been renamed to `MaximallyInflectedVerbSynthesis` for clarity, with updates
    to the metadata 
  - Aggregated inflectional categories datasets have been renamed to reflect the changes to the 
    synthesis module:
    - `VerbInflectionAndAgreementCountsByPosition` is now  `MaximallyInflectedVerbInflectionAndAgreementCountsByPosition`
    - `VerbInflectionCategoriesAggregatedPresence` is now  `MaximallyInflectedVerbInflectionCategoriesAggregatedPresence`
    - `VerbAgreementAggregatedByMarkerPosition` is now `MaximallyInflectedVerbAgreementAggregatedByMarkerPosition`
    - `VerbAgreementAggregatedByMarkerPositionBinned4` is now  `MaximallyInflectedVerbAgreementAggregatedByMarkerPositionBinned4`
    - `VerbAgreementAggregatedByMarkerPositionBinned5` is now  `MaximallyInflectedVerbAgreementAggregatedByMarkerPositionBinned5`
    - `VerbAgreementAggregatedByMarkerHasMultipleExponents` is now `MaximallyInflectedVerbAgreementAggregatedByMarkerHasMultipleExponents`
    - `VerbAgreementAggregatedByMarkerHasPostposedExponent` is now `MaximallyInflectedVerbAgreementAggregatedByMarkerHasPostposedExponent`
    - `VerbAgreementAggregatedByMarkerHasPreposedExponent` is now  `MaximallyInflectedVerbAgreementAggregatedByMarkerHasPreposedExponent`
    - `VerbInflectionCategoriesAggregatedByMarkerHasMultipleExponents` is now  `MaximallyInflectedVerbInflectionCategoriesAggregatedByMarkerHasMultipleExponents`
    - `VerbInflectionCategoriesAggregatedByMarkerHasPostposedExponent` is now  `MaximallyInflectedVerbInflectionCategoriesAggregatedByMarkerHasPostposedExponent`
    - `VerbInflectionCategoriesAggregatedByMarkerHasPreposedExponent` is now `MaximallyInflectedVerbInflectionCategoriesAggregatedByMarkerHasPreposedExponent`
    - `VerbInflectionCategoriesAggregatedByMarkerPosition` is now  `MaximallyInflectedVerbInflectionCategoriesAggregatedByMarkerPosition`
    - `VerbInflectionCategoriesAggregatedByMarkerPositionBinned4` is now `MaximallyInflectedVerbInflectionCategoriesAggregatedByMarkerPositionBinned4`
    - `VerbInflectionCategoriesAggregatedByMarkerPositionBinned5` is now `MaximallyInflectedVerbInflectionCategoriesAggregatedByMarkerPositionBinned5`
    - `VerbInflectionMacrocategoriesAggregatedByMarkerPosition` is now `MaximallyInflectedVerbInflectionMacrocategoriesAggregatedByMarkerPosition`
    - `VerbInflectionMacrocategoriesAggregatedByMarkerPositionBinned4` is now  `MaximallyInflectedVerbInflectionMacrocategoriesAggregatedByMarkerPositionBinned4`
    - `VerbInflectionMacrocategoriesAggregatedByMarkerPositionBinned5` is now  `MaximallyInflectedVerbInflectionMacrocategoriesAggregatedByMarkerPositionBinned5`
    - `VerbInflectionMacrocategoriesAggregatedByMarkerHasMultipleExponents` is now `MaximallyInflectedVerbInflectionMacrocategoriesAggregatedByMarkerHasMultipleExponents`
    - `VerbInflectionMacrocategoriesAggregatedByMarkerHasPostposedExponent` is now `MaximallyInflectedVerbInflectionMacrocategoriesAggregatedByMarkerHasPostposedExponent`
    - `VerbInflectionMacrocategoriesAggregatedByMarkerHasPreposedExponent` is now  `MaximallyInflectedVerbInflectionMacrocategoriesAggregatedByMarkerHasPreposedExponent`
  - `MorphologyPerLanguage$HasAgreement` has been retired in favor  
    ofMorphologyPerLanguage$HasAnyVerbAgreement`. The old variable used overly strict aggregation
    logic which could produce misleading results. The new `HasAnyVerbAgreement` reports presence 
    of agreement if any of the relevant AUTOTYP modules code agreement for verbs in any form or 
    fashion. The metadata has been updated to clarify this.   
  - Additional properties of grammatical relations are added to `GrammaticalRelationsRaw` 
  and `GrammaticalRelations`:
    - `SelectorTypeBinned4` (grammatical relation macrotype)
    - `IsOvertlyCoded` (whether the GR is overtly coded)
    - `SelectorLocusOfMarking`
    - `SelectorClauseScope` (does the GR operate within a clause or across multiple clauses)
    - `CoreferenceControllerOrControllee`
    - `CoreferenceArgumentTreatment` (treatment of arguments under co-reference in the GR)
  - Fixed some issues in the documentation ( #44)
  - Various data updates


# AUTOTYP 1.0.1

This is a bugfix release that focuses on JSON output and improving metadata for variables of type
value list. Notable changes:

- improved JSON output
- improved and corrected the metadata for multiple variables of the type value list
- improved the bibliography data, added Glottolog language and reference IDs (many thanks to 
  Robert Forkel for doing this work)
- minor data fixes (duplicate entries in datasets `Alienability`, `Gender` and `NumeralClassifiers`)

Many thanks to Robert Forkel for reporting many of these issues and curating the bibliography 
files!

Detailed changes:

- Fixed the DOI badge (now points to last released version 1.0.0)
- Added data type `logical` to the list of valid variable types
- Clarified that `value-list` is not actually a list
- Fixed an issue with JSON export where missing values were silently dropped
  by the serializer, they are now exported as `null` 
- If a value list variable has no values (all missing), the json value list metadata 
  is now serialized as an empty dictionary `{}` for consistency
- `NPStructurePresence` is no longer classified as a `PerLanguageSummaries` dataset
- `LID` field was sometimes serialized as string, fixed
- Missing glottocodes were sometimes serialized as explicit "NA" string, fixed
- Removed duplicate data entries from `Alienability`
- Removed duplicate data entries from `Gender`
- Removed duplicate data entries from `NumeralClassifiers`
- Added maps illustrating the geographical breakdown (by continent and area)
- improved the bibliography data, added Glottolog language and reference IDs (many thanks to 
  Robert Forkel for doing this work)
- Multiple metadata fixes:
  - Added value list descriptions for `PhonologicalFusion::FusionBinned6` and all variables that 
    rely on it (such as `GrammaticalMarkers::MarkerFusionBinned6`)
  - Added value list descriptions for `PositionalBehavior::MarkerBehaviorBinned4` and all variables 
    that rely on it (such as `GrammaticalMarkers::MarkerBehaviorBinned4`)
  - Value list description for `LocusOfMarking::LocusOfMarkingBinned5` was missing the value 
    'FloatingorClitic', fixed (this also fixes all the variables that rely on it, such as 
    `GrammaticalMarkers::LocusOfMarkingBinned5`)
  - Fixed value list description for `GrammaticalMarkers::MarkerPositionBinned4`
  - Fixed value list description for `GrammaticalMarkers::MarkerPositionBinned5`
  - Fixed data type of `GrammaticalMarkers::MarkerExpressesMultipleCategories` to be `logical`
  - Added value list descriptions for `ClauseLinkage::IntuitiveClassification`, value "?" is now
    recoded as NA (missing)  
  - Added value list descriptions for multiple fields in `ClauseLinkage` where they were missing. 
    The fields are: `AnticipatoryArgumentMarking`, `CataphoraConstraints`, `CategoricalSymmetry`, 
    `ClauseLayer`, `ClausePosition`, `Embedding`, `ExtractionConstraints`, `FinitenessSimplified`, 
    `FocusMarkingInDependent`, `FocusMarking`, `IllocutionaryMarking`, `IllocutionaryScope`,  
    `InterpropositionalSemanticRelation`, `ReferenceTrackingSystem`, `TenseMarking` and  
    `TenseScope`
  - Fixed the value list description for `ClauseWordOrder::WordOrderAPLex`
  - Fixed the value list description for `SemanticClass::SemanticClassBinned`
  - Removed invalid values from `GrammaticalRelationsRaw::SelectedArguments::SemanticCondition`
  - Fixed the value list description for `Register::OriginContinent`
  - Computed variables in `GrammaticalMarkersPerLanguage` now have correct value list metadata
  - Computed variables in `LocusOfMarkingPerLanguage` now have correct value list metadata
  - Computed variables `MorphologyPerLanguage::HasAny*` are now correctly annotated as logical
  - Computed variables `NPStructurePerLanguage::NPHas*` are now correctly annotated as logical
  - `NPStructurePerLanguage::NPStructureID` is now correctly annotated as integer
  - Computed variables in `VerbInflection*` summary datasets now have correct value list metadata
