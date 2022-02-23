# AUTOTYP (in progress)

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
  - Fixed the value list descriptionb for `Register::OriginContinent`
  - Computed variables in `GrammaticalMarkersPerLanguage` now have correct value list metadata
  - Computed variables in `LocusOfMarkingPerLanguage` now have correct value list metadata
  - Computed variables `MorphologyPerLanguage::HasAny*` are now correctly annotated as logical
  - Computed variables `NPStructurePerLanguage::NPHas*` are now correctly annotated as logical
  - `NPStructurePerLanguage::NPStructureID` is now correctly annotated as integer
  - Computed variables in `VerbInflection*` summary datasets now have correct value list metadata
  