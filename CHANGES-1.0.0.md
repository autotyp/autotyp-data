# Overview of changes in version 1.0.0

AUTOTYP version 1.0.0 is a completely new release that focuses on usability,
documentation and completeness. It has been radically overhauled compared to
the earlier 0.1.x version. The sheer number of differences makes it
impossible to provide a comprehensive list of changes. What follows is a
quick summary of the most important of the new release as well as notes on
migrating from the old database releases. 

## Major new features in version 1.0.0:

- New naming conventions for datasets and variables, focusing on usability 
  and clarity. All names now consistently follow the CamelCase convention and 
  are based on verbose descriptions that provide more context about the variable 
  (e.g. `Position` -> `VerbInflectionMarkerPosition`). Hundreds of variables have 
  been renamed to fit these criteria.

- The datasets are now organized into thematic modules, rather than each dataset
  constituting a module on its own. 

- Published data now includes the raw exported database data, in addition to the 
  previously published derived aggregated tables. All aggregation scripts used to
  compute derived data are published as well (see 
  [`aggregation-scripts`](aggregation-scripts)). Please feel free to inspect the 
  scripts and modify them to suit your own needs.   

- Many improvements to variable descriptions and metadata. The metadata YAML files 
  are now simpler and more compact, which should make the documentation more 
  accessible.

- Overhauled the data architecture to allow nested and repeated table fields (see 
  [Data Architecture](readme.md#data-architecture)). This allows many datasets to be
  expressed in a more natural, conceptually simpler fashion.  

- New R and JSON exports for users who want quick access to the data using their 
  preferred data wrangling environment. 

- Language name and glottocode is exported for every dataset in addition to the 
  internal language ID

## Major changes to individual datasets/modules:

- `GrammaticalRelations` module now encompasses all data on grammatical relations 
  and alignments. We now fully provide the underlying raw database data in addition 
  to the aggregated alignment data and the scripts used to produce these aggregations.

- `VerbSynthesis` has been overhauled to include detailed list of inflectional 
  categories expressed on verbs

- `LocusOfMarking` module now contains the raw database data in addition to the 
  previously published aggregations. 

- `GrammaticalMarkers` dataset has been overhauled to include a detailed list
  of marker hosts and marked categories 

- `MorphemeClasses` replaces the previous aggregated `Morpheme_types` dataset
  and exposes the information about individual language-specific morpheme classes. 
  The information previously available in `Morpheme_types` is now integrated into
  the improved `MorphologyPerLanguage` aggregated dataset. 

- New module `Categories` groups together datasets that provide information about 
  selected grammatical categories

- New module `Definitions` provides access to underlying definitions of categorical
  variables used across AUTOTYP

- New module `PerLanguageSummaries` groups together various per-language aggregated
  summaries (code to generate these summaries is available under 
  [`aggregation-scripts`](aggregation-scripts))


## Notes on migration from older AUTOTYP release

If you have been using the AUTOTYP version 0.1.x you will notice that many datasets
have been moved or renamed. The following list should help you to find the new 
location of the data:

- **`Agreement`** is now exported as `Categories/Agreement`
- **`Alienability`** is now exported as `Categories/Alienability`
- **`Alignment`** is now exported as `GrammaticalRelations/Alignment`
- **`Alignment_per_language`** is now `PerLanguageSummaries/AlignmentForDefaultPredicatesPerLanguage` 
- **`Clause_linkage`** is now `Sentence/ClauseLinkage` 
- **`Clause_word_order`** is now `Sentence/ClauseWordOrder` 
- **`Clusivity`** is now exported as `Categories/Clusivity`
- **`Gender`** is now exported as `Categories/Gender`
- **`Grammatical_markers`** is now exported as `Morphology/GrammaticalMarkers`
- **`GR_per_language`** has been superseded by `GrammaticalRelations/GrammaticalRelationCoverage`
- **`Locus_per_language`** is now `PerLanguageSummaries/LocusOfMarkingPerLanguage` 
- **`Locus_per_macrorelation`** has been superseded by `Morphology/DefaultLocusOfMarkingPerMacrorelation`
- **`Locus_per_microrelation`** has been superseded by `Morphology/LocusOfMarkingPerMicrorelation`
- **`Markers_per_language`** is now `PerLanguageSummaries/GrammaticalMarkersPerLanguage` 
- **`Morpheme_types`** has been superseded by `Morphology/MorphemeClasses` and 
  `PerLanguageSummaries/MorphologyPerLanguage` 
- **`Morphology_per_language`** is now `PerLanguageSummaries/MorphologyPerLanguage` 
- **`NP_per_language`** is now `PerLanguageSummaries/NPStructurePerLanguage` 
- **`NP_structure`** is now `NP/NPStructure`
- **`NP_structure_presence`** is now `PerLanguageSummaries/NPStructurePresence` 
- **`Numeral_classifiers`** is now exported as `Categories/NumeralClassifiers`
- **`Register`** is still `Register`
- **`Synthesis`** is now `Morphology/VerbSynthesis`
- **`Valence_classes`** is now `GrammaticalRelations/PredicateClasses`
- **`Valence_classes_per_language`** is now `PerLanguageSummaries/PredicateClassesSemanticsPerLanguage` 	
- **`VInfl_counts_per_position`** is now `PerLanguageSummaries/VerbInflectionAndAgreementCountsByPosition` 	
- **`VInfl_cat_*`** is now `PerLanguageSummaries/VerbInflectionCategoriesAggregatedBy*` 	
- **`VInfl_macrocat_*`** is now `PerLanguageSummaries/VerbInflectionMacrocategories*` 	
- **`VAgr_*`** is now `PerLanguageSummaries/VerbAgreementAggregatedBy*` 	
- **`Word_domains`** is now `Word/WordDomains` 	







