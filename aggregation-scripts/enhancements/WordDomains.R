# WordDomains enhancements
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

WordDomains <- WordDomains %>%
  mutate(
    MphmTypesInCohPrefixDomainPropBinned3 = cut(
      MphmTypesInCohPrefixDomainProp,
      3L,
      labels=c('low', 'medium', 'high')
    ),
    MphmTypesInCohSuffixDomainPropBinned3 = cut(
      MphmTypesInCohSuffixDomainProp,
      3L,
      labels=c('low', 'medium', 'high')
    ),
    MphmTypesInCohStemDomainPropBinned3 = cut(
      MphmTypesInCohStemDomainProp,
      3L,
      labels=c('low', 'medium', 'high')
    )
  )


# export word domains
descriptor <- .metadata$WordDomains
descriptor$fields$MphmTypesInCohPrefixDomainPropBinned3 <- describe_data(
  computed = "enhancements/WordDomains.R",
  ptype = factor(),
  levels = tibble(
    level = c("low", "medium", "high"),
    description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
  ),
  description = "
    Proportion of morpheme types included in a phonologically or
    grammatically coherent prefix domain, binned
  "
)
descriptor$fields$MphmTypesInCohSuffixDomainPropBinned3 <- describe_data(
  computed = "enhancements/WordDomains.R",
  ptype = factor(),
  levels = tibble(
    level = c("low", "medium", "high"),
    description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
  ),
  description = "
    Proportion of morpheme types included in a phonologically or grammatically
    coherent suffix domain, binned
  "
)
descriptor$fields$MphmTypesInCohStemDomainPropBinned3 <- describe_data(
  computed = "enhancements/WordDomains.R",
  ptype = factor(),
  levels = tibble(
    level = c("low", "medium", "high"),
    description = c("lowest 33% percentile", "middle 33% percentile", "top 33% percentile")
  ),
  description = "
    Proportion of morpheme types included in a phonologically or grammatically
    coherent stem domain, binned
  "
)

export_dataset("WordDomains", WordDomains, descriptor, "Word")
