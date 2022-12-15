library(biomformat)
library(dplyr)
library(tibble)

biom_exa<-read_biom("https://raw.githubusercontent.com/lentendu/V4_SSU_ASV_bioinformatic_pipeline/main/V4_SSU_example.ASV_table.biom")
mat_exa<-biom_data(biom_exa) %>%
  as.matrix() %>%
  t()
ranks<-c("kingdom","division","phylum","class","order","family","genus","species")
taxo_ranks<-function(x) ranks[as.numeric(sub(".*([0-9])$","\\1",x))]
taxo_exa<-observation_metadata(biom_exa) %>%
  rownames_to_column("repseq") %>%
  rename_with(taxo_ranks,starts_with("taxo")) %>%
  mutate(across(c(starts_with("boots"),similarity),~as.numeric(.))) %>%
  mutate(across(all_of(ranks),~as.factor(sub("^[a-z]:","",na_if(., "NA")))))

usethis::use_data(mat_exa, overwrite = TRUE)
usethis::use_data(taxo_exa, overwrite = TRUE)
