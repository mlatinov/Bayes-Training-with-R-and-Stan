
#### Function to Clean the islanders Data ####
clean_islanders <- function(data){

  data %>%

    ## Drop the personal names form the data
    select(-first_name,-last_name) %>%

    ## Change the variables
    mutate(

      ## Convert Happy and Sad group to a factor with levels 0 happy 1 sad
      mood = as.factor(ifelse(Happy_Sad_group == "S",yes = 1,no = 0)),

      ## Change the dosage levels to a factor with levels from 0 to 2
      dosage = as.factor(case_when(
        Dosage == 1 ~ 0,
        Dosage == 2 ~ 1,
        Dosage == 3 ~ 2
        )),

      ## Encode Drug type are factor with levels from 0 to 2
      drug_type = as.factor(case_when(
        Drug == "A" ~ 0,
        Drug == "S" ~ 1,
        Drug == "T" ~ 2
      ))
    ) %>%
    ## Drop old variables
    select(-Happy_Sad_group,-Dosage,-Drug) %>%

    ## Rename Variables ##
    rename(
      memory_score_before = Mem_Score_Before,
      memory_score_after = Mem_Score_After,
      delta_score = Diff
    )
}
