
#### Function for EDA on islanders data ####
eda_islanders <- function(clean_data){

  #### Global Theme
  set_theme(theme_minimal())

  #### Univariate Analysis ####

  ## Memory Score Before Taking the Drugs ##
  mem_before_dist <- ggplot(data = clean_data,aes(x = memory_score_before))+
    geom_histogram(fill = "pink",color = "black")+
    labs(
      title = "Memory Score Time to Complete Test Before Taking Drugs",
      x = "Memory Score",
      y = "Count"
      )

  ## Memory Score After Taking the Drugs ##
  mem_after_dist <- ggplot(data = clean_data,aes(x = memory_score_after))+
    geom_histogram(fill = "lightblue",color = "black")+
    labs(
      title = "Memory Score Time to complete Test After Taking Drugs",
      x = "Memory Score",
      y = "Count"
    )

  #### Bivariate Analysis ####

  ## Memory Score Before VS After
  before_after_comp <-
    clean_data %>%
    select(memory_score_before,memory_score_after) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Memory_Score_Type",
      values_to = "Score"
    ) %>%
    mutate(
      Memory_Score_Type = ifelse(
        Memory_Score_Type == "memory_score_before",
        yes = "Before",
        no = "After"
      )
    ) %>%
    ggplot(aes(x = Score,fill = Memory_Score_Type))+
    geom_boxplot()+
    scale_fill_discrete(palette = c("lightblue","pink"))+
    labs(
      title = "Memory Score Comparison Boxplot Before and After Drugs",
      fill = "Type"
    )

  ## Memory Score After and Drug Type ##
  after_drug_type <-
    ggplot(data = clean_data,aes(x = memory_score_after,fill = drug_type))+
    geom_boxplot()+
    scale_fill_viridis_d(option = "A",begin = 0.4,end = 0.8)+
    labs(
      title = "Memory Scores After Drugs by Drug Types",
      x = "Memory Score (Time to complete)",
      fill = "Drug Type"
    )

  ## Memory Score After and Dosage ##
  after_drug_dosage <-
    ggplot(data = clean_data,aes(x = memory_score_after,fill = dosage))+
    geom_boxplot()+
    scale_fill_viridis_d(option = "B",begin = 0.4,end = 0.9)+
    labs(
      title = "Memory Scores After Drugs by Dosage",
      x = "Memory Score (Time to complete)",
      fill = "Drug Dosage"
    )

  ## Delta Score  and Drug Type ##
  delta_drug_type <-
    ggplot(data = clean_data,aes(x = delta_score,fill = drug_type))+
    geom_boxplot()+
    scale_fill_viridis_d(option = "C",begin = 0.4,end = 0.9)+
    labs(
      title = "Delta Scores by Drug Type ",
      x = "Delta Scores",
      fill = "Drug Type"
    )

  #### Multivariate Analysis ####

  ## Memory Score After by Drug Type and Dosage
  after_drug_type_dosage <-
    ggplot(data = clean_data,aes(x = memory_score_after,fill = drug_type))+
    geom_boxplot()+
    scale_fill_viridis_d(option = "B",begin = 0.4,end = 0.9)+
    facet_wrap(~dosage)+
    labs(
      title = "Memory Scores After by Drug Type and Drug Dosage",
      x = "Memory Scores",
      fill = "Drug Type"
    )

  ## Memory Score After by Drug Type and Dosage Stat Table
  after_drug_type_dosage_table <- clean_data %>%
    group_by(dosage,drug_type) %>%
    summarise(
      mean_score = mean(memory_score_after),
      median_score = median(memory_score_after),
      sd_score = sd(memory_score_after)
    ) %>%
    arrange(desc(median_score))

  #### Statistical Hypothesis Testing Ref ####

  ## Anova Drug Type and Memory After
  aov_memory_after_drug_type <- aov(memory_score_after ~ drug_type,data = clean_data,qr = TRUE)
  post_hoc_memory_after_drug_type <- TukeyHSD(aov_memory_after_drug_type)

  ## Anova Delta Scores and Drug Type
  aov_delta_scores_drug_type = aov(delta_score ~ drug_type,data = clean_data,qr = TRUE)
  post_hoc_delta_scores_drug_type = TukeyHSD(aov_delta_scores_drug_type)

  #### Regression Analysis ####

  ## Memory Score Before ~ Drug Type
  lm_memory_before_drug_type <- summary(lm(memory_score_after ~ drug_type,data = clean_data))

  ## Delta Score Before ~ Drug Type
  lm_delta_score_drug_type <- summary(lm(delta_score ~ drug_type,data = clean_data))

  ## Memory Score Before ~ Drug Type + Dosage
  lm_memory_before_drug_type_dosage <- summary(
    lm(memory_score_after ~ drug_type + dosage,data = clean_data))

  #### Return ####
  return(list(
    univariate_analysis = list(
      distribution_memory_before = mem_before_dist,
      distribution_memory_after = mem_after_dist
    ),
    bivarite_analysis = list(
      comparison_memory = before_after_comp,
      memory_after_and_drug_type = after_drug_type,
      memory_after_and_dosage = after_drug_dosage,
      delta_score_and_drug_type = delta_drug_type
    ),
    multivariate_analysis = list(
      memory_after_by_drug_type_and_dosage = after_drug_type_dosage,
      stat_table = after_drug_type_dosage_table
    ),
    statistical_test_refrence = list(
      anova_memory_after_drug_type = list(
        anova = aov_memory_after_drug_type,
        post_hoc = post_hoc_memory_after_drug_type
      ),
      anova_delta_scores_drug_type = list(
        anova = aov_delta_scores_drug_type,
        post_hoc = post_hoc_delta_scores_drug_type
      )
    ),
    regression_analysis = list(
      memory_before_and_drug_type = lm_memory_before_drug_type,
      delta_score_and_drug_type = lm_delta_score_drug_type,
      memory_before_drug_type_dosage = lm_memory_before_drug_type_dosage
    )
  ))

}
