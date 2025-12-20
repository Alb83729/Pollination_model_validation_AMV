load_expert_tables <- function(path_file_expert_table){
  expert_table <- readr::read_csv(path_file_expert_table)

  # Habitat nesting-------------------
  # Extract expert info
  NS_table_big <- expert_table[,c("code","nesting_suitability_big")] %>%
    rename(nesting_suitability=nesting_suitability_big
    ) %>%
    mutate(species="big")

  NS_table_small <- expert_table[,c("code","nesting_suitability_small")] %>%
    rename(nesting_suitability=nesting_suitability_small
    ) %>%
    mutate(species="small")

  # Habitat nesting-------------------
  # Extract expert info
  # Big wild bees

  HF_table_big_season_1 <- expert_table[,c("code",
                                           "foraging_suitability_big_season_1",
                                           "fl_resource_weight_season_1"
  )] %>%
    rename(foraging_suitability=foraging_suitability_big_season_1,
           fl_resource_weight=fl_resource_weight_season_1
    ) %>%
    mutate(species="big",
           season = 1)


  HF_table_big_season_2 <- expert_table[,c("code",
                                           "foraging_suitability_big_season_2",
                                           "fl_resource_weight_season_2"
  )] %>%
    rename(foraging_suitability=foraging_suitability_big_season_2,
           fl_resource_weight=fl_resource_weight_season_2
    ) %>%
    mutate(species="big",
           season = 2)


  HF_table_big_season_3 <- expert_table[,c("code",
                                           "foraging_suitability_big_season_3",
                                           "fl_resource_weight_season_3"
  )] %>%
    rename(foraging_suitability=foraging_suitability_big_season_3,
           fl_resource_weight=fl_resource_weight_season_3
    ) %>%
    mutate(species="big",
           season = 3)

  HF_table_big <- bind_rows(HF_table_big_season_1,
                            HF_table_big_season_2,
                            HF_table_big_season_3)


  # Small wild bees

  HF_table_small_season_1 <- expert_table[,c("code",
                                             "foraging_suitability_small_season_1",
                                             "fl_resource_weight_season_1"
  )] %>%
    rename(foraging_suitability=foraging_suitability_small_season_1,
           fl_resource_weight=fl_resource_weight_season_1
    ) %>%
    mutate(species="small",
           season = 1)


  HF_table_small_season_2 <- expert_table[,c("code",
                                             "foraging_suitability_small_season_2",
                                             "fl_resource_weight_season_2"
  )] %>%
    rename(foraging_suitability=foraging_suitability_small_season_2,
           fl_resource_weight=fl_resource_weight_season_2
    ) %>%
    mutate(species="small",
           season = 2)


  HF_table_small_season_3 <- expert_table[,c("code",
                                             "foraging_suitability_small_season_3",
                                             "fl_resource_weight_season_3"
  )] %>%
    rename(foraging_suitability=foraging_suitability_small_season_3,
           fl_resource_weight=fl_resource_weight_season_3
    ) %>%
    mutate(species="small",
           season = 3)

  HF_table_small <- bind_rows(HF_table_small_season_1,
                              HF_table_small_season_2,
                              HF_table_small_season_3)


  return(list(NS_table_big,NS_table_small,HF_table_big,HF_table_small))
}
