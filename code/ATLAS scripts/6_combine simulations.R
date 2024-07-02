library(tidyverse)
library(progress)

for(ss in c("A", "B", "D")) {
  
  sim_files <- list.files(paste0("outputs/script_5/APHA output/site ", ss))
  
  for(i in sim_files) {
    if(i == sim_files[1]) pb <- progress_bar$new(total = length(sim_files), 
                                                 format = "[:bar] :percent eta::eta", 
                                                 clear = F); pb$tick(0)
    df_id <- readRDS(paste0("outputs/script_5/APHA output/site ", ss, "/", i))
    
    if(i == sim_files[1]) {
      all_df <- df_id
    } else {
      all_df <- rbind(all_df, df_id)
    }
    pb$tick()
  };saveRDS(all_df, "outputs/script_6/simulation_data.rds")
}
