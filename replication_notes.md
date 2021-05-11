## Replication Notes

- Fork the repo in it's entirety

- (If given access) load the CSS data in a parallel folder to the main project folder 311_bias, like this:
  
    + 311_bias/ 
      - Data/
      - Writing/
      - Code/
      - etc.
    + 311_protected_data/
      - KCMODF_ConsolidatedData_FY13_FY20.csv
      - KCMODF_ConsolidatedData_FY13_FY20.xlsx
  
- To recreate the PDF, just run the `.Rmd` file and ensure the `\extras` folder is in the right place (`Writing\extras\ ... `)

- To replicate the figures and analysis, do the following in order:

1. Run `\Code\kc_data_pull.R`
    - Outputs: 311 data, prop viol data, census data (saved `\Data\file_xyz.csv.gz`)
    - Requirements: Census API key for use with `tidycensus`
  
2. Run `\Code\kc_data_clean.R`
    - Outputs: `df_311_viol_full` (keep this in namespace envir)

3. Run `\Code\summ_stats.R`
    - Outputs: percentages and descriptive splits for 311-violation conversions
  
4. Run `\Code\maps.R`
    - Outputs: The maps
    - Note: Be careful with ggsave, this file takes a long time (1GB gg objects)
  
5. Run `\Code\model.R`
   - Outputs: Tables and Figure with results (ROC, descr stat, vip)
   - Requirements: Properly loaded the protected data
   - Note: Can take a while... I ran it chunk by chunk
  
If replicating from scratch (just using datasets), this should set up all the appropriate tables and files so the `Writing\311_bias_report.Rmd` can be run.


That should be it ?
  
