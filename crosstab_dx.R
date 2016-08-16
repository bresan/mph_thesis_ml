## Explore the associations between various highlighted predictors of interest and in-hospital mortality
library(gmodels)
load("H:/Thesis/data/02_prepped_data.RData")

## Look at deaths overall
CrossTable(master_data$death,prop.c=F,prop.chisq=F,prop.t=F)

## Look at sickle cell diagnosis and misdiagnosis related to death
CrossTable(master_data$dx_misdiag_67,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)
CrossTable(master_data$dx_final_67,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)
CrossTable(master_data$dx_admit_67,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)

## Cross-tab final diagnosis of UTI and death
CrossTable(master_data$dx_final_57,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)

## Cross-tab malaria and severe malaria with death
CrossTable(master_data$dx_final_28,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)
CrossTable(master_data$dx_final_29,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)

## Cross-tab paracetamol treatment with death
CrossTable(master_data$tr_admit_paracetamol,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)

## Cross-tab paracetamol treatment with severe malaria
CrossTable(master_data$dx_final_28,master_data$tr_admit_paracetamol,prop.c=F,prop.chisq=F,prop.t=F)

## Cross-tab paracetamol treatment with malnutrition
CrossTable(master_data$dx_final_62,master_data$tr_admit_paracetamol,prop.c=F,prop.chisq=F,prop.t=F)
CrossTable(master_data$dx_final_63,master_data$tr_admit_paracetamol,prop.c=F,prop.chisq=F,prop.t=F)

## Cross-tab Inability to sit, drink, or deep breathing with death
CrossTable(master_data$ss_unablesit,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)
CrossTable(master_data$ss_unabledrink,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)
CrossTable(master_data$ss_dpbreath,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)
CrossTable(master_data$ss_diffbreath,master_data$death,prop.c=F,prop.chisq=F,prop.t=F)


