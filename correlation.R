library(tidyverse)

x_test <- readRDS("ChallengeData/data_rds_files/x_test")

nwp1 <- x_test %>% filter(met_model == "NWP1")
nwp2 <- x_test %>% filter(met_model == "NWP2")
nwp3 <- x_test %>% filter(met_model == "NWP3")
nwp4 <- x_test %>% filter(met_model == "NWP4")


nwp1_2 <- merge(nwp1, nwp2, by = c("Time", "model_time", "forecast_time", "ID"))
nwp1_2 <- nwp1_2 %>% rename(
  WF1 = WF.x,
  U1 = U.x,
  V1 = V.x,
  T1 = T.x,
  met_model1 = met_model.x,
  CLCT1 = CLCT.x,
  U2 = U.y,
  V2 = V.y,
  T2 = T.y,
  WF2 = WF.y,
  met_model2 = met_model.y,
  CLCT2 = CLCT.y
  
)
nwp1_2_3 <- merge(nwp1_2, nwp3, by = c("Time", "model_time", "forecast_time", "ID"))
nwp1_2_3_4 <- merge(nwp1_2_3, nwp4, by = c("Time", "model_time", "forecast_time", "ID"))
nwp1_2_3_4 <- nwp1_2_3_4 %>% rename(
  WF3 = WF.x,
  U3 = U.x,
  V3 = V.x,
  T3 = T.x,
  met_model3 = met_model.x,
  CLCT3 = CLCT.x,
  U4 = U.y,
  V4 = V.y,
  T4 = T.y,
  WF4 = WF.y,
  met_model4 = met_model.y,
  CLCT4 = CLCT.y
)

df_U <- nwp1_2_3_4[c("U1", "U2", "U3", "U4")]
df_V <- nwp1_2_3_4[c("V1", "V2", "V3", "V4")]
df_T <- nwp1_2_3_4[c("T1", "T2", "T3", "T4")]

library(ggcorrplot)
ggcorrplot(cor(df_U), lab = T)
ggcorrplot(cor(df_V), lab = T)


