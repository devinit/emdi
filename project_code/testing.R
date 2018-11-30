list.of.packages <- c("data.table","emdi","maptools","rgeos","rgdal","ineq","parallel","nlme")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only=T)


#Loading PovcalNet data
if(.Platform$OS.type == "unix"){
  prefix = "~"
}else{
  prefix = "E:"
}
wd = paste0(prefix,"/git/emdi")
setwd(wd)

# Test data
data("eusilcA_smp")
table(eusilcA_smp$state)
summary(as.numeric(table(eusilcA_smp$district)))

# EMDI Direct
emdi_direct <- direct(
  y = "eqIncome",
  smp_data = eusilcA_smp,
  smp_domains = "district",
  weights = "weight",
  threshold = 10885.33,
  var = TRUE
)

#EMDI model
emdi_model <- ebp(
  fixed = eqIncome ~ gender + eqsize + cash + self_empl +
    unempl_ben + age_ben + surv_ben + sick_ben + dis_ben + rent +
    fam_allow + house_allow + cap_inv + tax_adj, pop_data = eusilcA_pop,
  pop_domains = "district", smp_data = eusilcA_smp,
  smp_domains = "district", threshold = 10885.33, MSE = TRUE,
  custom_indicator = list(
    my_max = function(y, threshold){max(y)},
    my_min = function(y, threshold){min(y)}
  )
)

summary(emdi_model)

plot(emdi_model, label = "no_title", color = c("red3", "red4"))

head(estimators(emdi_model, indicator = c("Gini", "Median"), MSE = FALSE, CV = TRUE))

compare_plot(emdi_direct, emdi_model, indicator = c("Gini", "Median"), label = "no_title", color = c("red3", "blue"))

load_shapeaustria()

map_plot(emdi_model, MSE = FALSE, CV = TRUE, map_obj = shape_austria_dis, indicator = "Median", map_dom_id = "PB")

write.excel(emdi_model, file = "project_data/excel_output.xlsx", indicator = "Median", MSE = FALSE, CV = TRUE)

# Incorporating external indicator
my_theil <- function(y, weights, threshold) {ineq(x = y, type = "Theil")}
my_indicators <- list(theil = my_theil)
emdi_direct2 <- direct(y = "eqIncome", smp_data = eusilcA_smp,
                       smp_domains = "district", weights = "weight", var = TRUE,
                       custom_indicator = my_indicators)

select_theil <- estimators(emdi_direct2, indicator = "theil", CV = TRUE)
subset(select_theil, Domain == "Wien")

# Parallelization
system.time(emdi_model1 <- ebp(fixed = eqIncome ~ gender + eqsize + cash +
                                 self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben +
                                 rent + fam_allow + house_allow + cap_inv + tax_adj,
                               pop_data = eusilcA_pop, pop_domains = "district",
                               smp_data = eusilcA_smp, smp_domains = "district", threshold = 10885.33,
                               MSE = TRUE, seed = 100, cpus = 1))

system.time(emdi_model2 <- ebp(fixed = eqIncome ~ gender + eqsize + cash +
                                 self_empl + unempl_ben + age_ben + surv_ben + sick_ben + dis_ben +
                                 rent + fam_allow + house_allow + cap_inv + tax_adj,
                               pop_data = eusilcA_pop, pop_domains = "district",
                               smp_data = eusilcA_smp, smp_domains = "district", threshold = 10885.33,
                               MSE = TRUE, seed = 100, cpus = 2))
