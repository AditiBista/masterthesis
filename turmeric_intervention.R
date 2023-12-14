library(decisionSupport)
#install.packages("readr")
library(readr)
turmeric_interv <- read_csv("turmeric_interv.csv")
#View(turmeric_interv)
make_variables <- function(est,n=1) { 
  x <- random(rho=est, n=n)
  for (i in colnames(x)) assign(i,as.numeric(x[1,i]), envir=.GlobalEnv)
}

make_variables(as.estimate(turmeric_interv))

# The model ####

Turmeric_function <- function(x, varnames){
 
  #implementation cost for the turmeric farmer
  turmeric_implementation_costs <- 
  Land_preparation_cost_turmeric +
   Seed_turmeric 
  
# Maintenance costs for the turmeric farmer
  turmeric_maintenance_cost <- 
    FYM_cost_turmeric +
    Weeding_cost_turmeric+
    Plantation_cost_turmeric +
  Mulch_collection_cost_turmeric

#harvesting cost of turmeric farmer
 Turmeric_harvesting_cost <- 
   Extracting_turmeric_costs +
 Removing_roots_costs +
 sorting_seed_costs +
 gathering_costs

#post harvesting cost of turmeric farmer
Turmeric_postharvesting_cost <-   
boiling_costs_turmeric +
drying_costs_turmeric +
grinding_costs_turmeric +
packaging_costs_turmeric +
storage_costs_turmeric

# first year cost for turmeric farmer
turmeric_initial_cost <- turmeric_implementation_costs
#Total annual cost for turmeric farmer
turmeric_cost <- 
Turmeric_postharvesting_cost +  
Turmeric_harvesting_cost + 
turmeric_maintenance_cost 
#turmeric annual cost with vv function
total_cost_turmeric <- vv(turmeric_cost, 
                        var_CV = CV_value, 
                        n = number_of_years, 
                        relative_trend = inflation_rate) 
#sum of total cost for turmeric farmer
total_cost <- total_cost_turmeric + turmeric_initial_cost
# Benefit of turmeric
#Turmeric yield with turmeric risk

turmeric_yield <- Total_turmeric_yield * (1-extream_climatic_events_turmeric * yield_climate_risk_turmeric) *
                  (1-disease_pests_turmeric * yield_disease_risk_turmeric)
##Turmeric revenue by selling turmeric powder

turmeric_revenue <- (turmeric_yield * Turmeric_price) +
                     Improve_livelihood
#total benefit of turmeric with vv function
total_benefit <- vv(turmeric_revenue, 
                     CV_value, 
                     number_of_years, 
                     relative_trend = inflation_rate) 

# turmeric result
Turmeric_interv_result <- total_benefit - total_cost
#implementation cost for the cereal farmer, initial cost
cereal_implementation_costs <- 
Seed_cereal +
Land_preparation_cost_cereal

# Maintenance costs for the cereal farmer
cereal_maintenance_cost <- 
FYM_cost_cereal +
Weeding_cost_cereal +
Plantation_cost_cereal 


#harvesting cost of cereal
cereal_harvesting_cost <- 
cutting_cereal_costs +
shellling_thresing_cereal_costs

#post harvesting cost of cereal
cereal_postharvesting_cost <-   
cleaning_costs_cereal +
storage_costs_cereal

#cereal initial cost
cereal_initial_cost <- cereal_implementation_costs 
#Total cost
cereal_cost <- 
cereal_maintenance_cost +  
cereal_harvesting_cost + 
cereal_postharvesting_cost 

# baseline costs with vv function
total_cost_cereal <- vv(cereal_cost, 
                    var_CV = CV_value, 
                    n = number_of_years, 
                    relative_trend = inflation_rate)  
#Total cost for cereal farmer  
total_cost_no <- total_cost_cereal + cereal_initial_cost 
   

  #maize benefit
Total_maize_yield <- maize_harvest * (1-disease_pests_maize_risk * yield_disease_pest_maize_risk) *
                  (1-extream_climatic_events_maize * yield_climate_maize_risk) *
                  (1-wild_animal_attack_maize * yield_wild_animal_maize_risk)
maize_benefit <- maize_price * Total_maize_yield 
#millet benefit
Total_millet_yield <- millet_harvest * (1-disease_pests_millet_risk * yield_disease_pest_millet_risk) *
  (1-extream_climatic_events_millet * yield_climate_millet_risk) *
  (1-wild_animal_attack_millet * yield_wild_animal_millet_risk)
millet_benefit <- Total_millet_yield * millet_price

#Total maizestem feed benefit for animal
Total_maizestem_yield <- maizestem_harvest * (1-extream_climatic_events_maize * yield_climate_maize_risk) *
  (1-disease_pests_maize_risk * yield_disease_pest_maize_risk) 
maizestem_benefit <- Total_maizestem_yield * maizestem_price 

#millet straw benefit
Total_milletstraw_yield <- milletstraw_harvest * (1-extream_climatic_events_millet * yield_climate_millet_risk) *
  (1-disease_pests_millet_risk * yield_disease_pest_millet_risk) 
milletstraw_benefit <- Total_milletstraw_yield * milletstraw_price 

#Total firewood benefit as remaining part of  maize after extraction of maize grain(cob) is used for firewood benefit
Total_firewood_yield <- firewood_harvest * (1-maize_risk * yield_maize_risk)
firewood_benefit <- firewood_harvest * firewood_price

#millet benefit
cereal_benefit <- millet_benefit + 
                   food_access + 
                   maize_benefit +
                   maizestem_benefit +
                   milletstraw_benefit +
                   firewood_benefit 
#The cost and benefit of existing cropping system maize and millet
#Cereal maize and millet benefit
#Total maize benefit with risk
#cereal benefit millet_price
  #cereal benefit with vv function
cereal_revenue <- vv(cereal_benefit, 
                         CV_value, 
                         number_of_years, 
                         relative_trend = inflation_rate) 


  #

  total_benefit_no <- cereal_revenue 
                     

  # subtract 
  
  no_intervention_result <- total_benefit_no - total_cost_no
  
  
  #Calculation of NPV
  #NPV with intervention
  NPV_interv <-
    discount(x = Turmeric_interv_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  
  # NPV no intervention 
  NPV_no_interv <-
    discount(x = no_intervention_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  #
  return(list(
    Turmeric_interv_result = NPV_interv,
    no_intervention_result = NPV_no_interv,
    NPV_decision = NPV_interv - NPV_no_interv,
    Cashflow_decision = Turmeric_interv_result))
  
}

library(sf)

# MC Simulation function

mcSimulation_results <- decisionSupport::mcSimulation(
  estimate = decisionSupport::estimate_read_csv("turmeric_interv.csv"),
  model_function = Turmeric_function,
  numberOfModelRuns = 1e3, #run 1,000 times
  functionSyntax = "plainNames"
)

#
decisionSupport::plot_distributions(
  mcSimulation_object = mcSimulation_results, 
  vars = c("NPV_decision"),
  method = 'smooth_simple_overlay',
  colors = c("#0000FF"),
  base_size = 6)

#
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Turmeric_interv_result",
                                             "no_intervention_result",
                                             "NPV_decision"),
                                    colors = c("#0000FF","brown","green"),
                                    method = 'boxplot',
                                    base_size = 10)
#
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Turmeric_interv_result",
                                             "no_intervention_result",
                                             "NPV_decision"),
                                    colors = c("#0000FF","brown","green"),
                                    method = 'smooth_simple_overlay',
                                    base_size = 10)

#
plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision")

#
pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)
#
input_table <- read.csv("turmeric_interv.csv")
#

plot_pls(pls_result, input_table = turmeric_interv, threshold = 0)
#

mcresults_table <- data.frame(mcSimulation_results$x,
                              mcSimulation_results$y[, c("Turmeric_interv_result", "no_intervention_result", "NPV_decision" )])
evpi<- multi_EVPI(mc= mcresults_table,
                  first_out_var = "NPV_decision", write_table = FALSE)
plot_evpi<-plot_evpi(evpi,
                     decision_vars = "NPV_decision",
                     new_name= "Turmeric and cereal",
                     unit= "USD",
                     bar_color= "yellow4",
                     base_size=12)

