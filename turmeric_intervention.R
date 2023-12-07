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
  
  # Costs####
  
  #If government provides 50% subsidy on the establishment of irrigation cost
  
  government_provide_subsidy_yes_no <- chance_event(if_government_provide_subsidy,
                                                 value_if = 1,
                                                 value_if_not = 0)
 irrigation_cost <-if(government_provide_subsidy_yes_no ==1){
    irrigation_establishment_cost * subsidy_cost_government_paid
  } else {
  irrigation_establishment_cost
  } 
 
 # If Government provides land for the establishment of storage and processing center 
 #which is 20-25% of the total establishment cost
  government_provide_land_yes_no <- chance_event(if_government_provide_land,
                                                 value_if = 1,
                                                 value_if_not = 0)
  storage_processing_cost <-if(government_provide_land_yes_no ==1){
    estabishment_cost * land_cost_government_paid
  } else {
    estabishment_cost 
  }
  #If government provides 50% subsidy on the purchase of farm vehicle for transportation
 government_provide_subsidy_yes_no <- chance_event(if_government_provide_subsidy,
                                                value_if = 1,
                                                value_if_not = 0)
 vehicle_purchase_cost <- if(government_provide_subsidy_yes_no ==1){
   vehicle_cost * subsidy_cost_government_paid
 } else {
  vehicle_cost
 }
 
  #first year establishment cost for the project
  initial_costs <- 
    storage_processing_cost +
    vehicle_cost +
    awareness_cost +
    guidance_report_preparation_cost +
    branding_cost


  
  # Maintenance costs of project including depreciation cost for farm vehicle, storage and processing machine and minitiller
#operation cost
  operation_cost <- 
  training_cost +
  depreciation_cost +
  formation_cooperative_cost +
  land_acquisition_cost +
  end_Report_preparation_cost +
  turning_land_suitable_for_cultivation_cost +
  depreciation_cost +
  irrigation_cost +
  production_input 
   
  
 # communication_marketing_cost
communication_marketing_cost <-
      Connection_negotiation_cost +
      advertisement_cost +
      meeting_cost 

#production, processing and distribution cost for women farmer
#turmeric_production_cost
  turmeric_production_cost <-
    organicmatter_fertiliser_women +
    Land_preparation_women +
    Farmyard_manure_application_women +  
    Mulch_collection_women +
    Plantation_cost_women +
    Weeding_cost_women +
    Harvesting_women 
  
  
#turmeric_processing_cost
    turmeric_processing_cost <- fuel_boiling_women + 
                                Cleaning_and_grading_women +
                                Processing_boiling_women +
                                Drying_women +
                                Administration_operation_cost_women +
                                certification_cost
    
    #turmeric_distribution_cost
    turmeric_distribution_cost <- 
    Marketing_cost_women +
    Primary_processing_women +
    Secondary_processing_women +
    Transportation_cost_women_distribution +
    Storage_cost_women +
    Packaging_cost_women 
   
   
  #cost of 1 year for project
    establishment_cost_year_one <- initial_costs
  
  
  # total annual  costs 
    annual_cost <- operation_cost + 
      communication_marketing_cost +
      turmeric_production_cost + 
      turmeric_processing_cost + 
      turmeric_distribution_cost
    # vv function for annual  costs 
  total_cost_annual <- vv(annual_cost, 
                   var_CV = CV_value, 
                   n = number_of_years, 
                   relative_trend = inflation_rate)
  # sum total cost including first year and annual cost with vv function
  total_cost <- establishment_cost_year_one + total_cost_annual 
 
  
  # Turmeric revenue by selling turmeric powder
  #Turmeric yield with turmeric risk
  turmeric_yield <- Total_turmeric_yield  * (1-turmeric_risk * yield_turmeric_risk)
  
  #Multiplying the turmeric powder with turmeric price
  turmeric_benefit <- turmeric_yield * recovery_rate * Turmeric_price

  #turmeric benefit with vv function
  turmeric_revenue <- vv(turmeric_benefit, 
                         CV_value, 
                         number_of_years, 
                         relative_trend = inflation_rate) 

  # 
  Turmeric_interv_result <- turmeric_revenue - total_cost
  
    #
    
  
  #The cost and benefit of existing cropping system maize and millet
  #Cereal maize and millet benefit
 #Total maize benefit with risk
  Total_maize_yield <- maize_harvest * (1-maize_risk * yield_maize_risk)
  maize_benefit <- Total_maize_yield * maize_price
  
  #Total animal feed benefit of millet straw and maize stem after harvesting maize and millet with risk
  Total_animalfeed_yield <- animalfeed_harvest * (1-maize_risk * yield_maize_risk)
  animalfeed_benefit <- Total_animalfeed_yield * animalfeed_price 
  
  
  #Total firewood benefit as remaining part of  maize after extraction of maize grain(cob) is used for firewood benefit
  Total_firewood_yield <- firewood_harvest * (1-maize_risk * yield_maize_risk)
  firewood_benefit <- firewood_harvest * firewood_price
  
  #Total millet benefit with risk
  Total_millet_yield <- millet_harvest * (1-millet_risk * yield_millet_risk)
  millet_benefit <- millet_harvest * millet_price
  
  #Total cereal benefit
  cereal_benefit <- maize_benefit +
    millet_benefit +
    animalfeed_benefit + 
    firewood_benefit
  # Total cereal benefit with vv function
  cereal_revenue <- vv(cereal_benefit, 
                       CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate)

  #
  total_benefit_no <- cereal_revenue 
  
  # baseline costs 
  cereal_annual_costs <- maize_production_cost +
                         millet_production_cost +
                         maize_processing_cost +
                         millet_processing_cost
                     
  # baseline costs with vv function
  total_cost_no <- vv(cereal_annual_costs, 
                      var_CV = CV_value, 
                      n = number_of_years, 
                      relative_trend = inflation_rate)  
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
    total_costs = sum(total_cost),
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

