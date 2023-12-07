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
  
  # Establishment costs  of project
  
  government_provide_subsidy_yes_no <- chance_event(if_government_provide_subsidy,
                                                 value_if = 1,
                                                 value_if_not = 0)
 irrigation_cost <-if(government_provide_subsidy_yes_no ==1){
    irrigation_establishment_cost * subsidy_cost_government_paid
  } else {
  irrigation_establishment_cost
  } 
 
 #
  government_provide_land_yes_no <- chance_event(if_government_provide_land,
                                                 value_if = 1,
                                                 value_if_not = 0)
  storage_processing_cost <-if(government_provide_land_yes_no ==1){
    estabishment_cost * land_cost_government_paid
  } else {
    estabishment_cost 
  }
 #
 government_provide_subsidy_yes_no <- chance_event(if_government_provide_subsidy,
                                                value_if = 1,
                                                value_if_not = 0)
 vehicle_purchase_cost <- if(government_provide_subsidy_yes_no ==1){
   vehicle_cost * subsidy_cost_government_paid
 } else {
  vehicle_cost
 }
  #
  initial_costs <- 
    storage_processing_cost +
    vehicle_cost +
    awareness_cost +
    guidance_report_preparation_cost +
    branding_cost

  #establishment costs for project
  
  #establishment costs for women
  #establishment_cost_year_one_womenfarmers <- landlease_cost 
  
  
  # Maintenance costs ####
  
  # maintenance costs for project annual cost
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
   
  
  ## maintenance costs for women farmers
  ## annual_cost
communication_marketing_cost <-
     Connection_negotiation_cost +
      advertisement_cost +
      meeting_cost 
#
  turmeric_production_cost <-
    organicmatter_fertiliser_women +
    Land_preparation_women +
    Farmyard_manure_application_women +  
    Mulch_collection_women +
    Plantation_cost_women +
    Weeding_cost_women +
    Harvesting_women 
  
  
    #
    turmeric_processing_cost <- fuel_boiling_women + 
                                Cleaning_and_grading_women +
                                Processing_boiling_women +
                                Drying_women +
                                Administration_operation_cost_women +
                                certification_cost
    #
    turmeric_distribution_cost <- 
    Marketing_cost_women +
    Primary_processing_women +
    Secondary_processing_women +
    Transportation_cost_women_distribution +
    Storage_cost_women +
    Packaging_cost_women 
   
   
  #cost of 1 year
    establishment_cost_year_one <- initial_costs
  
  
  # vv function for annual maintenance costs 
    annual_cost <- operation_cost + 
      communication_marketing_cost +
      turmeric_production_cost + 
      turmeric_processing_cost + 
      turmeric_distribution_cost
    #percentage of increase each year
  # vv function maintenance costs with women farmer
 
 
  
  #total_cost[1] <- establishment_cost_year_one 
  #
  total_cost_annual <- vv(annual_cost, 
                   var_CV = CV_value, 
                   n = number_of_years, 
                   relative_trend = inflation_rate)
  #
  total_cost <- establishment_cost_year_one + total_cost_annual 
  #
  
  # the first is establishment_cost_year_one
  # Calculate management plus establishment costs in the first year for women farmers

  
  
  
  # Add up all benefits #
  
  turmeric_yield <- Total_turmeric_yield  * (1-turmeric_risk * yield_turmeric_risk)
  #
  
  turmeric_benefit <- turmeric_yield * recovery_rate * Turmeric_price
  #postharvest_loss <- postharvest_losses_primary_processing_value +
                      #postharvest_losses_secondary_processing +
                      #postharvest_losses_trading 
  #
  #postharvest_loss_value <- vv(postharvest_loss, 
                               #var_CV = CV_value, 
                               #n = number_of_years, 
                               #relative_trend = inflation_rate) * turmeric_postharvest_risk
  #
  #turmeric_benefit_risk <- turmeric_benefit - (if_turmeric_risk * turmeric_benefit)
  #
  turmeric_revenue <- vv(turmeric_benefit, 
                         CV_value, 
                         number_of_years, 
                         relative_trend = inflation_rate) 
  #
  #total_benefit <- (women_empowernment_related_value + 
                      #turmeric_revenue +
                     # medicinal_related_value) - 
   # postharvest_loss_value
  # 
  Turmeric_interv_result <- turmeric_revenue - total_cost
  
    #
    
    #postharvest_loss_value 
  
  # Final result of the costs and benefits of womenfarmers
  #womenfarmers_result <-  total_benefit_womenfarmers - total_cost_womenfarmers
  
  # Final result of the costs and benefits 
  #Turmeric_interv_result <- total_benefit - total_cost
  
  #Cereal maize and milllet benefit


  #
  Total_maize_yield <- maize_harvest * (1-maize_risk * yield_maize_risk)
  maize_benefit <- Total_maize_yield * maize_price
  #
  Total_animalfeed_yield <- animalfeed_harvest * (1-maize_risk * yield_maize_risk)
  animalfeed_benefit <- Total_animalfeed_yield * animalfeed_price 
  #
  Total_firewood_yield <- firewood_harvest * (1-maize_risk * yield_maize_risk)
  firewood_benefit <- firewood_harvest * firewood_price
  #
  Total_millet_yield <- millet_harvest * (1-millet_risk * yield_millet_risk)
  millet_benefit <- millet_harvest * millet_price
  #
  cereal_benefit <- maize_benefit +
    millet_benefit +
    animalfeed_benefit + 
    firewood_benefit
  #
  cereal_revenue <- vv(cereal_benefit, 
                       CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate)
  #
  #value_of_cereal_land_use <- cereal_revenue
  #
  #land_used_yes_no <- chance_event(if_not_fallow, # some chance that the land will be fallow
                                   #value_if = 1, 
                                   #value_if_not = 0)
  
 # cereal_value <- if (land_used_yes_no == 1) {
    #vv(value_of_grass_land_use,
       #CV_value, 
       #number_of_years, 
       #relative_trend = inflation_rate) 
  #} else {
    #vv(value_of_cereal_land_use,
       #CV_value, 
       #number_of_years, 
       #relative_trend = inflation_rate)
  #}
  
  
  total_benefit_no <- cereal_revenue 
  
  #
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
  
  #
  
  NPV_interv <-
    discount(x = Turmeric_interv_result, 
             discount_rate = discount_rate, 
             calculate_NPV = TRUE)
  
  
  # NPV no intervention ####
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
                                    vars = "NPV_decision",
                                    method = 'boxplot_density')

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
                     new_name= "Turmeric and Maize",
                     unit= "USD",
                     bar_color= "yellow4",
                     base_size=12)

