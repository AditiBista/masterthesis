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
  
  government_provide_land_yes_no <- chance_event(if_government_provide_land,
                                                 value_if = 1,
                                                 value_if_not = 0)
  rent_cost <-if(government_provide_land_yes_no ==1){
    land_rent_cost * land_cost_government_paid
  } else {
    land_rent_cost
  } 
  #
  initial_costs <- processingcenter_cost +
    advertisement_cost +
    branding_cost +
    vehicle_cost +
    Connection_negotiation_cost +
    awareness_cost +
    land_acquisition_cost +
    turning_land_suitable_for_cultivation_cost +
    production_input +
    storing_storehouse_cost +
    storing_unsold_turmeric +
    rent_cost
  
  #establishment costs for project
  establishment_cost_year_one <- initial_costs 
  #establishment costs for women
  establishment_cost_year_one_womenfarmers <- landlease_cost 
  
  
  # Maintenance costs ####
  
  # maintenance costs for project annual cost
  
  maintenance_cost_annual_project <- meeting_cost +
    marketing_brand_cost +
    distribution_cost +
    Report_preparation_cost +
    training_cost +
    meeting_cost
  
  
  
  ## maintenance costs for women farmers
  ## annual_cost
  maintenance_cost_annual_women <-
    seed_women +
    organicmatter_fertiliser_women +
    fuel_boiling_women + 
    Land_preparation_women +
    Farmyard_manure_application_women +  
    Mulch_collection_women +
    Plantation_cost_women +
    Weeding_cost_women +
    Harvesting_women + 
    Cleaning_and_grading_women +
    Processing_boiling_women +
    Drying_women +
    marketing_women +
    Transportation_cost_women +
    Secondary_processing_women +
    Transportation_cost_women_secondary +
    Storage_cost_women +
    Packaging_cost_women +
    Marketing_cost_women +
    Administration_operation_cost_women 
  #
  total_maintenance_cost_annual_project <- maintenance_cost_annual_women +
    maintenance_cost_annual_project
  
  
  # vv function for annual maintenance costs 
  total_cost <- vv(total_maintenance_cost_annual_project, 
                   var_CV = CV_value, 
                   n = number_of_years, 
                   relative_trend = inflation_rate) #percentage of increase each year
  # vv function maintenance costs with womenfarmer
  total_cost_womenfarmers <- vv(maintenance_cost_annual_women, 
                                var_CV = CV_value, 
                                n = number_of_years, 
                                relative_trend = inflation_rate) #percentage of increase each year
  
  # Calculate management plus establishment costs in the first year
  
  
  total_cost[1] <- establishment_cost_year_one + 
                   maintenance_cost_annual_project + 
                   maintenance_cost_annual_women # the first is establishment_cost_year_one
  # Calculate management plus establishment costs in the first year for women farmers
  total_cost_womenfarmers[1] <- establishment_cost_year_one_womenfarmers + 
                                maintenance_cost_annual_women
  
  
  # Risks ####
  
  turmeric_function_risk <-  min(if_community_likes, 
                                 if_effective_manage,
                                 if_operation_risk,
                                 if_turmeric_yield_enough,
                                 if_effective_training,
                                 if_farmer_likes,
                                 if_market_access,
                                 if_government_support_market,
                                 if_postharvest_losses,
                                 if_government_provide_land)
  
  
  
  turmeric_health_risk <- min(if_effective_manage,
                              if_turmeric_yield_enough,
                              if_turmeric_organic,
                              if_effective_training)
  
  
  turmeric_postharvest_risk <- min(if_effective_manage,
                                   if_operation_risk,
                                   if_turmeric_yield_enough,
                                   if_effective_training,
                                   if_farmer_likes,
                                   if_postharvest_losses) 
  
  
  #health benefits from organic turmeric
  medicinal_related_value <- vv(organic_hospitalsaving_value,
                                CV_value,
                                number_of_years,
                                relative_trend = inflation_rate) * turmeric_health_risk
  
  
  
  #women empowerment benefit
  women_empowernment_related_value <- vv(women_empowernment_value, 
                                         CV_value, 
                                         number_of_years, 
                                         relative_trend = inflation_rate) * turmeric_function_risk
  
  
  # Add up all benefits ####
  
  
  turmeric_benefit <- Total_turmeric_yield * recovery_rate * Turmeric_price
  #
  postharvest_loss <- postharvest_losses_primary_processing_value +
    postharvest_losses_secondary_processing +
    postharvest_losses_trading 
  #
  postharvest_loss_value <- vv(postharvest_loss, 
                               var_CV = CV_value, 
                               n = number_of_years, 
                               relative_trend = inflation_rate) * turmeric_postharvest_risk
  #
  turmeric_benefit_risk <- turmeric_benefit - (if_turmeric_risk * turmeric_benefit)
  #
  turmeric_revenue <- vv(turmeric_benefit_risk, 
                         CV_value, 
                         number_of_years, 
                         relative_trend = inflation_rate) * turmeric_function_risk
  #
  total_benefit <- (women_empowernment_related_value + 
                      turmeric_revenue +
                      medicinal_related_value) - 
                      postharvest_loss_value
  # 
  total_benefit_womenfarmers <- (women_empowernment_related_value + 
                                   turmeric_revenue +
                                   medicinal_related_value) - 
                                 postharvest_loss_value 
  
  # Final result of the costs and benefits of womenfarmers
  womenfarmers_result <-  total_benefit_womenfarmers - total_cost_womenfarmers
  
  # Final result of the costs and benefits 
  Turmeric_interv_result <- total_benefit - total_cost
  
  #Cereal maize and wheat benefit
  #
  animalfeed_benefit <- animalfeed_harvest * animalfeed_price
  # 
  firewood_benefit <- firewood_harvest * firewood_price
  #                
  maize_benefit <- maize_harvest * maize_price
  #
  millet_benefit <- millet_harvest * millet_price
  #
  cereal_benefit <- maize_benefit +
    millet_benefit +
    animalfeed_benefit + 
    firewood_benefit
  #
  cereal_benefit_risk <- cereal_benefit - (cereal_risk * cereal_benefit)
  #
  cereal_revenue <- vv(cereal_benefit_risk, 
                       CV_value, 
                       number_of_years, 
                       relative_trend = inflation_rate)
  #
  value_of_cereal_land_use <- cereal_revenue
  #
  land_used_yes_no <- chance_event(if_not_fallow, # some chance that the land will be fallow
                                   value_if = 1, 
                                   value_if_not = 0)
  
  cereal_value <- if (land_used_yes_no == 1) {
    vv(value_of_grass_land_use,
       CV_value, 
       number_of_years, 
       relative_trend = inflation_rate) 
  } else {
    vv(value_of_cereal_land_use,
       CV_value, 
       number_of_years, 
       relative_trend = inflation_rate)
  }
  
  
  total_benefit_no <- cereal_value 
  
  #
  # baseline costs 
  cereal_annual_costs <- maize_production_cost +
    millet_production_cost +
    maize_drying_cost +
    millet_drying_cost
  
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
  #
  NPV_womenfarmers <-
    discount(x = womenfarmers_result, 
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
    womenfarmers_result = NPV_womenfarmers,
    NPV_decision = NPV_interv - NPV_no_interv,
    NPV_decision_womenfarmers = NPV_womenfarmers - NPV_no_interv,  
    total_costs = sum(total_cost),
    total_costs_womenfarmers = sum(total_cost_womenfarmers),
    Cashflow_decision = Turmeric_interv_result,
    cashflow_decision_women = womenfarmers_result))
  
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
  vars = c("NPV_decision","NPV_decision_womenfarmers"),
  method = 'smooth_simple_overlay',
  colors = c("#0000FF","green"),
  base_size = 6)

#
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = c("Turmeric_interv_result",
                                             "no_intervention_result",
                                             "NPV_decision",
                                             "NPV_decision_womenfarmers",
                                             "womenfarmers_result"),
                                    colors = c("#0000FF","brown","green","pink","yellow"),
                                    method = 'boxplot',
                                    base_size = 10)
#
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision",
                                    method = 'boxplot_density')
#
decisionSupport::plot_distributions(mcSimulation_object = mcSimulation_results, 
                                    vars = "NPV_decision_womenfarmers",
                                    method = 'boxplot_density')
#
plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "Cashflow_decision")
plot_cashflow(mcSimulation_object = mcSimulation_results, cashflow_var_name = "cashflow_decision_women")
#
pls_result <- plsr.mcSimulation(object = mcSimulation_results,
                                resultName = names(mcSimulation_results$y)[3], ncomp = 1)
#
input_table <- read.csv("turmeric_interv.csv")
#

plot_pls(pls_result, input_table = turmeric_interv, threshold = 0)
#######################

mcresults_table <- data.frame(mcSimulation_results$x,
                              mcSimulation_results$y[, c("Turmeric_interv_result", "no_intervention_result", "NPV_decision","womenfarmers_result" )])
evpi<- multi_EVPI(mc= mcresults_table,
                  first_out_var = "NPV_decision", write_table = FALSE)
plot_evpi<-plot_evpi(evpi,
                     decision_vars = "NPV_decision",
                     new_name= "Turmeric and Maize",
                     unit= "USD",
                     bar_color= "yellow4",
                     base_size=12)
#wmenfarmers
evpi<- multi_EVPI(mc= mcresults_table,
                  first_out_var = "NPV_decision_womenfarmers", write_table = FALSE)
plot_evpi<-plot_evpi(evpi,
                     decision_vars = "NPV_decision_womenfarmers",
                     new_name= "Turmeric and Maize",
                     unit= "USD",
                     bar_color= "yellow4",
                     base_size=12)

