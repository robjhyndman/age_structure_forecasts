### Load and clean industry employment data by category
read_industry <- function(file) {
  readxl::read_excel(file, skip = 10) |>
  janitor::clean_names() |>
  head(330) |>
  transmute(category = x2_digit_level_qalfp_non_school_qualification_field_of_study,
            qualification = x1_digit_level_qallp_non_school_qualification_level_of_education,
            industry = x1_digit_level_indp_industry_of_employment,
            persons = persons) |> 
  filter(category != "Total") 
}

### Summarise industry employment proportions by category
summarise_industry <- function(industry) {
  industry |> 
    group_by(category, industry) |> 
    summarise(persons = sum(persons), .groups = "drop") |> 
    group_by(category) |>  
    mutate(proportion = persons / sum(persons)) |> 
    ungroup() 
}

### Visualising industry employment breakdown by category  
# Extract unique categories  
# categories <- unique(industry_summary$category)  

# Function to generate plots for each category  
# plot_industry <- function(category_name) {  
#   industry_summary |>   
#     filter(category == category_name) |>   
#     ggplot(aes(x = reorder(industry, -proportion), y = proportion, fill = industry)) +  
#     geom_bar(stat = "identity", show.legend = FALSE) +  
#     labs(title = category_name, x = " ") +  
#     scale_y_continuous(labels = scales::percent) +    
#     theme(axis.text.x = element_text(angle = 90, hjust = 1))  
# }  

# Generate plots for all categories  
# industry_proportions <- lapply(categories, plot_industry)  

# View the plots  
# industry_proportions  

### Rescale proportions to sum to 1 within each category for top industries
find_top_industries <- function(industry_summary) {
  industry_summary |> 
  filter(proportion > 0.11) |> 
  group_by(category) |> 
  mutate(weighted_proportion = proportion / sum(proportion)) 
}
