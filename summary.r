library(dplyr)
summary_stats <- function(data)({
    total=nrow(data)
    eligible=sum(data$Eligibility == "Eligible")
    ineligible=sum(data$Eligibility == "Ineligible")
    missing=sum(data$Eligibility == "Missing Data")
    inconsistent=sum(data$Eligibility == "Inconsistent")
   
    list(
       
        total_subjects=total,
        eligible=eligible,
        eligible_pct = round(eligible/total*100,1),
        ineligible=ineligible,
        ineligible_pct=round(ineligible/total*100,1),
        missing=missing,
        missing_pct=round(missing/total*100,1),
        inconsistent=inconsistent,
        inconsistent_pct=round(inconsistent/total*100,1)
    
    )
})
eligibility_rate <- function(data)({
    
    round(mean(data$Eligibility=="Eligible")*100, 1)
    
})
missing_rate <- function(data)({
    
    round(mean(data$Eligibility=="Missing Data")*100, 1)
})
inconsistent_rate <- function(data)({
    
    round(mean(data$Eligibility=="Inconsistent")*100, 1)
})
#eligibility count
eligibility_summary <- function(data)({
    data %>% count(Eligibility)
}
)
#country summary
country_summary <- function(data)({
    data %>% group_by(Country, Eligibility) %>%
    summarise(Subjects=n(), .groups="drop")
})
#site summary
site_summary <- function(data)({
    data %>% group_by(Country, Site, Eligibility) %>% summarise(Subjects=n(), .groups="drop")
})
#reason summary
reason_summary <- function(data)({
    data %>% count(Reason)
})
#Tumor locationsummary
tumor_summary <- function(data)({
    data %>% count(PTUMLOC)
})

#missing variable summary
missing_summary <- function(data)({
    tibble(
        Variable=c("TNM_T", "TNM_N", "TNM_M"), 
        Missing=c(
            sum(is.na(data$TNM_T)),
            sum(is.na(data$TNM_N)),
            sum(is.na(data$TNM_M))
        )

    )
})
eligible_location <- function(data)({

  data %>%
    group_by(PTUMLOC) %>%
    summarise(

      Eligible=sum(Eligibility=="Eligible")

    )

})
#site risk summary
risk_summary <- function(data)({
    data %>% group_by(Site) %>% 
    summarise(
        Missing=sum(Eligibility=="Missing Data"),
        Inconsistent=sum(Eligibility=="Inconsistent"),
        Eligible=sum(Eligibility=="Eligible"),
        Total=n(),
        .groups="drop"
    )
})
#top risk site
top_site <- function(data)({
    risk_summary(data) %>% arrange(desc(Missing))
})
quality_summary <- function(data)({


  tibble(

    Metric=c(

      "Total Subjects",

      "Eligibility Rate",

      "Missing Data Rate",

      "Inconsistent Rate"

    ),

    Value=c(

      nrow(data),

      paste0(round(mean(data$Eligibility=="Eligible")*100,1),"%"),

      paste0(round(mean(data$Eligibility=="Missing Data")*100,1),"%"),

      paste0(round(mean(data$Eligibility=="Inconsistent")*100,1),"%")

    )

  )

})
high_risk_sites <- function(data){

  risk_summary(data) %>%
    filter(Missing + Inconsistent >= 20) %>%
    summarise(HighRiskSites = n()) %>%
    pull(HighRiskSites)

}