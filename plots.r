library(ggplot2)
library(dplyr)
library(scales)
#Eligibility Distribution
eligibility_plot <- function(data){

  df <- data %>%
    count(Eligibility) %>%
    mutate(
      Percent = round(n/sum(n)*100,1),
      Eligibility = factor(
        Eligibility,
        levels=c("Eligible","Ineligible","Missing Data","Inconsistent")
      )
    )

  ggplot(
    df,
    aes(
      x=n,
      y=Eligibility,
      fill=Eligibility,
      text=paste0(
        "<b>Status:</b> ", Eligibility,
        "<br><b>Subjects:</b> ", comma(n),
        "<br><b>Percent:</b> ", Percent,"%"
      )
    )
  )+

    geom_col(width=.45)+

    geom_text(
      aes(label=comma(n)),
      hjust=1.2,
      colour="black",
      fontface="bold",
      size=3
    )+

   scale_fill_manual(

values=c(

"Eligible" = "#1B7F3B",
      "Ineligible" = "#D94841",
      "Missing Data" = "#D18F00",
      "Inconsistent" = "#6A3FB5"

)

)+

    scale_x_continuous(expand=expansion(mult=c(0,0.15)))+

    labs(
      x="Subjects",
      y=NULL
    )+

    theme_minimal(base_size=11)+

    theme(
      legend.position="none",
      panel.grid.major.y=element_blank(),
      panel.grid.minor=element_blank()
    )

}
#country wise eligibility
country_plot <- function(data){

df <- data %>%
  count(Country, Eligibility, name="Subjects")

totals <- df %>%
  group_by(Country) %>%
  summarise(Total=sum(Subjects))

ggplot(df,
aes(
Country,
Subjects,
fill=Eligibility,
text=paste0(
"<b>Country:</b> ",Country,
"<br><b>Status:</b> ",Eligibility,
"<br><b>Subjects:</b> ",comma(Subjects)
)
))+

geom_col(width=.45)+

geom_text(
data=totals,
aes(
Country,
Total,
label=comma(Total)
),
inherit.aes=FALSE,
vjust=-0.5,
fontface="bold",
size=3
)+

scale_fill_manual(values=c(

"Eligible" = "#1B7F3B",
      "Ineligible" = "#D94841",
      "Missing Data" = "#D18F00",
      "Inconsistent" = "#6A3FB5"

))+

scale_y_continuous(
expand=expansion(mult=c(0,.08))
)+

labs(
x=NULL,
y="Subjects"
)+

theme_minimal(base_size=12)+

theme(

legend.position="top",

legend.title=element_blank(),

panel.grid.major.x=element_blank(),

panel.grid.minor=element_blank(),
axis.text.x = element_text(
  angle = 60,
  hjust = 1,
  size = 10
)
)
}
#Site-wise elgibility
site_plot <- function(data){

top_sites <-

data %>%
count(Site,name="Total") %>%
slice_max(
Total,
n=20
)

df<-

data %>%

filter(
Site %in% top_sites$Site
) %>%

count(
Site,
Eligibility,
name="Subjects"
)

totals<-

df %>% 

group_by(Site)%>%

summarise(
Total=sum(Subjects)
) 

df <- df %>%
  left_join(totals, by = "Site") %>%
  mutate(
    Site = reorder(Site, -Total)
  )

ggplot(

df,

aes(
  x = Site,
  y = Subjects,
  fill = Eligibility,
  text = paste0(
    "<b>Site:</b> ", Site,
    "<br><b>Status:</b> ", Eligibility,
    "<br><b>Subjects:</b> ", comma(Subjects)
  )
)

)+

geom_col(width=.7)+

geom_text(

data=totals,

aes(
  x = Site,
  y = Total,
  label = comma(Total)
),

inherit.aes=FALSE,

vjust=-0.4,

size=3

)+

scale_fill_manual(values=c(

"Eligible" = "#1B7F3B",
      "Ineligible" = "#D94841",
      "Missing Data" = "#D18F00",
      "Inconsistent" = "#6A3FB5"

))+

scale_y_continuous(

expand=expansion(mult=c(0,.08))

)+

labs(

x="",

y="Subjects"

)+

theme_minimal(base_size=12)+

theme(

legend.position="bottom",

legend.title=element_blank(),

axis.text.x=

element_text(

angle=45,

hjust=1

),

panel.grid.major.x=element_blank(),

panel.grid.minor=element_blank()

)

}

#tumor location 
tumor_plot <- function(tumor_summary){
    ggplot(tumor_summary, aes(reorder(PTUMLOC, n), n)) +
    geom_col(fill="darkgreen") +
    coord_flip() +
    labs(
        x="",
        y="Subjects"
    ) + 
    theme_minimal(base_size=11)
  
}
#missing data summary
missing_plot <- function(missing_summary){
    ggplot(missing_summary, aes(Variable, Missing)) +
    geom_col(fill="tomato") +
    geom_text(
        aes(label=Missing),
        vjust=-0.3,
        size=5
    ) +
    labs(
    x="",
    y="Missing Subjects") +
    theme_minimal(base_size=11)
   
}
#site risk plot
risk_plot <- function(data){

  risk_summary(data) %>%
    mutate(Risk = Missing + Inconsistent) %>%
    arrange(desc(Risk)) %>%
    slice(1:10) %>%

    ggplot(aes(
      x = reorder(Site, Risk),
      y = Risk
    )) +

    geom_col(fill = "#D73027") +

    coord_flip() +

    geom_text(
      aes(label = Risk),
      hjust = -0.2
    ) +

    labs(
      x = "",
      y = "Risk Score"
    ) +

    theme_minimal()
}
#Randmoization delay histogram
delay_plot <- function(data){
    ggplot(data, aes(Randomization_Delay)) +
    geom_histogram(bins=15, fill="steelblue", color="white")+
    labs(
        x="Days",
        y="Subjects"
    ) +
    theme_minimal(base_size=11)

}
#Enrollment trend
enrollment_plot <- function(data){
    summary <- data %>% mutate(Month=format(EnrollmentDate, "%Y-%m")) %>% 
    count(Month) 
    ggplot(summary, aes(Month, n, group=1)) + 
    geom_line(size=1.2) +
    geom_point(size=3) +
    labs(
        x="",
        y="Subjects"
    ) +
    theme_minimal(base_size=11)
   
}
#heatmap of eligibility by site
heatmap_plot <- function(site_summary){
    ggplot(site_summary, aes(Eligibility, Site, fill=Subjects)) +
    geom_tile() +
    theme_minimal(base_size=11)
  
}
high_risk_sites <- function(data){

  risk_summary(data) %>%
    filter(Missing + Inconsistent > 0) %>%
    nrow()

}
plot_cumulative <- function(data){

  df <- data %>%

    filter(!is.na(enrollment_date)) %>%

    mutate(
      Month = as.Date(
        format(enrollment_date, "%Y-%m-01")
      )
    ) %>%

    count(Month, Eligibility) %>%

    arrange(Month) %>%

    group_by(Eligibility) %>%

    mutate(
      Cumulative = cumsum(n)
    )

  ggplot(
    df,
    aes(
      x = Month,
      y = Cumulative,
      colour = Eligibility,
      group = Eligibility,
      text = paste0(
        "<b>Month:</b> ", format(Month, "%b-%Y"),
        "<br><b>Status:</b> ", Eligibility,
        "<br><b>Cumulative Subjects:</b> ", comma(Cumulative)
      )
    )
  ) +

    geom_line(size = 1.2) +

    geom_point(size = 2) +

    scale_colour_manual(values = c(
      "Eligible" = "#1B7F3B",
      "Ineligible" = "#D94841",
      "Missing Data" = "#D18F00",
      "Inconsistent" = "#6A3FB5"
    )) +

    scale_x_date(
      date_breaks = "6 months",
      date_labels = "%b\n%Y"
    ) +

    labs(
      x = "",
      y = "Subjects"
    ) +

    theme_minimal(base_size = 12) +

    theme(
      legend.position = "bottom",
      legend.title = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      )
    )
}
plot_enrollment_status <- function(data){

  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)

  # Prepare data
  df <- data %>%
    filter(!is.na(enrollment_date)) %>%
    mutate(
      Quarter = paste0(
        year(enrollment_date),
        " Q",
        quarter(enrollment_date)
      )
    ) %>%
    count(
      Quarter,
      Eligibility,
      name = "Subjects"
    ) %>%
    group_by(Quarter) %>%
    mutate(
      Percent = Subjects / sum(Subjects)
    ) %>%
    ungroup()

  # Preserve chronological order
  df$Quarter <- factor(
    df$Quarter,
    levels = unique(df$Quarter)
  )

  ggplot(
    df,
    aes(
      x = Quarter,
      y = Percent,
      fill = Eligibility,
      text = paste0(
        "<b>Quarter:</b> ", Quarter,
        "<br><b>Status:</b> ", Eligibility,
        "<br><b>Subjects:</b> ", comma(Subjects),
        "<br><b>Percentage:</b> ", percent(Percent, accuracy = 0.1)
      )
    )
  ) +

    geom_col(
      width = 0.75,
      colour = "white",
      position = "fill"
    ) +

    scale_fill_manual(
      values = c(
        "Eligible" = "#1B5E20",
        "Ineligible" = "#C62828",
        "Missing Data" = "#D97706",
        "Inconsistent" = "#5E35B1"
      )
    ) +

    scale_y_continuous(
      labels = percent_format(),
      expand = expansion(mult = c(0, 0.02))
    ) +

    labs(
      
      x = "",
      y = "Percentage of Subjects",
      fill = "Eligibility"
    ) +

    theme_minimal(base_size = 13) +

    theme(
      legend.position = "bottom",
      axis.text.x = element_text(
        angle = 45,
        hjust = 1
      ),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      )
    )
}
plot_enrollment_vs_randomization <- function(data){

  library(dplyr)
  library(ggplot2)
  library(lubridate)
  library(scales)

  # Enrollment trend
  enroll <- data %>%
    filter(!is.na(enrollment_date)) %>%
    mutate(
      Month = floor_date(enrollment_date, "month")
    ) %>%
    count(Month, name = "Enrolled") %>%
    arrange(Month) %>%
    mutate(
      Cumulative = cumsum(Enrolled),
      Type = "Enrolled"
    ) %>%
    rename(Count = Cumulative)

  # Randomization trend
  random <- data %>%
    filter(!is.na(randomization_date)) %>%
    mutate(
      Month = floor_date(randomization_date, "month")
    ) %>%
    count(Month, name = "Randomized") %>%
    arrange(Month) %>%
    mutate(
      Cumulative = cumsum(Randomized),
      Type = "Randomized"
    ) %>%
    rename(Count = Cumulative)

  plot_data <- bind_rows(
    enroll %>% select(Month, Count, Type),
    random %>% select(Month, Count, Type)
  )

  ggplot(
    plot_data,
    aes(
      x = Month,
      y = Count,
      colour = Type,
      group = Type,
      text = paste0(
        "<b>Month:</b> ", format(Month, "%b %Y"),
        "<br><b>Status:</b> ", Type,
        "<br><b>Cumulative Subjects:</b> ", comma(Count)
      )
    )
  ) +

    geom_line(size = 1.3) +

    geom_point(size = 2.8) +

    scale_colour_manual(
      values = c(
        "Enrolled" = "#1976D2",
        "Randomized" = "#2E7D32"
      )
    ) +

    scale_y_continuous(
      labels = comma
    ) +

    labs(
     
      x = "",
      y = "Subjects",
      colour = ""
    ) +

    theme_minimal(base_size = 13) +

    theme(
      legend.position = "top",
      plot.title = element_text(
        face = "bold",
        hjust = 0.5
      ),
      panel.grid.minor = element_blank()
    )

}