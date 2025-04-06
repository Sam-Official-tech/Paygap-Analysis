
# Load necessary libraries
library(ggplot2)
library(dplyr)
library(readr)
library(gridExtra)
library(tidyr)

# Read the dataset from CSV file
df <- read_csv("/Users/shamikchakraborty/Downloads/paygap.ie.csv")

# Rename column for clarity
paygap_data <- rename(df, ReportYear = `Report Year`)

# (1) Summarize the number of companies by report year
companies_by_year <- df %>%
  group_by(`Report Year`) %>%
  summarise(Num_Companies = n())

# Print summary table for Report Year
cat("Table 1: Number of Companies by Report Year\n")
print(companies_by_year)

# Filter dataset for the most recent Report Year to avoid double-counting
latest_year <- max(df$`Report Year`, na.rm = TRUE)
latest_data <- filter(df, `Report Year` == latest_year)

# Count the number of companies by GICS Sector and ICB Industry
gics_sector_counts <- latest_data %>%
  count(`GICS Sector`, sort = TRUE)

icb_industry_counts <- latest_data %>%
  count(`ICB Industry`, sort = TRUE)

# Create bar plots for GICS Sector and ICB Industry
gics_plot <- ggplot(gics_sector_counts, aes(x = reorder(`GICS Sector`, -n), y = n)) +
  geom_bar(stat = "identity", fill = "black") +
  labs(title = "Number of Companies by GICS Sector", x = "GICS Sector", y = "Number of Companies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

icb_plot <- ggplot(icb_industry_counts, aes(x = reorder(`ICB Industry`, -n), y = n)) +
  geom_bar(stat = "identity", fill = "green") +
  labs(title = "Number of Companies by ICB Industry", x = "ICB Industry", y = "Number of Companies") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the two plots side by side
grid.arrange(gics_plot, icb_plot, ncol = 2)

# (2) Range of pay gaps by company
percentiles <- quantile(df$`Mean Hourly Gap`, probs = c(0.05, 0.95), na.rm = TRUE)
p5 <- percentiles[1]
p95 <- percentiles[2]

max_M <- max(df$`Mean Hourly Gap`, na.rm = TRUE)
min_M <- min(df$`Mean Hourly Gap`, na.rm = TRUE)

histogram_plot <- ggplot(df, aes(x = `Mean Hourly Gap`)) +
  geom_histogram(binwidth = 0.5, fill = "blue", color = "black") +
  annotate("segment", x = p5, xend = p5, y = 0, yend = Inf, color = "red", linetype = "dashed", size = 1) +
  annotate("segment", x = p95, xend = p95, y = 0, yend = Inf, color = "green", linetype = "dashed", size = 1) +
  ggtitle("Histogram with 5th and 95th Percentiles") +
  xlab("Mean Hourly Gap") + ylab("Frequency")

print(histogram_plot)

# (3) Means vs. medians
scatterplot <- ggplot(paygap_data, aes(x = `Median Hourly Gap`, y = `Mean Hourly Gap`)) +
  geom_point(color = "red", alpha = 0.6) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "blue") +
  labs(title = "Comparison of Mean vs. Median Hourly Gaps",
       x = "Median Hourly Gap (%)",
       y = "Mean Hourly Gap (%)") +
  theme_minimal()

print(scatterplot)

# (4) Variation among employee categories and types of pay
df_filtered <- df %>%
  filter(between(`Median Hourly Gap`, -100, 100),
         between(`Median Hourly Gap Part Time`, -100, 100),
         between(`Median Hourly Gap Part Temp`, -100, 100),
         between(`Median Bonus Gap`, -100, 100))

paygap_long <- df_filtered %>%
  select(`Median Hourly Gap`, `Median Hourly Gap Part Time`, `Median Hourly Gap Part Temp`, `Median Bonus Gap`) %>%
  pivot_longer(cols = everything(), names_to = "Type", values_to = "Gap")

ggplot(paygap_long, aes(x = Gap, fill = Type)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.4, color = "black") +
  scale_fill_manual(values = c("blue", "green", "orange", "purple")) +
  labs(title = "Comparison of Median Hourly and Bonus Pay Gaps",
       x = "Pay Gap (%)",
       y = "Frequency",
       fill = "Gap Type") +
  xlim(-100, 300)

# (5) Breakdown by industrial sector and change over time
df_filtered_years <- df %>%
  filter(`Report Year` %in% c(2022, 2023))

sector_sum <- df_filtered_years %>%
  group_by(Sector = `GICS Sector`, `Report Year`) %>%
  summarise(`Median Hourly Gap` = median(`Median Hourly Gap`, na.rm = TRUE)) %>%
  pivot_wider(names_from = `Report Year`, values_from = `Median Hourly Gap`, names_prefix = "Year") %>%
  mutate(Difference = Year2023 - Year2022)

print(sector_sum)

ggplot(sector_sum, aes(x = Sector, y = Difference, fill = Difference > 0)) +
  geom_bar(stat = "identity", color = "black") +
  labs(title = "Change in Median Hourly Gap from 2022 to 2023 by Sector",
       x = "Sector",
       y = "Difference in Median Hourly Gap (%)") +
  scale_fill_manual(values = c("TRUE" = "red", "FALSE" = "blue"),
                    labels = c("Increasing", "Decreasing"),
                    name = "Change") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# (6) Lack of women in top positions: defining an index
LWTP <- function(Q1, Q2, Q3, Q4) {
  ((Q1 + Q2) / 2) - Q4
}

# Test the function
LWTP(30, 56, 23, 31)

# (7) LWTP vs. pay gaps
df <- df %>%
  mutate(LWTP_index = LWTP(`Q1 Female`, `Q2 Female`, `Q3 Female`, `Q4 Female`))

sector_summary <- df %>%
  group_by(`GICS Sector`) %>%
  summarise(Average_LWTP = mean(LWTP_index, na.rm = TRUE),
            Average_Mean_Hourly_Gap = mean(`Mean Hourly Gap`, na.rm = TRUE))

print(sector_summary)

ggplot(sector_summary, aes(x = Average_LWTP, y = Average_Mean_Hourly_Gap)) +
  geom_point(color = "blue", size = 3) +
  geom_text(aes(label = `GICS Sector`), hjust = -0.1, vjust = -0.1, size = 3) +
  labs(title = "LWTP Index vs. Mean Hourly Gap by Sector",
       x = "Average LWTP Index",
       y = "Average Mean Hourly Gap (%)") +
  theme_minimal()
