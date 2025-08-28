# S&P 500 Financial Fundamentals Data Analysis
# Author: Daniel Volkov
# Description: Analyzes the relationship between valuation metrics and fundamentals for S&P 500 companies using R.

# ---- Load Libraries ----
library(tidyverse)
library(boot)

# ---- Data Loading ----
df <- readr::read_csv("financials.csv")
glimpse(df)
head(df)

# ---- Data Cleaning ----
df <- df %>%
  rename(
    PE = `Price/Earnings`,
    EPS = `Earnings/Share`,
    DivYield = `Dividend Yield`,
    Low52 = `52 Week Low`,
    High52 = `52 Week High`,
    PriceToBook = `Price/Book`,
    PriceToSales = `Price/Sales`
  )

print(paste("Class of Market Cap column:", class(df$`Market Cap`)))

if (is.character(df$`Market Cap`) | is.factor(df$`Market Cap`)) {
  df <- df %>%
    mutate(`Market Cap` = as.character(`Market Cap`)) %>%
    mutate(MarketCapNum = parse_number(`Market Cap`))
} else {
  df <- df %>%
    rename(MarketCapNum = `Market Cap`)
}

print("Missing values per column:")
print(colSums(is.na(df)))

df_clean <- df %>%
  filter(!is.na(PE), !is.na(PriceToBook), !is.na(MarketCapNum))

print(paste("Rows before cleaning:", nrow(df)))
print(paste("Rows after cleaning:", nrow(df_clean)))
print("Missing values after cleaning:")
print(colSums(is.na(df_clean)))

# ---- Exploratory Data Analysis (EDA) ----
print("Summary statistics of key numeric variables:")
print(summary(select(df_clean, Price, PE, EPS, DivYield, PriceToBook, MarketCapNum)))

p_hist <- ggplot(df_clean, aes(x = PE)) +
  geom_histogram(binwidth = 5, fill = "#619CFF", color = "white", alpha = 0.85) +
  labs(
    title = "Distribution of P/E Ratios (S&P 500)",
    x = "Price/Earnings Ratio (P/E)",
    y = "Number of Companies"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )
print(p_hist)

p_bar <- df_clean %>%
  group_by(Sector) %>%
  summarise(mean_PE = mean(PE, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(Sector, mean_PE), y = mean_PE)) +
  geom_col(fill = "#56B4E9", width = 0.7) +
  labs(
    title = "Mean P/E Ratio by Sector (S&P 500)",
    x = "Sector",
    y = "Mean P/E Ratio"
  ) +
  coord_flip() +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5),
    axis.text.y = element_text(size = 12)
  )
print(p_bar)

p_scatter <- ggplot(df_clean, aes(x = EPS, y = PE)) +
  geom_point(alpha = 0.65, color = "#204080", size = 2) +
  labs(
    title = "Relationship Between EPS and P/E Ratio",
    x = "Earnings Per Share (EPS)",
    y = "Price/Earnings Ratio (P/E)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )
print(p_scatter)

p_marketcap <- ggplot(df_clean, aes(x = MarketCapNum, y = PE)) +
  geom_point(alpha = 0.65, color = "#800080", size = 2) +
  scale_x_log10(labels = scales::comma) +
  labs(
    title = "Market Capitalization vs. P/E Ratio (Log Scale)",
    x = "Market Cap (log scale, USD)",
    y = "Price/Earnings Ratio (P/E)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )
print(p_marketcap)

df_numeric <- df_clean %>%
  select(Price, PE, EPS, DivYield, PriceToBook, MarketCapNum)
cor_matrix <- cor(df_numeric, use = "complete.obs")
print("Correlation matrix (rounded):")
print(round(cor_matrix, 2))

# ---- Sampling and Bootstrapping ----
set.seed(123)
sample_30 <- df_clean %>% sample_n(30)
print(paste("Mean P/E in random sample of 30 companies:", round(mean(sample_30$PE), 2)))

boot_mean_pe <- function(data, indices) {
  sample_data <- data[indices, ]
  mean(sample_data$PE, na.rm = TRUE)
}

boot_result <- boot(data = df_clean, statistic = boot_mean_pe, R = 1000)
print(paste("Bootstrap estimate of mean P/E:", round(boot_result$t0, 2)))
print("95% Confidence Interval for mean P/E:")
print(boot.ci(boot_result, type = "perc"))

boot_means <- boot_result$t
p_boot <- ggplot(data.frame(mean_PE = boot_means), aes(x = mean_PE)) +
  geom_histogram(bins = 30, fill = "#009E73", color = "white", alpha = 0.85) +
  labs(
    title = "Bootstrap Distribution of Mean P/E Ratio",
    x = "Mean P/E Ratio",
    y = "Frequency"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )
print(p_boot)

# ---- Hypothesis Testing ----
print("Available sectors:")
print(unique(df_clean$Sector))

df_clean <- df_clean %>%
  mutate(IsTech = ifelse(Sector == "Information Technology", "Tech", "Non-Tech"))

group_means <- df_clean %>%
  group_by(IsTech) %>%
  summarise(avg_PE = mean(PE, na.rm = TRUE), count = n())
print("Average P/E by Tech vs Non-Tech:")
print(group_means)

t_test_result <- t.test(PE ~ IsTech, data = df_clean)
print("Two-sample t-test results (P/E: Tech vs Non-Tech):")
print(t_test_result)

anova_result <- aov(PE ~ Sector, data = df_clean)
print("ANOVA results (P/E across sectors):")
print(summary(anova_result))

# ---- Regression Modeling ----
model1 <- lm(PE ~ EPS, data = df_clean)
print("Simple linear regression: P/E ~ EPS")
print(summary(model1))

model2 <- lm(PE ~ EPS + DivYield + PriceToBook + MarketCapNum, data = df_clean)
print("Multiple regression: P/E ~ EPS + DivYield + PriceToBook + MarketCapNum")
print(summary(model2))

p_regression <- ggplot(df_clean, aes(x = EPS, y = PE)) +
  geom_point(alpha = 0.65, color = "#205090", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "#E69F00", linetype = "solid", linewidth = 1.2) +
  labs(
    title = "Linear Regression: P/E Ratio vs. EPS",
    x = "Earnings Per Share (EPS)",
    y = "Price/Earnings Ratio (P/E)"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold", size = 16, hjust = 0.5)
  )
print(p_regression)

par(mfrow = c(2, 2))
plot(model2)
par(mfrow = c(1, 1))

# ---- Summary of Findings ----
cat("
Key findings:
- Dividend Yield is significantly negatively related to P/E.
- Market Cap is positively related to P/E and statistically significant.
- No strong effect found for EPS or Price to Book.
- Differences in P/E between Tech and Non-Tech or across sectors are not statistically significant at the 95% level.
- The regression model explains a small but significant portion of P/E variation.
")
