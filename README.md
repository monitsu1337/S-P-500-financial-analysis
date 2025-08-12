#  S&P 500 Financial Fundamentals — Exploratory Analysis

##  How to Run
Download the `.html` file from this repository and open it in any web browser to view the full analysis.

##  Project Structure
- `S&P 500 Financial Fundamentals Analysis.Rmd` — R Markdown source file.
- `S-P-500-Financial-Fundamentals-Analysis.html` — Knitted HTML report.
- `S&P 500 Financial Analysis.R` — Supporting R script (optional).
- `financials.csv` — Dataset used.

##  Methods
- **Exploratory Data Analysis (EDA)** — summary stats, histograms, scatter plots.
- **Bootstrapping** — mean P/E estimation with confidence intervals.
- **Hypothesis Testing** — t-tests and ANOVA for sector comparisons.
- **Regression Modeling** — simple and multiple linear regression.

##  Key Findings
- Dividend Yield is significantly negatively related to P/E.
- Market Cap is positively related to P/E and statistically significant.
- No strong effect found for EPS or Price to Book.
- Differences in P/E between Tech and Non-Tech or across sectors are not statistically significant at the 95% level.
- The regression model explains a small but significant portion of P/E variation.
