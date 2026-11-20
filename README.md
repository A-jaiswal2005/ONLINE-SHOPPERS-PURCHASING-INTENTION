# DMA Project - Online Shoppers Purchasing Intention (R)

## How to run
1. Open RStudio, set the working directory to this folder (Session > Set Working Directory > To Source File Location).
2. Run `run_all.R` (or run scripts 01 -> 02 -> 03, then `shiny::runApp()`).
   Packages are installed automatically by `00_setup.R`.

## Files
| File | Workflow step |
|---|---|
| `data/online_shoppers_intention.csv` | 1. Dataset (UCI) |
| `01_preprocessing_eda.R` | 2-4. Understanding, preprocessing, EDA |
| `02_data_mining.R` | 5. K-means, Apriori rules, decision tree |
| `03_modelling_evaluation.R` | 6-7. Logistic Regression vs Random Forest, metrics |
| `app.R` | 8. Shiny dashboard |
| `output/` | Plots, tables, saved models |

## 9. Key insights & recommendations
(Figures below are from the dataset: 12,330 sessions, 15.5% purchase, no missing values, 125 duplicates.)

- **PageValues is the strongest signal.** Sessions with PageValues > 0 convert at ~56% vs ~4% for those with 0. Guide visitors to pages that carry value (product detail, cart, checkout) and shorten the path to them.
- **Bounce and exit rates hurt conversion.** Improve landing-page relevance, load speed and mobile UX on high-exit pages.
- **New visitors convert better (~25%) than returning visitors (~14%).** Strengthen acquisition campaigns, and create win-back offers/loyalty incentives for returning visitors who browse but do not buy.
- **Seasonality matters.** November (~25%) and October (~21%) are peak months; Feb (~2%) is weakest. Concentrate ad spend and stock in peak periods and run promotions in slow months.
- **Weekend sessions convert slightly better** (~17% vs ~15%) - consider weekend-targeted offers.
- **Clusters** separate low-engagement bouncers from high-engagement product browsers; target the engaged-but-not-buying cluster with retargeting/discounts.
- **Models:** Random Forest typically beats Logistic Regression on ROC-AUC and F1 because it captures non-linear effects. Because only ~15% buy, look at recall/F1, not just accuracy. Use the predicted probability for real-time interventions (e.g. pop-up offers for high-probability, hesitating sessions).
