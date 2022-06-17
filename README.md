# asset_manager

## App Goals

This is a shiny app designed to do the following:
1. Visualize & report account asset percentages.
2. Report whether rebalancing is needed (based on 5/25 rebalancing bands).
3. Visualize cost basis vs. market value over time.
4. Calculate which assets to purchase for a given dollar amount to get assets as close to targets as possible.

Note. I don't have a way to incorporate selling shares into the app yet. For now, I just remove the rows corresponding to the lots sold in the 'FinancialActivity.csv' dataset.

## Using the App

### Data preparation

Ensure you have all packages installed as listed in the app script.

This app assumes you have the following datasets in your working directory:
- 'FinancialActivity.csv'
- 'accountAssetTargets.csv'

Sample files are included in the repository.

#### FinancialActivity.csv

This file should contain the following columns:
- DATE: date of each transaction in the format of mm/dd/yyy
- ACCOUNT: which account the transaction was in (e.g., "Roth IRA", "401(k)", etc).
- ASSET: the ticker symbol for the asset being transacted.
- AMOUNT: the number of shares transacted.
- PRICE: the dollar amount paid for each share of the asset.
- COST: AMOUNT * PRICE.

#### accountAssetTargets.csv

This file should contain the following columns:
- ACCOUNT: should correspond to each unique ACCOUNT entry in 'FinancialActivity.csv'
- ASSET: should correspond to each unique ASSET entry in 'FinancialActivity.csv'
- CLASS: Description of the type/class of the ASSET (e.g., US Market, Emerging Markets, Fixed Income)
- TARGET: decimal representing the % the ASSET should take up in the ACCOUNT (TARGET values must sum to 1.0 in each ACCOUNT)

### Launching & Interacting with the App

Press 'run app' in R studio. This should launch a new window. During this time, the app is gathering the ASSET prices.

#### "Choose an account to view:"

Use this drop down to select the account of interest. Make sure you have the correct account selected before using the 'Calculate' or 'Add Shares' buttons.

#### "Enter new contribution:"

Enter a dollar amount you are contributing to the account selected above. Pressing 'Calculate' will show which assets to purchase to get back (as close as possible) to account targets.

#### "Add Shares"

After purchasing on your brokerage website (no API yet), you can record the purchases into your FinancialActivity.csv dataset here or manually by opening up the .csv.

After entering the asset, number of shares, and cost per share, click the 'add shares' button. The data is only recorded when you click the button, so don't worry if the presented text is changing as you enter the information for an additional purchase.
