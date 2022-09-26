# asset_manager

## App Goals

This is a shiny app designed to do the following:
1. Visualize & report account asset percentages, asset drift.
2. Report whether rebalancing is needed (based on 5/25 rebalancing bands).
3. Visualize cost basis vs. market value over time.
4. Calculate which assets to purchase for a given dollar amount to get assets as close to targets as possible.
5. Calculate what is needed to buy/sell to rebalance each account.

## Using the App

### Data preparation

Ensure you have all packages installed as listed in the app script.

This app assumes you have the following datasets in your working directory:
- 'financialActivity.csv'
- 'accountAssetTargets.csv'

Sample files are included in the repository.

#### FinancialActivity.csv

This file should contain the following columns:
- DATE: date of each transaction in the format of mm/dd/yyy
- ACCOUNT: which account the transaction was in (e.g., "Roth IRA", "401(k)", etc).
- FUND: the ticker symbol for the asset being transacted.
- ASSET: Naming convention for the asset class of the FUND (e.g., "US Small Value")
- AMOUNT: the number of shares transacted.
- PRICE: the dollar amount paid for each share of the asset.
- COST: AMOUNT * PRICE.
- TRANSACTION: "BUY" or "SELL"
- LOT_DATE: If TRANSACTION = "SELL", this should list the "BUY" date of the specific lot that was sold. Add a new row for each lot sold from.

#### accountGlidePath.csv

This file details the starting and ending asset allocation for a glidepath to retirement. If you don't want to enter a glidepath, just set the STARTING and ENDING columns to be the same.

This file should contain the following columns:
- ACCOUNT: should correspond to each unique ACCOUNT entry in 'FinancialActivity.csv'
- TYPE: Parent asset class description for each asset (e.g., "US Equities")
- FUND: should correspond to each unique FUND entry in 'FinancialActivity.csv'
- ASSET: Description of the class of the FUND (e.g., "US Small Value", "Intl Large Blend", "Fixed Income")
- STARTING: decimal representing the % the ASSET should take up in the ACCOUNT at the beginning of the glidepath.
- ENDING: decimal representing the % the ASSET should take up in the ACCOUNT at the end of the glidepath.

### Launching & Interacting with the App

Press 'run app' in R studio. This should launch a new window. During this time, the app is gathering the FUND prices.

#### "Choose an account to view:"

Use this drop down to select the account of interest. Make sure you have the correct account selected before using the new contrition or rebalance calculators.

#### "Enter new contribution:"

Enter a dollar amount you are contributing to the account selected above. Pressing 'Calculate' will show which assets to purchase to get back (as close as possible) to account targets.

#### "Rebalance Calculator"

Click 'calculate' here to determine how many shares of each asset needs to be bought/sold to return to asset targets.
