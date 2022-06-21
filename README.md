# Deep Partial Least Squares
Matthew Dixon, Nick Polson, and Kemen Goicoechea, Deep Partial Least Squares for Empirical Asset Pricing, 2022.
This code accompanies the paper: https://papers.ssrn.com/sol3/papers.cfm?abstract_id=4137647

The code is a working repository, still being tested, and subject to further change.

## Configuration 
### Anaconda
 - Anaconda environment configuration is in DPLS.yml

### R Packages
 - Install conda 4.12 or a later version
 - Create the conda virtual environment by typing ```conda env create -f DPLS.yml``` and link it to R: In RStudio: Tools > Global Options > Python > Select > Conda Environment
 - Once the anaconda environment is linked, you can download the Packrat packages bundle R here: https://www.dropbox.com/s/bahii8c5h2as0ip/DPLS-2022-06-14.tar.gz?dl=0
 - Then you use ```packrat::unbundle``` and ```packrat::restore``` to get the packages


## Source Code
The repository contains one main file: main.R, which calls functions in other files.
This code contains 4 types of models: LASSO (glmnet package), PLS (pls package), Neural Network (keras), Deep Partial Least Squares (pls + keras).

Trained models for NN and DPLS are in the data folder. However, these are Tensorflow Python saved models and may not work across different CPU architectures.
Tensorflow saved models are needed to use automatic differentiation. If you do not wish to use this functionnality, you can train the models using RKeras. However this will take approximately one hour to perform across all 330 periods.


## Data
The factor data has been collected from a financial data vendor and santized to avoid violation of data licensing agreement and non-commercial utility. The data, for non-commercial use only, can be downloaded from: https://www.dropbox.com/s/4o86a3p2n7kawst/ScaledData.RData?dl=0


The actual symbols have been remapped and the factors have been normalized in each period. The stocks are characterized by GICS and use dummy variables to represent the four difference catergories:

industry=[10, 20, 30 ,40 ,50, 60, 70] 

subindustry=[10, 15, 20, 25, 30 ,35, 40 ,45, 50, 60, 70 ,80]

sector=[10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60]

indgroup=[10, 20, 30 ,40, 50]

Note that the first element in each list is dummatized as 1 0 0 0 .. and the next as 0 1 0 0 ... etc.


ID | Symbol  | Value Factors |
| --- | --- | --- |
|1 | B/P | Book to Price|
|2 | CF/P | Cash Flow to Price|
|3 | E/P | Earning to Price|
|4 | S/EV | Sales to Enterprise Value (EV). EV is given by |
||| EV=Market Cap + LT Debt + max(ST Debt-Cash,0), |
|| | where LT (ST) stands for long (short) term|
|5| EB/EV|   EBIDTA to EV |
|6| FE/P | Forecasted E/P. Forecast Earnings are calculated from Bloomberg earnings consensus estimates data. |
|| | For coverage reasons, Bloomberg uses the 1-year and 2-year forward earnings.|
|17| DIV | Dividend yield. The exposure to this factor is just the most recently announced annual net dividends ||  divided by the market price. |
||| Stocks with high dividend yields have high exposures to this factor.|
| --- | --- | --- |
|| | Size Factors|
| --- | --- | --- |
|8 | MC | Log (Market Capitalization)|
|9| S | Log (Sales)|
|10 | TA | Log (Total Assets)|
| --- | --- | --- |
|| |  Trading Activity Factors|
| --- | --- | --- |
|11| TrA | Trading Activity is a turnover based measure. |
|| | Bloomberg focuses on turnover which is trading volume normalized by shares outstanding. |
||| This indirectly controls for the Size effect. |
|||The exponential weighted average (EWMA) of the ratio of shares traded to shares outstanding:| 
||| In addition, to mitigate the impacts of those sharp shortlived spikes in trading volume, |
||| Bloomberg winsorizes the data: |
||| first daily trading volume data is compared to the long-term EWMA volume(180 day half-life), |
||| then the data is capped at 3 standard deviations away from the EWMA average.|
| --- | --- | --- |
||| Earnings Variability Factors|
| --- | --- | --- |
|12 |EaV/TA | Earnings Volatility to Total Assets. |
||| Earnings Volatility is measured |
||| over the last 5 years/Median Total Assets over the last 5 years|
|13 | CFV/TA | Cash Flow Volatility to Total Assets.| 
||| Cash Flow Volatility is measured over the last 5 years/Median Total Assets over the last 5 years|
|14 | SV/TA |  Sales Volatility to Total Assets.| 
||| Sales Volatility over the last 5 years/Median Total Assets over the last 5 year|
| --- | --- | --- |
|| | Volatility Factors|
| --- | --- | --- |
|15 | RV| Rolling Volatility which is the return volatility over the latest 252 trading days|
|16 | CB | Rolling CAPM Beta which is the regression coefficient|
|| | from the rolling window regression of stock returns on local index returns|
| --- | --- | --- |
||| Growth Factors||
| --- | --- | --- |
|7| TAG | Total Asset Growth is the 5-year average growth in Total Assets || divided by the Average Total Assets over the last 5 years|
|18 | EG | Earnings Growth is the 5-year average growth in Earnings ||  divided by the Average Total Assets over the last 5 years|


