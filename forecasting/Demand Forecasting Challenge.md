## Store Item Demand Forecasting Challenge

This competition is provided as a way to explore different time series techniques on a relatively simple and clean dataset.
You are given 5 years of store-item sales data, and asked to predict 3 months of sales for 50 different items at 10 different stores.
What's the best way to deal with seasonality? Should stores be modeled separately, or can you pool them together? Does deep learning work better than ARIMA? Can either beat xgboost?

> This is a great competition to explore different models and improve your skills in forecasting.


**The objective of this competition is to predict 3 months of item-level sales data at different store locations.**

**Data Description**

_File descriptions_  

    train.csv - Training data
    test.csv - Test data (Note: the Public/Private split is time based)
    sample_submission.csv - a sample submission file in the correct format

_Data fields_

    date - Date of the sale data. There are no holiday effects or store closures.
    store - Store ID
    item - Item ID
    sales - Number of items sold at a particular store on a particular date.
