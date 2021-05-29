# MBFQuantitativeTextAnalysis

## Authors
* Tim Graf
* Ella Stanisch
* David Klug
* Swen Hartlieb


## Background

This repository is part of a project from the course "Quantitative Text Analysis" at the University of St. Gallen which was held in the spring semester semester 2021.

## About the project

The aim of the following project was to analyze the dependency of party affiliation and presidents on U.S. foreign policy over time. Specifically we analyze U.S. Executive Orders on the topics, addressed geographies, and sentiment using various dictionaries (e.g. Lexicoder, NRC, AFINN) and supervised-machine learning for classification (package Newsmap). The Executive Orders are scraped from the Presidency project and contain ~ 6'000 unique Executive Orders from 1876 to 2021.

## How to execute this code

1. Run 01_Webscraper to obtain all Executive Orders

2. Run 02_DataCleaning for feature engineering and cleaning

3. Run the other files for individual analysis (topic, geographical classification, sentiment analysis)


## Disclaimer

This code was used for research-purposes only. 