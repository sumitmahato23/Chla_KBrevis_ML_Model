
# Monitoring harmful algal blooms in surface water bodies across U.S. using Sentinel-2 images and Machine Learning 

This repository contains the code for the manuscript titled 'Monitoring harmful algal blooms in surface water bodies across U.S. using Sentinel-2 images and Machine Learning'. The source code was used to develop machine learning (ML) models for estimating Chlorophyll-a concentrations in U.S. rivers and K. Brevis in the Gulf of Mexico using river reflectance data retrieved from the Sentinel-2 archive.

Overview: We compare the estimation accuracy of four ML models: XGBLinear, Randomized Random Forest (RRF), XGBTree, and K-Nearest Neighbours, followed by a bias correction method to improve over- and under-estimation of our ML models. 

- The Chl-a and KB folder contains R scripts for the various ML models.
- The function folder contains R scripts with user-defined functions utilized in the ML models.
- The features folder includes data files.
- The Bias correction folder includes a Jupyter notebook used after ML models.

To reproduce the results:
1. Download this repository.
2. Update the file paths with specific locations.
source("C:\\Research\\Library.R") (available in "function" folder)
source("C:\\Research\\Features.R") (available in "Features" folder)

If you have any questions or requests for additional data related to this manuscript, please contact the corresponding author: Sumit Mahato (mahatost@mail.uc.edu).
