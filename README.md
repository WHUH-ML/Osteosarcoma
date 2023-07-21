# Machine Learning-Based Individualized Survival Prediction Model for Prognosis in Osteosarcoma: Data From the SEER Database

Data preprocessing is handled by R code: [data_preprocessing.R](data_preprocessing.R), which imports codes for data cleaning from [preprocessing.R](preprocessing.R)

Model construction, hyperparameters tuning and evaluation are handled with [pysurvival](https://github.com/square/pysurvival), [scikit-learn](https://github.com/scikit-learn/scikit-learn) and [lifelines](https://github.com/CamDavidsonPilon/lifelines) packages: [ModelDevelopment-Without_tunning_output.ipynb](ModelDevelopment-Without_tunning_output.ipynb)

Web application based on [streamlit](https://github.com/streamlit/streamlit) package: [app.py](app.py)

The original data read in R code is not provided in this repository and needs to be extracted in the [SEER](https://seer.cancer.gov/) database according to inclusion criteria (AYA site = 4.1 Osteosarcoma)

The [data](/data/data_surv.csv) after data preprocessing is provided. To reproduce this study, first run the following codes to install packages:
```
git clone https://github.com/WHUH-ML/Osteosarcoma.git
pip install -r requirements.txt
```
Then run [ModelDevelopmentWithoutTuningOutput.ipynb](ModelDevelopmentWithoutTuningOutput.ipynb) in Jupyter Notebook.

Run ```streamlit run app.py``` in terminal to open the web application locally.

[Online web application](https://survivalofosteosarcoma.streamlit.app)

[Paper link](https://pubmed.ncbi.nlm.nih.gov/)(To be updated)
