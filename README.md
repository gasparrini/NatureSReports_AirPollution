# NatureSReports_AirPollution
Data and R code to support Schneider et al., (2022). Differential impact of government lockdown policies on reducing air pollution levels and related mortality in Europe. Nature's Scientific Reports (DOI: 10.1038/s41598-021-04277-6).

OPEN ACCESS: https://www.nature.com/articles/s41598-021-04277-6

The work was supported by the European Centre for Medium-Range Weather Forecasts (ECMWF) on behalf the European Union (commercial contract Ref. CAMS_95p) and the European Union’s Horizon 2020 Project Exhaustion (Grant ID: 820655).

This code has been implemented to reproduce the statistical analysis, figures, and tables implemented in the study referenced above. The dataset provided by the European Commission’s Copernicus Atmosphere Monitoring Service (https://atmosphere.copernicus.eu/) contains daily NO2, O3, PM10, and PM2.5 concentrations from February 1st to July 31st 2020 for 50 European cities (see full list here - https://github.com/CopernicusAtmosphere/air-quality-covid19-response/blob/master/CAMS_AQ_LOCATIONS_V1.csv).

The analysis is divided in three parts: (1) Estimation of the air pollution reduction related to SI levels (0-80%), (2) Estimation of the specific impact of each sub-policy measure on the poluttion change, (3) Health Impact Assessment with the quantification of avoided/excess deaths due to short-term exposure to air pollution
