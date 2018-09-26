# cholera-paramEstPlots

[Model distinguishability and inference robustness in mechanisms of cholera transmission and loss of immunity](https://www.ncbi.nlm.nih.gov/pubmed/28130096)

Please refer to the main model source code at :octocat: [epimath/cholera-distinguishability](https://github.com/epimath/cholera-distinguishability)

Mathematical models of cholera and waterborne disease vary widely in their structures, in terms of transmission pathways, loss of immunity, and a range of other features. These differences can affect model dynamics, with different models potentially yielding different predictions and parameter estimates from the same data. Given the increasing use of mathematical models to inform public health decision-making, it is important to assess model distinguishability (whether models can be distinguished based on fit to data) and inference robustness (whether inferences from the model are robust to realistic variations in model structure). In this paper, we examined the effects of uncertainty in model structure in the context of epidemic cholera, testing a range of models with differences in transmission and loss of immunity structure, based on known features of cholera epidemiology. We fit these models to simulated epidemic and long-term data, as well as data from the 2006 Angola epidemic. We evaluated model distinguishability based on fit to data, and whether the parameter values, model behavior, and forecasting ability can accurately be inferred from incidence data. In general, all models were able to successfully fit to all data sets, both real and simulated, regardless of whether the model generating the simulated data matched the fitted model. However, in the long-term data, the best model fits were achieved when the loss of immunity structures matched those of the model that simulated the data. Two parameters, one representing person-to-person transmission and the other representing the reporting rate, were accurately estimated across all models, while the remaining parameters showed broad variation across the different models and data sets. The basic reproduction number (R0) was often poorly estimated even using the correct model, due to practical unidentifiability issues in the waterborne transmission pathway which were consistent across all models. Forecasting efforts using noisy data were not successful early in the outbreaks, but once the epidemic peak had been achieved, most models were able to capture the downward incidence trajectory with similar accuracy. Forecasting from noise-free data was generally successful for all outbreak stages using any model. Our results suggest that we are unlikely to be able to infer mechanistic details from epidemic case data alone, underscoring the need for broader data collection, such as immunity/serology status, pathogen dose response curves, and environmental pathogen data. Nonetheless, with sufficient data, conclusions from forecasting and some parameter estimates were robust to variations in the model structure, and comparative modeling can help to determine how realistic variations in model structure may affect the conclusions drawn from models and data.


