# BIODICAPT (model V0)

## Context
The BIODICAPT project is a French initiative that aims at monitoring biodiversity of agricultural lands on a large (national) scale through the use of various recording devices.

As it is common in biodiversity studies, data is scarce while objectives are large scale. Therefore, the BIODICAPT project wants to explore optimal experimentation design, as early as possible, to inform the strategy of data collection for the duration of the project.

Before any data collection, I will will work on purely theoretical data. The idea is that a thoeretical model, as simple as it can be, is better than doing things randomly [(reference needed)](https://en.wikipedia.org/wiki/Who_Said_That%3F). Then, when data is collected, we will refine our models to the next best optimal experimental design, based on the observations.

## A very simple model
For this theoretical model of species distribution, we'll start with a very simple design. The goal here, is just to prepare the workflow, and get a rough idea of what the optimal design looks like. Here is the definition of the model:

$$
\begin{align}
y_i &\sim \text{Bernoulli}(\theta_i) \\
\text{logit}(\theta_i) &= \beta_{j} + \beta_1 \tilde{T_i} \\
\beta_{j}, \beta_1 &\sim \text{Normale}(0, 1.5) \\
\end{align}
$$

In simple words, this model will show presence-absence of species distribution in France depending on temperature ($\tilde{T_i}$) and land cover ($j$).

### Dataset
To simulate the observations, we need real data of temperature and land cover. These are easily available but they need cleaning, filtering and formatting.

In particular, we need to have our observation data distributed into an evenly spaced out grid. In Python, this can be done using meshgrid. 

#### Temperatures
We use data from Meteo France, available on [data.gouv.fr](www.data.gouv.fr/datasets/donnees-climatologiques-de-base-mensuelles). It contains point that are scattered, unevenly on the map of France. To get a grid from this dataset, we use linear interpolation, similarly to [Philips et al. (2006)](https://linkinghub.elsevier.com/retrieve/pii/S030438000500267X) (to cite them: it is "likely more realistic than simply using nearest-neighbor interpolation").

BIODICAPT is considering the use of "SAFRAN" data : simulation made by meteo-france, instead of just using observation data. IMO, the [CHELSA](https://www.chelsa-climate.org/) dataset is also interesting and should be considered.

#### Land Cover
France's governement has several public programs aiming at categorizing land usage, the most used ones are :
- [OCS GE (Land Cover at large scale)](https://www.data.gouv.fr/datasets/ocs-ge)
- [CoSIA (Land Cover by IA)](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_COSIA)
- [CORINE Land Cover](https://land.copernicus.eu/en/products/corine-land-cover)

Here, we'll use CORINE as it is made for a larger scale than CoSIA, and the categories are more adapted to our goals than OCS GE.
