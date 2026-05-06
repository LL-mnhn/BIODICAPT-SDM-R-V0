# BIODICAPT (model V0)

## Context
The BIODICAPT project is a French initiative that aims at monitoring biodiversity of agricultural lands on a large (national) scale through the use of various recording devices.

As it is common in biodiversity studies, data is scarce while objectives are large. Therefore, I will try to find and optimal experimentation design (OED) so that the strategy of data collection for the project can be optimized regarding our constraints.

To find this OED, I will (at first) work on purely theoretical data. The idea is that a thoeretical model for data collection, as simple as it can be, is better than doing things randomly.


## Dataset
The BIODICAPT project aims at finding links between biodiversity and agricultural practices on a large scale.

In practice, this means we will be trying to find if a species is absent or present depending on local agro-ecological variables.

As I aim at making a very simple model (for now), we'll assume that we are considering the modeling the presence of a species that depends entirely on temperature and land cover (mainly, the type of agriculture).These are easily available datasets but they need cleaning, filtering and formatting.

### Temperatures
We use [CHELSA-monthly](https://www.chelsa-climate.org/): a global, kilometer-scale climate dataset. It consists of monthly surface variables summarized as monthly means, minima, maxima, or, in some cases, monthly accumulations.

### Land Cover
France's governement has several public programs aiming at categorizing land usage. Here, we'll use [CORINE](https://land.copernicus.eu/en/products/corine-land-cover) (larger scale than [CoSIA](https://cartes.gouv.fr/rechercher-une-donnee/dataset/IGNF_COSIA), and with more relevant categories [OCS GE](https://www.data.gouv.fr/datasets/ocs-ge) for our task.)

## Model : complete pooling
### Definition
For this theoretical model of species distribution, we'll start with a very simple design. The goal here, is just to prepare the workflow, and get a rough idea of what the optimal design looks like. 

For each observation $y_i$, we define a model such that:

$$\begin{align}
y_i &\sim \text{Bernoulli}(\theta_i) \\
\text{logit}(\theta_i) &= \beta_0(i) + \beta_1 \tilde{T_i} \\
\beta_0(i), \beta_1 &\sim \text{Normal}(0, 1.5) \\
\end{align}$$

With $y_i \in \{0,1\}$, our species can either be present ($y_i = 1$) or absent ($y_i = 0$), making this an absence-presence model. $\theta_i$ is the probability of observation of the species at location $i$. It depends on measured temperature at this location ($\tilde{T_i}$), a regression parameter $\beta_1$ and another parameter $\beta_0(i)$, which is an habitat specific parameter such that:
$$
\beta_0(i) = 
\begin{cases}
\beta_{a} & \text{if } i \text{ in a crop field} \\
\beta_{b} & \text{if } i \text{ in a semi-natural habitat} \\
\end{cases}
$$

### Optimal Experimental Design

For BIODICAPT, there is a fixed pool of agricultural fields in which we'll sample data every year, called the "research network". There is also a second pool of agricultural fields available, within 500 ENI. However, it contain many (too many) plots for us to sample. We have to change the ones that will bring us the most information to estimate the parameters of our model.

One common way to find an Optimal Experimental Design (OED, i.e. the best locations where our data should be collected) is to use the D-optimality criterion, which seeks to maximize the determinant of the information matrix of the experimental design.

With $\mathcal{LL}(\theta)$ the log-likelihood of the Bernoulli distribution, we first compute the Fisher information with respect to $\theta$ :

$$
\begin{align*}
\mathcal{I_i}(\theta) &= - \mathbb{E} \left( \frac{\partial^2}{\partial \theta_i^2} \mathcal{log(L}(\theta_i)) \right)\\
&= - \mathbb{E} \left( \frac{\partial^2}{\partial \theta_i^2} \left[\mathcal{log}\left(\theta_i^{y_i} (1 - \theta_i)^{1-y_i}\right) \right]\right)\\
&= - \mathbb{E} \left( \frac{\partial^2}{\partial \theta_i^2} \left[ y_i log(\theta_i) + (1-y_i) log(1 - \theta_i) \right]\right)\\
&= - \mathbb{E} \left( \frac{\partial}{\partial \theta_i} \left[\frac{y_i}{\theta_i} - \frac{1-y_i}{1 - \theta_i} \right]\right)\\
&= - \mathbb{E} \left( - \frac{y_i}{\theta_i^2} - \frac{1-y_i}{(1 - \theta_i)^2} \right) \\
&= \mathbb{E} \left( \frac{y_i}{\theta_i^2} + \frac{1-y_i}{(1 - \theta_i)^2} \right) \\
&= \frac{1}{\theta_i} + \frac{1}{1 - \theta_i} \text{, \quad with } \mathbb{E}(y_i) = \theta_i\\
&= \boxed{\frac{1}{\theta_i(1 - \theta_i)}}
\end{align*}
$$

When applied to all observations, this result is matches the one found in [(Atkinson et al., 2012)](https://pdf.sciencedirectassets.com/271628/1-s2.0-S0378375813X00114/1-s2.0-S0378375812003060/dx.doi.org/10.1016/j.jspi.2012.09.012). 

Next, using the chain rule, the Fisher information with respect to the linear predictor $\eta_i = \beta_0(i) + \beta_1 \tilde{T_i}$ is written as:

$$
\begin{align*}
\mathcal{I_i}(\eta_i) &= \left(\frac{\partial\theta_i}{\partial \eta_i}\right)^2 \mathcal{I}(\theta_i)
\end{align*}
$$

With $\theta_i = \sigma(\eta_i)$, we have $\sigma$ the sigmoid function. Its derivative is simply $\sigma'(\eta_i) = \sigma(\eta_i)(1 - \sigma(\eta_i)) = \theta_i(1-\theta_i)$. Therefore, we obtain:


$$
\begin{align*}
\mathcal{I_i}(\eta_i) &= \left[\theta_i(1-\theta_i)\right]^2 \, \frac{1}{\theta_i(1 - \theta_i)}\\
&= \boxed{\theta_i(1-\theta_i)}
\end{align*}
$$

Using the parameter vector $\beta = (\beta_a, \beta_b, \beta_1)^T$ this can be expressed as:

$$
\begin{align*}
\mathcal{I}(\beta) &= \boxed{X^T W X}
\end{align*}
$$

With $X$ the $n \times 3$ design matrix of our experiment and $W = \text{diag}\left(\theta_i(1-\theta_i)\right)_{i = 1}^n$