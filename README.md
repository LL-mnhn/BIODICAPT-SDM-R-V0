# BIODICAPT-SDM-R-V0

## Overview
This project is dedicated to Optimal Experimental Design (OED) for the BIODICAPT
project.

## Data sources
This project uses the following databases :

- [CORINE Land Cover 2018](https://land.copernicus.eu/en/products/corine-land-cover/clc2018#download) : Raster with 100m resolution
- [Météo France dataset](www.data.gouv.fr/datasets/donnees-climatologiques-de-base-mensuelles) : monthly average of temperatures for 2018

## Structure
This repository is structured as follows:

```bash
.
├── main
├── utils
├── resources
│   ├── raw_data
│   └── transformed_data
└── outputs
    ├── figures
    └── results
```

## Usage

### Get repository on your machine
Run `git clone https://github.com/LL-mnhn/BIODICAPT-SDM-R-V0.git` in your 
terminal to clone this repository.

### Scripts
In R, run :

- `0-import_datasets.R`: to fetch/get instructions to manually download datasets


## Contact

Feel free to contact me at [loic.lehnhoff@mnhn.fr](mailto:loic.lehnhoff@mnhn.fr) 
for any question regarding this project.