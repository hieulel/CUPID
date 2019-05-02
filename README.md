# CUPID
[![Build Status](https://travis-ci.com/hieulel/CUPID.svg?token=hqvxSi657pY2e4YwZhwS&branch=master)](https://travis-ci.com/hieulel/CUPID) ![License](https://img.shields.io/apm/l/vim-mode.svg)

## Introduction
CUPID is a comprehensive soil-plant-atmosphere model that uses inputs of leaf physiological characteristics (photosynthesis, stomatal conductance and respiration), canopy architecture, soil characteristics (heat and water properties) with boundary conditions at the bottom of the root zone and above the canopy to predict predict plant-environment interactions of many kinds. Examples include water budgets of irrigated crops, plant-pest-microenvironment interactions, canopy light-use efficiency, water- use efficiency, canopy energy budgets, leaf wetness duration and remote sensing applications.

## Structure of CUPID
### Makefile Structure
The makefile consists this structure
![`CUPID` Makefile Structure](makefile.png)
### Subroutine structure
![CUPID Subroutine Documentation](./Documentation/subroutines/subroutine.md}
### Subroutine `CALL` pipeline
![CUPID Subroutine CALL pipeline](cupid_sql.png)


## CUPID Research Pipeline
![`CUPID` Makefile Structure](cupidpipeline.png)

## Current task
- [x] Compile CUPID and generate sample output file
- [x] Create table to show the relationship between sub-programs
- [x] Add the conceptual structure of CUPID to whole research timeline
- [ ] Explain each subroutine equation, on paper (using Latex - maybe)
- [ ] Stock and flow diagram for CUPID
