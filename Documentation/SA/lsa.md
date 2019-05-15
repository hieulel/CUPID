# Local Sensitivity Analysis
Written by Hieu Le

Wed, 15 May 2019 12:12:41 +0930 

## Introduction
Throughout the simulation data on CUPID, Sensitivity Analysis allow the simulated data and the underlying system to be monitored and compared. The process does not only help the analyst to understand the key parameters, but it help the unimportant to be isolated and monitored in certain ways. As the result, it maximise the efficiency of Cupid simulation over time, helping them to achieve the understanding of the environmental system. 

### The process
In order to initiate the sensitivity process, the data was needed to be analysed and screened. This helps the key parameters to be isolated for further analysis. These are the steps will be taken in local sensitivity analysis. 
1. Preliminary Screening
2. CUPID
3. Tolerance
4. SA
5. Convergence and validation (GSA applied)

#### Data
Please refer to [sample.c7.in](/Document/GitHub/CUPID/sample.c7.in)

Firstly, the chosen key parameters in the input file are:
1. Initial water content `(WATER)`
2. Number of leave angle class `ITOT`
3. Clumping factor for canopy structure `CLUMP` it is essential to convert effective Plant or Leaf Area Index into actual LAI or PAI, which has previously been shown to have a significant impact on biophysical parameter retrieval using optical remote sensing techniques in forests, woodlands, and savannah. Here, a simulation framework was applied to assess the performance of existing in-situ clumping retrieval methods in a 3D virtual forest canopy, which has a high degree of architectural realism [Journal](https://www.sciencedirect.com/science/article/pii/S0168192317302496)
4. Row space `ROWSPACE`

Output:
1. Diabatic wind profile correction factor above the canopy `PSIMA`
2. Diabatic wind profile correction factor above the soil `PSIMS` 
