## README 
### FlexCAT - Thesis Project

Anastasios (Tasos) Psychogyiopoulos

Research Master of Child Development and Education, 

University of Amsterdam

### This is the repository including all relevant code for my thesis project entitled:

**Using the unrestricted latent class model to estimate the density of item-score vectors: Towards a flexible computerized adaptive test,** 

supervised by  Niels Smits and  L. Andries van der Ark

<aside>
🐈‍⬛ Data are not included, yet.

</aside>

- **Abstract:**
    
    The construction of computer adaptive tests (CATs) with item response theory (IRT) models is considered standard practice. However, for many tests and questionnaires in psychological research IRT models may be suboptimal due to strict statistical assumptions. Recently, Van der Ark and Smits ([2022](https://osf.io/dfxv3/?view_only=6e23ff42202940718e738a7591db7f55)) proposed replacing the IRT-model in CAT with a latent class model (LCM) and replacing the estimated latent trait with any score convenient for communication (e.g., sum score); they illustrated this approach in a proof-of-principle study. In the construction of the CAT, the LCM estimates the joint density of the item scores (π) and the density of the (total) score (π+). The question at hand is: “Which information criterion (IC) should be used to obtain an adequate estimate of π and π+?” As the estimation of π typically requires many latent classes, the current literature does not provide an answer. In a large-scale simulation study using an experimental design, we investigated the effects of IC, sample size, item format, number of items, and the number of true latent classes on the bias and accuracy of estimated π, and estimated π+. On the basis of the outcomes, we will provide guidelines for calibrating sound CATs using LCM.
    

### Files:

- `0.Functions.R` All relevant helping functions for teh analysis (do not need to run, it is sourced by otherscripts)
- `0.Data-Preparation.R` It reads, cleans and manipulates the data. Please run it first.
- `SVLdata_generation.R` Generates data according to six different latent class models which will serve as population models in the upcoming simulation. Stores them in a dataframe.
- `model_estimation.R` Runs the whole simulation design. Note: It needs time!

<aside>
🐈‍⬛ I set up large virtual machines in AWS to save some time. Code and explanation pending...

</aside>

- `xANALYSIS.R` Results analysis and tables
- `VIZ.R` Code for the visualisations. You can find some output in the *Figures* folder

---

> *An extensive plan of the simulation study is in progress and will be uploaded soon.*
> 

Best,

Tasos