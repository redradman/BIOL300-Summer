# BIOL300-Summer-2022

This project was completed in Summer of 2022 as part of the course work for BIOL 300 provided by University of British Columbia (UBC).
In this project, only R language is used in accordance with the course requirements. However, bash was used to mediate and combined files behind the scenes. The bash script is NOT included in this project 
The R code for phase 2 was used to generate the charts for the project.
The R code for phase 3 was used to test the validity of the hypotheses by using the appropriate statistical test based on the data. 
*Note: Styling guidelines were ignored and the comments are used in a manner to satisfy the submission requirements for the project*

## Topic of the Project
AID (auxin-induced degradation) is a hormone degradation system in plants. Auxin environment will stimulate AID protein, causing degradation. Such a system can be used in C. elegan to control the timing of a gene’s function. 

#### Overarching focus
In order to test if the system works properly, mutation of unc-43 LOF (loss of function) is used to compare with degraded unc-43 by auxin. unc-43 is a gene function on C. elegan neuron synaptic formation. The LOF mutant shows a non-uniform synaptic pattern. RFP (red fluorescent protein) and GFP (green fluorescent protein) are tagged with vesicle proteins and active zones on the synapses to visualize the synapse.

#### Data collection
20 individual C. elegan Confocal images are taken for the same neuron, which has ~ 10 puncta (which represents synapses) on each image. The intensity of GFP and RFP, and area of GFP are measured by a program for each puncta. The following are the data analyses.

#### Original Data
Due to the significant size of raw data, they are not included in this repo. 

#### Null hypotheses
Null hypotheses are based on three variables, the intensity of GFP and RFP and the area of GFP.
· Auxin degradation gives the same puncta area of GFP as wildtype
· Auxin degradation gives the same puncta intensity of GFP as wildtype
· Auxin degradation gives the same puncta intensity of RFP as wildtype

#### Prediction
Compared to unc-43 LOF, auxin degradation will give similar fluorescence intensity for both GFP and RFP and size of GFP will be similar to unc-43 LOF.
Limitation of study: The limitation of tagging all isoforms was the cause of the experiment result. The experiment did not meet the prediction as the AID tag was on the 5’ ends of unc-43 gene. Leftover isoforms were able to cover the function of unc-43. Therefore, the auxin tagged unc-43 gene failed to present a LOF phenotype, and there was no difference between wildtype and auxin treatment.

### Conclusion
Failed to reject the null hypothesis that the puncta area of GFP for Auxin and Wild type are similar. However, we can strongly reject the equivalency of light intensity for both GFP and RFP in Auxin degradation and Wild type.

#### Works Referenced
*Kurashina, M., Wang, J., Lin, J., Lee, K. K., Johal, A., & Mizumoto, K. (2021). Sustained expression of UNC-4 homeobox gene and UNC-37/Groucho in postmitotic neurons specifies the spatial organization of the cholinergic synapses in C. elegans. ELife, 10. https://doi.org/10.7554/elife.66011*

*Ashley, G. E., Duong, T., Levenson, M. T., Martinez, M. A., Johnson, L. C., Hibshman, J. D., Saeger, H. N., Palmisano, N. J., Doonan, R., Martinez-Mendez, R., Davidson, B. R., Zhang, W., Ragle, J. M., Medwig-Kinney, T. N., Sirota, S. S., Goldstein, B., Matus, D. Q., Dickinson, D. J., Reiner, D. J., & Ward, J. D. (2021). An expanded auxin-inducible degron toolkit for caenorhabditis elegans. Genetics, 217(3). https://doi.org/10.1093/genetics/iyab006*

