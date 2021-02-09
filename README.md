# CovidSurg

COVIDSurg Collaborative is led by the NIHR Global Surgery Research Unit. This machine learning risk stratification tool has been developed with the Health Data Research UK (HDRUK) team at the Institute of Translational Medicine, hosted within the University of Birmingham. The risk calculator is based upon data from patients infected with SARS-CoV-2 within the 7-days before to 30-days after surgery (perioperative infection). Patients underwent surgery of all types and for any indication during the COVID-19 pandemic (February 2020 to July 2020). The link to the study protocol is available at: https://globalsurg.org/covidsurg/ .A machine learning technique was used to generate the CovidSurg Risk calculator and has the potential to inform surgeons, patients, and healthcare decision makers to reach a better understanding of risk when treating surgical patients during the COVID-19 pandemic

Data is available upon reasonable request, and the code contains an Rmarkdown example (.html) as well as figures demonstrating the analysis pipleine. The paper was done using BlueBear High Performance Computing resources from the University of Bimringham and so have been adapted here for regular use, with different constratints. Regardless the flow of work is the exact same one. 

Instructions of use: 
It is based on the drake R package and so after setting all .R and .Rmd scripts found in the R folder to the same working directory, make sure all packages found in  CovidSurgPackages.R are downloaded and then, just running the main file CovidSurg.R should yield the results. Wth no input parameter changes, all files found in folder ResultFiles are created. For a general overview of the pipeline see CovidSurgReport.html in ResultFiles folder. 

The associated risk calculator app can be found at: https://covidsurgrisk.app/
