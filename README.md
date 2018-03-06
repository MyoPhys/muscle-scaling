# muscle-scaling
Code and sample data for analyzing the scaling of muscle contractile properties

The data files and R scripts in this repository were used for data analysis in Olberding and Deban, 2018. “Scaling of work and power in a locomotor muscle of a frog”  Journal of Comparative Physiology B

The data are results of in vitro muscle contractile experiments using the plantaris muscle of Cuban tree frogs (Osteopilus septentrionalis) capture in and around Tampa, Fl. 

WORKFLOW

1. Analyzing single muscle contractions
Raw Data Files are .txt files containing the output of individual muscle contractions. These files are contained in separate folders for each individual. These files contain 20 rows of metadata followed by 6 data columns with headers. 
Column 1 = time (ms)
Column 2 = lever position (V)
Column 3 = time (ms)
Column 4 = force (V)
Column 5 = time (ms)
Column 6 = stimulation (V)

File names are of the format
[Individual Name]_[Temperature]_[Contraction Type]_[Trial Number]

Files of contraction type “P0” or “P02” are in a separate folder and analyzed using P0_AnalysisCalcsv2.R. The P0 file with the greatest peak force (P0) is used for analyses.

All other isotonic contractions are analyzed using “Analyze Individual Trial.R”. This code automatically runs through every trial contained in the folder. The output of these files is concatenated into Individual Output Files (name format: [Individual Name]_output.csv).

2. Fitting curves and extracting values from each individual
Each Individual Output file is analyzed using Fitting_F_V_Curves.R and the results of that code for all individuals are concatenated into Muscle_Scaling_Data.csv.

3. Statistical analyses across individuals
Statistical analyses are run on Muscle_Scaling_Data.csv using Power_Law_Analysis v2.R.

Please contact me with any questions, or if you notice any errors of inconsistancies.

Jeffrey Olberding, PhD
March 6, 2018
