# KAA052
Here comes a short explination of the files created during the project.

variation_mug.m
Creates a variation analysis, customized for the cup. The set parameters is adjusted according to the conditions for the cup. 
The parameter limits can be adjusted in dT_dt_mug.m. For further information and instructions, read the comments in the file.

test_variation.m
Creates a variation analysis, customized for the beaker. 
The parameter limits can be adjusted in dT_dt_RG2.m. For further information and instructions, read the comments in the file.

beakeranalysis.m
Initial analysis of part A data.

Anova_residual_test.m
Creates plots for the heat and mass transfer from the glass beaker. It also generates ANOVA tables for the models and residual plots. Uses dT_dt_G.m.

test_cup.m
Generarates the plot over heat and mass tranfer for the constructed cup in aluminium. Uses dT_dt_cup.m

pr_air.m
Generates Pr for air

kv.m
Generates mass transfer coefficient for covective mass transfer between water and air

heat_precentage.m
Generates plots over the different heat transport phenomenons contridution to the total heat transfer. 

h_top.m
Generates generates the conductive heat transfer coefficient for the top of the beaker/cup.

h_cyl.m
Generates generates the conductive heat transfer coefficient for the side of the beaker/cup.

Cond.m 
Calculates the conduction on the sides of the beaker/glas.

betag_v2.m 
Generates beta*g/v^2. 
