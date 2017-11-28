#!/bin/bash 

#SBATCH -J NPV1999 

#SBATCH -o NPV1999%j.txt

#SBATCH -N 2 

#SBATCH -n 272

#SBATCH -p development

#SBATCH -t 01:00:00 

#SBATCH -A TG-TRA150002

#SBATCH --mail-user=trb9259@uncw.edu

#SBATCH --mail-type=end 



cd $HOME # Change to home directory first, just to make sure SLURM reads from the proper directory

module load Rstats # Load the R module along with some popular packages so it will run R files

Rscript All1999Stampede2.R # You may need to change this depending on what exactly With.R is, but if it's a standard R script that can be run from command line, this would suffice

### 4.4min for core=276, n.boot=100