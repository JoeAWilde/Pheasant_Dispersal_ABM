#!/bin/bash

#SBATCH --job-name="Ex0_disp_sim"  # Job name
#SBATCH --output=job_output_%j.txt                # Output file (%j = job ID)
#SBATCH --error=job_error_%j.txt                  # Error file
#SBATCH --time=48:00:00                           # Time limit (hh:mm:ss)
#SBATCH --ntasks=1                                # Number of tasks (1 for running a single script)
#SBATCH --cpus-per-task=100                         # Number of CPUs per task
#SBATCH --mem=124G                                 # Memory per node
#SBATCH --export=ALL                              # Export all environment variables
#SBATCH --partition=long                        # Set which partition to use
#SBATCH --mail-user=joe.wilde@bioss.ac.uk
#SBATCH --mail-type=END,FAIL

# Print job details
echo "Running R script: code/PA sites/6_run simulation.R"

# Run the R script
Rscript "6_run simulation.R"

# Print completion message
echo "Job finished at $(date)"
