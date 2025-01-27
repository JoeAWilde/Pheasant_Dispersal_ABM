#!/bin/bash

#SBATCH --job-name="step_length_angle_regression"  # Job name
#SBATCH --output=job_output_%j.txt                # Output file (%j = job ID)
#SBATCH --error=job_error_%j.txt                  # Error file
#SBATCH --time=24:00:00                           # Time limit (hh:mm:ss)
#SBATCH --ntasks=1                                # Number of tasks (1 for running a single script)
#SBATCH --cpus-per-task=4                         # Number of CPUs per task
#SBATCH --mem=16G                                 # Memory per node
#SBATCH --export=ALL                              # Export all environment variables
#SBATCH --partition=medium                        # Set which partition to use
#SBATCH --mail-user=joewilde@bioss.ac.uk
#SBATCH --mail-type=END,FAIL

# Print job details
echo "Running R script: code/2_step length and turning angle regression.R"

# Load R module (if needed)
module load R/4.4.2                                # Adjust version to match your system

# Run the R script
Rscript "code/2_step length and turning angle regression.R"

# Print completion message
echo "Job finished at $(date)"
