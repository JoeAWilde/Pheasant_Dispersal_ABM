#!/bin/bash

#SBATCH --job-name="script_5"           # Job name
#SBATCH --output=job_output_%A.txt   # Output file (%A = job array ID, %a = task ID)
#SBATCH --error=job_error_%A.txt     # Error file
#SBATCH --time=168:00:00                 # Time limit
#SBATCH --cpus-per-task=1               # Each task gets 1 CPU core
#SBATCH --mem=16G                        # Reduce memory request (was 124G)
#SBATCH --export=ALL                    # Export environment variables
#SBATCH --partition=long               # Use high-memory partition
#SBATCH --mail-user=joe.wilde@bioss.ac.uk

echo "Running script_5"
Rscript "5_prep covariates for simulation.R"
echo "Task completed at $(date)"
