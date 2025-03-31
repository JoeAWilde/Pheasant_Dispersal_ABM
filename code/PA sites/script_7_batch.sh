#!/bin/bash

#SBATCH --job-name="script_7"           # Job name
#SBATCH --output=job_output_%A.txt   # Output file (%A = job array ID, %a = task ID)
#SBATCH --error=job_error_%A.txt     # Error file
#SBATCH --time=168:00:00                 # Time limit
#SBATCH --cpus-per-task=5               # Each task gets 1 CPU core
#SBATCH --mem=16G                        # Reduce memory request (was 124G)
#SBATCH --export=ALL                    # Export environment variables
#SBATCH --partition=long               # Use high-memory partition
#SBATCH --mail-user=joe.wilde@bioss.ac.uk
#SBATCH --mail-type=END,FAIL
echo "Running script_7"
Rscript "7_combine simulations.R"
echo "Task completed at $(date)"
