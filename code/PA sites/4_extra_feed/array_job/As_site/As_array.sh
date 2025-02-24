#!/bin/bash

#SBATCH --job-name="As_array"           # Job name
#SBATCH --output=job_output_%A_%a.txt   # Output file (%A = job array ID, %a = task ID)
#SBATCH --error=job_error_%A_%a.txt     # Error file
#SBATCH --time=96:00:00                 # Time limit
#SBATCH --array=1-1000%100              # Create an array job with 100 tasks, max 9 running at once
#SBATCH --cpus-per-task=1               # Each task gets 1 CPU core
#SBATCH --mem=5G                        # Reduce memory request (was 124G)
#SBATCH --export=ALL                    # Export environment variables
#SBATCH --partition=long               # Use high-memory partition
#SBATCH --mail-user=joe.wilde@bioss.ac.uk
#SBATCH --mail-type=ARRAY_TASKS

echo "Running task ID: $SLURM_ARRAY_TASK_ID"
Rscript "6_run simulation array.R" $SLURM_ARRAY_TASK_ID
echo "Task $SLURM_ARRAY_TASK_ID completed at $(date)"
