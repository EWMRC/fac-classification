#!/bin/bash
#SBATCH --job-name=test_write         # Job name
#SBATCH --ntasks=1                    # Run on a 4 cores per node
#SBATCH --partition=skylake           # Select partition
#SBATCH --mem=10gb                     # Job memory request
#SBATCH --time=10-00:00:00                # Time limit hrs:min:sec
#SBATCH --output=test_write_%j.log    # Standard output and error log

echo "====================================================="
pwd; hostname; date
echo "====================================================="

module load containers/rocker-with-extensions
echo "Running test_write"

singularity run $ROCKER_CONT Rscript "test_write.R"
echo "====================================================="
date