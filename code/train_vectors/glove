#!/bin/bash
#SBATCH --account=modelscape
#SBATCH --time=6:00:00
#SBATCH --partition=teton-gpu
#SBATCH --nodes=1
#SBATCH --cpus-per-task=4
#SBATCH --gres=gpu:1

module load swset/2018.05
module load cuda/10.1.243
module load miniconda3/4.3.30

source activate /pfs/tc1/home/asiefer1/conda/gpu/tensorflow/tensorflow_env

cd ~
srun python ~/projects/speciesVectors/code/train_vectors/local/model10.py

source deactivate
