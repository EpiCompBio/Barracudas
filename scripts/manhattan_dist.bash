#!/bin/sh
#PBS -l walltime=72:00:00
#PBS -l select=1:ncpus=1:mem=96gb
#PBS -N manhattan


cd /rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/code/HDA_multi-morbidity/Barracudas/scripts/

module load anaconda3/personal

Rscript manhattan_dist_HPC.R 

