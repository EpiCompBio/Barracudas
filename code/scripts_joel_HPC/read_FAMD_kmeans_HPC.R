#!/bin/sh
#PBS -l walltime=100:00:00
#PBS -l select=1:ncpus=8:mem=20gb
#PBS -N FAMD_HPC
#PBS -q med-bio


cd /rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/Barracudas
module load anaconda3/personal

Rscript code/scripts_joel_HPC/FAMD_kmeans_HPC.R

