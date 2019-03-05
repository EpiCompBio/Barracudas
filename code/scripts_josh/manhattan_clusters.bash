#!/bin/sh
#PBS -l walltime=24:00:00
#PBS -l select=1:ncpus=1:mem=24gb
#PBS -N manhattan_cluster
#PBS -q med-bio

cd /rds/general/project/medbio-berlanga-group/live/projects/group_multi_morbidity/Barracudas/code/scripts_josh/

module load anaconda3/personal

Rscript clusters_manhattan.R 

