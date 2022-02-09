
# Datalad stuff to fetch data
# args:
#   subject
#   dataset_url = string where dataset lives (e.g. 'https://github.com/datalad-datasets/hcp-functional-connectivity.git')
#   file name

# %%
# Imports
from pathlib import Path
import nest_asyncio
nest_asyncio.apply()
import datalad.api
import os
import subprocess
import sys
import pandas as pd
import numpy

# %%
# Variables (relative path to current working directory (CWD))
#wd = Path('/data/project/impulsivity/dataset/')
wd = Path('/tmp/')
#wd = sys.argv[1]

#subject = '100206'
#subject = '100307'
subject = sys.argv[1]

print('current conda env:')
print(os.environ['CONDA_DEFAULT_ENV'])
print(f'INPUT working dir {wd}')
print(f'Running subject: {subject}')

wd = Path(wd)
subject = Path(subject)

# %%
# Install datalad dataset
dataset_path = wd / 'hcp-functional-connectivity' # also relative to CWD)
dataset_url = 'https://github.com/datalad-datasets/hcp-functional-connectivity.git'

print(f'Cloning dataset {dataset_path}')
dataset = datalad.api.install(path = dataset_path, source = dataset_url)
print('Dataset cloned')
#rerunning will do nothing - idenpotent

# %%
# Get rsFMRI
# paths relative to datalad dataset
subject_d_path = Path(subject) / 'MNINonLinear' / 'Results' / 'rfMRI_REST1_LR'
rsfmri_fname = subject_d_path / 'rfMRI_REST1_LR_hp2000_clean.nii.gz'

print(f'Getting data {rsfmri_fname}')
dataset.get(rsfmri_fname)
print('Got')
