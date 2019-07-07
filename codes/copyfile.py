#!/usr/local/bin/python3
import sys
import subprocess

sourceDir = sys.argv[1]
fnames = sys.argv[2]
targetDir = sys.argv[3]
with open(fnames, 'r') as f:
    for line in f:
        fname = line.strip()
#         print(fname)
        res1 = subprocess.run(['cp', sourceDir + fname, targetDir],
                              stdout=subprocess.PIPE, text=True)
        
print("All files are copied!")
