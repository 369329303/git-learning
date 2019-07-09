#!/usr/local/bin/python3
'''
usage: copyfile.py src_dir fnames.txt dst_dir
'''

import sys
import os
import shutil

src_dir = sys.argv[1]
fnames = sys.argv[2]
dst_dir = sys.argv[3]
with open(fnames, 'r') as f:
    for line in f:
        fname = line.strip()
        print(fname)
        dirname = os.path.dirname(fname)
        basename = os.path.basename(fname)
        src_full_path = os.path.join(src_dir, fname)
        dst_full_dir = os.path.join(dst_dir, dirname)
        if (not os.path.exists(dst_full_dir)):
            os.makedirs(dst_full_dir)

        shutil.copy2(src_full_path, dst_full_dir)

print("-------All files are copied!----------")
