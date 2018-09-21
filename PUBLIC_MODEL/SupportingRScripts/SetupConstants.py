#!/usr/bin/python
# -*- coding: utf-8 -*-


import io
import os
import datetime
import numpy 
import subprocess
from subprocess import call

cwd = os.getcwd()
wd = cwd + '/SupportingRScripts'
os.chdir(wd)
 
with open("CONSTANTS.txt", "r") as filestream:
    with open("CONSTANTS.h", "w") as filestreamtwo:
        for line in filestream:
            currentline = line.split(",")
            total = str( currentline[0] + ' ' +   currentline[1]  + '=' + currentline[2].rstrip() +';') + "\n"
            if "define" in total:
                total = str('#' + currentline[0] + ' ' +   currentline[1]  +' '+ currentline[2].rstrip() ) + "\n"
            filestreamtwo.write(total)

os.rename(wd+'/CONSTANTS.h', cwd + '/CONSTANTS.h')

