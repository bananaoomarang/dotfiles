#!/usr/bin/env python3
import subprocess
import psutil

def process_running(processName):
    '''
    Check if there is any running process that contains the given name processName.
    '''
    #Iterate over the all the running process
    for proc in psutil.process_iter():
        try:
            # Check if process name contains the given name string.
            if processName.lower() in proc.name().lower():
                return True
        except (psutil.NoSuchProcess, psutil.AccessDenied, psutil.ZombieProcess):
            pass
    return False;

if process_running("redshift"):
    subprocess.call("killall redshift", shell=True)
else:
    subprocess.call("redshift", shell=True)
