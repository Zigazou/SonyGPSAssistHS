# SonyGPSAssistHS
Sony cameras which embed a GPS are able to use assist data to accelerate the
positioning.

This program downloads and writes Sony GPS assist data to SD cards under Linux.

# Requirements
SonyGPSAssist needs the Curl development files

# How it works
SonyGPSAssist works in the following order:

- look for each mount point which is FAT formatted and contains a PRIVATE/SONY
  directory that is also writable by the current user,
- download the GPS assist data from Sony website,
- download the MD5 from Sony website,
- compare the MD5,
- create the directory PRIVATE/SONY/GPS if it does not already exist,
- write the GPS data into this directory.
