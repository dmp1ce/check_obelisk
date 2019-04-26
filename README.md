# check_obelisk

[![CircleCI](https://circleci.com/gh/dmp1ce/check_obelisk.svg?style=svg)](https://circleci.com/gh/dmp1ce/check_obelisk)

Monitoring (Nagios) plugin for checking Obelisk miner temperatures from the Obelisk API.

## Help

```
$ check_obelisk --help
check_obelisk - Nagios monitoring plugin for Obelisk miner API

Usage: check_obelisk [-v|--version] [-H|--host HOST] [-P|--port PORT]
                     [-u|--username STRING] [-p|--password STRING]
                     [-t|--temp_warn NUMBER] [-T|--temp_crit NUMBER]
  Return Nagios formatted string based on Obelisk miner API returned values

Available options:
  -h,--help                Show this help text
  -v,--version             Show version information
  -H,--host HOST           Hostname of miner API (default: "127.0.0.1")
  -P,--port PORT           Port of miner API (default: "80")
  -u,--username STRING     Username of login to API (default: "admin")
  -p,--password STRING     Password of login to API (default: "admin")
  -t,--temp_warn NUMBER    Warning temperature threshold in
                           Celsius (default: 90.0)
  -T,--temp_crit NUMBER    Critical temperature threshold in
                           Celsius (default: 100.0)
```

## Usage


```
$ check_cgminer -H 10.0.0.55 -t 50
WARNING: Temperature over warning threshold of 50.0 C | exhaustTemp=67.5;50.0;100.0;20.0;120.0
```
