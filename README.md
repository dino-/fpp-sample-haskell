# fpp-sample-haskell

## !!!!! ATTENTION !!!!!

This project has been permanently moved to Codeberg
([fpp-sample-haskell](https://codeberg.org/dinofp/fpp-sample-haskell)) and is no longer actively
maintained on Github. Do not use the Issues system on Github to report to us.
Don't bother forking or getting source from here as it will not be updated.

Microsoft is not a friend of open-source and we do ourselves a disservice
giving them this impressive power over our work.

Never forget 2020 when Github (a Microsoft product) removed the popular
open-source `youtube-dl` project, sparking enormous controversy. The issue is
not that pushback eventually prompted reinstatement - Github can and will act
like this against us at any time.

## !!!!! ATTENTION !!!!!

## Synopsis

Forecast post-processing utilities


## Description

Utilities to run post-processing tasks (such as grad script runs
and file post) for a variety of models.

This is a partial sample of some of my past work. This project is no longer
complete, likely won't build and certainly won't run.


## Building from source

Get the directory ready, first time:

    $ stack setup
    $ stack build

From here you can work normally:

    $ stack build
    $ stack test
    $ ./util/install.hs

In that install directory..

    $ ./bin/simulate -n para 2016022500 ctl

Back on the dev system

    $ stack clean
    $ stack build
    ...


Final deployment checklist before committing into the opt repo's dist branch:

- Edit `resources/enkf_conus.config`
   - `FOS_DEST_PREFIX` switched to `/output_files`
   - `FOS_ENABLED=1`
- In `resources/modules/fpp` and, of course `../Modules/modulesfiles/fpp-enkf_conus`
   - Make sure the correct `FPP_ROOT` is being used (NOT `devA/projects`)

Once this is done, you may commit and push


Also, if you need to update the module:

    $ cd /cfs/devA/projects/opt
    $ cp fpp-enkf_conus/resources/modulefiles/fpp-enkf_conus Modules/modulefiles/
