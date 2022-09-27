# bufr-tranjb
Software necessary for bufr decoders and obsproc satingest to file bufr format data into bufr tanks in $DCOMROOT


To install:

Clone repository:
```bash
git clone https://github.com/noaa-emc/bufr-tranjb
```

Move into desired branch and then run:

```bash
INSTALL_PREFIX=/path/you/wish/to/install/bufr-tranjb ./ush/build.sh
```

or install in local clone space:

```bash
./ush/build.sh
```

There is also the option to build and install in your local clone space but install the modulefile elsewhere:

```bash
MODULEFILE_INSTALL_PREFIX=/path/you/wish/to/install/bufr-tranjb/module ./ush/build.sh
```
