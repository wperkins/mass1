# Link Auxiliary File

Information in the [link](link.md) and [point](point.md) files can
only supply enough information to represent a very few link types.
More complicated link types require extra information, which is
specified in the *link auxiliary* file.

This file must be named `link_auxiliary.dat` and exist in the same
directory as [`mass1.cfg`](configuration.md).  This file is only
required to exist if a link type used needs it.

The link auxiliary file uses the [JavaScript Object Notation
(JSON)](https://www.json.org/json-en.html) format.  The file contains
one object for each link that needs auxiliary information, with the
link id as it's name.  Some examples are shown below. 

## Offline Storage Link

The offline storage link describes a storage area that is directly
connected to the channel.  

```
{
    "3" : {
        "InletElevation" : 5.0,
        "Storage" : {
            "type" : "simple",
            "Area" : 1000000.0,
            "BottomElevation" : 4.0,
        }
    }
}
```

## Pumped Storage Link


```
{
    "3" : {
        "Storage" : {
            "type" : "simple",
            "Area" : 1000000.0,
            "BottomElevation" : 4.0,
        }
    }
}
```
