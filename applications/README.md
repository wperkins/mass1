# MASS1 Applications

This repository contains several PNNL
[MASS1](http://mass1dev.pnl.gov/) applications.  

These are not for general distribution.  

* `BonnevillePool`
  
  The Columbia River from Bonneville Dam (RM ??) to The Dalles Dam (RM
  ??).  This was used to supply downstream boundary conditions for
  higher dimensioned models (MASS2, StarCD, Star-CCM) in The Dalles
  tailrace in several projects for the U.S. Army Corps of Engineers,
  Portland District.  
  
* `DGAS`
  
  Application of MASS1 to the Lower Snake River (mouth to Anatone, RM ??),
  and Columbia (mouth to Priest Rapid Dam, RM ??) including the
  Clearwater River from the mouth to Orofino (RM ??) for the Dissolved
  Gas Abatement Study for the U.S. Army Corps of Engineers, Walla
  Walla District.  This was before the PID dam boundary condition was
  implemented, so it only has 3 years for which dam discharge
  corrections (as lateral inflow) were computed.   Temperature and
  total dissolved gas concentrations were simulated.  

* `HanfordReach`

  MASS1 applied to the Columbia River from McNary Dam (RM ??) to
  Priest Rapids Dam (RM ??) and the Snake River from the mouth to Ice
  Harbor Dam (RM ??).  This application has been used for a variety of
  projects for the U.S. Department of Energy, Grant County Public
  Utility District, Washington Department of Fish and Wildlife, and
  U.S. Army Corps of Engineers.   

* `LakeRoosevelt`

  This application was used to support the Confederated Colville
  Tribes in their study of white sturgeon movement in very early life
  stages.  

* `LowerSnakeColumbia`
  
  This is the `DGAS` application with updated bathymetry and PID dam
  boundary conditions.  

* `Wanapum`



# Checking Out One Application

Most of the time, only one application is needed. To do that, one uses 
"sparse checkout".  Here is an example that checks out the
`LakeRoosevelt` applications files into `myapp/LakeRoosevelt`. 

```
> git clone --no-checkout \
    https://D3G096@stash.pnnl.gov/scm/~d3g096/mass1-applications.git myapp
> cd myapp
> git config core.sparseCheckout true
> echo "LakeRoosevelt/*" > .git/info/sparse-checkout
> git checkout master
> ls
total 16
4 ./  4 ../  4 .git/  4 LakeRoosevelt/
> ls LakeRoosevelt/
total 40
4 ./          8 ChangeLog            4 Rampdown/
4 ../         4 GIS/                 4 Rampdown.truncated/
4 BaseFiles/  4 LongTerm.truncated/  4 Spring2016.truncated/
```



