Magnus Lindskog 5 Dec 2021

Readme file for fetching data from fetstat strfun generation
and pointing and relevant files. Then instructons to compile
fediacov, and run to generate some more statistics files.
Finally instructions on how to plot .xy files using python. 

--------------------------------------------------------------

-----Take data from fetstat output firectory where you generate 
structure functions and place in DATADIR.
Here:
/nobackup/smhid18/users/sm_mlind/jbdata_all_oper/jbdiagconv_hirlam/fediacov/festatdir
cortp.xy  covdp.xy  covqd.xy  covqp.xy	covqt.xy  covtd.xy  covtp.xy  
explq_divu.xy  explq_pb.xy  explq_tpsu.xy  expltps_divu.xy  expltps_pb.xy
METCOOP25D_65_2021100300-2021111006_412.bal  METCOOP25D_65_2021100300-2021111006_412.cv  
METCOOP25D_65_2021100300-2021111006_412.cvt 

All these files I copy to:
/nobackup/smhid18/users/sm_mlind/jbdata_all_oper/jbdiagconv_hirlam/fediacov/Bmatrix_cy43/clean/data
cp /nobackup/smhid18/users/sm_mlind/jbdata_all_oper/jbdiagconv_hirlam/fediacov/festatdir/*
/nobackup/smhid18/users/sm_mlind/jbdata_all_oper/jbdiagconv_hirlam/fediacov/Bmatrix_cy43/clean/data/.

--------------------------------------------------------------

----- Compile diacov by in util//makeup

change line
$(LIBDISK)/ACADFA1D $(LIBDISK)/SODA $(LIBDISK)/FESTAT $(LIBDISK)/CONVERT_ECOCLIMAP_PARAM $(LIBDISK)/ALTO \
to 
$(LIBDISK)/ACADFA1D $(LIBDISK)/SODA $(LIBDISK)/FESTAT $(LIBDISK)/CONVERT_ECOCLIMAP_PARAM $(LIBDISK)/ALTO $(LIBDISK)/DIACOV \\
and then recompile (we should have it compiled by default system group?)

in bindir you will find:
/nobackup/smhid18/users/sm_mlind/hm_home/cy43mc_modarl/bin/R64/DIACOV

(note diacov source code are in src/utilities/bcov_lam, the changeste proposed by Antonin B already
in our Cy43 code)

------------------------------------------------------------

x Run diacov- Run script rundiacov.ksh
in scr check your paths. On Bi I launced by first logging
into interactive fat node by typing, 'interactive -C fat'
followed by ./rundiacov.ksh
(produces a lot of data in DATADIR)

---------------------------------------------------------------

----Two D Plotting of .xy files from festat and diacov
x Modify scr/runplotxy.ksh DATADIR and BASEDIR. Make sure
DATADIR as in previous step. Then placed
in $BASEDIR/scr type ./runplotxy.ks. Plots from founf Data 
in DATADIR will be produced and put in $BASEDIR/plots. 
As a crude sanity check compare your results with (Berre,2000),
https://journals.ametsoc.org/view/journals/mwre/128/3/1520-0493_2000_128_0644_eosamf_2.0.co_2.xml

-----------------------------------------------

Next step is to generate runploty and ploty.py.
Although not so urgent since HARMONIE tools exist
for most 1d quantities, but for lenthscales etc
it would be interesting.

--------------------------------------------------
