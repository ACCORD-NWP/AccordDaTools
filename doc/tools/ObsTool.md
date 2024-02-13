# Documentation about How to use Obstool

 1. Get the ascii file from odb: see HoWtogetINPUTfiles_obstool
 1. Set and Run scripts/1prepare_obstool.sh
 1. Set and Run scripts/2monitor_obstool.sh

## How to get INPUT files_obstool:

HOW to get the input files: From ODB files : examples for gnss ztd, radar, and sat obs

## gnss ztd obs
The SQL from ODB2
```bash
odb sql 'select statid,lat,lon,an_depar,fg_depar,obsvalue where varno=128' -i ccma.odb > ${DIROUT}/gnss_odb_${yyyy}${mm}${dd}${hh}
grep -v "obsvalue" ${DIROUT}/gnss_odb_${yyyy}${mm}${dd}${hh} > ${DIROUT}/ccma_mon_${yyyy}${mm}${dd}${hh}
```

Using conv.40h1.sql like:
```
CREATE VIEW mondb AS
SELECT
    type@desc                  ,
    expver@desc                ,
    class@desc                 ,
    stream@desc                ,
    andate@desc                ,
    antime@desc                ,
    reportype@hdr              ,
    numtsl@desc                ,
    timeslot@timeslot_index    ,
    seqno@hdr                  ,
    bufrtype@hdr               ,
    subtype@hdr                ,
    groupid@hdr                ,
    obstype@hdr                ,
    codetype@hdr               ,
    sensor@hdr                 ,
    statid@hdr                 ,
    date@hdr                   ,
    time@hdr                   ,
    report_status@hdr          ,
    report_event1@hdr          ,
    report_rdbflag@hdr         ,
    degrees(lat) as lat@hdr    ,
    degrees(lon) as lon@hdr    ,
    lsm@modsurf                ,
    orography@modsurf          ,
    stalt@hdr                  ,
    flight_phase@conv          ,
    anemoht@conv               ,
    baroht@conv                ,
    station_type@conv          ,
    sonde_type@conv            ,
    entryno@body               ,
    obsvalue@body              ,
    varno@body                 ,
    vertco_type@body           ,
    vertco_reference_1@body    ,
    datum_anflag@body          ,
    datum_status@body          ,
        datum_event1@body          ,
    datum_rdbflag@body         ,
    biascorr@body              ,
    biascorr_fg@body           ,
    an_depar@body              ,
    fg_depar@body              ,
    obs_error@errstat          ,
    repres_error@errstat       ,
    pers_error@errstat         ,
    fg_error@errstat           ,
    eda_spread@errstat         ,
    mf_vertco_type@body        ,
    mf_log_p@body              ,
    mf_stddev@body
FROM desc,timeslot_index,hdr,modsurf,conv,body,errstat
WHERE obstype>=1 AND obstype<=6;
```

##Radar RH and DOW
```bash
odb sql 'select statid,varno,lat,lon,vertco,an_depar,fg_depar,obsvalue where obstype=13 and varno=29' -i ccma.odb > ${DIROUT}/radar_RH_${yyyy}${mm}${dd}${hh}
odb sql 'select statid,varno,lat,lon,an_depar,fg_depar,obsvalue where obstype=13 and varno=195' -i ccma.odb > ${DIROUT}/radar_DOW_${yyyy}${mm}${dd}${hh}
grep -v "obsvalue" ${DIROUT}/radar_RH_${yyyy}${mm}${dd}${hh} > ${DIROUT}/ccma_mon_${yyyy}${mm}${dd}${hh}
# or 
grep -v "obsvalue" ${DIROUT}/radar_DOW_${yyyy}${mm}${dd}${hh} > ${DIROUT}/ccma_mon_${yyyy}${mm}${dd}${hh}
```

Using radarv.cy40.sql like:
```
CREATE VIEW radarv
SELECT
obstype@hdr,codetype@hdr,statid,varno,lat@hdr,lon@hdr,vertco_type@body,vertco_reference_1@body,sensor@hdr,date,time@hdr,report_status.active@hdr,report_status.blacklisted@hdr,report_status.passive@hdr,report_status.rejected@hdr,datum_status.active@body,datum_status.blacklisted@body,datum_status.passive@body,datum_status.rejected@body,an_depar,fg_depar,obsvalue,final_obs_error@errstat,elevation@radar_body,distance@radar_body,azimuth@radar_body
FROM  hdr,desc,body,errstat,sat,radar,radar_body
WHERE (varno /= 91 ) AND (an_depar is not NULL)
```

## Satem obs

odbsql  -T -q 'SELECT
statid,vertco_reference_1,sensor,rad2deg(lat),rad2deg(lon),an_depar,tdiff(date,time,andate,antime)/60,fg_depar,obsvalue
FROM hdr,body,desc where obstype=7' > ${DIROUT}/ccma_mon_${yyyy}${mm}${dd}${hh}


using  satem.40h1.sql like:
CREATE VIEW mondb AS
SELECT
    type@desc                  ,
    expver@desc                ,
    class@desc                 ,
    stream@desc                ,
    andate@desc                ,
    antime@desc                ,
    numtsl@desc                ,
    timeslot@timeslot_index    ,
    latlon_rad@desc            ,
    enda_member@desc           ,
    seqno@hdr                  ,
    subseqno@hdr               ,
    bufrtype@hdr               ,
    subtype@hdr                ,
    subsetno@hdr               ,
    groupid@hdr                ,
    reportype@hdr              ,
    obstype@hdr                ,
    codetype@hdr               ,
    sensor@hdr                 ,
    retrtype@hdr               ,
    instrument_type@hdr        ,
    stalt@hdr                  ,
    date@hdr                   ,
    time@hdr                   ,
    numlev@hdr                 ,
    numactiveb@hdr             ,
    checksum@hdr               ,
    sortbox@hdr                ,
    areatype@hdr               ,
    report_status@hdr          ,
    report_event1@hdr          ,
    report_rdbflag@hdr         ,
    report_blacklist@hdr       ,
    report_event2@hdr          ,
    thinningkey_1@hdr          ,
    thinningkey_2@hdr          ,
    thinningkey_3@hdr          ,
    thinningtimekey@hdr        ,
    sitedep@hdr                ,
    source@hdr                 ,
    degrees(lat) as lat@hdr    ,
    degrees(lon) as lon@hdr    ,
    statid@hdr                 ,
    seaice@modsurf             ,
    orography@modsurf          ,
    snow_depth@modsurf         ,
    t2m@modsurf                ,
    albedo@modsurf             ,
    windspeed10m@modsurf       ,
    u10m@modsurf               ,
    v10m@modsurf               ,
    surface_class@modsurf      ,
    tsfc@modsurf               ,
    satellite_identifier@sat   ,
    satellite_instrument@sat   ,
    zenith@sat                 ,
    azimuth@sat                ,
    solar_zenith@sat           ,
    solar_azimuth@sat          ,
    gen_centre@sat             ,
    gen_subcentre@sat          ,
    datastream@sat             ,
    scanline@radiance          ,
    scanpos@radiance           ,
    orbit@radiance             ,
    typesurf@radiance          ,
    corr_version@radiance      ,
    cldcover@radiance          ,
    cldptop_1@radiance         ,
    cldptop_2@radiance         ,
    cldptop_3@radiance         ,
    cldne_1@radiance           ,
    cldne_2@radiance           ,
    cldne_3@radiance           ,
    skintemper@radiance        ,
    skintemp_1@radiance        ,
    skintemp_2@radiance        ,
    skintemp_3@radiance        ,
    scatterindex_89_157@radiance,
    scatterindex_23_89@radiance,
    scatterindex_23_165@radiance,
    lwp_obs@radiance           ,
    asr_pclear@radiance        ,
    asr_pcloudy@radiance       ,
    asr_pcloudy_low@radiance   ,
    asr_pcloudy_middle@radiance,
    asr_pcloudy_high@radiance  ,
    csr_pclear@radiance_body   ,
    cld_fg_depar@radiance_body ,
    rank_cld@radiance_body     ,
    tausfc@radiance_body       ,
    skintemp_retr@radiance_body,
    emis_rtin@radiance_body    ,
    emis_atlas@radiance_body   ,
    emis_retr@radiance_body    ,
    emis_fg@radiance_body      ,
    entryno@body               ,
    obsvalue@body              ,
    varno@body                 ,
    vertco_type@body           ,
    vertco_reference_1@body    ,
    vertco_reference_2@body    ,
    datum_anflag@body          ,
    datum_status@body          ,
    datum_event1@body          ,
    datum_rdbflag@body         ,
    datum_blacklist@body       ,
    datum_event2@body          ,
    varbc_ix@body              ,
    biascorr@body              ,
    biascorr_fg@body           ,
    tbcorr@body                ,
    wdeff_bcorr@body           ,
    an_depar@body              ,
    fg_depar@body              ,
    actual_depar@body          ,
    qc_a@body                  ,
    qc_l@body                  ,
    qc_pge@body                ,
    fc_sens_obs@body           ,
    an_sens_obs@body           ,
    jacobian_peak@body         ,
    jacobian_peakl@body        ,
    jacobian_hpeak@body        ,
    jacobian_hpeakl@body       ,
    mf_vertco_type@body        ,
    mf_log_p@body              ,
    mf_stddev@body             ,
    nlayer@body                ,
    final_obs_error@errstat    ,
    obs_error@errstat          ,
    repres_error@errstat       ,
    pers_error@errstat         ,
    fg_error@errstat           ,
    eda_spread@errstat         ,
    obs_ak_error@errstat       ,
    obs_corr_ev_1@errstat      ,
    obs_corr_mask@errstat      ,
    obs_corr_diag_1@errstat
FROM desc,timeslot_index,hdr,modsurf,sat,radiance,radiance_body,body,errstat WHERE obstype@hdr=7;






