function (install_scripts)
  foreach (file ${ARGV})
    get_filename_component (name_without_extension ${file} NAME_WE)
    install (FILES ${file}
             PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
             DESTINATION bin
             RENAME ${name_without_extension})
  endforeach ()
endfunction()

install_scripts (jbdiagnose.sh plotjbbal.py plotjbdiag.py)
install_scripts (diacov.sh 1_extract_varbc_cycle.sh 2_varbc_fig.sh varbc_plot.Rsh)
#install_scripts (diacov.sh festat.sh)
