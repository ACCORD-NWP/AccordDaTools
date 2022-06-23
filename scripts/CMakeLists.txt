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
install_scripts (varbcdiagnose.sh plotvarbcpred.py)
install_scripts (diacov.sh)
install_scripts (dfscomp.sh plotdfs.py)
#install_scripts (diacov.sh festat.sh)
