function (install_modules )
  foreach (file ${ARGV})
    get_filename_component (name_without_extension ${file} NAME_WE)
    install (FILES ${file}
             PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
             DESTINATION modules  )
  endforeach ()
endfunction()

# Python modules For tuneBR 
install_modules  (gsacov.py  odb.py  config_env.py  sigma_bo.py __init__.py)
install_modules  (readgsa.so)


