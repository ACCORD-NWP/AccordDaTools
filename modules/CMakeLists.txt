function (install_modules )
  foreach (file ${ARGV})
    get_filename_component (name_without_extension ${file} NAME_WE)
    install (FILES ${file}
             PERMISSIONS OWNER_EXECUTE OWNER_WRITE OWNER_READ GROUP_EXECUTE GROUP_READ
             DESTINATION modules  )
  endforeach ()
endfunction()

# Py  modules For tuneBR 
install_modules  (__init__.py  
	          gsacov.py  
	          read_odb.py  
		  config_env.py  
		  sigma_bo.py )

# Py modules for obstool 
install_modules  (build_sql.py  
	         conv_stats.py  
		 handle_df.py  
		 io_base.py  
		 setting.py  
		 utils.py
		 obstype_info.py)
		  


