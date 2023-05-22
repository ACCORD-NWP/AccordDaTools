list( APPEND tools_tests_scripts
             test_jbdiagnose_help.sh
             test_plotjbbal_help.sh
             test_plotjbdiag_help.sh
             test_diag_diacov_help.sh
             test_plotdiacov_help.sh
             test_varbcdiagnose_help.sh
             test_plotvarbcpred_help.sh
     )


foreach( _script ${tools_tests_scripts} )
    add_test(
        NAME ${_script}
        COMMAND ${CMAKE_CURRENT_SOURCE_DIR}/${_script}
    )
    set_tests_properties( ${_script} PROPERTIES
        ENVIRONMENT PATH=${CMAKE_SOURCE_DIR}/scripts:$ENV{PATH}
    ) 
endforeach()
#        TEST_DEPENDS jbbal_data

