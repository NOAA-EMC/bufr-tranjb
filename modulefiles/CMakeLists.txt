configure_file("bufrtranjb.lua.tmpl" "${PROJECT_VERSION}.lua")

install(FILES       "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_VERSION}.lua"
        DESTINATION "${MODULEFILE_INSTALL_PREFIX}/bufr_tranjb")
