pkg_check_modules(Jsoncpp_PKGCONF jsoncpp)

# Include dir
find_path(Jsoncpp_INCLUDE_DIR
        NAMES json/features.h
        PATH_SUFFIXES jsoncpp
        PATHS ${Jsoncpp_PKGCONF_INCLUDE_DIRS} # /usr/include/jsoncpp/json
        )

# Finally the library itself
find_library(Jsoncpp_LIBRARY
        NAMES jsoncpp
        PATHS ${Jsoncpp_PKGCONF_LIBRARY_DIRS}
        #  PATH ./jsoncpp/
        )

set(Jsoncpp_PROCESS_INCLUDES Jsoncpp_INCLUDE_DIR)
set(Jsoncpp_PROCESS_LIBS Jsoncpp_LIBRARY)