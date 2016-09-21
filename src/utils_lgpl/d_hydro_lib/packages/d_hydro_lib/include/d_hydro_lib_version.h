#ifndef D_HYDRO_LIB_VERSION
#define D_HYDRO_LIB_VERSION

#define d_hydro_lib_major "1"
#define d_hydro_lib_minor "00"
#define d_hydro_lib_revision "00"
#define d_hydro_lib_build "000000"

#define d_hydro_lib_company "Deltares"
#define d_hydro_lib_company_url  = "http://www.deltares.nl"
#define d_hydro_lib_program "D_HYDRO_LIB"

/*=================================================== DO NOT MAKE CHANGES BELOW THIS LINE ===================================================================== */

static char d_hydro_lib_version [] = {d_hydro_lib_major"."d_hydro_lib_minor"."d_hydro_lib_revision"."d_hydro_lib_build};
static char d_hydro_lib_version_full [] = {"Deltares, "d_hydro_lib_program" Version "d_hydro_lib_major"."d_hydro_lib_minor"."d_hydro_lib_revision"."d_hydro_lib_build", "__DATE__", "__TIME__""};

extern char * getfullversionstring_d_hydro_lib(void);

#endif /* D_HYDRO_LIB_VERSION */

