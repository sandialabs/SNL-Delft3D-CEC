#ifndef DELFTONLINE_VERSION
#define DELFTONLINE_VERSION

#define delftonline_major "2"
#define delftonline_minor "00"
#define delftonline_revision "00"
#define delftonline_build "000000"

#define delftonline_company "Deltares"
#define delftonline_company_url  = "http://www.deltares.nl"
#define delftonline_program "DELFTONLINE"

/*=================================================== DO NOT MAKE CHANGES BELOW THIS LINE ===================================================================== */

static char delftonline_version [] = {delftonline_major"."delftonline_minor"."delftonline_revision"."delftonline_build};
static char delftonline_version_full [] = {"Deltares, "delftonline_program" Version "delftonline_major"."delftonline_minor"."delftonline_revision"."delftonline_build", "__DATE__", "__TIME__""};

char * getfullversionstring_delftonline(void);

#endif DELFTONLINE_VERSION
