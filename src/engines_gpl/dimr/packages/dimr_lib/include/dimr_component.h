#pragma once
#include "xmltree.h"
#include "bmi.h"
#include "log.h"
#include "clock.h"
#include <mpi.h>
#if HAVE_CONFIG_H
#   include <sys/time.h>
#else
#   include <windows.h>
#endif

// Define the exact api of the entry points in the dlls
#if HAVE_CONFIG_H
#define CDECLOPT
#else
#define CDECLOPT __cdecl
#endif

/* logger to be set from outside so we can log messages */
//typedef int  (CDECLOPT *BMI_SET_LOGGER)		(void(*)(int, char *));
typedef int  (CDECLOPT *BMI_SET_LOGGER)		(BMILogger);
typedef int  (CDECLOPT *BMI_DIMR_SET_LOGGER)(Log *);

typedef int  (CDECLOPT *BMI_INITIALIZE)     (const char *);
typedef int  (CDECLOPT *BMI_UPDATE)         (double);
typedef void (CDECLOPT *BMI_FINALIZE)       (void);
typedef void (CDECLOPT *BMI_GETSTARTTIME)   (double *);
typedef void (CDECLOPT *BMI_GETENDTIME)     (double *);
typedef void (CDECLOPT *BMI_GETTIMESTEP)    (double *);
typedef void (CDECLOPT *BMI_GETCURRENTTIME) (double *);
typedef void (CDECLOPT *BMI_GETATTRIBUTE)   (const char *, char *);
typedef void (CDECLOPT *BMI_GETVAR)         (const char *, void **);
typedef void (CDECLOPT *BMI_SETVAR)         (const char *, const void *);




// A component is an instance of D-FlowFM, RTC-Tools, WAQ, WAVE or Delft3D-FLOW(flow2d3d)
// Corresponds with a component block in config.xml
typedef struct dimr_component dimr_component;
struct dimr_component {
	const char    *    name;              // Component name: must be unique in the config.xml file (e.g. myNameFlow)
	char          *    library;           // Component library name, without extension/prefix
	int                type;              // COMP_TYPE_FM, COMP_TYPE_RTC or COMP_TYPE_WAVE
#if HAVE_CONFIG_H
	void          *    libHandle;         // (Linux) Handle to the loaded library for this component.
#else
	HINSTANCE          libHandle;         // (Windows) Handle to the loaded library for this component.
#endif
	char          *    inputFile;         // Component inputFile name
	char          *    workingDir;        // Component working directory
	int           *    processes;         // (Optional) list of processes ranks that this component needs to run in.
	int                numProcesses;      // Count of processes array.
	bool               onThisRank;        // Whether this component needs to run on current process rank.
	char          *    mpiCommVar;        // (Optional) Variable name for component's MPI communicator (must be accesible via BMI).
	MPI_Comm           mpiComm;           // An MPI communicator specific for this component (may run on less processes than master dimr).
	BMI_INITIALIZE     dllInitialize;     // entry point in dll
	BMI_UPDATE         dllUpdate;         // entry point in dll
	BMI_FINALIZE       dllFinalize;       // entry point in dll
	BMI_GETSTARTTIME   dllGetStartTime;   // entry point in dll
	BMI_GETENDTIME     dllGetEndTime;     // entry point in dll
	BMI_GETTIMESTEP    dllGetTimeStep;    // entry point in dll
	BMI_GETCURRENTTIME dllGetCurrentTime; // entry point in dll
	BMI_GETVAR         dllGetVar;         // entry point in dll
	BMI_SETVAR         dllSetVar;         // entry point in dll
	BMI_GETATTRIBUTE   dllGetAttribute;   // entry point in dll
	BMI_SET_LOGGER	   setLogger;   // entry point in dll
	int                result;            // return value when calling an entry point in dll
	keyValueLL      *  settings;          // list of settings
	keyValueLL      *  parameters;        // list of parameters
	int                dllSetKeyVals(keyValueLL * kv);   // pass parameters/settings to the component
	Clock::Timestamp  timerStart;
	Clock::Timestamp  timerSum;
};
