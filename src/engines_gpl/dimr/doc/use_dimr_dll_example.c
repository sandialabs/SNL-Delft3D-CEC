// This is how Dimr dll should be used, written in pseudo C code

// Add binary directories to environmentparameter PATH
environmentParameter.set("PATH") = "...dimr_6779\win64\dimr\bin"     + ";" + environmentParameter.get("PATH");
environmentParameter.set("PATH") = "...dimr_6779\win64\flow1d2d\bin" + ";" + environmentParameter.get("PATH");
environmentParameter.set("PATH") = "...dimr_6779\win64\RTCTools\bin" + ";" + environmentParameter.get("PATH");

typedef int  (CDECLOPT *BMI_INITIALIZE)     (const char *);
typedef void (CDECLOPT *BMI_UPDATE)         (double);
typedef void (CDECLOPT *BMI_FINALIZE)       (void);
typedef void (CDECLOPT *BMI_GETSTARTTIME)   (double *);
typedef void (CDECLOPT *BMI_GETENDTIME)     (double *);
typedef void (CDECLOPT *BMI_GETTIMESTEP)    (double *);
typedef void (CDECLOPT *BMI_GETCURRENTTIME) (double *);
typedef void (CDECLOPT *BMI_GETVAR)         (const char *, void **);
typedef void (CDECLOPT *BMI_SETVAR)         (const char *, const void *);

    BMI_INITIALIZE     dllInit;
    BMI_UPDATE         dllUpdate;
    BMI_FINALIZE       dllFinal;
    BMI_GETSTARTTIME   dllGetStr;
    BMI_GETENDTIME     dllGetEnd;
    BMI_GETTIMESTEP    dllGetStp;
    BMI_GETCURRENTTIME dllGetCur;
    BMI_GETVAR         dllGetVar;
    BMI_SETVAR         dllSetVar;

dllHandle = LoadLibrary("...dimr_6779\win64\dimr\bin\dimr_dll.dll");
dllInit   = GETPROCADDRESS (dllHandle, "initialize");
dllUpdate = GETPROCADDRESS (dllHandle, "update");
dllFinal  = GETPROCADDRESS (dllHandle, "finalize");
dllGetStr = GETPROCADDRESS (dllHandle, "get_start_time");
dllGetEnd = GETPROCADDRESS (dllHandle, "get_end_time");
dllGetStp = GETPROCADDRESS (dllHandle, "get_time_step");
dllGetCur = GETPROCADDRESS (dllHandle, "get_current_time");
dllGetVar = GETPROCADDRESS (dllHandle, "get_var");
dllSetVar = GETPROCADDRESS (dllHandle, "set_var");



    double tStart;
    double tEnd;
    double tStep;
    double tCur;
    double * transfer = NULL;
    double value;

    (dllGetStr) (&tStart);
    (dllGetEnd) (&tEnd);
    (dllGetStp) (&tStep);
    (dllGetCur) (&tCur);
    do {
        (dllUpdate) (tStep);
        (dllGetCur) (&tCur);

    } while (tCur < tEnd);


// Example of getting/setting values (not needed to run this example "as is")
        (dllGetVar) ("water flow 1d/observations/HEESBN/water_level", (void**)(&transfer));
        // IMPORTANT: the value that "transfer" points to must be copied into another memory location ("value")
        // Inside dllSetVar, Dimr::send might be called to obtain the pointer to the variable inside the kernel that has to be set
        // This will reset the value that "transfer" points to into the current value.
        value = *transfer;
        (dllSetVar) ("real-time control/input_HEESBN_Water level (op)", (void*)(&value));

        (dllGetVar) ("real-time control/output_Maeslant_drempel_Crest width (s)", (void**)(&transfer));
        // IMPORTANT: the value that "transfer" points to must be copied into another memory location ("value")
        // Inside dllSetVar, Dimr::send might be called to obtain the pointer to the variable inside the kernel that has to be set
        // This will reset the value that "transfer" points to into the current value.
        value = *transfer;
        (dllSetVar) ("water flow 1d/weirs/Maeslant_drempel/structure_crest_width", (void*)(&value));
