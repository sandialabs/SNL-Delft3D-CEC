/*
 * $Id: ncio.c 5406 2015-09-10 14:52:05Z mourits $
 */

#if defined(_CRAY)
#   include "ffio.c"
#else
#   include "posixio.c"
#endif
