//---- LGPL --------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2015.
//
// This library is free software; you can redistribute it and/or
// modify it under the terms of the GNU Lesser General Public
// License as published by the Free Software Foundation version 2.1.
//
// This library is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// Lesser General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public
// License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
// contact: delft3d.support@deltares.nl
// Stichting Deltares
// P.O. Box 177
// 2600 MH Delft, The Netherlands
//
// All indications and logos of, and references to, "Delft3D" and "Deltares"
// are registered trademarks of Stichting Deltares, and remain the property of
// Stichting Deltares. All rights reserved.
//
//------------------------------------------------------------------------------
// $Id: dio_shm_handle.h 4612 2015-01-21 08:48:09Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/utils_lgpl/delftio/packages/delftio_shm/include/dio_shm_handle.h $

//
//  dio_shm.h: DelftIO Shared Memory
//
//  include file DioShmClass
//
//  stef.hummel@deltares.nl
//
//  (c) Deltares, Jan 2000
//


#if (!defined(DIOSHM_HANDLE_H))
#define DIOSHM_HANDLE_H

#if (defined(WIN32))
#include <windows.h>
#endif


class DioShmHandle {

public:
#if (defined(WIN32))
    HANDLE  mmfHandle;		// Windows Handle (Memory Mapped File)
#else
    void * mmfHandle;		// Unix Handle (ESM)
#endif

    void * shmBlock;		// Handle to data (MapView of file)

    DioShmHandle(int iSize, char * name);
    ~DioShmHandle();

};


#endif
