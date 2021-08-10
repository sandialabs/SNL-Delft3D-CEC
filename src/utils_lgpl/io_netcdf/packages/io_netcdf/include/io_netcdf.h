//----- LGPL --------------------------------------------------------------------
//
//  Copyright (C)  Stichting Deltares, 2011-2020.
//
//  This library is free software; you can redistribute it and/or
//  modify it under the terms of the GNU Lesser General Public
//  License as published by the Free Software Foundation version 2.1.
//
//  This library is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
//  Lesser General Public License for more details.
//
//  You should have received a copy of the GNU Lesser General Public
//  License along with this library; if not, see <http://www.gnu.org/licenses/>.
//
//  contact: delft3d.support@deltares.nl
//  Stichting Deltares
//  P.O. Box 177
//  2600 MH Delft, The Netherlands
//
//  All indications and logos of, and references to, "Delft3D" and "Deltares"
//  are registered trademarks of Stichting Deltares, and remain the property of
//  Stichting Deltares. All rights reserved.
//
//-------------------------------------------------------------------------------

// $Id$
// $HeadURL$

//!
//! Tries to open a NetCDF file and initialize based on its specified conventions.
//!
int ionc_open(char* c_path, int* mode, int* ioncid, int* iconvtype, double* convversion);


//!
//! Tries to close an open io_netcdf data set.
//!
int ionc_close(int* ioncid);

//!
//! Gets the number of mesh from a data set.
//!
int ionc_get_mesh_count(int* ioncid, int* nmesh);

//!
//! Gets the number of nodes in a single mesh from a data set.
//!
int ionc_get_node_count(int* ioncid, int* meshid, int* nnode);

//!
//! Gets the number of edges in a single mesh from a data set.
//!
int ionc_get_edge_count(int* ioncid, int* meshid, int* nedge);

//!
//! Gets the number of faces in a single mesh from a data set.
//!
int ionc_get_face_count(int* ioncid, int* meshid, int* nface);

//!
//! Gets the maximum number of nodes for any face in a single mesh from a data set.
//!
int ionc_get_max_face_nodes(int* ioncid, int* meshid, int* nmaxfacenodes);

//!
//! Gets the x,y coordinates for all nodes in a single mesh from a data set.
//!
int ionc_get_node_coordinates(int* ioncid, int* meshid, void** c_xptr, void** c_yptr, int* nnode);

//!
//! Gets the edge-node connectivity table for all edges in the specified mesh.
//!
int ionc_get_edge_nodes(int* ioncid, int* meshid, void** c_edge_nodes_ptr, int *nedge);

//!
//! Gets the face-node connectvit table for all faces in the specified mesh.
//! The output face_nodes array is supposed to be of exact correct size already.
//!
int ionc_get_face_nodes(int* ioncid, int* meshid, void** c_face_nodes_ptr, int* nface, int* nmaxfacenodes);
