//---- GPL ---------------------------------------------------------------------
//
// Copyright (C)  Stichting Deltares, 2011-2020.
//
// This program is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation version 3.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
// $Id: el.h 65778 2020-01-14 14:07:42Z mourits $
// $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_gpl/vs/packages/vs/include/el.h $

#ifndef _EL_INCLUDED
#   define _EL_INCLUDED
    extern struct St_elm * EL_find_element_in_tree (
                                         struct St_elm *, const BText ) ;
    extern BVoid            EL_remove_element_branche (
                                         struct St_elm * ) ;
    extern  struct St_elm * EL_add_element_to_tree (
                                         struct St_elm *, const BText ) ;
#endif
