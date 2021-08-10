function H=qp_toolbartoggle(hP,cmd,sep,tooltip)
%QP_TOOLBARTOGGLE Create a toolbar toggle button.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2020 Stichting Deltares.                                     
%                                                                               
%   This library is free software; you can redistribute it and/or                
%   modify it under the terms of the GNU Lesser General Public                   
%   License as published by the Free Software Foundation version 2.1.                         
%                                                                               
%   This library is distributed in the hope that it will be useful,              
%   but WITHOUT ANY WARRANTY; without even the implied warranty of               
%   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU            
%   Lesser General Public License for more details.                              
%                                                                               
%   You should have received a copy of the GNU Lesser General Public             
%   License along with this library; if not, see <http://www.gnu.org/licenses/>. 
%                                                                               
%   contact: delft3d.support@deltares.nl                                         
%   Stichting Deltares                                                           
%   P.O. Box 177                                                                 
%   2600 MH Delft, The Netherlands                                               
%                                                                               
%   All indications and logos of, and references to, "Delft3D" and "Deltares"    
%   are registered trademarks of Stichting Deltares, and remain the property of  
%   Stichting Deltares. All rights reserved.                                     
%                                                                               
%-------------------------------------------------------------------------------
%   http://www.deltaressystems.com
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_toolbartoggle.m $
%   $Id: qp_toolbartoggle.m 65778 2020-01-14 14:07:42Z mourits $

offon={'off','on'};
sep=offon{1+sep};
H = uitoggletool('Parent',hP, ...
    'CData',qp_icon(cmd,'nan'), ...
    'ClickedCallback',cat(2,'d3d_qp ',cmd), ...
    'Separator',sep, ...
    'Serializable','off', ...
    'Tag',cmd);
qp_tooltip(H,tooltip)
