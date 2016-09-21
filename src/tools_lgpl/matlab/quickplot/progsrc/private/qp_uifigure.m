function H=qp_uifigure(Name,closecom,tag,pos,callbackfcn)
%QP_UIFIGURE Create a new empty dialog figure.

%----- LGPL --------------------------------------------------------------------
%                                                                               
%   Copyright (C) 2011-2015 Stichting Deltares.                                     
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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_uifigure.m $
%   $Id: qp_uifigure.m 4612 2015-01-21 08:48:09Z mourits $

Inactive=qp_settings('UIInActiveColor');%[0.5 0.5 1];%
%
if nargin<5
    callbackfcn='d3d_qp';
end
%
uicontrolfont = qp_fontsettings('DefaultUicontrolFont');
%
%Force onscreen:
%MATLAB 6 and one screen: movegui(H,'onscreen')
MonPos = qp_getscreen(pos);
pos(1:2)=max(pos(1:2),MonPos(1:2));
pos(1:2)=min(pos(1:2),MonPos(1:2)+MonPos(3:4)-pos(3:4)-[0 70]);
% -[0 70] added such that toolbar, menu and window title will be on screen
% also in R13 compiled mode
%
if ~isempty(closecom)
    closecom=sprintf('%s %s',callbackfcn,closecom);
end
%
H = figure('Visible','off', ...
    'DefaultUicontrolBackgroundColor',Inactive, ...
    'DefaultUicontrolForegroundColor',qp_settings('UIForeGroundColor'), ...
    uicontrolfont, ...
    'Units','pixels', ...
    'Color',Inactive, ...
    'IntegerHandle','off', ...
    'MenuBar','none', ...
    'Name',Name, ...
    'CloseRequestFcn',closecom, ...
    'NumberTitle','off', ...
    'Resize','off', ...
    'Position',pos, ...
    'Handlevisibility','off', ...
    'Tag',tag);
setappdata(H,'WL_UserInterface',1)
if matlabversionnumber >= 7
    set(H,'WindowStyle','normal','DockControls','off')
end
