function data = qp_dimsqueeze(data,axestype,multiple,DimFlag,Props)
%QP_DIMSQUEEZE Average data for non-plotted data dimensions.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_dimsqueeze.m $
%   $Id: qp_dimsqueeze.m 4612 2015-01-21 08:48:09Z mourits $

T_=1; ST_=2; M_=3; N_=4; K_=5;

keepext = zeros(1,5);
switch axestype
    case 'X-Y'
        keepext([M_ N_]) = 1;
    case 'X-Z'
        %keepext([M_||N_ K_]) = 1;
end

di=1+~multiple(T_);
for i=[K_ N_ M_]
    if ~multiple(i) && DimFlag(i) && ~keepext(i)
       for d=1:length(data)
          geom='';
          if isfield(data,'Geom') && ~isempty(data(d).Geom)
             geom=data(d).Geom;
          elseif isfield(Props,'Geom')
             geom=Props.Geom;
          end
          if ~strcmp(geom,'POLYL') && ~strcmp(geom,'POLYG')
             if isfield(data,'X') && size(data(d).X,i-di)>1
                data(d).X=mean(data(d).X,i-di);
             end
             if isfield(data,'Y') && size(data(d).Y,i-di)>1
                data(d).Y=mean(data(d).Y,i-di);
             end
             if isfield(data,'Z') && size(data(d).Z,i-di)>1
                data(d).Z=mean(data(d).Z,i-di);
             end
          end
       end
    end
end
