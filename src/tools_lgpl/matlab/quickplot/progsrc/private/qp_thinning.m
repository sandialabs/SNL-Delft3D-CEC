function [data,s] = qp_thinning(data,Ops)
%QP_THINNING Reduce number of values in QP data structure.
%   DATA = QP_THINNING(DATA,OPS) reduces the number of values in DATA based
%   on the options specified in OPS where OPS is a structure containing the
%   following fields:
%    * thinningmode     = 'none', 'uniform', or 'distance'
%    * thinningfactors  = sampling step if the thinningmode is equal to
%                         'uniform'
%    * thinningdistance = minimum sampling distance if the thinningmode is
%                         equal to 'distance'.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_thinning.m $
%   $Id: qp_thinning.m 5510 2015-10-20 14:36:00Z jagers $

multi_time = isfield(data,'Time') && length(data(1).Time)>1;
s = [];
if isfield(data,'TRI')
    switch lower(Ops.thinningmode)
        case 'none'
            data.X=data.XYZ(:,:,:,1);
            if size(data.XYZ,4)>1
                data.Y=data.XYZ(:,:,:,2);
            end
            if size(data.XYZ,4)>2
                data.Z=data.XYZ(:,:,:,3);
            end
        case 'uniform'
            Fld=logical(zeros(1,size(data.XYZ,2)));
            Fld(1:Ops.thinningfactors(1):end)=1;
            data.X=data.XYZ(:,Fld,:,1);
            data.Y=data.XYZ(:,Fld,:,2);
            if size(data.XYZ,4)>2
                data.Z=data.XYZ(:,Fld,:,3);
            end
            if isfield(data,'XComp')
                data.XComp=data.XComp(:,Fld,:);
            end
            if isfield(data,'YComp')
                data.YComp=data.YComp(:,Fld,:);
            end
            if isfield(data,'ZComp')
                data.ZComp=data.ZComp(:,Fld,:);
            end
            if isfield(data,'Val')
                data.Val  =data.Val(:,Fld,:);
            end
        case 'distance'
            I=all(~isnan(data.XYZ(1,:,1,:)),4);
            II=find(I);
            Ind=reducepoints(Ops.thinningdistance,data.XYZ(1,I,1,1),data.XYZ(1,I,1,2));
            I=logical(zeros(size(I))); I(II(Ind))=1;
            data.X=data.XYZ(:,I,:,1);
            data.Y=data.XYZ(:,I,:,2);
            if size(data.XYZ,4)>2
                data.Z=data.XYZ(:,I,:,3);
            end
            if multi_time
                sel = {':' I};
            else
                sel = {I};
            end
            for fcell = {'XComp','YComp','ZComp','Val'}
                f = fcell{1};
                if isfield(data,f)
                    data.(f)=data.(f)(sel{:});
                end
            end
        case 'regrid'
            rangemin = squeeze(min(data.XYZ,[],2));
            rangemax = squeeze(max(data.XYZ,[],2));
            drange = rangemax - rangemin;
            x = (0:floor(drange(1)/Ops.thinningdistance))*Ops.thinningdistance;
            x = x + (drange(1)-x(end))/2 + rangemin(1);
            y = (0:floor(drange(2)/Ops.thinningdistance))*Ops.thinningdistance;
            y = y + (drange(2)-y(end))/2 + rangemin(2);
            [X,Y]=meshgrid(x,y);
            if size(data.XYZ,4)>2
                data.Z=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.XYZ(1,:,1,3),X,Y);
            end
            if isfield(data,'XComp')
                data.XComp=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.XComp,X,Y);
            end
            if isfield(data,'YComp')
                data.YComp=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.YComp,X,Y);
            end
            if isfield(data,'ZComp')
                data.ZComp=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.ZComp,X,Y);
            end
            if isfield(data,'Val')
                data.Val=griddata(data.XYZ(1,:,1,1),data.XYZ(1,:,1,2),data.Val,X,Y);
            end
            data = rmfield(data,'XYZ');
            data.X = X;
            data.Y = Y;
    end
elseif isfield(Ops,'thinningmode')
    switch lower(Ops.thinningmode)
        case 'none'
        case 'uniform'
            if isfield(data,'Val')
                sz = size(data.Val);
            else
                sz = size(data.XComp);
            end
            sel = cell(size(sz));
            start = 1 + multi_time;
            if multi_time
                sel{1} = ':';
            end
            for i = start:length(sel)
                sel{i} = 1:Ops.thinningfactors(i-multi_time):sz(i);
                sz(i) = length(sel{i});
            end
            for fcell = {'X','Y','Z','XComp','YComp','ZComp','Val'}
                f = fcell{1};
                if isfield(data,f)
                    if multi_time
                        sz(1) = size(data.(f),1);
                    end
                    data.(f)=reshape(data.(f)(sel{:}),sz);
                end
            end
        case 'distance'
            if multi_time
                sel = {1,':'};
            else
                sel = {':'};
            end
            if isfield(data,'Y') && isfield(data,'Z')
                x = data.X(sel{:});
                y = data.Y(sel{:});
                z = data.Z(sel{:});
                I=~isnan(x) & ~isnan(y) & ~isnan(z);
                II=find(I);
                Ind=reducepoints(Ops.thinningdistance,x(I),y(I),z(I));
            elseif isfield(data,'Y')
                x = data.X(sel{:});
                y = data.Y(sel{:});
                I=~isnan(x) & ~isnan(y);
                II=find(I);
                Ind=reducepoints(Ops.thinningdistance,x(I),y(I));
            elseif isfield(data,'Z')
                x = data.X(sel{:});
                z = data.Z(sel{:});
                I=~isnan(x) & ~isnan(z);
                II=find(I);
                Ind=reducepoints(Ops.thinningdistance,x(I),z(I));
            else
                x = data.X(sel{:});
                I=~isnan(x);
                II=find(I);
                Ind=reducepoints(Ops.thinningdistance,x(I));
            end
            I=logical(zeros(size(I))); I(II(Ind))=1;
            if nargout>1 && ~multi_time && xor(size(data.X,1)>1,size(data.X,2)>1) && size(data.X,3)>1 && isfield(data,'Y')
                if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                    s=pathdistance(data.X(:,:,1),data.Y(:,:,1),'geographic');
                else
                    s=pathdistance(data.X(:,:,1),data.Y(:,:,1));
                end
                s=reshape(repmat(s,[1 1 size(data.X,3)]),size(data.X));
                s=s(I);
            end
            if multi_time
                sel{1} = ':';
            end
            sel{end} = I;
            for fcell = {'X','Y','Z','XComp','YComp','ZComp','Val'}
                f = fcell{1};
                if isfield(data,f)
                    data.(f)=data.(f)(sel{:});
                end
            end
            for fcell = {'dX_tangential','dY_tangential'}
                f = fcell{1};
                if isfield(data,f)
                    data = rmfield(data,f);
                end
            end
    end
end
