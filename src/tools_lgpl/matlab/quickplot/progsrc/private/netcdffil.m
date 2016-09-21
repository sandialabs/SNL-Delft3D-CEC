function varargout=netcdffil(FI,domain,field,cmd,varargin)
%NETCDFFIL QP support for netCDF files.
%   Domains                 = XXXFIL(FI,[],'domains')
%   DataProps               = XXXFIL(FI,Domain)
%   Size                    = XXXFIL(FI,Domain,DataFld,'size')
%   Times                   = XXXFIL(FI,Domain,DataFld,'times',T)
%   StNames                 = XXXFIL(FI,Domain,DataFld,'stations')
%   SubFields               = XXXFIL(FI,Domain,DataFld,'subfields')
%   [TZshift   ,TZstr  ]    = XXXFIL(FI,Domain,DataFld,'timezone')
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'data',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'celldata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'griddata',subf,t,station,m,n,k)
%   [Data      ,NewFI]      = XXXFIL(FI,Domain,DataFld,'gridcelldata',subf,t,station,m,n,k)
%                             XXXFIL(FI,[],'options',OptionsFigure,'initialize')
%   [NewFI     ,cmdargs]    = XXXFIL(FI,[],'options',OptionsFigure,OptionsCommand, ...)
%
%   The DataFld can only be either an element of the DataProps structure.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/netcdffil.m $
%   $Id: netcdffil.m 5632 2015-12-09 08:50:03Z jagers $

%========================= GENERAL CODE =======================================
T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
    error('Not enough input arguments')
end

OrigFI = FI;
if FI.NumDomains>1
    if isempty(domain)
        % continue
    elseif domain>FI.NumDomains || domain==0
        % continue
    else
        FI = FI.Partitions{domain};
        domain = 1;
    end
end

if nargin==2
    varargout={infile(FI,domain)};
    return
elseif ischar(field)
    switch field
        case 'options'
            [varargout{1:2}]=options(FI,cmd,varargin{:});
        case 'domains'
            varargout={domains(FI)};
        case 'dimensions'
            varargout={dimensions(FI)};
        case 'locations'
            varargout={locations(FI)};
        case 'quantities'
            varargout={quantities(FI)};
        case 'getparams'
            varargout={[]};
        case 'data'
            [varargout{1:2}]=getdata(FI,cmd,varargin{:});
    end
    return
else
    Props=field;
end

cmd=lower(cmd);
switch cmd
    case 'size'
        varargout={getsize(FI,Props)};
        return
    case 'times'
        varargout={readtim(FI,Props,varargin{:})};
        return
    case 'timezone'
        [varargout{1:2}]=gettimezone(FI,domain,Props);
        return
    case 'stations'
        varargout={readsts(FI,Props,0)};
        return
    case 'subfields'
        varargout={getsubfields(FI,Props,varargin{:})};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

if FI.NumDomains>1
    for i = 1:FI.NumDomains
        Data2 = netcdffil(FI,i,field,cmd,varargin{:});
        if i==1
            Data = Data2;
        else
            flds = fieldnames(Data2);
            for j = 1:length(flds)
                Data(i).(flds{j}) = Data2.(flds{j});
            end
        end
    end
    varargout = {Data OrigFI};
    return
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 0 0};
fidx=find(DimFlag);

[subf,rec]=getsubfields(FI,Props);
if isempty(subf)
    % initialize and read indices ...
    Props.SubFld=rec;
    idx(fidx(1:length(varargin)))=varargin;
else
    % initialize and read indices ...
    rec.Val = rec.Val(varargin{1},:);
    Props.SubFld=rec;
    idx(fidx(1:(length(varargin)-1)))=varargin(2:end);
end

% select appropriate dimensions ...
sz=getsize(FI,Props);
for d_ = 1:length(DimFlag)
    if DimFlag(d_)
        %
        % Rule: if time not specified, use last time
        %
        if d_==T_ && isempty(idx{T_})
            idx{T_}=sz(T_);
        end
        %
        % Rule: if specified 0, the use all
        %
        if isequal(idx{d_},0)
            idx{d_}=1:sz(d_);
        end
    end
end

% expand SubFld into higher order dimensions
if ~isempty(Props.SubFld)
    Props.DimName = cat(2,Props.DimName,Props.SubFld.Fld);
    for i=1:length(Props.SubFld.Fld)
        idx{end+1} = Props.SubFld.Val(i);
    end
end
% read data ...
ivar = get_varid(Props);
Info=FI.Dataset(ivar+1);
%
if isfield(Info.Attribute,'Name')
    Attribs = {Info.Attribute.Name};
else
    Attribs = {};
end
%
removeTime   = 0;
activeloaded = 0;
if DataRead && Props.NVal>0
    if iscell(Props.varid)
        switch Props.varid{1}
            case 'stream_function'
                edge_idx = idx;
                edge_idx{3} = 1:FI.Dimension(Info.TSMNK(3)+1).Length;
                [Discharge, status] = qp_netcdf_get(FI,ivar,Props.DimName,edge_idx);
                %
                meshInfo    = FI.Dataset(Info.Mesh{2});
                meshAttribs = {meshInfo.Attribute.Name};
                connect     = strmatch('edge_node_connectivity',meshAttribs,'exact');
                [EdgeConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
                EdgeConnect(EdgeConnect<0) = NaN;
                %
                % Compute stream function psi (u = dpsi/dy, v = -dpsi/dx)
                Psi = NaN(sz(3),1);
                Psi(1) = 0;
                found = true;
                while found
                    found = false;
                    for i = 1:size(EdgeConnect,1)
                        if ~isnan(Psi(EdgeConnect(i,1))) && isnan(Psi(EdgeConnect(i,2)))
                            Psi(EdgeConnect(i,2)) = Psi(EdgeConnect(i,1)) + Discharge(i);
                            found = true;
                        elseif isnan(Psi(EdgeConnect(i,1))) && ~isnan(Psi(EdgeConnect(i,2)))
                            Psi(EdgeConnect(i,1)) = Psi(EdgeConnect(i,2)) - Discharge(i);
                            found = true;
                        end
                    end
                end
                Psi = Psi - min(Psi);
                %
                Ans.Val = Psi(idx{3});
            otherwise
                error('Special case "%s" not yet implemented.',Props.varid{1})
        end
    else
        for ii=1:length(Props.varid)
            if ~isempty(Info.CharDim)
                cdim = Info.Dimension(Info.CharDim==Info.Dimid);
                cidx = 1:FI.Dimension(Info.CharDim+1).Length;
                [data, status] = qp_netcdf_get(FI,Props.varid(ii),[Props.DimName cdim],[idx {cidx}]);
                data = num2cell(data,ndims(data));
            else
                [data, status] = qp_netcdf_get(FI,Props.varid(ii),Props.DimName,idx);
            end
            szData = size(data);
            %
            if Props.DimFlag(T_) && length(idx{T_})==1
                szV = [size(data) 1];
                data = reshape(data,szV(2:end));
                removeTime = 1;
            end
            %
            %positive = strmatch('positive',Attribs,'exact');
            %if ~isempty(positive)
            %   if isequal(lower(Info.Attribute(positive).Value),'down')
            %      data = -data;
            %   end
            %end
            %
            if ii==1
                if length(Props.varid)==1
                    Ans.Val = data;
                else
                    Ans.XComp = data;
                end
            elseif ii==2
                Ans.YComp = data;
            else
                Ans.ZComp = data;
            end
        end
    end
    %
    switch Props.VectorDef
        case 4
            Ans.Angle     = Ans.YComp*pi/180;
            Ans.Magnitude = Ans.XComp;
            Ans.XComp = Ans.Magnitude.*sin(Ans.Angle);
            Ans.YComp = Ans.Magnitude.*cos(Ans.Angle);
            Ans = rmfield(Ans,'Angle');
            Ans = rmfield(Ans,'Magnitude');
        case 5
            Ans.NormalComp = Ans.XComp;
            Ans.TangentialComp = Ans.YComp;
            % rotation at end of function
        otherwise
            % no rotation
    end
    %
    hdim = 1-cellfun('isempty',Props.DimName);
    hdim(M_) = hdim(M_)*2;
    hdim(N_) = hdim(N_)*2;
    hdim(hdim==0)=[];
    hdim = hdim==2;
else
    szData = cellfun('length',idx(sz>0));
    hdim   = logical([0 0 1 1 0]);
    hdim   = hdim(sz>0);
    Ans    = [];
end
%
% Read coordinates
%
if Props.hasCoords
    if Props.ClosedPoly && DataInCell
        coordname={'XBounds','YBounds'};
        isbounds = 1;
        %
        vdim = Info.('XBounds');
        CoordInfo = FI.Dataset(vdim);
        specificCoordDims = setdiff(CoordInfo.Dimension,Props.DimName(~cellfun('isempty',Props.DimName)));
        npolpntDim = specificCoordDims{1};
        npolpntDimid = CoordInfo.Dimid(strcmp(CoordInfo.Dimension,npolpntDim));
        npolpnt = FI.Dimension(npolpntDimid+1).Length+2; % add one to close polygon and one for NaN
    else
        coordname={'X','Y'};
        isbounds = 0;
        npolpnt = NaN;
    end
else
    coordname={};
    isbounds = 0;
    npolpnt = NaN;
end

if ~isnan(npolpnt)
    for f = {'Val','XComp','YComp','Angle','Magnitude','NormalComp','TangentialComp'}
        fc = f{1};
        if isfield(Ans,fc)
            Ans.(fc) = repmat(Ans.(fc)(:)',npolpnt,1);
            Ans.(fc) = Ans.(fc)(:);
        end
    end
end

if XYRead
    npolpnt = 0;
    if strncmp(Props.Geom,'UGRID',5)
        %ugrid
        mesh_settings = Info.Mesh;
        meshInfo      = FI.Dataset(mesh_settings{2});
        %
        for c = 'XY'
            CoordInfo2 = FI.Dataset(meshInfo.(c));
            [Ans.(c), status] = qp_netcdf_get(FI,CoordInfo2);
            %
            if ~isempty(CoordInfo2.Attribute)
                Attribs = {CoordInfo2.Attribute.Name};
                j = strmatch('units',Attribs,'exact');
                if ~isempty(j)
                    unit = CoordInfo2.Attribute(j).Value;
                    units = {'degrees_east','degree_east','degreesE','degreeE', ...
                        'degrees_north','degree_north','degreesN','degreeN'};
                    if ismember(unit,units)
                        unit = 'deg';
                    end
                    Ans.([c 'Units']) = unit;
                end
            end
        end
        %
        meshAttribs = {meshInfo.Attribute.Name};
        connect = strmatch('face_node_connectivity',meshAttribs,'exact');
        if ~isempty(connect)
            iconnect = strmatch(meshInfo.Attribute(connect).Value,{FI.Dataset.Name},'exact');
            if isempty(iconnect)
                ui_message('error','Face_node_connectivity not found!')
            else
                [Ans.FaceNodeConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
                istart = strmatch('start_index',{FI.Dataset(iconnect).Attribute.Name},'exact');
                if isempty(istart)
                    maxNode = max(Ans.FaceNodeConnect(:));
                    minNode = min(Ans.FaceNodeConnect(Ans.FaceNodeConnect>=0));
                    if minNode==1 && maxNode==length(Ans.X)
                        start = 1;
                        ui_message('warning','No start_index found on %s.\nDefault value is 0, but data suggests otherwise.\nUsing start_index=1.',meshInfo.Attribute(connect).Value)
                    else
                        start = 0;
                    end
                else
                    start = FI.Dataset(iconnect).Attribute(istart).Value;
                end
                Ans.FaceNodeConnect = Ans.FaceNodeConnect - start + 1;
                Ans.FaceNodeConnect(Ans.FaceNodeConnect<1) = NaN;
            end
        end
        %
        Ans.ValLocation = Props.Geom(7:end);
        connect = strmatch('edge_node_connectivity',meshAttribs,'exact');
        if strcmp(Ans.ValLocation,'EDGE') || ~isfield(Ans,'FaceNodeConnect') || (~DataRead && ~isempty(connect))
            % "~DataRead" is a hack to load EdgeNodeConnect if available for use in GridView
            iconnect = strmatch(meshInfo.Attribute(connect).Value,{FI.Dataset.Name},'exact');
            if isempty(iconnect)
                error('Edge_node_connectivity not found!')
            end
            [Ans.EdgeNodeConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
            Ans.EdgeNodeConnect(Ans.EdgeNodeConnect<0) = NaN;
        end
        if isfield(Ans,'EdgeNodeConnect')
            istart = strmatch('start_index',{FI.Dataset(iconnect).Attribute.Name},'exact');
            if isempty(istart)
                maxNode = max(Ans.EdgeNodeConnect(:));
                minNode = min(Ans.EdgeNodeConnect(Ans.EdgeNodeConnect>=0));
                if minNode==1 && maxNode==length(Ans.X)
                    start = 1;
                    ui_message('warning','No start_index found on %s.\nDefault value is 0, but data suggests otherwise.\nUsing start_index=1.',meshInfo.Attribute(connect).Value)
                else
                    start = 0;
                end
            else
                start = FI.Dataset(iconnect).Attribute(istart).Value;
            end
            Ans.EdgeNodeConnect = Ans.EdgeNodeConnect - start + 1;
            Ans.EdgeNodeConnect(Ans.EdgeNodeConnect<1) = NaN;
        end
        %
        switch Ans.ValLocation
            case 'NODE'
                Ans.X = Ans.X(idx{M_});
                Ans.Y = Ans.Y(idx{M_});
                if isfield(Ans,'FaceNodeConnect')
                    Cnct = all(ismember(Ans.FaceNodeConnect,idx{M_}) | isnan(Ans.FaceNodeConnect),2);
                    renum(idx{M_}) = 1:length(idx{M_});
                    Ans.FaceNodeConnect = Ans.FaceNodeConnect(Cnct,:);
                    Ans.FaceNodeConnect(~isnan(Ans.FaceNodeConnect)) = renum(Ans.FaceNodeConnect(~isnan(Ans.FaceNodeConnect)));
                end
                %if isfield(Ans,'EdgeNodeConnect')
                %    Cnct = all(ismember(Ans.EdgeNodeConnect,idx{M_}),2);
                %    renum(idx{M_}) = 1:length(idx{M_});
                %    Ans.EdgeNodeConnect = Ans.EdgeNodeConnect(Cnct,:);
                %    Ans.EdgeNodeConnect(~isnan(Ans.EdgeNodeConnect)) = renum(Ans.EdgeNodeConnect(~isnan(Ans.EdgeNodeConnect)));
                %end
            case 'EDGE'
                Ans.EdgeNodeConnect = Ans.EdgeNodeConnect(idx{M_},:);
            case 'FACE'
                Ans.FaceNodeConnect = Ans.FaceNodeConnect(idx{M_},:);
        end
        %
        %[Ans.XFace, status] = qp_netcdf_get(FI,'mesh2d_face_x');
        %[Ans.YFace, status] = qp_netcdf_get(FI,'mesh2d_face_y');
    else
        firstbound = 1;
        for iCoord = 1:length(coordname)
            vdim = getfield(Info,coordname{iCoord});
            if isempty(vdim)
                continue
            end
            CoordInfo = FI.Dataset(vdim);
            if isempty(CoordInfo.Attribute)
                CoordAttribs = {};
            else
                CoordAttribs = {CoordInfo.Attribute.Name};
            end
            %
            dims = Props.DimName;
            dimvals = idx;
            if isbounds
                % when plotting the coordinate bounds variable, the
                % variable itself already has a dimension like "Two" or
                % "nMaxmesh2_face_nodes" which conflicts with the dimension
                % of the bounds to be added later. Since these coordinates
                % are non-spatial anyway, remove them.
                dims(6:end) = [];
                dimvals(6:end) = [];
                %
                vdim2 = getfield(Info,coordname{iCoord}(1));
                if isempty(vdim2)
                    CoordInfo2 = [];
                else
                    CoordInfo2 = FI.Dataset(vdim2);
                end
                %
                dims(end+1) = setdiff(CoordInfo.Dimension,dims(~cellfun('isempty',dims)));
                id = strmatch(dims{end},{FI.Dimension.Name},'exact');
                dimvals{end+1} = 1:FI.Dimension(id).Length;
                coordname{iCoord}=coordname{iCoord}(1);
            else
                CoordInfo2 = CoordInfo;
            end
            [Coord, status] = qp_netcdf_get(FI,CoordInfo,dims,dimvals);
            Coord = expand_hdim(Coord,szData,hdim);
            %
            %--------------------------------------------------------------------
            % Delft3D special
            %
            active_id = strmatch('active',CoordAttribs,'exact');
            if ~isempty(active_id)
                activevar = CoordInfo.Attribute(active_id).Value;
                [Active, status] = qp_netcdf_get(FI,activevar,Props.DimName,idx);
                Coord(Active~=1,:)=NaN; % Active~=1 excludes boundary points, Active==0 includes boundary points
            end
            %--------------------------------------------------------------------
            %
            %--------------------------------------------------------------------
            % ROMS special
            %
            if ~isnan(CoordInfo.TSMNK(3))
                xcoord = FI.Dimension(CoordInfo.TSMNK(3)+1).Name;
                underscore = strfind(xcoord,'_');
                if activeloaded==0
                    activeloaded = -1;
                    if ~isempty(underscore)
                        location = xcoord(underscore+1:end);
                        maskvar = ['mask_' location];
                        actvarid = strmatch(maskvar,{FI.Dataset.Name},'exact');
                        ActiveInfo = FI.Dataset(actvarid);
                        if ~isempty(ActiveInfo)
                            [Active, status] = qp_netcdf_get(FI,ActiveInfo,Props.DimName,idx);
                            activeloaded = 1;
                        end
                    end
                end
            end
            if activeloaded>0 && isequal(ActiveInfo.Dimension,CoordInfo.Dimension)
                Coord(~Active)=NaN;
            end
            %--------------------------------------------------------------------
            %
            if removeTime
                szCoord = size(Coord);
                Coord = reshape(Coord,[szCoord(2:end) 1]);
            end
            %
            szCoord = size(Coord);
            if length(Coord)>2
                Coord = reshape(Coord,[prod(szCoord(1:end-1)) szCoord(end)]);
            end
            if isbounds
                % only polygons supported
                if size(Coord,2)==2
                    % This could be either
                    %   1) 1D edge or
                    %   2) a 2D grid with 1D coordinates.
                    % In the latter case the variable will have both X and Y
                    % dimensions and the dimension names will match the coordinate
                    % variable names.
                    if ~isempty(Info.X) && ~isempty(Info.Y) && isequal(CoordInfo2.Dimension{1},CoordInfo2.Name)
                        % This is the 2D grid case:
                        % Expand the 2-valued bounds to rectangular bounds, but
                        % first of all make sure that we have [min max] order for
                        % the coordinates.
                        Coord = sort(Coord,2);
                        Coord(:,3:6) = NaN;
                        if firstbound % X
                            Coord(:,1:5) = Coord(:,[1 2 2 1 1]);
                        else % Y
                            Coord(:,1:5) = Coord(:,[2 2 1 1 2]);
                        end
                    else
                        % This is the 1D edge case:
                        % Just add a NaN column to separate the edges.
                        Coord(:,3) = NaN;
                    end
                else
                    % add two NaNs
                    % the first one will be used to close the bounds polygons
                    % the second one will be used to separate the polygons
                    Coord(:,end+1:end+2) = NaN;
                    for j=1:size(Coord,1)
                        for k=1:size(Coord,2)
                            if isnan(Coord(j,k))
                                Coord(j,k) = Coord(j,1);
                                break
                            end
                        end
                    end
                end
                firstbound = 0;
                Coord = Coord';
                Coord = Coord(:);
            end
            %
            Ans.(coordname{iCoord}) = Coord;
            if ~isempty(CoordInfo2.Attribute)
                Attribs = {CoordInfo2.Attribute.Name};
                j = strmatch('units',Attribs,'exact');
                if ~isempty(j)
                    unit = CoordInfo2.Attribute(j).Value;
                    units = {'degrees_east','degree_east','degreesE','degreeE', ...
                        'degrees_north','degree_north','degreesN','degreeN'};
                    if ismember(unit,units)
                        unit = 'deg';
                    end
                    Ans.([coordname{iCoord} 'Units']) = unit;
                end
            end
        end
    end
    %
    if ~isempty(Info.Z) && Props.hasCoords
        vdimid = Info.Z;
        CoordInfo = FI.Dataset(vdimid);
        %
        Attribs = {CoordInfo.Attribute.Name};
        j=strmatch('formula_terms',Attribs,'exact');
        formula = '';
        if ~isempty(j)
            formula = CoordInfo.Attribute(j).Value;
            FormulaTerms = multiline(formula,' ','cell');
            i = 1;
            while i<=length(FormulaTerms)
                if isempty(FormulaTerms{i})
                    FormulaTerms(i,:)=[];
                else
                    i=i+1;
                end
            end
            FormulaTerms = reshape(FormulaTerms,2,length(FormulaTerms)/2)';
        end
        %
        j=strmatch('positive',Attribs,'exact');
        if ~isempty(j)
            switch lower(CoordInfo.Attribute(j).Value)
                case 'up'
                    signup = 1;
                case 'down'
                    signup = -1;
                otherwise
                    ui_message('warning','Unknown value for attribute ''positive'': %s',CoordInfo.Attribute(j).Value)
                    signup = 1;
            end
        else
            signup = 1;
        end
        j=strmatch('standard_name',Attribs,'exact');
        if ~isempty(j)
            standard_name = CoordInfo.Attribute(j).Value;
            switch standard_name
                case 'atmosphere_sigma_coordinate'
                    [sigma  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                    [ps     , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                    [ptop   , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                    Z = zeros(szData);
                    for t=1:size(Z,1)
                        for k=1:length(sigma)
                            Z(t,:,:,k) = ptop+sigma(k)*(ps(t,:,:)-ptop);
                        end
                    end
                case 'atmosphere_hybrid_sigma_pressure_coordinate'
                    if isequal(FormulaTerms{1,1},'a:')
                        [a      , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                        [b      , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                        [ps     , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                        [p0     , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx);
                        Z = zeros(szData);
                        for t=1:size(Z,1)
                            for k=1:length(a)
                                Z(t,:,:,k) = a(k)*p0+b(k)*ps(t,:,:);
                            end
                        end
                    else
                        [ap     , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                        [b      , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                        [ps     , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                        Z = zeros(szData);
                        for t=1:size(Z,1)
                            for k=1:length(ap)
                                Z(t,:,:,k) = ap(k)+b(k)*ps(t,:,:);
                            end
                        end
                    end
                case 'atmosphere_hybrid_height_coordinate'
                    [tau     , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                    [eta     , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                    [ztop    , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                    [zsurface, status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx);
                    Z = zeros(szData);
                    for t=1:size(Z,1)
                        for k=1:length(tau)
                            Z(t,:,:,k) = tau(k)*zsurface(t,:,:)+eta(k)*ztop;
                        end
                    end
                case 'atmosphere_sleve_coordinate'
                    [a       , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                    [b1      , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                    [b2      , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                    [ztop    , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx);
                    [zsurf1  , status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx);
                    [zsurf2  , status] = qp_netcdf_get(FI,FormulaTerms{6,2},Props.DimName,idx);
                    Z = zeros(szData);
                    for t=1:size(Z,1)
                        for k=1:length(a)
                            Z(t,:,:,k) = a(k)*ztop+b1(k)*zsurf1(t,:,:)+b2(k)*zsurf2(t,:,:);
                        end
                    end
                case 'ocean_sigma_coordinate'
                    [sigma  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                    [eta    , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                    [depth  , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                    Z = zeros(szData);
                    for t=1:size(Z,1)
                        for k=1:length(sigma)
                            Z(t,:,:,k) = eta(t,:,:)+(depth+eta(t,:,:))*sigma(k);
                        end
                    end
                case 'ocean_s_coordinate'
                    [s      , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                    [eta    , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                    [depth  , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                    [a      , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx);
                    [b      , status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx);
                    [depth_c, status] = qp_netcdf_get(FI,FormulaTerms{6,2},Props.DimName,idx);
                    C = (1-b)*sinh(a*s)/sinh(a) + b*(tanh(a*(s+0.5))/(2*tanh(0.5*a))-0.5);
                    Z = zeros(szData);
                    for t=1:size(Z,1)
                        for k=1:length(s)
                            Z(t,:,:,k) = eta(t,:,:)*(1+s(k))+depth_c*s(k)+(depth-depth_c)*C(k);
                        end
                    end
                case 'ocean_sigma_z_coordinate'
                    [sigma  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                    [eta    , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                    [depth  , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                    [depth_c, status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx);
                    [nsigma , status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx);
                    [zlev   , status] = qp_netcdf_get(FI,FormulaTerms{6,2},Props.DimName,idx);
                    K=idx{K_};
                    Z = zeros(szData);
                    for t=1:size(Z,1)
                        for k=1:length(s)
                            if K(k)<=nsigma
                                Z(t,:,:,k) = eta(t,:,:) + sigma(k)*(min(depth_c,depth)+eta(t,:,:));
                            else
                                Z(t,:,:,k) = zlev(k);
                            end
                        end
                    end
                case 'ocean_double_sigma_coordinate'
                    [sigma  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx);
                    [depth  , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx);
                    [z1     , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx);
                    [z2     , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx);
                    [a      , status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx);
                    [href   , status] = qp_netcdf_get(FI,FormulaTerms{6,2},Props.DimName,idx);
                    [k_c    , status] = qp_netcdf_get(FI,FormulaTerms{7,2},Props.DimName,idx);
                    K=idx{K_};
                    Z = zeros(szData);
                    for t=1:size(Z,1)
                        for k=1:length(s)
                            f = 0.5*(z1+z2) + 0.5*(z1-z2)*tanh(2*a/(z1-z2)*(depth-href));
                            if K(k)<=k_c
                                Z(t,:,:,k) = sigma(k)*f;
                            else
                                Z(t,:,:,k) = f + (sigma(k)-1)*(depth-f);
                            end
                        end
                    end
                otherwise
                    if ~isempty(formula)
                        ui_message('warning','Formula for %s not implemented',standard_name)
                    end
                    [Z, status] = qp_netcdf_get(FI,CoordInfo,Props.DimName,idx);
                    if signup<0
                        Z=-Z;
                    end
                    Z = expand_hdim(Z,szData,hdim);
            end
        else
            [Z, status] = qp_netcdf_get(FI,CoordInfo,Props.DimName,idx);
            if signup<0
                Z=-Z;
            end
            Z = expand_hdim(Z,szData,hdim);
        end
        %
        j = strmatch('units',Attribs,'exact');
        if ~isempty(j)
            Ans.ZUnits = CoordInfo.Attribute(j).Value;
        end
        %--------------------------------------------------------------------
        %
        if removeTime
            szCoord = size(Z);
            Z = reshape(Z,[szCoord(2:end) 1]);
        end
        %--------------------------------------------------------------------
        if npolpnt>0
            Z = repmat(Z(:)',npolpnt,1);
            Z = Z(:);
        end
        Ans.Z = Z;
        %
        if isfield(Ans,'X')
            szZ = size(Z);
            szX = ones(size(szZ));
            szX(1:ndims(Ans.X)) = size(Ans.X);
            if all(szZ>=szX)
                rep = szZ./szX;
                Ans.X = repmat(Ans.X,rep);
                Ans.Y = repmat(Ans.Y,rep);
            elseif all(szX>=szZ)
                rep = szX./szZ;
                Ans.Z = repmat(Ans.Z,rep);
            end
        end
    end
    %
    if ~Props.hasCoords
        %
        % Define a simple regular grid
        %
        Ans.X = repmat(idx{M_}(:),1,length(idx{N_}));
        Ans.Y = repmat(idx{N_}(:)',length(idx{M_}),1);
        if Props.DimFlag(T_) && length(idx{T_})>1
            Ans.X = reshape(Ans.X,[1 size(Ans.X)]);
            Ans.Y = reshape(Ans.Y,[1 size(Ans.Y)]);
        end
        if Props.DimFlag(K_)
            Ans.Z = idx{K_};
        end
    end
end

if isfield(Ans,'NormalComp')
    if Props.MNK==1
        dx = diff(Ans.X(Ans.EdgeNodeConnect),1,2);
        dy = diff(Ans.Y(Ans.EdgeNodeConnect),1,2);
        ln = sqrt(dx.^2+dy.^2); % should never be 0, so no need to protect
        dx = dx./ln;
        dy = dy./ln;
        szTC = size(Ans.TangentialComp);
        Ans.XComp = zeros(szTC);
        Ans.YComp = zeros(szTC);
        if Props.DimFlag(T_) && length(idx{T_})>1
            id = {0,':',0};
            ND = prod(szTC(3:end));
            dx = dx';
            dy = dy';
        else
            id = {':',0};
            ND = prod(szTC(2:end));
        end
        for t = 1:max(1,length(idx{T_}))
            if Props.DimFlag(T_) && length(idx{T_})>1
                id{1} = t;
            end
            for nd = 1:ND
                id{end} = nd;
                Ans.XComp(id{:}) = Ans.TangentialComp(id{:}).*dx + Ans.NormalComp(id{:}).*dy;
                Ans.YComp(id{:}) = Ans.TangentialComp(id{:}).*dy - Ans.NormalComp(id{:}).*dx;
            end
        end
        Ans = rmfield(Ans,'NormalComp');
        Ans = rmfield(Ans,'TangentialComp');
    else
        Ans = rmfield(Ans,'XComp');
        Ans = rmfield(Ans,'YComp');
    end
end

% read time ...
T=[];
if Props.DimFlag(T_)
    T=readtim(FI,Props,idx{T_});
end
Ans.Time=T;

% Work around stupid DeltaShell files that write lat/lon, but actually store metric coordinates.
ds = strcmp('FileVersion_DeltaShell',{FI.Attribute.Name});
if any(ds) && isfield(Ans,'XUnits') && strcmp(Ans.XUnits,'deg')
    if min(Ans.X(:))<-360 || max(Ans.X(:))>360
        Ans.XUnits = 'm';
        Ans.YUnits = 'm';
    end
end

varargout={Ans OrigFI};
% -----------------------------------------------------------------------------

% -------------------------------------------------------------------------
function [TZshift,TZstr]=gettimezone(FI,domain,Props)
TZstr = '';
timevar = FI.Dataset(get_varid(Props)+1).Time;
if isempty(timevar)
    TZshift = NaN;
else
    TZshift = FI.Dataset(timevar).Info.TZshift;
end
% -------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                   'Units' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'SubFld' 'MNK' 'varid'  'DimName' 'hasCoords' 'VectorDef' 'ClosedPoly' 'UseGrid'};
DataProps={'dummy field'            ''      ''     ''      [0 0 0 0 0]  0           0      []       0     []          {}          0         0          0          0};
Out=cell2struct(DataProps,PropNames,2);
%Out.MName='M';
%Out.NName='N';
%Out.KName='K';
% -----------------------------------------------------------------------------
ndims = length(FI.Dimension);
nvars = length(FI.Dataset);
ngatts = length(FI.Attribute);
%
Dummy = Out;
Dummy.Name='-------';
%
% Following lines are a hack to detect vector quantities for XBeach netcdf files
% NOTE: also [min(x-component) min(y-component)] is incorrectly detected as vector quantity
%
% for i=1:length(FI.Dataset)
%     for j=1:length(FI.Dataset(i).Attribute)
%         if strcmp(FI.Dataset(i).Attribute(j).Name,'long_name')
%             nm=FI.Dataset(i).Attribute(j).Value;
%             if strfind(nm,'x-')
%                 nm=strrep(nm,'x-','');
%                 nm=[nm ', x-component'];
%             elseif strfind(nm,'y-')
%                 nm=strrep(nm,'y-','');
%                 nm=[nm ', y-component'];
%             end
%             FI.Dataset(i).Attribute(j).Value=nm;
%         end
%     end
% end
%
if nvars==0
    Out = Out([]);
else
    %
    % Process variables and add them to the list
    %
    for ivar=1:nvars
        Insert=Out(1);
        %
        % Get variable attributes
        %
        Info = FI.Dataset(ivar);
        if isfield(Info.Attribute,'Name')
            Attribs = {Info.Attribute.Name};
        else
            Attribs = {};
        end
        %
        % Show long name, or standard name, or variable name
        %
        j = strmatch('standard_name',Attribs,'exact');
        if ~isempty(j)
            standard_name = Info.Attribute(j).Value;
        else
            standard_name = '';
        end
        j = strmatch('long_name',Attribs,'exact');
        if ~isempty(j) && ~isempty(Info.Attribute(j).Value)
            Insert.Name = Info.Attribute(j).Value;
        else
            if ~isempty(standard_name)
                Insert.Name = standard_name;
            else
                Insert.Name = Info.Name;
            end
        end
        %
        % Show units
        %
        j = strmatch('units',Attribs,'exact');
        if ~isempty(j)
            Insert.Units = Info.Attribute(j).Value;
        else
            Insert.Units = '';
        end
        %
        % Scalar variables by default
        %
        Insert.NVal = 1;
        Insert.DimName = cell(1,5);
        if strcmp(Info.Datatype,'char')
            Insert.NVal = 4;
        end
        %
        % Link to dimension variables
        %
        for i=1:5
            if ~isnan(Info.TSMNK(i))
                if i==ST_ && ~isempty(Info.Station)
                    Insert.DimFlag(i)=5;
                else
                    Insert.DimFlag(i)=1;
                end
                Insert.DimName{i}=FI.Dimension(Info.TSMNK(i)+1).Name;
            end
        end
        for i=6:length(Info.TSMNK)
            Insert.DimFlag(i)=1;
            Insert.DimName{i}=FI.Dimension(Info.TSMNK(i)+1).Name;
        end
        %
        % Any extra dimensions are wrapped into the subfields.
        %
        if ~isempty(Info.SubFieldDim)
            Insert.SubFld={};
            for d = Info.SubFieldDim+1
                Insert.SubFld(end+1,1:2) = {FI.Dimension(d).Name FI.Dimension(d).Length};
            end
        end
        %
        if ~isempty(Info.Mesh)
            Insert.Geom = 'UGRID';
            Insert.Coords = 'xy';
            Insert.hasCoords=1;
            switch Info.Mesh{3}
                case -1 % the mesh itself
                    Insert.Geom = 'UGRID-NODE';
                    Insert.DimFlag(3) = 6;
                case 0 % node
                    Insert.Geom = 'UGRID-NODE';
                    Insert.DimFlag(3) = 6;
                case 1 % edge
                    Insert.Geom = 'UGRID-EDGE';
                    Insert.DimFlag(3) = 6;
                case 2 % face
                    Insert.Geom = 'UGRID-FACE';
                    Insert.DimFlag(3) = 6;
                    Insert.DataInCell = 1;
                case 3 % volume
                    Insert.Geom = 'UGRID-VOLUME';
                    Insert.DimFlag(3) = 6;
                    Insert.DataInCell = 1;
            end
            if strcmp(Info.Type,'ugrid_mesh')
                Insert.NVal = 0;
            end
        elseif ~isempty(Info.X) && ~isempty(Info.Y)
            Insert.hasCoords=1;
            if ~isempty(Info.XBounds) && ~isempty(Info.YBounds)
                Insert.Geom = 'POLYG';
                Insert.ClosedPoly = 1;
                Insert.Coords = 'xy';
                Insert.DataInCell = 1;
            elseif ~Insert.DimFlag(N_) % 1D data set or unstructured data set
                if Insert.DimFlag(K_)
                    Insert.Geom = 'PNT+';
                    Insert.Coords = 'xy+z';
                else
                    Insert.Geom = 'PNT';
                    Insert.Coords = 'xy';
                end
            end
        end
        %
        Insert.varid = Info.Varid;
        %
        Out(end+1)=Insert;
        %
        if strcmp(standard_name,'discharge') && strcmp(Insert.Geom,'UGRID-EDGE')
            Insert.Name = 'stream function'; % previously: discharge potential
            Insert.Geom = 'UGRID-NODE';
            Insert.varid = {'stream_function' Insert.varid};
            %
            Out(end+1)=Insert;
        end
    end
    Out(1)=[];
end

%
% detect vector quantities
%
% VecStdNameTable: component_1, component_2, vector_type, quickplot_combined_name
%   vector_type = 0: x and y
%                 4: magnitude and direction
%                 5: normal and tangential (on ugrid edge)
VecStdNameTable = {
    'sea_water_speed',               'direction_of_sea_water_velocity',4,'sea_water_velocity'
    'sea_ice_speed',                 'direction_of_sea_ice_speed',     4,'sea_water_velocity'
    'wind_speed',                    'wind_to_direction',              4,'air_velocity'
    'eastward_sea_water_velocity',   'northward_sea_water_velocity',   0,'velocity'
    'eastward_sea_ice_velocity',     'northward_sea_ice_velocity',     0,'sea_ice_velocity'
    'eastward_wind_shear',           'northward_wind_shear',           0,'wind_shear'
    'surface_downward_eastward_wind','surface_downward_northward_wind',0,'surface_downward_wind'
    'eastward_wind',                 'northward_wind',                 0,'air_velocity'};
nVecStdNames = size(VecStdNameTable,1);

i=1;
while i<length(Out)
    if iscell(Out(i).varid)
        i=i+1;
        continue
    end
    stdname = FI.Dataset(Out(i).varid+1).StdName;
    j = strmatch(stdname,VecStdNameTable(:,1:2),'exact');
    y=[];
    if ~isempty(j)
        % standard name matches known standard name of a vector component
        if j<=nVecStdNames
            j2 = j+nVecStdNames;
            Name = VecStdNameTable{j,4};
            VectorDef = VecStdNameTable{j,3};
        else
            j2 = j-nVecStdNames;
            Name = VecStdNameTable{j2,4};
            VectorDef = VecStdNameTable{j2,3};
        end
        Ystr = VecStdNameTable{j2};
        for i2=i+1:length(Out)
            stdname = FI.Dataset(get_varid(Out(i2))+1).StdName;
            if strcmp(stdname,Ystr)
                y=i2;
                break
            end
        end
    elseif length(Out(i).Name)>12 && strcmp(Out(i).Name(end-12:end),', n-component')
        Ystr = Out(i).Name; Ystr(end-10)='t';
        Name = Out(i).Name(1:end-13);
        j=1; j2=2;
        VectorDef = 5; % normal and tangential
        for i2=1:length(Out)
            if strcmp(Out(i2).Name,Ystr)
                y=i2;
                break
            end
        end
    elseif length(Out(i).Name)>12 && strcmp(Out(i).Name(end-12:end),', x-component')
        Ystr = Out(i).Name; Ystr(end-10)='y';
        Name = Out(i).Name(1:end-13);
        j=1; j2=2;
        VectorDef = 0; % x and y
        for i2=1:length(Out)
            if strcmp(Out(i2).Name,Ystr)
                y=i2;
                break
            end
        end
    end
    if ~isempty(y)
        Out(i).NVal=2;
        Out(i).Name = Name;
        Out(i).VectorDef = VectorDef;
        Out(i).MNK = VectorDef==5;
        if j<j2
            Out(i).varid=[Out(i).varid Out(y).varid];
        else
            Out(i).varid=[Out(y).varid Out(i).varid];
        end
        Out(y)=[];
        if y<i
            i=i-1;
        end
    end
    i=i+1;
end

hasCoords = [Out.hasCoords]==1;
OutNoCoords = Out(~hasCoords);
Out = Out(hasCoords);
%
matchDims = 1:length(Out);
for i = 1:length(Out)
    for j = 1:i-1
        if isequal(Out(i).DimName,Out(j).DimName)
            matchDims(i) = j;
            break
        end
    end
end
%
OutCoords = [];
if ~isempty(matchDims)
   for m = unique(matchDims)
      OutCoords = [OutCoords Out(matchDims==m) Dummy];
   end
end
Out = [OutCoords OutNoCoords];
%
for i = length(OutCoords)+(1:length(OutNoCoords))
    if Out(i).DimFlag(M_)
        Out(i).MName = Out(i).DimName{M_};
        [V,cnt,err] = sscanf(Out(i).MName,'%f',2);
        if cnt == 1 && isempty(err)
            Out(i).MName = 'M';
        end
    end
    if Out(i).DimFlag(N_)
        Out(i).NName = Out(i).DimName{N_};
        [V,cnt,err] = sscanf(Out(i).NName,'%f',2);
        if cnt == 1 && isempty(err)
            Out(i).NName = 'N';
        end
    end
    if Out(i).DimFlag(K_)
        Out(i).KName = Out(i).DimName{K_};
        [V,cnt,err] = sscanf(Out(i).KName,'%f',2);
        if cnt == 1 && isempty(err)
            Out(i).KName = 'K';
        end
    end
end
%
for i = 1:length(Out)
   if strcmp(Out(i).Geom,'PNT')
      Out(i).Name = [Out(i).Name ' (points)'];
   elseif strncmp(Out(i).Geom,'UGRID',5)
      %Out(i).Name = [Out(i).Name ' (' lower(Out(i).Geom) ')'];
   end
end
%
for i = 1:length(Out)
    if iscell(Out(i).varid)
        %TODO
    elseif ~isempty(Out(i).varid)
        Info = FI.Dataset(Out(i).varid(1)+1);
        for j = 1:length(Info.Attribute)
            if strcmp(Info.Attribute(j).Name,'cell_methods')
                Out(i).Name = [Out(i).Name ' - ' Info.Attribute(j).Value];
            end
        end
    end
end
%
Meshes = zeros(0,2);
for loop = 1:2
    for i = 1:length(Out)
        switch Out(i).Geom
            case {'UGRID-NODE','UGRID-EDGE','UGRID-FACE'}
                varid = get_varid(Out(i));
                thisMesh = FI.Dataset(varid+1).Mesh;
                if loop == 1
                    if thisMesh{3} == -1
                        Meshes(end+1,:) = [thisMesh{2} i];
                    end
                else % loop == 2
                    if thisMesh{3} == -1
                        Out(i).UseGrid = i;
                    else
                        j = find(Meshes(:,1) == thisMesh{2});
                        Out(i).UseGrid = Meshes(j,2);
                    end
                end
        end
    end
end
%
if domain==FI.NumDomains+1
    for i =1 :length(Out)
        if Out(i).DimFlag(M_)
            Out(i).DimFlag(M_) = inf;
        end
        if Out(i).DimFlag(N_)
            Out(i).DimFlag(N_) = inf;
        end
    end
end
% -----------------------------------------------------------------------------


function ivar = get_varid(Props)
if iscell(Props.varid)
    ivar = Props.varid{2};
else
    ivar = Props.varid(1);
end


% -----------------------------------------------------------------------------
function [subf,rec]=getsubfields(FI,Props,f)
if isempty(Props.SubFld)
    subf={};
    rec.Fld={};
    rec.Val=[];
else
    subf={''};
    Fld=Props.SubFld(:,1)';
    Val=zeros(1,0);
    for d = 1:length(Fld)
        nval = Props.SubFld{d,2};
        newsubf = {};
        newVal = [];
        for sf = 1:length(subf)
            subf_f = subf{sf};
            Val_f = Val(sf,:);
            if ~isempty(subf_f)
                subf_f = [subf_f ', '];
            end
            for v = 1:Props.SubFld{d,2}
                if nval>1
                    newsubf{end+1}=sprintf('%s%s=%i',subf_f,Fld{d},v);
                else
                    newsubf{end+1}=subf_f;
                end
                newVal(end+1,1:d)=[Val_f v];
            end
        end
        subf = newsubf;
        Val = newVal;
    end
    if isequal(subf,{''})
        subf={};
    end
    rec.Fld=Fld;
    rec.Val=Val;
end
if nargin>2 && f~=0
    subf = subf(f);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
ndims = length(Props.DimFlag);
sz = zeros(1,ndims);
if iscell(Props.varid)
    switch Props.varid{1}
        case 'stream_function'
            % get underlying discharge on edge variable
            Info = FI.Dataset(Props.varid{2}+1);
            sz(1) = FI.Dimension(Info.TSMNK(1)+1).Length;
            % get the x-coordinates variable for the nodes of the mesh
            XVar = FI.Dataset(Info.Mesh{2}).X;
            % get the node dimension
            dimNodes = FI.Dataset(XVar).TSMNK(3)+1;
            sz(3) = FI.Dimension(dimNodes).Length;
        otherwise
            error('Size function not yet implemented for special case "%s"',Props.varid{1})
    end
elseif ~isempty(Props.varid)
    Info=FI.Dataset(Props.varid(1)+1);
    for d_ = 1:ndims
        if Props.DimFlag(d_)
            sz(d_) = FI.Dimension(Info.TSMNK(d_)+1).Length;
        end
    end
end
%======================== SPECIFIC CODE =======================================

% -----------------------------------------------------------------------------
function Domains=domains(FI)
if FI.NumDomains > 1
    Domains = multiline(sprintf('partition %4.4d-',0:FI.NumDomains-1),'-','cell');
    Domains{end} = 'all partitions';
else
    Domains = {};
end


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
varid = get_varid(Props);
tvar = FI.Dataset(varid+1).Time;
if isempty(tvar)
    tinfo = [];
    T = [];
else
    tinfo = FI.Dataset(tvar).Info;
    T = double(nc_varget(FI.Filename,FI.Dataset(tvar).Name));
end
if ~isstruct(tinfo) % likely even empty
    % continue with T = T;
elseif ischar(tinfo.RefDate)
    switch tinfo.RefDate
        case 'day as %Y%m%d.%f'
            Y = floor(T/10000);
            T = T - Y*10000;
            M = floor(T/100);
            T = T - M*100;
            D = floor(T);
            T = T - D;
            T = datenum(Y,M,D) + T;
        otherwise
            T = tinfo.DT * T;
    end
elseif ~isempty(tinfo.RefDate)
    T = tinfo.RefDate + tinfo.DT * T;
else
    T = tinfo.DT * T;
end
%if ~isnan(tinfo.TZshift)
%    T = T - tinfo.TZshift/24;
%end
if t~=0
    T=T(t);
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
stcrd = FI.Dataset(get_varid(Props)+1).Station;
[Stations, status] = qp_netcdf_get(FI,stcrd-1,FI.Dataset(stcrd).Dimension);
if t~=0
    Stations = Stations(t,:);
end
S=cellstr(Stations);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Dimension = matchdims(CoordDims,VarDims)
Dimension = zeros(1,length(VarDims));
%
for i = 1:length(CoordDims)
    idim = strmatch(CoordDims{i},VarDims,'exact');
    if ~isempty(idim)
        Dimension(idim) = i;
    else
        error('Coordinate dimension ''%s'' not matched by variable dimension.',CoordDims{i})
    end
end
%
for i = 1:length(VarDims)
    if Dimension(i) == 0
        Dimension(i) = max(Dimension)+1;
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Coord = expand_hdim(Coord,szData,hdim)
szC = size(Coord);
if length(szC)<length(hdim)
    szC(end+1:length(hdim))=1;
end
if length(szData)<length(hdim)
    szData(end+1:length(hdim))=1;
end
if ~isequal(szC(hdim),szData(hdim))
    ndim = length(hdim); % catch bounds dimension
    repC = szData(1:ndim)./szC(1:ndim);
    repC(~hdim)=1;
    Coord = repmat(Coord,repC);
end
