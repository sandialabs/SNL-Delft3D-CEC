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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/progsrc/private/netcdffil.m $
%   $Id: netcdffil.m 65866 2020-01-26 20:25:09Z jagers $

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
        %
        % Rule: if dimension = 0, then error
        %
        if sz(d_)==0
            switch d_
                case T_
                    error('No time steps available in the file.')
                case ST_
                    error('No stations available in the file.')
                otherwise
                    error('Empty dimension encountered: unable to read the data.')
            end
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
XYneeded = false;
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
                meshInfo    = FI.Dataset(Info.Mesh{3});
                if isempty(meshInfo.Attribute)
                    meshAttribs = {};
                else
                    meshAttribs = {meshInfo.Attribute.Name};
                end
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
            case 'erosion_sedimentation'
                [data, status] = qp_netcdf_get(FI,Props.varid{2},Props.DimName,idx);
                if any(idx{T_}==1)
                    first = find(idx{T_}==1);
                    data1 = data(first(1),:);
                else
                    idx1 = idx;
                    idx1{T_} = 1;
                    [data1, status] = qp_netcdf_get(FI,Props.varid{2},Props.DimName,idx1);
                end
                for i = 1:length(idx{T_})
                    data(i,:) = data(i,:) - data1;
                end
                szData = size(data);
                %
                if length(idx{T_})==1
                    szV = [size(data) 1];
                    data = reshape(data,szV(2:end));
                    removeTime = 1;
                end
                %
                Ans.Val = data;
            case {'node_index','edge_index','face_index'}
                Ans.Val = idx{3}(:);
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
            XYneeded = true;
        otherwise
            % no rotation
    end
    %
    if Props.NVal==6
        fm = ustrcmpi('flag_meanings',Attribs);
        Ans.Classes = strsplit(Info.Attribute(fm).Value,' ');
        fv = ustrcmpi('flag_values',Attribs);
        if fv>0
            [dummy,Ans.Val] = ismember(Ans.Val,Info.Attribute(fv).Value);
        else
            fm = ustrcmpi('flag_masks',Attribs);
            if fm>0
                ui_message('warning','The quantity %s uses ''flag_masks'' which are not yet supported.',Info.Name)
            else
                ui_message('warning','The quantity %s uses ''flag_meanings'' attribute,\nbut the associated ''flag_values'' or ''flag_masks'' attribute can''t be found.', Info.Name)
            end
        end
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

getOptions = {};
if XYRead || XYneeded
    if strncmp(Props.Geom,'UGRID',5)
        %ugrid
        mesh_settings = Info.Mesh;
        msh = mesh_settings{3};
        meshInfo      = FI.Dataset(msh);
        %
        dimNodes = meshInfo.Mesh{5};
        dimEdges = meshInfo.Mesh{6};
        dimFaces = meshInfo.Mesh{7};
        allDims = {FI.Dimension.Name};
        MeshSubset = {};
        switch mesh_settings{4}
            case 0 % data at NODE
                MeshSubset = {'NODE' dimNodes idx{M_}
                              'EDGE' dimEdges -1
                              'FACE' dimFaces -1};
                if ~isempty(dimEdges)
                    MeshSubset{2,3} = 1:FI.Dimension(strcmp(dimEdges,allDims)).Length;
                end
                if ~isempty(dimFaces)
                    MeshSubset{3,3} = 1:FI.Dimension(strcmp(dimFaces,allDims)).Length;
                end
            case 1 % data at EDGE
                MeshSubset = {'NODE' dimNodes 1:FI.Dimension(strcmp(dimNodes,allDims)).Length
                    'EDGE' dimEdges idx{M_}
                    'FACE' dimFaces -1};
                if ~isempty(dimFaces)
                    MeshSubset{3,3} = 1:FI.Dimension(strcmp(dimFaces,allDims)).Length;
                end
            case 2 % data at FACE
                MeshSubset = {'NODE' dimNodes 1:FI.Dimension(strcmp(dimNodes,allDims)).Length
                              'EDGE' dimEdges -1
                              'FACE' dimFaces idx{M_}};
                if ~isempty(dimEdges)
                    MeshSubset{2,3} = 1:FI.Dimension(strcmp(dimEdges,allDims)).Length;
                end
        end
        if isempty(MeshSubset)
            getOptions = {};
        else
            getOptions = {'mesh_subsets' MeshSubset};
        end
        %
        for c = 'XY'
            if isempty(meshInfo.(c))
                error('No %s coordinate found for %s.',c,meshInfo.Name)
            end
            CoordInfo2 = FI.Dataset(meshInfo.(c));
            [Ans.(c), status] = qp_netcdf_get(FI,CoordInfo2);
            %
            unit = get_unit(CoordInfo2);
            if ischar(unit)
                Ans.([c 'Units']) = unit;
            end
        end
        if strcmp(mesh_settings{1},'ugrid1d_network')
            % Ans.X contains mesh node branch index
            % Ans.Y contains mesh node offset/chainage
            %
            attcsp = strmatch('coordinate_space',{meshInfo.Attribute.Name});
            csp = strmatch(meshInfo.Attribute(attcsp).Value,{FI.Dataset.Name},'exact');
            [BrX,BrY,xUnit,BrL] = get_edge_geometry(FI,csp);
            %
            si = strmatch('start_index',{FI.Dataset(meshInfo.X).Attribute.Name});
            if ~isempty(si)
                start_index = FI.Dataset(meshInfo.X).Attribute(si).Value;
            else
                start_index = 0;
            end
            if min(Ans.X)~=start_index
                % trigger warning ...?
            end
            Ans.X = Ans.X-start_index+1;
            %
            % Get edge_node_connectivity
            attENC= strmatch('edge_node_connectivity',{meshInfo.Attribute.Name});
            [e2n, status] = qp_netcdf_get(FI,meshInfo.Attribute(attENC).Value);
            %
            % Get mesh_edge branch affinity (read from edge_coordinates or reconstructed)
            attECO = strmatch('edge_coordinates',{meshInfo.Attribute.Name});
            if ~isempty(attECO)
                ecoords = strsplit(meshInfo.Attribute(attECO).Value);
                for iec = 1:length(ecoords)
                    i_eBrNr = strmatch(ecoords{iec},{FI.Dataset.Name});
                    ecAtt = {FI.Dataset(i_eBrNr).Attribute.Name};
                    if ismember('units',ecAtt) || ismember('standard_name',ecAtt)
                        % x-coordinate, y-coordinate, offset
                        continue
                    end
                    % branch_id
                    [eBrNr, status] = qp_netcdf_get(FI,FI.Dataset(i_eBrNr));
                    if any(eBrNr<0)
                        ui_message('warning','Invalid %s data: negative branch ids encountered. Ignoring this data.',FI.Dataset(i_eBrNr).Name)
                        eBrNr = [];
                    end
                    break
                end
                si = strmatch('start_index',{FI.Dataset(i_eBrNr).Attribute.Name});
                if ~isempty(si)
                    start_index = FI.Dataset(i_eBrNr).Attribute(si).Value;
                else
                    start_index = 0;
                end
                eBrNr = eBrNr-start_index+1;
            else
                eBrNr = [];
            end
            if isempty(eBrNr)
                % TODO: create networknode(i)
                % networknode(i) = N if mesh node i coincides with network node N
                % networknode(i) = -1 if mesh node i does not coincide with a network node
                networknode = -ones(size(Ans.X));
                networknode(Ans.Y==0 | Ans.Y==BrL(Ans.X)) = 1;
                %
                % reconstruct mesh_edge branch affinity
                eBrNr = Ans.X(e2n);
                for i = 1:size(eBrNr,1)
                    n1 = networknode(e2n(i,1));
                    if n1<0
                        % start node isn't a network node, so edge must
                        % be on same branch.
                        % eBrNr(i,1) is correct.
                        continue
                    end
                    n2 = networknode(e2n(i,2));
                    if n2<0
                        % end node isn't a network node, so edge must
                        % be on same branch.
                        eBrNr(i,1) = eBrNr(i,2);
                        continue
                    end
                    % both start and end node of edge match a network node.
                    % identify the branches between the network nodes.
                    %
                    if 1
                        % if one branch, select that one.
                        1
                    else
                        % if multiple branches, select one and give warning.
                        2
                    end
                end
                eBrNr = eBrNr(:,1);
            end
            %
            if ischar(xUnit)
                Ans.XUnits = xUnit;
                Ans.YUnits = xUnit;
            end
            [Ans.X,Ans.Y,Ans.EdgeGeometry.X,Ans.EdgeGeometry.Y] = branch2xy(BrX,BrY,xUnit,BrL,Ans.X,Ans.Y,eBrNr,e2n);
        end
        %
        if isempty(meshInfo.Attribute)
            meshAttribs = {};
        else
            meshAttribs = {meshInfo.Attribute.Name};
        end
        connect = strmatch('face_node_connectivity',meshAttribs,'exact');
        if ~isempty(connect)
            iconnect = strmatch(meshInfo.Attribute(connect).Value,{FI.Dataset.Name},'exact');
            if isempty(iconnect)
                ui_message('error','Face_node_connectivity not found!')
            else
                [Ans.FaceNodeConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
                nNodes = sum(~isnan(Ans.FaceNodeConnect),2);
                min_nNodes = min(nNodes);
                if min_nNodes<3
                    nError = sum(nNodes==min_nNodes);
                    error('%i faces found with %i nodes. Number of nodes per face should be at least 3.',nError,min_nNodes)
                end
                if isempty(FI.Dataset(iconnect).Attribute)
                    istart = [];
                else
                    istart = strmatch('start_index',{FI.Dataset(iconnect).Attribute.Name},'exact');
                end
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
                    maxNode = max(Ans.FaceNodeConnect(:));
                    minNode = min(Ans.FaceNodeConnect(Ans.FaceNodeConnect>=0));
                    if minNode-start+1<1
                        error('File specifies start_index %g, but lowest node index in file is %g.',start,minNode)
                    elseif maxNode-start+1>length(Ans.X)
                        error('File specifies start_index %g and the largest node index in file is %g, but the number of nodes is only %g.',start,maxNode,length(Ans.X))
                    end
                end
                Ans.FaceNodeConnect = Ans.FaceNodeConnect - start + 1;
                Ans.FaceNodeConnect(Ans.FaceNodeConnect<1) = NaN;
            end
        end
        %
        Ans.ValLocation = Props.Geom(max(strfind(Props.Geom,'-'))+1:end);
        connect = strmatch('edge_node_connectivity',meshAttribs,'exact');
        iconnect = [];
        if strcmp(Ans.ValLocation,'EDGE') || ~isfield(Ans,'FaceNodeConnect') || (~DataRead && ~isempty(connect))
            % "~DataRead" is a hack to load EdgeNodeConnect if available for use in GridView
            iconnect = strmatch(meshInfo.Attribute(connect).Value,{FI.Dataset.Name},'exact');
            if isempty(iconnect)
                ui_message('warning','EDGE-NODE connectivity variable %s not found! Using empty set.',meshInfo.Attribute(connect).Value)
                Ans.EdgeNodeConnect = zeros(0,2);
            else
                [Ans.EdgeNodeConnect, status] = qp_netcdf_get(FI,meshInfo.Attribute(connect).Value);
                Ans.EdgeNodeConnect(Ans.EdgeNodeConnect<0) = NaN;
            end
        end
        if isfield(Ans,'EdgeNodeConnect') && ~isempty(iconnect)
            if isempty(FI.Dataset(iconnect).Attribute)
                istart = [];
            else
                istart = strmatch('start_index',{FI.Dataset(iconnect).Attribute.Name},'exact');
            end
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
        if mesh_settings{2}==1 % also: if strncmp(Props.Geom,'UGRID1D',7)
            aEG = strcmp({meshInfo.Attribute.Name},'edge_geometry');
            if any(aEG)
                % get edge geometry
                [BrX,BrY,xUnit] = get_edge_geometry(FI,msh);
                Ans.EdgeGeometry.X = BrX;
                Ans.EdgeGeometry.Y = BrY;
            end
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
                if isfield(Ans,'Edge')
                    Ans.EdgeGeometry.X = Ans.EdgeGeometry.X(idx{M_});
                    Ans.EdgeGeometry.Y = Ans.EdgeGeometry.Y(idx{M_});
                end
            case 'FACE'
                Ans.FaceNodeConnect = Ans.FaceNodeConnect(idx{M_},:);
        end
        %
        %[Ans.XFace, status] = qp_netcdf_get(FI,'mesh2d_face_x');
        %[Ans.YFace, status] = qp_netcdf_get(FI,'mesh2d_face_y');
    elseif strcmp(Info.Type,'simple_geometry')
        gt = ustrcmpi('geometry_type',Attribs);
        switch Info.Attribute(gt).Value
            case {'multiline'}
                Ans.X = qp_netcdf_get(FI,FI.Dataset(Info.X));
                Ans.Y = qp_netcdf_get(FI,FI.Dataset(Info.Y));
            otherwise
                error('Simple geometry type "%s" not yet implemented.',Info.Attribute(gt).Value)
        end
    else
        firstbound = 1;
        for iCoord = 1:length(coordname)
            vdim = getfield(Info,coordname{iCoord});
            if isempty(vdim)
                if length(Props.varid)>1
                    Info2 = FI.Dataset(Props.varid(2)+1);
                    vdim = getfield(Info2,coordname{iCoord});
                    if ~isempty(vdim)
                        Info = Info2;
                    else
                        continue
                    end
                end
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
        vCoordExtended = false;
        vdimid = Info.Z;
        CoordInfo = FI.Dataset(vdimid);
        if ~isempty(strfind(CoordInfo.Name,'_layer_'))
            iName  = strrep(CoordInfo.Name,'_layer_','_interface_');
            iDimid = ustrcmpi(iName,{FI.Dataset.Name});
            CoordInfo = FI.Dataset(iDimid);
            idx{K_} = unique([idx{K_} idx{K_}+1]);
            Props.DimName{K_} = CoordInfo.Dimension{1};
            vCoordExtended = true;
        end
        %
        if isempty(CoordInfo.Attribute)
            Attribs = {};
        else
            Attribs = {CoordInfo.Attribute.Name};
        end
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
            FTerror = sprintf('Empty attribute ''formula_terms'' on vertical coordinate variable ''%s''.',CoordInfo.Name);
        else
            FormulaTerms = cell(0,2);
            FTerror = sprintf('Missing attribute ''formula_terms'' on vertical coordinate variable ''%s''.',CoordInfo.Name);
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
        zLocVar = '';
        zUnitVar = '';
        try
            if ~isempty(j)
                standard_name = CoordInfo.Attribute(j).Value;
                if isnan(Info.TSMNK(N_))
                    HDIMS = {':'};
                    hdims = 2;
                else
                    HDIMS = {':',':'};
                    hdims = 2:3;
                end
                switch standard_name
                    case 'atmosphere_ln_pressure_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        [p0  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [lev , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{1,2}; % p0
                        %
                        Z = zeros(szData);
                        for t=1:size(Z,1)
                            for k=1:length(lev)
                                Z(t,HDIMS{:},k) = p0 * exp(-lev(k));
                            end
                        end
                    case 'atmosphere_sigma_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        [sigma  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [ps     , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [ptop   , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{2,2}; % ps
                        zLocVar  = FormulaTerms{2,2}; % ps
                        szZData  = updateSize(szData,size(ps),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(sigma)
                                Z(t,HDIMS{:},k) = ptop+sigma(k)*(ps(t,HDIMS{:})-ptop);
                            end
                        end
                    case 'atmosphere_hybrid_sigma_pressure_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        if isequal(FormulaTerms{1,1},'a:')
                            [a      , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                            [b      , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                            [ps     , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                            [p0     , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx,getOptions{:});
                            zUnitVar = FormulaTerms{3,2}; % ps
                            zLocVar  = FormulaTerms{3,2}; % ps
                            szZData  = updateSize(szData,size(ps),hdims);
                            %
                            Z = zeros(szZData);
                            for t=1:size(Z,1)
                                for k=1:length(a)
                                    Z(t,HDIMS{:},k) = a(k)*p0+b(k)*ps(t,HDIMS{:});
                                end
                            end
                        else
                            [ap     , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                            [b      , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                            [ps     , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                            zUnitVar = FormulaTerms{3,2}; % ps
                            zLocVar  = FormulaTerms{3,2}; % ps
                            szZData  = updateSize(szData,size(ps),hdims);
                            %
                            Z = zeros(szZData);
                            for t=1:size(Z,1)
                                for k=1:length(ap)
                                    Z(t,HDIMS{:},k) = ap(k)+b(k)*ps(t,HDIMS{:});
                                end
                            end
                        end
                    case 'atmosphere_hybrid_height_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        [a     , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [b     , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [orog  , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{1,2}; % a
                        zLocVar  = FormulaTerms{3,2}; % orog
                        szZData  = updateSize(szData,size(orog),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(a)
                                Z(t,HDIMS{:},k) = a(k) + b(k)*orog(t,HDIMS{:});
                            end
                        end
                    case 'atmosphere_sleve_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        [a       , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [b1      , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [b2      , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        [ztop    , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx,getOptions{:});
                        [zsurf1  , status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx,getOptions{:});
                        [zsurf2  , status] = qp_netcdf_get(FI,FormulaTerms{6,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{4,2}; % ztop
                        zLocVar  = FormulaTerms{5,2}; % zsurf1
                        szZData  = updateSize(szData,size(zsurf1),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(a)
                                Z(t,HDIMS{:},k) = a(k)*ztop+b1(k)*zsurf1(t,HDIMS{:})+b2(k)*zsurf2(t,HDIMS{:});
                            end
                        end
                    case 'ocean_sigma_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        [sigma  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{2,2}; % eta
                        zLocVar  = FormulaTerms{2,2}; % eta
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        % some hacks for D-Flow FM
                        if strcmp(FormulaTerms{3,1},'bedlevel:')
                            depth = -depth;
                        elseif length(FormulaTerms{3,2})>10 && strcmp(FormulaTerms{3,2}(end-9:end),'waterdepth')
                            depth = depth-eta;
                        end
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(sigma)
                                Z(t,HDIMS{:},k) = eta(t,HDIMS{:})+(depth+eta(t,HDIMS{:}))*sigma(k);
                            end
                        end
                    case 'ocean_s_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        [s      , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        [a      , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx,getOptions{:});
                        [b      , status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx,getOptions{:});
                        [depth_c, status] = qp_netcdf_get(FI,FormulaTerms{6,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{2,2}; % eta
                        zLocVar  = FormulaTerms{2,2}; % eta
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        C = (1-b)*sinh(a*s)/sinh(a) + b*(tanh(a*(s+0.5))/(2*tanh(0.5*a))-0.5);
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(s)
                                Z(t,HDIMS{:},k) = eta(t,HDIMS{:})*(1+s(k))+depth_c*s(k)+(depth-depth_c)*C(k);
                            end
                        end
                    case 'ocean_s_coordinate_g1'
                        if isempty(FormulaTerms), error(FTerror), end
                        [s      , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [C      , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx,getOptions{:});
                        [depth_c, status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{3,2}; % eta
                        zLocVar  = FormulaTerms{3,2}; % eta
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(s)
                                S = depth_c*s(k) + (depth - depth_c)*C(k);
                                Z(t,HDIMS{:},k) = S + eta(t,HDIMS{:})*(1+S./depth);
                            end
                        end
                    case 'ocean_s_coordinate_g2'
                        if isempty(FormulaTerms), error(FTerror), end
                        [s      , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [C      , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx,getOptions{:});
                        [depth_c, status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{3,2}; % eta
                        zLocVar  = FormulaTerms{3,2}; % eta
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(s)
                                S = (depth_c*s(k) + depth*C(k))./(depth_c + depth);
                                Z(t,HDIMS{:},k) = eta(t,HDIMS{:}) + (eta(t,HDIMS{:}) + depth).*S;
                            end
                        end
                    case 'ocean_sigma_z_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        [sigma  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [eta    , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        [depth_c, status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx,getOptions{:});
                        [nsigma , status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx,getOptions{:});
                        [zlev   , status] = qp_netcdf_get(FI,FormulaTerms{6,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{2,2}; % eta
                        zLocVar  = FormulaTerms{2,2}; % eta
                        szZData  = updateSize(szData,size(eta),hdims);
                        %
                        K=idx{K_};
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(sigma)
                                if K(k)<=nsigma
                                    Z(t,HDIMS{:},k) = eta(t,HDIMS{:}) + sigma(k)*(min(depth_c,depth)+eta(t,HDIMS{:}));
                                else
                                    Z(t,HDIMS{:},k) = zlev(k);
                                end
                            end
                        end
                    case 'ocean_double_sigma_coordinate'
                        if isempty(FormulaTerms), error(FTerror), end
                        [sigma  , status] = qp_netcdf_get(FI,FormulaTerms{1,2},Props.DimName,idx,getOptions{:});
                        [depth  , status] = qp_netcdf_get(FI,FormulaTerms{2,2},Props.DimName,idx,getOptions{:});
                        [z1     , status] = qp_netcdf_get(FI,FormulaTerms{3,2},Props.DimName,idx,getOptions{:});
                        [z2     , status] = qp_netcdf_get(FI,FormulaTerms{4,2},Props.DimName,idx,getOptions{:});
                        [a      , status] = qp_netcdf_get(FI,FormulaTerms{5,2},Props.DimName,idx,getOptions{:});
                        [href   , status] = qp_netcdf_get(FI,FormulaTerms{6,2},Props.DimName,idx,getOptions{:});
                        [k_c    , status] = qp_netcdf_get(FI,FormulaTerms{7,2},Props.DimName,idx,getOptions{:});
                        zUnitVar = FormulaTerms{3,2}; % z1
                        zLocVar  = FormulaTerms{2,2}; % depth
                        szZData  = updateSize(szData,size(depth),hdims);
                        %
                        K=idx{K_};
                        Z = zeros(szZData);
                        for t=1:size(Z,1)
                            for k=1:length(sigma)
                                f = 0.5*(z1+z2) + 0.5*(z1-z2)*tanh(2*a/(z1-z2)*(depth-href));
                                if K(k)<=k_c
                                    Z(t,HDIMS{:},k) = sigma(k)*f;
                                else
                                    Z(t,HDIMS{:},k) = f + (sigma(k)-1)*(depth-f);
                                end
                            end
                        end
                    otherwise
                        if ~isempty(formula)
                            ui_message('warning','Formula for %s not implemented',standard_name)
                        end
                        [Z, status] = qp_netcdf_get(FI,CoordInfo,Props.DimName,idx);
                        nZ = length(Z);
                        if signup<0
                            Z=-Z;
                        end
                        %
                        % hack for z-layers in FM
                        %
                        us = strfind(CoordInfo.Name,'_');
                        if ~isempty(us)
                            waterlevel = [CoordInfo.Name(1:us(1)) 's1'];
                            izw = strmatch(waterlevel,{FI.Dataset.Name});
                            %
                            zb_t_dependent = true;
                            bedlevel   = [CoordInfo.Name(1:us(1)) 'mor_bl'];
                            izb = strmatch(bedlevel,{FI.Dataset.Name});
                            if isempty(izb)
                                zb_t_dependent = false;
                                bedlevel   = [CoordInfo.Name(1:us(1)) 'flowelem_bl'];
                                izb = strmatch(bedlevel,{FI.Dataset.Name});
                            end
                            %
                            if ~isempty(izw) && ~isempty(izb)
                                [zw, status] = qp_netcdf_get(FI,FI.Dataset(izw),Props.DimName,idx,getOptions{:});
                                %
                                szZwData = size(zw);
                                szZData = szData;
                                szZData(hdims) = szZwData(hdims);
                                Z = expand_hdim(Z,szZData,hdim);
                                %
                                for k=1:nZ
                                    Z(:,HDIMS{:},k) = min(Z(:,HDIMS{:},k),zw);
                                end
                                %
                                [zb, status] = qp_netcdf_get(FI,FI.Dataset(izb),Props.DimName,idx,getOptions{:});
                                if zb_t_dependent
                                    for k=1:nZ
                                        Z(:,HDIMS{:},k) = max(Z(:,HDIMS{:},k),zb);
                                    end
                                else
                                    for t=1:size(Z,1)
                                        for k=1:nZ
                                            Z(t,HDIMS{:},k) = max(Z(t,HDIMS{:},k),zb(1,HDIMS{:}));
                                        end
                                    end
                                end
                                %
                                zLocVar = waterlevel;
                            else
                                Z = expand_hdim(Z,szData,hdim);
                            end
                        else
                            Z = expand_hdim(Z,szData,hdim);
                        end
                end
            else
                [Z, status] = qp_netcdf_get(FI,CoordInfo,Props.DimName,idx);
                if signup<0
                    Z=-Z;
                end
                Z = expand_hdim(Z,szData,hdim);
            end
        catch Ex
            qp_error({'Retrieving vertical coordinate failed, continuing with layer index as vertical coordinate.','The error message encountered reads:'},Ex,'netcdffil')
            kDim = length(szData);
            kVec = ones(1,kDim);
            kVec(kDim) = length(idx{K_});
            if vCoordExtended
                Z = repmat(reshape(idx{K_},kVec)-0.5,szData(1:kDim-1));
            else
                Z = repmat(reshape(idx{K_},kVec),szData(1:kDim-1));
            end
        end
        %
        if ~isempty(zLocVar)
            zLocVar = strmatch(zLocVar,{FI.Dataset.Name},'exact')-1;
            Info = FI.Dataset(zLocVar+1);
            if iscell(Info.Mesh)
                switch Info.Mesh{4}
                    case 0
                        Ans.ZLocation = 'NODE';
                    case 1
                        Ans.ZLocation = 'EDGE';
                    case 2
                        Ans.ZLocation = 'FACE';
                end
            end
        elseif isfield(Ans,'ValLocation')
            Ans.ZLocation = Ans.ValLocation;
        end
        %
        if ~isempty(zUnitVar)
            zUnitVar = strmatch(zUnitVar,{FI.Dataset.Name},'exact')-1;
            Info = FI.Dataset(zUnitVar+1);
            zUnitAtt = strmatch('units',{Info.Attribute.Name},'exact');
            Ans.ZUnits = Info.Attribute(zUnitAtt).Value;
        else
            j = strmatch('units',Attribs,'exact');
            if ~isempty(j)
                Ans.ZUnits = CoordInfo.Attribute(j).Value;
            end
        end
        %--------------------------------------------------------------------
        %
        if removeTime
            szCoord = size(Z);
            Z = reshape(Z,[szCoord(2:end) 1]);
        end
        %--------------------------------------------------------------------
        Ans.Z = Z;
        %
        if isfield(Ans,'X')
            % z coordinates may be defined at ...
            % * value locations (e.g. z and values in faces),
            % * grid locations (e.g. z at nodes), or
            % * other location (e.g. z in faces, values at edges).
            % Be careful when trying to synchronize dimensions!
            %
            szZ = size(Z);
            if isfield(Ans,'Val')
                szV = size(Ans.Val);
            elseif isfield(Ans,'XComp')
                szV = size(Ans.XComp);
            else
                szV = NaN*szZ;
            end
            if ~isequal(szZ,szV)
                if npolpnt>0 && prod(szZ)*npolpnt==szV(1)
                    % polygons
                    Z = repmat(Z(:)',npolpnt,1);
                    Ans.Z = Z(:);
                elseif length(idx{K_})>1
                    % The last dimension of Z is the vertical dimension
                    zdim = ndims(Z);
                    kZ = szZ(zdim);
                    % That dimensions of X/Y should be one.
                    kX = size(Ans.X,kZ);
                    if kX==1
                        rep = ones(1,zdim);
                        rep(zdim) = kZ;
                        Ans.X = repmat(Ans.X,rep);
                        Ans.Y = repmat(Ans.Y,rep);
                    end
                end
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

if XYneeded && ~XYRead
    f = {'X','Y','FaceNodeConnect','EdgeNodeConnect'};
    f(~isfield(Ans,f)) = [];
    Ans = rmfield(Ans,f);
end

% read time ...
T=[];
if Props.DimFlag(T_)
    T=readtim(FI,Props,idx{T_});
end
Ans.Time=T;

% Work around stupid DeltaShell files that write lat/lon, but actually store metric coordinates.
if ~isempty(FI.Attribute)
    ds = strcmp('FileVersion_DeltaShell',{FI.Attribute.Name});
    if any(ds) && isfield(Ans,'XUnits') && strcmp(Ans.XUnits,'deg')
        if min(Ans.X(:))<-360 || max(Ans.X(:))>360
            Ans.XUnits = 'm';
            Ans.YUnits = 'm';
        end
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
PropNames={'Name'                   'Units' 'TemperatureType' 'Geom' 'Coords' 'DimFlag' 'DataInCell' 'NVal' 'SubFld' 'MNK' 'varid'  'DimName' 'hasCoords' 'VectorDef' 'ClosedPoly' 'UseGrid'};
DataProps={'dummy field'            ''      ''                ''     ''      [0 0 0 0 0]  0           0      []       0     []          {}          0         0          0          0};
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
nmesh  = 0;
meshes = cell(0,2);
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
        switch standard_name
            case {'air_potential_temperature','air_temperature', 'air_temperature_at_cloud_top', ...
                    'air_temperature_at_effective_cloud_top_defined_by_infrared_radiation', ...
                    'air_temperature_threshold','brightness_temperature', ...
                    'brightness_temperature_at_cloud_top','canopy_temperature', ...
                    'dew_point_temperature','dynamic_tropopause_potential_temperature', ...
                    'equivalent_potential_temperature','equivalent_temperature', ...
                    'fire_temperature','freezing_temperature_of_sea_water', ...
                    'land_ice_temperature','product_of_air_temperature_and_omega', ...
                    'product_of_air_temperature_and_specific_humidity', 'product_of_eastward_sea_water_velocity_and_temperature', ...
                    'product_of_eastward_wind_and_air_temperature', 'product_of_northward_sea_water_velocity_and_temperature', ...
                    'product_of_northward_wind_and_air_temperature','product_of_omega_and_air_temperature', ...
                    'product_of_upward_air_velocity_and_air_temperature','pseudo_equivalent_potential_temperature', ...
                    'pseudo_equivalent_temperature','sea_ice_surface_temperature', ...
                    'sea_ice_temperature','sea_surface_foundation_temperature', ...
                    'sea_surface_skin_temperature','sea_surface_subskin_temperature', ...
                    'sea_surface_temperature','sea_water_conservative_temperature', ...
                    'sea_water_potential_temperature','sea_water_potential_temperature_at_sea_floor', ...
                    'sea_water_temperature','soil_temperature','square_of_air_temperature', ...
                    'square_of_sea_surface_temperature','surface_brightness_temperature', ...
                    'surface_temperature','temperature_in_surface_snow','temperature_of_sensor_for_oxygen_in_sea_water', ...
                    'toa_brightness_temperature','toa_brightness_temperature_assuming_clear_sky', ...
                    'toa_brightness_temperature_of_standard_scene','tropical_cyclone_eye_brightness_temperature', ...
                    'tropopause_air_temperature','virtual_temperature','wet_bulb_temperature'}
                Insert.TemperatureType = 'absolute';
            otherwise
                Insert.TemperatureType = 'unspecified';
        end
        %
        % Scalar variables by default
        %
        Insert.NVal = 1;
        Insert.DimName = cell(1,5);
        if strcmp(Info.Type,'simple_geometry')
            Insert.NVal = 0;
        elseif strcmp(Info.Datatype,'char')
            Insert.NVal = 4;
        elseif any(strcmp('flag_values',Attribs))
            Insert.NVal = 6;
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
            nmesh = nmesh+1;
            switch Info.Mesh{1}
                case 'ugrid'
                    tpd = Info.Mesh{2};
                    if tpd<0
                        Insert.Geom = 'UGRID-CONTACT';
                    else
                        Insert.Geom = sprintf('UGRID%iD',tpd);
                    end
                otherwise
                    Insert.Geom = upper(Info.Mesh{1});
            end
            Insert.Coords = 'xy';
            Insert.hasCoords=1;
            BaseGeom = Insert.Geom;
            switch Info.Mesh{4}
                case -1 % the mesh itself
                    Insert.Geom = [Insert.Geom '-NODE'];
                    Insert.DimFlag(3) = 6;
                case 0 % node
                    Insert.Geom = [Insert.Geom '-NODE'];
                    Insert.DimFlag(3) = 6;
                case 1 % edge
                    Insert.Geom = [Insert.Geom '-EDGE'];
                    Insert.DimFlag(3) = 6;
                case 2 % face
                    Insert.Geom = [Insert.Geom '-FACE'];
                    Insert.DimFlag(3) = 6;
                    Insert.DataInCell = 1;
                case 3 % volume
                    Insert.Geom = [Insert.Geom '-VOLUME'];
                    Insert.DimFlag(3) = 6;
                    Insert.DataInCell = 1;
            end
            if strcmp(Info.Type,'ugrid_mesh')
                Insert.NVal = 0;
            end
        elseif ~isempty(Info.X) && ~isempty(Info.Y)
            Insert.hasCoords=1;
            mesh = [Info.X Info.Y];
            found = false;
            for m = 1:size(meshes,1)
                if isequal(meshes{m,2},mesh)
                    found = true;
                    break
                end
            end
            if ~found
                meshes(end+1,:) = {size(Out,1)+1 mesh};
            end
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
        if ~isempty(Info.Mesh) && isequal(Info.Mesh{4},-1)
            Insert.varid = {'node_index' Insert.varid};
        end
        %
        Out(end+1)=Insert;
        %
        if ~isempty(Info.Mesh) && isequal(Info.Mesh{4},-1)
            Nm = Insert.Name;
            %
            Insert.Name = [Nm ' - node indices'];
            Insert.NVal = 1;
            Insert.varid{1} = 'node_index';
            Out(end+1) = Insert;
            %
            if ~isempty(Info.Mesh{6})
                Insert.Name = [Nm ' - edge indices'];
                Insert.Geom = [BaseGeom '-EDGE'];
                Insert.varid{1} = 'edge_index';
                Out(end+1) = Insert;
            end
            %
            if length(Info.Mesh)>=7 && ~isempty(Info.Mesh{7})
                Insert.Name = [Nm ' - face indices'];
                Insert.Geom = [BaseGeom '-FACE'];
                Insert.DataInCell = 1;
                Insert.varid{1} = 'face_index';
                Out(end+1) = Insert;
            end
        end
        %
        %if strcmp(standard_name,'discharge') && strncmp(Insert.Name,'Discharge',9) && strcmp(Insert.Geom,'UGRID-EDGE') && Insert.DimFlag(K_)>0
        %    Insert.Name = ['Depth integrated d' Insert.Name(2:end)];
        %    Insert.DimFlag(K_)=0;
        %    %
        %    Out(end+1)=Insert;
        %end
        %
        if strcmp(standard_name,'discharge') && strcmp(Insert.Geom,'UGRID2D-EDGE') && Insert.DimFlag(K_)==0
            Insert.Name = 'stream function'; % previously: discharge potential
            Insert.Geom = 'UGRID2D-NODE';
            Insert.varid = {'stream_function' Insert.varid};
            %
            Out(end+1)=Insert;
        else
            switch lower(Insert.Name)
                case 'time-varying bottom level in flow cell center'
                    if FI.Dimension(Info.TSMNK(T_)+1).Length>1
                        Insert.Name = 'cum. erosion/sedimentation';
                        Insert.varid = {'erosion_sedimentation' Insert.varid};
                        %
                        Out(end+1)=Insert;
                    end
            end
        end
    end
    Out(1)=[];
end
nOut = length(Out);
for m = size(meshes,1):-1:1
    mo = nOut + m;
    Out(mo)=Out(meshes{m,1});
    crd = meshes{m,2};
    XInfo = FI.Dataset(crd(1));
    YInfo = FI.Dataset(crd(2));
    %
    if nmesh==0 && size(meshes,1)==1
        name = 'grid';
    else
        xname = XInfo.Name;
        yname = YInfo.Name;
        name = ['grid (' xname ,', ', yname ')'];
    end
    Out(mo).Name = name;
    Out(mo).Units = '';
    Out(mo).NVal = 0;
    Out(mo).varid = crd-1;
    %
    Out(mo).DimName = cell(1,5);
    Out(mo).DimFlag = ~isnan(XInfo.TSMNK) | ~isnan(YInfo.TSMNK);
    for d = 1:length(XInfo.TSMNK)
        if ~isnan(XInfo.TSMNK(d))
            Out(mo).DimName{d} = FI.Dimension(XInfo.TSMNK(d)+1).Name;
        elseif ~isnan(YInfo.TSMNK(d))
            Out(mo).DimName{d} = FI.Dimension(YInfo.TSMNK(d)+1).Name;
        end
    end
    if Out(mo).DimFlag(M_) && Out(mo).DimFlag(N_)
        Out(mo).Geom = '';
    else
        Out(mo).Geom = 'PNT';
    end
    Out(mo).hasCoords   = 1;
    if isempty(XInfo.XBounds)
        Out(mo).ClosedPoly = 0;
    else
        Out(mo).Geom = 'POLYG';
        Out(mo).ClosedPoly = 1;
    end
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
[Out.AppendName] = deal('');
for i = 1:length(Out)
    if iscell(Out(i).varid)
        %TODO
    elseif ~isempty(Out(i).varid)
        Info = FI.Dataset(Out(i).varid(1)+1);
        for j = 1:length(Info.Attribute)
            if strcmp(Info.Attribute(j).Name,'cell_methods')
                Out(i).AppendName = [' - ' Info.Attribute(j).Value];
            end
        end
    end
end
%
for i = 1:length(Out)
   if strcmp(Out(i).Geom,'PNT')
      Out(i).AppendName = [Out(i).AppendName ' (points)'];
   elseif strncmp(Out(i).Geom,'UGRID',5)
      %Out(i).AppendName = [Out(i).AppendName ' (' lower(Out(i).Geom) ')'];
   end
end
%
Meshes = zeros(0,2);
for loop = 1:2
    for i = 1:length(Out)
        switch Out(i).Geom
            case {'UGRID1D_NETWORK-NODE','UGRID1D_NETWORK-EDGE','UGRID1D-NODE','UGRID1D-EDGE','UGRID2D-NODE','UGRID2D-EDGE','UGRID2D-FACE'}
                varid = get_varid(Out(i))+1;
                thisMesh = FI.Dataset(varid).Mesh;
                if loop == 1
                    if thisMesh{4} == -1 && Out(i).NVal == 0
                        Meshes(end+1,:) = [thisMesh{3} i];
                    end
                else % loop == 2
                    if thisMesh{4} == -1
                        Out(i).UseGrid = i;
                    else
                        j = find(Meshes(:,1) == thisMesh{3});
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
%
% detect vector quantities
%
% VecStdNameTable: component_1, component_2, vector_type, quickplot_combined_name
%   vector_type = 0: x and y
%                 4: magnitude and direction
%                 5: normal and tangential (on ugrid edge)
VecStdNameTable = {
    'sea_water_speed',               'direction_of_sea_water_velocity',4,'sea_water_velocity'
    'sea_ice_speed',                 'direction_of_sea_ice_speed',     4,'sea_ice_velocity'
    'wind_speed',                    'wind_to_direction',              4,'air_velocity'
    'eastward_sea_water_velocity',   'northward_sea_water_velocity',   0,'sea_water_velocity'
    'sea_water_x_velocity',          'sea_water_y_velocity',           0,'sea_water_velocity'
    'eastward_sea_ice_velocity',     'northward_sea_ice_velocity',     0,'sea_ice_velocity'
    'eastward_wind_shear',           'northward_wind_shear',           0,'wind_shear'
    'surface_downward_eastward_wind','surface_downward_northward_wind',0,'surface_downward_wind'
    'eastward_wind',                 'northward_wind',                 0,'air_velocity'};
nVecStdNames = size(VecStdNameTable,1);
i=1;
varid_Out = get_varid(Out);
while i<length(varid_Out)
    if iscell(Out(i).varid) || isempty(Out(i).varid)
        i=i+1;
        continue
    end
    %
    % own convention has preference over CF convention of standard names
    % since there may be multiple quantities with the same standard name
    % (but defined on different meshes or derived using different cell
    % methods).
    %
    y=[];
    ncmp = strfind(Out(i).Name,', n-component');
    xcmp = strfind(Out(i).Name,', x-component');
    if ~isempty(ncmp)
        Ystr = Out(i).Name; Ystr(ncmp+2)='t';
        Name = Out(i).Name([1:ncmp-1 ncmp+13:end]);
        j=1; j2=2;
        VectorDef = 5; % normal and tangential
        %
        names = {Out.Name};
        y = find(strcmp(names,Ystr) & strcmp({Out.AppendName},Out(i).AppendName));
        if length(y)>1
            y = [];
        end
    elseif ~isempty(xcmp)
        Ystr = Out(i).Name; Ystr(xcmp+2)='y';
        Name = Out(i).Name([1:xcmp-1 xcmp+13:end]);
        j=1; j2=2;
        VectorDef = 0; % x and y
        %
        names = {Out.Name};
        y = find(strcmp(names,Ystr) & strcmp({Out.AppendName},Out(i).AppendName));
        if length(y)>1
            y = [];
        end
    end
    %
    if isempty(y)
        % no (unique) match found yet
        stdname = FI.Dataset(Out(i).varid+1).StdName;
        j = strmatch(stdname,VecStdNameTable(:,1:2),'exact');
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
            %
            ii = i+1:length(Out);
            varid = varid_Out(ii);
            ii(varid<0) = [];
            varid(varid<0) = [];
            stdnames = {FI.Dataset(varid+1).StdName};
            y = ii(strcmp(stdnames,Ystr) & strcmp({Out(ii).AppendName},Out(i).AppendName));
            if length(y)>1
                y = []; %y(1);
            end
        end
    end
    %
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
        varid_Out(y) = [];
        if y<i
            i=i-1;
        end
    end
    i=i+1;
end
%
for i = 1:length(Out)
    Out(i).Name = [Out(i).Name Out(i).AppendName];
end
Out = rmfield(Out,'AppendName');
% -----------------------------------------------------------------------------


function ivar = get_varid(Props)
ivar = repmat(-1,size(Props));
for i = 1:numel(Props)
    if iscell(Props(i).varid)
        ivar(i) = Props(i).varid{2};
    elseif ~isempty(Props(i).varid)
        ivar(i) = Props(i).varid(1);
    end
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
            XVar = FI.Dataset(Info.Mesh{3}).X;
            % get the node dimension
            dimNodes = FI.Dataset(XVar).TSMNK(3)+1;
            sz(3) = FI.Dimension(dimNodes).Length;
        case 'node_index'
            Info = FI.Dataset(Props.varid{2}+1);
            if Info.Mesh{2}<0
                sz(3) = 123;
            else
                sz(3) = FI.Dimension(strcmp({FI.Dimension.Name},Info.Mesh{5})).Length;
            end
        case 'edge_index'
            Info = FI.Dataset(Props.varid{2}+1);
            sz(3) = FI.Dimension(strcmp({FI.Dimension.Name},Info.Mesh{6})).Length;
        case 'face_index'
            Info = FI.Dataset(Props.varid{2}+1);
            sz(3) = FI.Dimension(strcmp({FI.Dimension.Name},Info.Mesh{7})).Length;
        otherwise
            Props.varid = Props.varid{2};
            sz = getsize(FI,Props);
    end
elseif ~isempty(Props.varid)
    for q = 1:length(Props.varid)
        Info=FI.Dataset(Props.varid(q)+1);
        for d_ = 1:ndims
            if Props.DimFlag(d_) && Info.TSMNK(d_)>=0
                sz(d_) = FI.Dimension(Info.TSMNK(d_)+1).Length;
            end
        end
    end
end
%======================== SPECIFIC CODE =======================================

% -----------------------------------------------------------------------------
function Domains=domains(FI)
if FI.NumDomains > 1
    format = sprintf('%%%d.%dd-',FI.DomainCount.Digits,FI.DomainCount.Digits);
    Domains = multiline(sprintf(['partition ' format],FI.DomainCount.Offset+(0:FI.NumDomains-1)),'-','cell');
    Domains{end} = 'all partitions';
else
    Domains = {};
end


% -----------------------------------------------------------------------------
function T=readtim(FI,Props,t)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
varid = get_varid(Props)+1;
tvar = FI.Dataset(varid).Time;
if isempty(tvar)
    tinfo = [];
    T = [];
else
    tinfo = FI.Dataset(tvar).Info;
    if nargin>2
        if isequal(t,0)
            T = nc_varget(FI.Filename,FI.Dataset(tvar).Name);
        elseif length(t)==1
            T = nc_varget(FI.Filename,FI.Dataset(tvar).Name,t-1,1);
        elseif isequal(t,t(1):t(2)-t(1):t(end))
            T = nc_varget(FI.Filename,FI.Dataset(tvar).Name,t(1)-1,(t(end)-t(1))/(t(2)-t(1))+1,t(2)-t(1));
        else
            T = nc_varget(FI.Filename,FI.Dataset(tvar).Name,t(1)-1,t(end)-t(1)+1);
            T = T(t-t(1)+1);
        end
    else
        T = nc_varget(FI.Filename,FI.Dataset(tvar).Name);
    end
    T = double(T);
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
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props,t)
%======================== SPECIFIC CODE =======================================
stcrd = FI.Dataset(get_varid(Props)+1).Station;
if FI.Dataset(stcrd).CharDim==FI.Dataset(stcrd).Dimid(1)
    % PRESERVE_FVD=true
    [Stations, status] = qp_netcdf_get(FI,stcrd-1,fliplr(FI.Dataset(stcrd).Dimension));
else
    [Stations, status] = qp_netcdf_get(FI,stcrd-1,FI.Dataset(stcrd).Dimension);
end
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
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function szZData  = updateSize(szData,szFld,hdims)
szZData = szData;
szZData(hdims) = szFld(hdims);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [X,Y,EdgeX,EdgeY] = branch2xy(BrX,BrY,xUnit,BrL,BrNr,BrOffset,eBrNr,EdgeNode)
EdgeX = cell(size(eBrNr));
EdgeY = EdgeX;
X = zeros(size(BrNr));
Y = X;
if strcmp(xUnit,'deg')
    cUnit = {'Geographic'};
else
    cUnit = {};
end
uBrNr = unique(eBrNr);
doublePoints = false(size(uBrNr));
for i = 1:length(uBrNr)
    bN = uBrNr(i);
    bX = BrX{bN};
    bY = BrY{bN};
    Mask = diff(bX)==0 & diff(bY)==0;
    if any(Mask)
        doublePoints(i) = true;
        bX(Mask)=[];
        bY(Mask)=[];
    end
    bS = pathdistance(bX,bY,cUnit{:});
    %
    for j = find(BrNr==bN)'
        s  = (BrOffset(j)/BrL(bN))*bS(end);
        if s>bS(end)
            error('Offset %g larger than branch length %g',BrOffset(j),BrL(bN));
        else
            x = interp1(bS,bX,s);
            y = interp1(bS,bY,s);
        end
        X(j) = x;
        Y(j) = y;
    end
    %
    for j = find(eBrNr==bN)'
        n = EdgeNode(j,:);
        nBranches = BrNr(n);
        if all(nBranches==bN)
            % both nodes on this branch, select the segment
            s  = (sort(BrOffset(n))/BrL(bN))*bS(end);
            I = bS>s(1) & bS<s(2);
            x = interp1(bS,bX,s);
            y = interp1(bS,bY,s);
            EdgeX{j} = [x(1);bX(I);x(2)];
            EdgeY{j} = [y(1);bY(I);y(2)];
        elseif all(nBranches~=bN)
            % both nodes on other branches, select the whole branch
            EdgeX{j} = bX;
            EdgeY{j} = bY;
        elseif nBranches(1)==bN
            % second node on other branch ...
            % assume we need the end part of the branch
            n = n(1);
            s  = (BrOffset(n)/BrL(bN))*bS(end);
            I = bS>s;
            x = interp1(bS,bX,s);
            y = interp1(bS,bY,s);
            EdgeX{j} = [x;bX(I)];
            EdgeY{j} = [y;bY(I)];
        else % nBranches(2)==bN
            % first node on other branch ...
            % assume we need the beginning of the branch
            n = n(2);
            s  = (BrOffset(n)/BrL(bN))*bS(end);
            I = bS<s;
            x = interp1(bS,bX,s);
            y = interp1(bS,bY,s);
            EdgeX{j} = [bX(I);x];
            EdgeY{j} = [bY(I);y];
            % first node on other branch ...
        end
    end
end
if any(doublePoints)
    if sum(doublePoints)==1
        ui_message('warning','Double geometry points encountered on branch: %i',find(doublePoints))
    else
        ui_message('warning','Double geometry points encountered on branches: %s',vec2str(find(doublePoints),'nobrackets'))
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function unit = get_unit(Info)
unit = [];
if ~isempty(Info.Attribute)
    Attribs = {Info.Attribute.Name};
    j = strmatch('units',Attribs,'exact');
    if ~isempty(j)
        unit = Info.Attribute(j).Value;
        units = {'degrees_east','degree_east','degreesE','degreeE', ...
            'degrees_north','degree_north','degreesN','degreeN'};
        if ismember(unit,units)
            unit = 'deg';
        end
    end
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function [BrX,BrY,xUnit,BrL] = get_edge_geometry(FI,csp)
CSP = FI.Dataset(csp);
atteg = strmatch('edge_geometry',{CSP.Attribute.Name});
veg = strmatch(CSP.Attribute(atteg).Value,{FI.Dataset.Name},'exact');
% node count dimension
VEG = FI.Dataset(veg);
attnc = strmatch('node_count',{VEG.Attribute.Name});
ndc = [];
if ~isempty(attnc)
    ndc = strmatch(VEG.Attribute(attnc).Value,{FI.Dataset.Name},'exact');
    ndcd = strmatch(VEG.Attribute(attnc).Value,{FI.Dimension.Name},'exact');
    if isempty(ndc)
        if ~isempty(ndcd)
            ui_message('error','Geometry %s attribute node_count reads "%s". This is a dimension, but should be a variable.',VEG.Name,VEG.Attribute(attnc).Value)
        else
            ui_message('error','Geometry %s attribute node_count reads "%s". Variable not found.',VEG.Name,VEG.Attribute(attnc).Value)
        end
    end
end
if isempty(ndc)
    attnc = strmatch('part_node_count',{VEG.Attribute.Name});
    ui_message('error','Incorrect attribute "part_node_count" used for specifying the node_count for geometry variable "%s".',VEG.Name)
    ndc = strmatch(VEG.Attribute(attnc).Value,{FI.Dataset.Name},'exact');
end
%
if isempty(FI.Dataset(veg).X)
    error('Missing X coordinate for geometry variable "%s".',VEG.Name)
elseif isempty(FI.Dataset(veg).Y)
    error('Missing Y coordinate for geometry variable "%s".',VEG.Name)
end
[BrX, status] = qp_netcdf_get(FI,FI.Dataset(FI.Dataset(veg).X));
[BrY, status] = qp_netcdf_get(FI,FI.Dataset(FI.Dataset(veg).Y));
[NDC, status] = qp_netcdf_get(FI,FI.Dataset(ndc));
BrX = mat2cell(BrX,NDC,1);
BrY = mat2cell(BrY,NDC,1);
%
xUnit = get_unit(FI.Dataset(FI.Dataset(veg).X));
%
if nargout>3
    attbl = strmatch('edge_length',{CSP.Attribute.Name});
    if isempty(attbl)
        attbl = strmatch('branch_lengths',{CSP.Attribute.Name});
        if ~isempty(attbl)
            ui_message('error','Incorrect attribute "branch_lengths" used for specifying the edge_length for 1D UGRID variable "%s".',CSP.Name)
        end
    end
    if ~isempty(attbl)
        vbl = strmatch(CSP.Attribute(attbl).Value,{FI.Dataset.Name},'exact');
        [BrL, status] = qp_netcdf_get(FI,FI.Dataset(vbl));
    else
        BrL = zeros(size(BrX));
        for i = 1:length(BrX)
            brl = pathdistance(BrX{i},BrY{i}); % Cartesian or spherical?
            BrL(i) = brl(end);
        end
    end
end


% -----------------------------------------------------------------------------
function [NewFI,cmdargs]=options(FI,mfig,cmd,varargin)
T_=1; ST_=2; M_=3; N_=4; K_=5;
%======================== SPECIFIC CODE =======================================
Inactive=get(0,'defaultuicontrolbackground');
Active=[1 1 1];
NewFI=FI;
cmd=lower(cmd);
cmdargs={};
switch cmd
    case 'initialize'
        optfig(mfig);
        set(findobj(mfig,'tag','ncdump'),'enable','on')
        set(findobj(mfig,'tag','ncdumpto=?'),'enable','on','backgroundcolor',Active)
    case 'ncdump'
        out = get(findobj(mfig,'tag','ncdumpto=?'),'value');
        switch out
            case 1
                [f,p] = uiputfile('*.ncdump','Specify Dump File');
                if ischar(f)
                    fid = fopen([p,f],'w');
                    nc_dump(FI.FileName,fid)
                    fclose(fid);
                end
            case {2,3}
                f = tempname;
                fid = fopen(f,'w');
                nc_dump(FI.FileName,fid);
                fclose(fid);
                C = getfile(f);
                delete(f);
                if out==2
                    clipboard('copy',sprintf('%s\n',C{:}));
                else
                    C = strrep(C,sprintf('\t'),'   ');
                    C(cellfun(@isempty,C)) = {' '};
                    ui_message('message',C);
                end
        end
    otherwise
        error(['Unknown option command: ',cmd])
end
% -----------------------------------------------------------------------------

% -----------------------------------------------------------------------------
function optfig(h0)
Inactive=get(0,'defaultuicontrolbackground');
FigPos=get(h0,'position');
FigPos(3:4) = getappdata(h0,'DefaultFileOptionsSize');
set(h0,'position',FigPos)

voffset=FigPos(4)-30;
uicontrol('Parent',h0, ...
    'Style','pushbutton', ...
    'BackgroundColor',Inactive, ...
    'Callback','d3d_qp fileoptions ncdump', ...
    'Position',[11 voffset-3 140 24], ...
    'String','NetCDF Dump to', ...
    'Horizontalalignment','left', ...
    'Enable','off', ...
    'Tag','ncdump');
uicontrol('Parent',h0, ...
    'Style','popupmenu', ...
    'BackgroundColor',Inactive, ...
    'Position',[161 voffset 170 20], ...
    'String',{'File','Clipboard','Message Window'}, ...
    'Enable','off', ...
    'Tag','ncdumpto=?');
% -----------------------------------------------------------------------------

function C = getfile(file)
if ischar(file)
    localfopen = true;
    fid = fopen(file,'r');
else
    localfopen = false;
    fid = file;
end
C = cell(1000,1);
i = 0;
while 1
    L = fgetl(fid);
    if ischar(L)
        i = i+1;
        if i>length(C)
            C{2*i} = [];
        end
        C{i} = L;
    else
        break
    end
end
C = C(1:i);
if localfopen
    fclose(fid);
end