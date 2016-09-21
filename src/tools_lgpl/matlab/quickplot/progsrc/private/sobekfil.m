function varargout=sobekfil(FI,domain,field,cmd,varargin)
%SOBEKFIL QP support for SOBEK-RE and Rural/Urban/River network and output files.
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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/sobekfil.m $
%   $Id: sobekfil.m 5295 2015-07-25 05:45:18Z jagers $

%========================= GENERAL CODE =======================================

T_=1; ST_=2; M_=3; N_=4; K_=5;

if nargin<2
    error('Not enough input arguments')
elseif nargin==2
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
        varargout={readsts(FI,Props,varargin{:})};
        return
    case 'subfields'
        varargout={{}};
        return
    case 'plot'
        Parent=varargin{1};
        Ops=varargin{2};
        switch Props.Name
            case 'network*'
                hNew=sobek('plot',FI);
                recolor(hNew,'b',Ops.colour)
        end
        varargout={hNew FI};
        return
    otherwise
        [XYRead,DataRead,DataInCell]=gridcelldata(cmd);
end

DimFlag=Props.DimFlag;

% initialize and read indices ...
idx={[] [] 0 [] []};
fidx=find(DimFlag);
idx(fidx(1:length(varargin)))=varargin;
sz=getsize(FI,Props);
if isempty(idx{T_})
    idx{T_}=sz(T_);
end
if idx{M_}==0
    idx{M_}=1:sz(M_);
end
if idx{ST_}==0
    idx{ST_}=1:sz(ST_);
end

Ans.XY = [];
Ans.SEG = zeros(0,2);
Ans.ValLocation = Props.Geom(5:end);
if strcmp(FI.FileType,'SOBEK River network')
    % Always ValLocation = 'NODE'
    if strcmp(Props.Name,'network (nodes only)')
        Ans.XY=FI.Node.XY;
        Ans.SEG=cat(2,FI.Branch.IFrom,FI.Branch.ITo);
    else
        Ans.XY=cat(1,FI.Branch.XY{FI.Grid.IBranch});
        BrLen=cellfun('size',FI.Branch.XY,1)-1;
        BrFirstPoint = zeros(1,FI.nBranches);
        BrLastPoint = zeros(1,FI.nBranches);
        SEG = zeros(sum(BrLen),2);
        offset = 0;
        Noffset = 0;
        for i = FI.Grid.IBranch
            BL = BrLen(i);
            SEG(offset+(1:BL),1) = Noffset+(1:BL)';
            offset = offset+BL;
            BrFirstPoint(i) = Noffset+1;
            BrLastPoint(i)  = Noffset+BL+1;
            %
            Noffset = Noffset+BL+1;
        end
        SEG(:,2)=SEG(:,1)+1;
        %
        for i=length(FI.Node.ID):-1:1
            Points = [BrFirstPoint(FI.Branch.IFrom==i) BrLastPoint(FI.Branch.ITo==i)];
            nodeSEG{i} = zeros(length(Points)*(length(Points)-1),2);
            j=0;
            for p1 = Points
                for p2 = Points(2:end)
                    j=j+1;
                    nodeSEG{i}(j,:) = [p1 p2];
                end
            end
        end
        SEG = cat(1,SEG,nodeSEG{:});
        %
        Ans.SEG=SEG;
        %
        Renum = zeros(FI.nNodes,1);
        Renum(idx{M_}) = 1:length(idx{M_});
        Ans.XY = Ans.XY(idx{M_},:);
        Ans.SEG = Renum(Ans.SEG);
        Ans.SEG(any(Ans.SEG==0,2),:) = [];
        %
        DataIndex = idx{M_};
        NodeHasData = 1:length(idx{M_});
    end
elseif strcmp(Ans.ValLocation,'NODE')
    if Props.DFil==-1
        nodetype = strtok(Props.Name);
        if strcmp(nodetype,'all')
            inodes=idx{M_};
        else
            inodes=strmatch(nodetype,FI.Node.Type);
            inodes=inodes(idx{ST_});
        end
    elseif Props.NVal==0
        inodes=idx{M_};
    else
        inodes=idx{M_};
        HisFile=FI.Data{Props.DFil};
        [NodeHasData,DataIndex]=ismember(FI.Node.ID(idx{M_}),HisFile.SegmentName);
        DataIndex(~NodeHasData)=[];
    end
    Ans.XY=FI.Node.XY(inodes,:);
    if DimFlag(M_)
        Ans.SEG=cat(2,FI.Branch.IFrom,FI.Branch.ITo);
        Renum=zeros(FI.nNodes,1);
        Renum(inodes)=1:length(inodes);
        Ans.SEG=Renum(Ans.SEG);
        Ans.SEG(any(Ans.SEG==0,2),:)=[];
    end
elseif strcmp(Ans.ValLocation,'EDGE')
    Ans.XY=FI.Node.XY;
    iedges=idx{M_};
    Ans.SEG=cat(2,FI.Branch.IFrom(iedges,1),FI.Branch.ITo(iedges,1));
    if Props.DFil~=-2
        %
        % Process whole network since some of the selected edges may be
        % linked to the his-file content via edges not selected.
        %
        HisFile=FI.Data{Props.DFil};
        [EdgeHasData,DataIndex]=ismember(FI.Branch.ID,HisFile.SegmentName);
        %
        found = true;
        while found
            found = false;
            for i = find(~EdgeHasData)'
                ifn = FI.Branch.IFrom(i);
                if ismember(FI.Node.Type(ifn), ...
                        {'SBK_PROFILE','SBK_MEASSTAT','SBK_SBK-3B-REACH','SBK_WEIR','SBK_CULVERT','SBK_PUMP','SBK_LATERALFLOW'})
                    ibr = find(EdgeHasData & FI.Branch.ITo==ifn);
                    if ~isempty(ibr)
                        EdgeHasData(i) = true;
                        DataIndex(i) = DataIndex(ibr);
                        found = true;
                    end
                end
            end
        end
        %
        % Now clip data set to selected edges
        %
        EdgeHasData = EdgeHasData(iedges);
        DataIndex = DataIndex(iedges);
        DataIndex(~EdgeHasData) = [];
    end
end

if Props.DFil==-1
    switch Props.Name
        case 'all nodes: ID'
            fld='ID';
        case 'all nodes: name'
            fld='Name';
        case 'all nodes: type'
            fld='Type';
        otherwise
            fld='ID';
    end
    Ans.Val=FI.Node.(fld)(inodes)';
elseif Props.DFil==-2
    switch Props.Name
        case 'all reach segments: ID'
            fld='ID';
        case 'all reach segments: type'
            fld='Type';
        case 'all reach segments: name'
            fld='Name';
        case 'all reach segments: reach number'
            fld='BrReach';
    end
    Ans.Val=FI.Branch.(fld)(iedges);
elseif Props.NVal>0
    HisFile=FI.Data{Props.DFil};
    Ans.Val = NaN(length(idx{T_}),length(idx{M_}));
    if strcmp(Ans.ValLocation,'NODE')
        [t,Ans.Val(:,NodeHasData)]=delwaq('read',HisFile,Props.Subs,DataIndex,idx{T_});
    else
        [t,Ans.Val(:,EdgeHasData)]=delwaq('read',HisFile,Props.Subs,DataIndex,idx{T_});
    end
    Ans.Time=t;
end

varargout={Ans FI};
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function Out=infile(FI,domain)
%
%======================== SPECIFIC CODE =======================================
PropNames={'Name'                           'Geom'  'Coords' 'DimFlag' 'DataInCell' 'NVal' 'DFil' 'Subs' 'UseGrid'};
%          'network*'                       'SEG-NODE' 'xy' [0 0 4 0 0]   0          -1      0      0       0
DataProps={'network'                        'SEG-NODE' 'xy' [0 0 6 0 0]   0           0      0      0       1};
if strcmp(FI.FileType,'SOBEK River network')
    DataProps(2,:)=DataProps(1,:);
    DataProps{2,1}='network (nodes only)';
    DataProps{2,9}=2;
else
    nodetypes = unique(FI.Node.Type);
    DataProps(2,:)=DataProps(1,:);
    DataProps{2,1}='all nodes: ID';
    DataProps{2,6}=4;
    DataProps{2,7}=-1;
    DataProps(3,:)=DataProps(2,:);
    DataProps{3,1}='all nodes: name';
    DataProps(4,:)=DataProps(2,:);
    DataProps{4,1}='all nodes: type';
    i0 = size(DataProps,1);
    for i=length(nodetypes):-1:1
        DataProps(i+i0,:)=DataProps(1,:);
        DataProps{i+i0,1}=[nodetypes{i} ' nodes'];
        DataProps{i+i0,4}=[0 3 0 0 0];
        DataProps{i+i0,6}=4;
        DataProps{i+i0,7}=-1;
        DataProps{i+i0,9}=0;
    end
    DataProps(end+1,:)=DataProps(1,:);
    DataProps{end,1}='all reach segments: ID';
    DataProps{end,2}='SEG-EDGE';
    DataProps{end,5}=1;
    DataProps{end,6}=4;
    DataProps{end,7}=-2;
    DataProps(end+1,:)=DataProps(end,:);
    DataProps{end,1}='all reach segments: name';
    DataProps(end+1,:)=DataProps(end,:);
    DataProps{end,1}='all reach segments: type';
    DataProps(end+1,:)=DataProps(end,:);
    DataProps{end,1}='all reach segments: reach number';
    DataProps{end,6}=1;
end
for i=1:length(FI.Data)
    [pn,fn]=fileparts(FI.Data{i}.FileName);
    fn=lower(fn);
    DataProps(end+1,:)={'-------'                  ''         ''   [0 0 0 0 0]   0           0      0      0       0};
    for j=1:length(FI.Data{i}.SubsName)
        DataProps(end+1,:)={FI.Data{i}.SubsName{j} 'SEG-NODE' 'xy' [1 0 6 0 0]   0           1      i      j       1};
        if strcmp(fn,'reachseg')
            DataProps{end,2} = 'SEG-EDGE';
            DataProps{end,5}  = 1; % DataInCell
        end
    end
end
Out=cell2struct(DataProps,PropNames,2);
%======================== SPECIFIC CODE REMOVE ================================

%======================== SPECIFIC CODE ADD ===================================

% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function sz=getsize(FI,Props)
T_=1; ST_=2; M_=3; N_=4; K_=5;
sz=[0 0 0 0 0];
if Props.DimFlag(T_)
    FIH=FI.Data{Props.DFil};
    sz(T_)=FIH.NTimes;
    if strcmp(Props.Geom(5:end),'NODE')
        sz(M_)=FI.nNodes; %length(FI.Node.ID); %FIH.NumSegm;
    else % EDGE
        sz(M_)=FI.nBranches; %length(FI.Branch.ID);
    end
else
    if strcmp(FI.FileType,'SOBEK River network')
        if strcmp(Props.Name,'network (nodes only)')
            sz(M_)=length(FI.Node.ID);
        else
            sz(M_)=sum(cellfun('size',FI.Branch.XY,1));
        end
    elseif Props.DFil==-1
        nodetype = strtok(Props.Name);
        if strcmp(nodetype,'all')
            sz(M_)=length(FI.Node.Type);
        else
            sz(ST_)=length(strmatch(nodetype,FI.Node.Type));
        end
    elseif Props.DFil==-2
        sz(M_)=length(FI.Branch.ID);
    else
        sz(M_)=length(FI.Node.ID);
    end
end
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function [T,Tsc]=readtim(FI,Props,t)
%======================== SPECIFIC CODE =======================================
T=delwaq('read',FI.Data{Props.DFil},1,1,t);
% -----------------------------------------------------------------------------


% -----------------------------------------------------------------------------
function S=readsts(FI,Props)
%======================== SPECIFIC CODE =======================================
switch Props.DFil
    case -1
        inodes=strmatch(strtok(Props.Name),FI.Node.Type);
        S=FI.Node.ID(inodes);
    case -2
        S=FI.Branch.ID;
        for i=1:length(S)
            ID = S{i};
            Name = FI.Branch.Name{i};
            if isempty(ID)
                S{i} = Name;
            elseif ~isempty(Name)
                S{i} = [ID ':' Name];
            end
        end
end
% -----------------------------------------------------------------------------
