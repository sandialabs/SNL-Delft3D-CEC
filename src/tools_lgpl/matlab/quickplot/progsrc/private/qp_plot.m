function [hNewVec,Error,FileInfo,PlotState]=qp_plot(PlotState)
%QP_PLOT Plot function of QuickPlot.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_plot.m $
%   $Id: qp_plot.m 5590 2015-11-16 10:05:03Z jagers $

T_=1; ST_=2; M_=3; N_=4; K_=5;

hNewVec=0;
Error=1;
FileInfo=PlotState.FI;

Domain=PlotState.Domain;
Props=PlotState.Props;
SubField=PlotState.SubField;
Selected=PlotState.Selected;
Parent=PlotState.Parent;
hOld=PlotState.Handles;
stats=PlotState.Stations;
Ops=PlotState.Ops;

if iscell(hOld)
    hOldVec=cat(1,hOld{:});
else
    hOldVec=hOld;
    hOld={hOld};
end
DimFlag=Props.DimFlag;

Ops=qp_state_version(Ops);
if isfield(Ops,'horizontalalignment') && isequal(Ops.horizontalalignment,'centre')
    Ops.horizontalalignment='center';
end

%
% Determine all objects in the axes. If this is a new plot then the new
% object will be added on top, but if this is an update of an existing
% object then the new object will be located at the location of the old
% object in the object stack.
%
hNewTag='';
lParents=get(hOldVec(ishandle(hOldVec)),'Parent');
if iscell(lParents)
    lParents=unique([lParents{:}]);
end
nParents = length(lParents);
%
% PchildBefore lists objects plotted on top of this object.
% PchildAfter lists objects plotted below this object.
%
PchildBefore = cell(1,nParents);
PchildAfter = cell(1,nParents);
SortObjs=1;
for lP=1:nParents
    Pchild=allchild(lParents(lP));
    if isempty(hOldVec) || ~ishandle(hOldVec(1))
        %
        % If there is no old object, then plot new object on top.
        %
        PchildBefore{lP}=[];
        PchildAfter{lP}=Pchild;
    else
        hNewTag=get(hOldVec(1),'tag');
        IdxObj=find(Pchild==hOldVec(1));
        if isempty(IdxObj)
            %
            % If there is an old object, but not in the current Parent then
            % switch off object sorting.
            %
            SortObjs=0;
            PchildBefore{lP}=[];
            PchildAfter{lP}=[];
        else
            %
            % Old object exists in current axes, identify objects before
            % and after this object.
            %
            PchildBefore{lP}=Pchild(1:IdxObj);
            PchildBefore{lP}=PchildBefore{lP}(~ismember(PchildBefore{lP},hOldVec));
            PchildAfter{lP}=Pchild(IdxObj:end);
            PchildAfter{lP}=PchildAfter{lP}(~ismember(PchildAfter{lP},hOldVec));
        end
    end
end
Level = -1;
for i = 1:length(hOldVec)
    if ishandle(hOldVec(i))
        iLevel = getappdata(hOldVec(i),'Level');
        if ~isempty(iLevel)
            Level = iLevel;
        end
    end
end
if Level<0
    Level = 0;
    Pchild=allchild(Parent);
    for i = 1:length(Pchild)
        iLevel = getappdata(Pchild(i),'Level');
        if ~isempty(iLevel)
            Level = max(Level,iLevel);
        end
    end
    Level = Level+500;
end
Thresholds=[]; % Thresholds is predefined to make sure that Thresholds always exists when its value is checked at the end of this routine

for i=5:-1:1
    multiple(i) = (length(Selected{i})>1) | isequal(Selected{i},0);
end

SubSelected=Selected;
SubSelected(~DimFlag)=[];
FT=FileInfo.FileType;
if isfield(Props,'MNK') && Props.MNK
    Props.MNK = xyz_or_mnk(Ops,Selected,Props.MNK);
end

DataInCell=0;
if Props.NVal<0
    data=[];
else
    if isfield(Ops,'extend2edge') && Ops.extend2edge
        [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'griddefdata',SubField{:},SubSelected{:});
    else
        switch Ops.presentationtype
            case {'patches','patches with lines','patch centred vector','polygons'}%,'edge'}
                [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'gridcelldata',SubField{:},SubSelected{:});
                DataInCell=1;
            otherwise
                [Chk,data,FileInfo]=qp_getdata(FileInfo,Domain,Props,'griddata',SubField{:},SubSelected{:});
        end
    end
    if isempty(data)
        ui_message('error','Did not get any data from %s file.',FT)
        return
    elseif ~Chk
        ui_message('error','Error retrieving data from file.');
        return
    end
end

if isfield(Ops,'axestimezone_shift') && ~isnan(Ops.axestimezone_shift)
    [Chk,datatimezone_shift,datatimezone_str] = qp_getdata(FileInfo,Domain,Props,'timezone');
    if isnan(datatimezone_shift)
        error('Cannot convert unknown time zone to %s',Ops.axestimezone_str)
    else
        for i = length(data):-1:1
            data(i).Time = data(i).Time + (Ops.axestimezone_shift-datatimezone_shift)/24;
        end
    end
end

Quant=Props.Name;
Units='';
if ~isempty(data)
    Units=data(1).Units;
end
%
if isfield(Ops,'units')
    if isequal(Ops.units,'**Hide**')
        Units='';
    elseif isfield(data,'XComp')
        % data conversion of vector component will be done in computecomponent
        if isempty(Ops.units) % default values
            if (isfield(Ops,'vectorcomponent') && strcmp(Ops.vectorcomponent,'angle')) || ...
                    (isfield(Ops,'vectorcolour') && strcmp(Ops.vectorcolour,'angle'))
                Units='radians';
            else
                % data units used above should be correct
            end
        else
            Units=Ops.units;
        end            
    elseif ~isempty(Ops.units) && ~isempty(Units)
        dataX=qp_unitconversion(Units,Ops.units,data);
        if ~ischar(dataX)
            data=dataX;
            dataX=[];
            Units=data(1).Units;
        end
    end
end

Ops.basicaxestype = Ops.axestype;
if ~isempty(strfind(Props.Name,'level')) && ...
        (~ischar(qp_unitconversion(Units,'m')) || ...
        ~ischar(qp_unitconversion(Units,'')))
    Ops.axestype = strrep(Ops.axestype,'Z',['Z [',Units,']']);
elseif ~isempty(strfind(Ops.axestype,'Val'))
    Ops.axestype = strrep(Ops.axestype,'Val',['Val [',Units,']']);
end

FirstFrame=isempty(hOldVec);

if strcmp(Ops.presentationtype,'vector') || ...
        strcmp(Ops.presentationtype,'markers') || ...
        strcmp(Ops.presentationtype,'values')
    % data = geom2pnt(data);
    if isfield(data,'ValLocation')
        if strcmp(data.ValLocation,'EDGE')
            if isfield(data,'Geom') && strcmp(data.Geom,'sQUAD')
                data.EdgeNodeConnect = [1:length(data.X)-1;2:length(data.X)]';
            end
            data.X = mean(data.X(data.EdgeNodeConnect),2);
            data.Y = mean(data.Y(data.EdgeNodeConnect),2);
        elseif strcmp(data.ValLocation,'FACE')
            missing = isnan(data.FaceNodeConnect);
            nNodes = size(missing,2)-sum(missing,2);
            data.FaceNodeConnect(missing) = 1;
            data.X = data.X(data.FaceNodeConnect);
            data.X(missing) = 0;
            data.X = sum(data.X,2)./nNodes;
            data.Y = data.Y(data.FaceNodeConnect);
            data.Y(missing) = 0;
            data.Y = sum(data.Y,2)./nNodes;
        end
        for c = {'FaceNodeConnection','EdgeNodeConnection','ValLocation'}
            s = c{1};
            if isfield(data,s)
                data = rmfield(data,s);
            end
        end
        data.Geom = 'sSEG';
    end
end

[data,s] = qp_thinning(data,Ops);

vpt='';
[NVal,NValStr]=convertnval(Props.NVal);
if NVal==0.6 || NVal==0.9
    % 0.6 = thindam optionally coloured
    % 0.9 = coloured thindam
    NVal=0.5;
elseif  NVal==1.9 
    if isequal(Ops.presentationtype,'edge')
        % 1.9 = coloured thindam or vector perpendicular to thindam
        NVal=0.5;
    else
        % vector case: vector location is determined by computecomponent
        NVal=2;
        data.XComp = data.XDamVal;
        data.YComp = data.YDamVal;
        data = rmfield(data,{'XDam','YDam','XDamVal','YDamVal'});
        Ops.vectorcomponent='edge';
    end
end

VectorPlot=0;
%if isfield(data,'XComp')
if ~isempty(Ops.vectorcomponent)
    [data,scalar]=computecomponent(data,Ops);
    % HACK: if it's the edge normal or tangential component, take the
    % absolute value to avoid problems with direction
    if strncmp(Ops.vectorcomponent,'edge ',5)
        data.Val = abs(data.Val);
    end
    %
    if strcmp(Ops.vectorcomponent,'edge')
        if isempty(Ops.vectorcolour)
            Units='';
        end
    elseif ~isempty(strfind(Ops.vectorcomponent,'vector'))
        % no text added for vector and patch centred vector
        if ~isempty(Ops.vectorcolour)
            Quant=[Quant ', ' Ops.vectorcolour];
            Units = data(1).Units;
        elseif any(strcmp(Ops.basicaxestype,{'X-Y','Lon-Lat'}))
            Units='';
        end
    else
        Quant=[Quant ', ' Ops.vectorcomponent];
        Units = data(1).Units;
    end
    if strcmp(Ops.units,'**Hide**')
        Units = '';
    end
    if scalar
        NVal=1;
    else
        VectorPlot=1;
    end
end

if isfield(Ops,'operator') && ~strcmp(Ops.operator,'none')
    flds = {'Val','XDamVal','YDamVal'};
    for i = 1:length(flds)
        fldi = flds{i};
        if isfield(data,fldi)
            for d = 1:numel(data)
                data(d).(fldi) = feval(Ops.operator,data(d).(fldi));
            end
        end
    end
end

minx=inf;
miny=inf;
minz=inf;
minv=inf;
maxx=-inf;
maxy=-inf;
maxz=-inf;
maxv=-inf;
for d=1:length(data)
    if isfield(data,'X') % time series may not have an X co-ordinate !
        minx=min(minx,min(data(d).X(:)));
        maxx=max(maxx,max(data(d).X(:)));
    end
    if isfield(data,'Y')
        miny=min(miny,min(data(d).Y(:)));
        maxy=max(maxy,max(data(d).Y(:)));
    end
    if isfield(data,'Z')
        minz=min(minz,min(data(d).Z(:)));
        maxz=max(maxz,max(data(d).Z(:)));
    end
end
npnt=0;
if isfield(data,'Val') && ~iscell(data(1).Val) && ~VectorPlot
    for d=length(data):-1:1
        if isfield(data,'X')
            szX=size(data(d).X);
        elseif isfield(data,'Y')
            szX=size(data(d).Y);
        elseif isfield(data,'Z')
            szX=size(data(d).Z);
        else
            szX=-1;
        end
        if  isequal(szX,size(data(d).Val))
            if isfield(data,'X')
                I=isnan(data(d).X);
                data(d).Val(I)=NaN;
                %data(d).X(I)=maxx;
            end
            if isfield(data,'Y')
                I=isnan(data(d).Y);
                data(d).Val(I)=NaN;
                %data(d).Y(I)=maxy;
            end
            if isfield(data,'Z')
                I=isnan(data(d).Z);
                data(d).Val(I)=NaN;
                %data(d).Z(I)=maxz;
            end
        end
        npnt=npnt+sum(~isnan(data(d).Val(:)));
        maxv=max(maxv,max(data(d).Val(:)));
        minv=min(minv,min(data(d).Val(:)));
    end
elseif isfield(data,'XComp')
    for d=1:length(data)
        I = data(d).XComp(:)==0;
        if isfield(data,'YComp')
            I = I & data(d).YComp(:)==0;
        end
        if isfield(data,'ZComp')
            I = I & data(d).ZComp(:)==0;
        end
        I = I | isnan(data(d).XComp(:));
        npnt=npnt+sum(~I);
    end
    npnt=max(npnt,1);
elseif isfield(data,'XDamVal')
    for d=1:length(data)
        npnt = npnt + sum(data(d).XDamVal(:)~=0);
        npnt = npnt + sum(data(d).YDamVal(:)~=0);
    end
    npnt=max(npnt,1);
end

if isfield(Ops,'vectorscalingmode')
    switch Ops.vectorscalingmode
        case ''
            quivopt={};
        case 'automatic'
            quivopt={'automatic'};
        case 'manual'
            for d=length(data):-1:1
                if isfield(data,'XComp')
                    data(d).XComp=Ops.vectorscale*data(d).XComp;
                end
                if isfield(data,'YComp')
                    data(d).YComp=Ops.vectorscale*data(d).YComp;
                end
                if isfield(data,'ZComp')
                    data(d).ZComp=Ops.vectorscale*data(d).ZComp;
                end
            end
            quivopt={0};
        case {'manual normalised','automatic normalised'}
            for d=length(data):-1:1
                if strcmp(vpt,'magnitude')
                    VecMag=data(d).Val;
                else
                    VecMag=data(d).XComp.^2;
                    if isfield(data,'YComp');
                        VecMag=VecMag+data(d).YComp.^2;
                    end
                    if isfield(data,'ZComp');
                        VecMag=VecMag+data(d).ZComp.^2;
                    end
                    VecMag=sqrt(VecMag);
                end
                %VecMag(VecMag<1e-3)=NaN;
                VecMag(VecMag==0)=1;
                if isfield(data,'XComp')
                    data(d).XComp=Ops.vectorscale*data(d).XComp./VecMag;
                end
                if isfield(data,'YComp')
                    data(d).YComp=Ops.vectorscale*data(d).YComp./VecMag;
                end
                if isfield(data,'ZComp')
                    data(d).ZComp=Ops.vectorscale*data(d).ZComp./VecMag;
                end
            end
            if strcmp(Ops.vectorscalingmode,'manual normalised')
                quivopt={0};
            else
                quivopt={'automatic'};
            end
    end
else
    quivopt={};
end

if isequal(quivopt,{'automatic'})
    del2=0;
    if isfield(data,'X')
        del2 = del2 + (maxx-minx).^2/npnt;
    end
    if isfield(data,'Y')
        del2 = del2 + (maxy-miny).^2/npnt;
    end
    if isfield(data,'Z') && isfield(data,'ZComp')
        del2 = del2 + (maxz-minz).^2/npnt;
    end
    maxlen=0;
    for d=1:length(data)
        V=0;
        if isfield(data,'XComp')
            V=V+data(d).XComp.^2;
        elseif isfield(data,'XDamVal')
            maxlen=max(maxlen,max(abs(data(d).XDamVal(:))));
        end
        if isfield(data,'YComp')
            V=V+data(d).YComp.^2;
        elseif isfield(data,'YDamVal')
            maxlen=max(maxlen,max(abs(data(d).YDamVal(:))));
        end
        if isfield(data,'ZComp') && isfield(data,'Z')
            V=V+data(d).ZComp.^2;
        end
        if isfield(data,'X')
            V(isnan(data(d).X))=NaN;
        end
        if isfield(data,'Y')
            V(isnan(data(d).Y))=NaN;
        end
        if isfield(data,'Z')
            V(isnan(data(d).Z))=NaN;
        end
        maxlen=max(maxlen,max(V(:)));
    end
    if del2~=0
        maxlen=sqrt(maxlen/del2);
        if maxlen>0
            autoscale = 0.9 / maxlen;
        else
            autoscale = 0.9;
        end
    else
        autoscale = 0.9;
    end
    %
    % apply "automatic" scaling
    %
    for d=length(data):-1:1
        if isfield(data,'XComp')
            data(d).XComp=autoscale*data(d).XComp;
        elseif isfield(data,'XDamVal')
            data(d).XDamVal=autoscale*data(d).XDamVal;
        end
        if isfield(data,'YComp')
            data(d).YComp=autoscale*data(d).YComp;
        elseif isfield(data,'YDamVal')
            data(d).YDamVal=autoscale*data(d).YDamVal;
        end
        if isfield(data,'ZComp')
            data(d).ZComp=autoscale*data(d).ZComp;
        end
    end
    %
    quivopt={0};
end

LocLabelClass=0;
LocStartClass=0;
if isfield(Ops,'presentationtype') && strcmp(Ops.presentationtype,'coloured contour lines')
    LocLabelClass=1;
    LocStartClass=1;
end

if Props.NVal==6
    Ops.Thresholds = 0.5:1:length(data(1).Classes);
elseif isfield(Ops,'thresholds') && ~strcmp(Ops.thresholds,'none')
    miv = inf;
    mv  = -inf;
    for d = 1:length(data)
        miv = min(miv,min(data(d).Val(:)));
        mv  = max(mv ,max(data(d).Val(:)));
    end
    Ops.Thresholds = compthresholds(Ops,[miv mv],LocStartClass);
    if miv<Ops.Thresholds(1) && ~LocLabelClass
        Ops.Thresholds = [-inf Ops.Thresholds];
    end
else
    Ops.Thresholds = 'none';
end

stn='';
if any(cellfun('isclass',Selected,'cell'))
    stn='';
elseif length(Selected{ST_})>1 || isequal(Selected{ST_},0)
    stn='<multiple>';
elseif ~isempty(stats)
    if iscell(stats)
        stn=stats{Selected{ST_}};
    else
        stn=deblank(stats(Selected{ST_},:));
    end
elseif DimFlag(ST_)
    [Chk,stn]=qp_getdata(FileInfo,Domain,Props,'stations');
    if iscell(stn)
        stn=stn{Selected{ST_}};
    else
        stn=stn(Selected{ST_},:);
    end
elseif DimFlag(M_) && DimFlag(N_) && DimFlag(K_) && none(multiple([M_ N_ K_]))
    stn=sprintf('point (%i,%i,%i)',Selected{M_},Selected{N_},Selected{K_});
elseif DimFlag(M_) && ~DimFlag(N_) && DimFlag(K_) && none(multiple([M_ K_]))
    stn=sprintf('point (%i,%i)',Selected{M_},Selected{K_});
elseif DimFlag(M_) && DimFlag(N_) && none(multiple([M_ N_]))
    stn=sprintf('point (%i,%i)',Selected{M_},Selected{N_});
elseif DimFlag(M_) && ~DimFlag(N_) && ~multiple(M_)
    stn=sprintf('point %i',Selected{M_});
end
stn=strrep(stn,'\','\\');
stn=strrep(stn,'_','\_');

if ~isempty(SubField)
    [Chk,subfs]=qp_getdata(FileInfo,Domain,Props,'subfields',SubField{1});
    if Chk && ~isempty(subfs)
        Quant=cat(2,Quant,': ',subfs{1});
    end
end

TStr='';
if isfield(data,'Time') && length(data(1).Time)==1
    TStr = qp_time2str(data(1).Time,DimFlag(T_));
    if ~isnan(Ops.axestimezone_shift)
        TStr = [TStr ' (' Ops.axestimezone_str ')'];
    end
end

fld = 'XYZ';
for dir = 1:3
    X = fld(dir);
    if isfield(data,[X 'Units']) && ~isempty(data(1).([X 'Units']))
        fac = qp_unitconversion(data.([X 'Units']),'m');
        if ischar(fac) %distance unit not compatible with m
            % conversion not possible
        elseif fac==1
            % conversion not needed
        elseif isfield(data,X)
            for d = length(data):-1:1
                data(d).(X) = data(d).(X)*fac;
                data(d).([X 'Units']) = 'm';
            end
        elseif isfield(data,'XYZ')
            for d = length(data):-1:1
                sz      = size(data(d).XYZ);
                if dir<=sz(end)
                    sl      = repmat({':'},size(sz));
                    sl(end) = dir;
                    data(d).XYZ(sl{:}) = data(d).XYZ(sl{:})*fac;
                    data(d).([X 'Units']) = 'm';
                end
            end
        end
    end
end

clippingspatial = 0;
for clipi=1:3
    switch clipi
        case 1
            % clipping based on components gives all kinds of practical problems
            % (e.g. rotated components), so not implemented.
            fld={'Val','XDamVal','YDamVal'};
            clp='clippingvalues';
        case 2
            fld={'X'};
            clp='xclipping';
        case 3
            fld={'Y'};
            clp='yclipping';
    end
    if isfield(Ops,clp)
        clippingvals = Ops.(clp);
    else
        clippingvals = [];
    end
    %
    if ~isempty(clippingvals)
        if clipi>1
            clippingspatial = 1;
        end
        for f=1:length(fld)
            fldf = fld{f};
            if isfield(data,fldf)
                for d=1:length(data)
                    val=getfield(data,{d},fldf);
                    data=setfield(data,{d},fldf,[]);
                    if isnumeric(clippingvals)
                        check = logical(ismember(val,clippingvals));
                        if any(check(:))
                            if ~isa(val,'double') && ~isa(val,'single')
                                val = double(val);
                            end
                            val(check)=NaN;
                        end
                    else
                        val=realset(clippingvals,val);
                    end
                    data=setfield(data,{d},fldf,val);
                end
            end
        end
    end
    val=[];
end
if isfield(data,'XYZ') && clippingspatial
    for d = length(data):-1:1
       val = data(d).XYZ;
       szVal = size(val);
       val = reshape(val,prod(szVal(1:end-1)),szVal(end));
       for clipi = 2:3
           switch clipi
               case 2
                   dim = 1;
                   clippingvals = Ops.xclipping;
               case 3
                   dim = 2;
                   clippingvals = Ops.yclipping;
           end
           if isempty(clippingvals)
               % nothing
           elseif isnumeric(clippingvals)
               val(logical(ismember(val(:,dim),clippingvals)),dim)=NaN;
           else
               val(:,dim)=realset(clippingvals,val(:,dim));
           end
       end
       val = reshape(val,szVal);
       data(d).XYZ = val;
    end
    val=[];
end

if ~isempty(Parent) && all(ishandle(Parent)) && strcmp(get(Parent(1),'type'),'axes')
    pfig=get(Parent(1),'parent');
    set(0,'currentfigure',pfig)
    set(pfig,'currentaxes',Parent(1))
end

if isfield(Ops,'linestyle') && isfield(Ops,'marker') && ~strcmp(Ops.presentationtype,'markers')
    Ops.LineParams={'color',Ops.colour, ...
        'linewidth',Ops.linewidth, ...
        'linestyle',Ops.linestyle, ...
        'marker',Ops.marker, ...
        'markersize',Ops.markersize, ...
        'markeredgecolor',Ops.markercolour, ...
        'markerfacecolor',Ops.markerfillcolour};
elseif isfield(Ops,'marker')
    Ops.LineParams={'linestyle','none', ...
        'marker',Ops.marker, ...
        'markersize',Ops.markersize, ...
        'markeredgecolor',Ops.markercolour, ...
        'markerfacecolor',Ops.markerfillcolour};
end
if isfield(Ops,'fontsize')
    Ops.FontParams={'color',Ops.colour, ...
        'fontunits','points', ...
        'fontsize',Ops.fontsize, ...
        'horizontalalignment',Ops.horizontalalignment, ...
        'verticalalignment',Ops.verticalalignment};
    if matlabversionnumber>=6.05
        if strcmp(Ops.textboxfacecolour,'none')
            TextBoxParams={'edgecolor','none','backgroundcolor','none'};
        else
            TextBoxParams={'edgecolor',Ops.colour,'backgroundcolor',Ops.textboxfacecolour};
        end
        Ops.FontParams=cat(2,Ops.FontParams,TextBoxParams);
    end
end

%
% If horizontal units is degrees, change to longitude and latitude plot
% type.
%
if isfield(data,'XUnits') && ...
        (strcmp(data(1).XUnits,'deg') || strcmp(data(1).XUnits,'degree'))
    switch Ops.axestype
        case 'X-Y'
            Ops.axestype='Lon-Lat';
        case 'X-Y-Z'
            Ops.axestype='Lon-Lat-Z';
        case 'X-Y-Val'
            Ops.axestype='Lon-Lat-Val';
    end
end
%
% If it the plot contains a Z co-ordinate.
%
if isfield(Ops,'basicaxestype') && ~isempty(strfind(Ops.basicaxestype,'Z'))
    %
    % If the elevation unit has not yet been specified, do so now.
    %
    if isempty(strfind(Ops.axestype,'Z ['))
        %
        % Get it either from the ZUnit field or assume it to be metres.
        %
        if isfield(data,'ZUnit') && ~isempty(data(1).ZUnit)
            Zunit=cat(2,'[',data(1).ZUnit,']');
        else
            Zunit='[m]';
        end
        Ops.axestype=cat(2,Ops.axestype,' ',Zunit);
    end
end

ChangeCLim=1;

diststr = 'x coordinate';
isdist  = 0;
if isfield(Ops,'plotcoordinate') && ~isempty(Ops.plotcoordinate)
    switch Ops.plotcoordinate
        case {'path distance','reverse path distance'}
            isdist = 1;
            if multiple(M_) && ~multiple(N_)
                if isempty(Selected{N_})
                    diststr = 'distance';
                else
                    name=sprintf('n=%i',Selected{N_});
                    diststr = sprintf('distance along cross-section %s',name);
                end
            elseif multiple(N_) && ~multiple(M_)
                if isempty(Selected{M_})
                    diststr = 'distance';
                else
                    name=sprintf('m=%i',Selected{M_});
                    diststr = sprintf('distance along cross-section %s',name);
                end
            else
                diststr = 'distance along path';
            end
        case 'x coordinate'
            if isfield(data,'XUnits') && isequal(data(1).XUnits,'deg')
                diststr = 'longitude';
            else
                diststr = 'x coordinate';
            end
        case 'y coordinate'
            if ~isfield(data,'Y')
                error('No Y data to plot against.')
            elseif isfield(data,'XUnits') && isequal(data(1).XUnits,'deg')
                diststr = 'latitude';
            else
                diststr = 'y coordinate';
            end
    end
end

if NVal==4
    switch Ops.presentationtype
        case {'markers','tracks'}
            NVal = 0;
    end
end

%==========================================================================
% Begin of actual plotting
%==========================================================================
Quant = protectstring(Quant);
if NVal==-2
    [Chk,hNewVec,FileInfo]=qp_getdata(FileInfo,Domain,Props,'plot',Parent,Ops,hOld,SubField{:},SubSelected{:});
    return
elseif NVal==-1
    [Chk,hNewVec,FileInfo]=qp_getdata(FileInfo,Domain,Props,'plot',Parent,Ops,hOld,SubField{:},SubSelected{:});
    hNew{1}=hNewVec;
    hObsolete=setdiff(hOld{1},hNew{1});
    delete(hObsolete(ishandle(hObsolete)));
    if isfield(Ops,'axestype') && ~isempty(Ops.axestype)
        setaxesprops(Parent,Ops.axestype)
    else
        set(Parent,'layer','top')
        %   elseif ~isappdata(Parent,'AxesType')
        %      setaxesprops(Parent,'<blocking>')
    end
else
    Param.ChangeCLim=1;
    Param.NVal=NVal;
    Param.multiple=multiple;
    Param.FirstFrame=FirstFrame;
    Param.Quant=Quant;
    Param.Units=Units;
    Param.TStr=TStr;
    Param.Selected=Selected;
    Param.quivopt=quivopt;
    Param.stats=stats;
    Param.stn=stn;
    Param.s=s;
    Param.compat7={};
    if matlabversionnumber>=7
        Param.compat7={'v6'};
    end

    for d = length(data):-1:1
        do=min(length(hOld),d);
        plotargs={hOld{do},Parent,Param,data(d),Ops,Props};
        geom='';
        if isfield(data,'Geom') && ~isempty(data(d).Geom)
            geom=data(d).Geom;
        elseif isfield(Props,'Geom')
            geom=Props.Geom;
        end
        if strcmp(geom,'POLYG') && ~DataInCell
            if DimFlag(M_) && DimFlag(N_)
                geom='';
            else
                geom='PNT';
            end
        end
        switch geom
            case {'SEG','SEG-NODE','SEG-EDGE'}
                [hNew{d},Thresholds,Param]=qp_plot_seg(plotargs{:});
            case 'PNT'
                [hNew{d},Thresholds,Param,Parent]=qp_plot_pnt(plotargs{:});
            case {'POLYL','POLYG'}
                [hNew{d},Thresholds,Param]=qp_plot_polyl(plotargs{:});
            case {'UGRID-NODE','UGRID-FACE','UGRID-EDGE'}
                [hNew{d},Thresholds,Param]=qp_plot_ugrid(plotargs{:});
            otherwise
                [hNew{d},Thresholds,Param,Parent]=qp_plot_default(plotargs{:});
                PlotState.Parent=Parent;
        end
    end
    for d = length(data)+1:length(hNew)
        delete(hNew{d});
    end
    hNew = hNew(1:length(data));
    
    ChangeCLim = strcmp(Thresholds,'none') || ~isempty(Ops.colourlimits);

    hNewVec=cat(1,hNew{:});
end

if isfield(Ops,'basicaxestype') && ~isempty(Ops.basicaxestype) && length(Parent)==1
    axestype = multiline(strtok(Ops.basicaxestype),'-','cell');
    nAxes = length(axestype);
    %
    dimension = cell(1,nAxes);
    unit = cell(1,nAxes);
    %
    for d = 1:nAxes
        switch axestype{d}
            case 'Time'
                dimension{d} = 'time';
                if isnan(Ops.axestimezone_shift)
                    unit{d} = '';
                else
                    unit{d} = Ops.axestimezone_str;
                end
            case 'Distance'
                dimension{d} = 'distance';
                if isfield(data,'XUnits') && ~isempty(data(1).XUnits)
                    unit{d} = data(1).XUnits;
                    if strcmp(data(1).XUnits,'deg')
                        unit{d} = 'm';
                    end
                end
            case 'Val'
                dimension{d} = Quant;
                unit{d} = Units;
            case 'Y'
                dimension{d} = 'y coordinate';%'distance';
                if isfield(Props,'NName') && ~isempty(Props.NName)
                    dimension{d} = Props.NName;
                end
                if isfield(data,'YUnits') && ~isempty(data(1).YUnits)
                    unit{d} = data(1).YUnits;
                end
            case 'Z'
                dimension{d} = 'elevation';
                if isfield(data,'ZUnits')
                    if ~isempty(data(1).ZUnits)
                        unit{d} = data(1).ZUnits;
                    end
                elseif isfield(data,'Units') && ~isempty(data(1).Units)
                    % only use data unit for "elevation" if this is a level unit as
                    % recognized in qp_interface_update_options. The requirements
                    % are:
                    % 1) Name should contain level
                    if ~isempty(strfind(data(1).Name,'level'))
                        % 2) Unit should be compatible with m
                        if ~ischar(qp_unitconversion(Units,'m')) || ...
                                ~ischar(qp_unitconversion(Units,''))
                            unit{d} = data(1).Units;
                        end
                    end
                end
            otherwise
                dimension{d} = diststr;%'distance';
                if isfield(Ops,'plotcoordinate') && strcmp(Ops.plotcoordinate,'y coordinate')
                    if isfield(Props,'NName') && ~isempty(Props.NName)
                        dimension{d} = protectstring(Props.NName);
                    end
                    if isfield(data,'YUnits') && ~isempty(data(1).YUnits)
                        unit{d} = data(1).YUnits;
                    end
                else
                    if isfield(Props,'MName') && ~isempty(Props.MName)
                        dimension{d} = protectstring(Props.MName);
                    end
                    if isfield(data,'XUnits') && ~isempty(data(1).XUnits)
                        unit{d} = data(1).XUnits;
                    end
                end
                if isdist && strcmp(unit{d},'deg')
                    unit{d} = 'm';
                end
        end
    end
    setaxesprops(Parent,Ops.axestype,dimension,unit)
end
%==========================================================================
% End of actual plotting
%==========================================================================

Error=0;

if isfield(Ops,'colourlimits') && ~isempty(Ops.colourlimits)
    if ChangeCLim
        set(Parent,'clim',Ops.colourlimits)
    end
elseif isfield(Ops,'symmetriccolourlimits') && Ops.symmetriccolourlimits
    lastCLIMSYMM=0;
    if ~isempty(hOldVec)
        xde=get(hOldVec(end),'xdata');
        lastCLIMSYMM=isequal(size(xde),[1 2]) & all(isnan(xde(:)));
    end
    clim=limits(hNewVec,'clim'); clim=max(abs(clim));
    if lastCLIMSYMM
        set(hOldVec(end),'cdata',[-1 1;-1 1]*clim)
        hNew{end+1}=hOldVec(end);
    else
        hNew{end+1}=surface([NaN NaN],[NaN NaN],[NaN NaN;NaN NaN],'cdata',[-1 1;-1 1]*clim);
    end
end

if isfield(Ops,'colourmap') && ~isempty(Ops.colourmap)
    if ischar(Ops.colourmap)
        colormap(qp_colormap(Ops.colourmap));
    else
        ui_message('warning','Colourmap option not yet implemented.');
    end
end

if isfield(Ops,'colourbar') && ~strcmp(Ops.colourbar,'none')
    Chld =allchild(pfig);
    isAx =strcmp(get(Chld,'type'),'axes');
    nonAx=Chld(~isAx);
    Ax   =Chld(isAx);
    h=qp_colorbar(Ops.colourbar,'peer',Parent);
    if ~isempty(Units)
        PName = sprintf('%s (%s)',Quant,Units);
    else
        PName = Quant;
    end
    switch Ops.colourbar
        case 'vert'
            ylabel(h,PName)
        case 'horiz'
            xlabel(h,PName)
    end
    if ~isempty(h)
        set(pfig,'children',[nonAx;h;Ax(ishandle(Ax) & (Ax~=h))])
        cbratio = qp_settings('colorbar_ratio');
        if cbratio>1
            switch Ops.colourbar
                case 'horiz'
                    set(h,'PlotBoxAspectRatio',[cbratio 1 1])
                case 'vert'
                    set(h,'PlotBoxAspectRatio',[1 cbratio 1])
            end
        end
        if ~strcmp(Ops.Thresholds,'none')
            if Props.NVal==6
                classbar(h,1:length(Thresholds),'labelcolor','label',Thresholds,data(1).Classes,'plotall','climmode','new')
            elseif LocLabelClass
                classbar(h,1:length(Thresholds),'labelcolor','label',Thresholds,'plotall','climmode','new')
            else
                classbar(h,1:length(Thresholds),'label',Thresholds,'plotall','climmode','new')
            end
        end
    end
end

if isempty(hNewVec)
    hNewVec=line('xdata',[],'ydata',[],'zdata',[]);
    hNew{end+1}=hNewVec;
end
if isempty(hNewTag)
    hNewTag=sprintf('QPPlotTag [%bx-%bx]',now,rand);
end
set(hNewVec,'hittest','off','tag',hNewTag)

IUD.PlotState=PlotState;
IUD.PlotState.FI=FileInfo;
IUD.PlotState.Handles=hNew;
IUD.XInfo=[];
if isfield(data,'XInfo')
    IUD.XInfo=data.XInfo;
end
if ~isempty(Thresholds)
    IUD.XInfo.Thresholds=Thresholds;
end
set(hNewVec,'userdata',[])
set(hNewVec(1),'userdata',IUD)
%
% If object sorting is enabled (almost always) then let's put the new
% object at the right location in the object stack.
%
if SortObjs
    for lP=1:nParents
        Parent=lParents(lP);
        %
        % Identify objects that are children of this parent ...
        %
        childlist=allchild(Parent);
        hNew_lP=hNewVec(ismember(hNewVec,childlist));
        %
        % ... and put them in the right location.
        %
        children=[PchildBefore{lP};hNew_lP(:);PchildAfter{lP}];
        children(~ismember(children,childlist))=[];
        %
        % Put any children that didn't exist initially and that aren't
        % listed as being part of the new object, on top.
        %
        children=cat(1,childlist(~ismember(childlist,children)),children);
        set(Parent,'children',children)
    end
end
%
if ~isfield(Ops,'basicaxestype') || (~strcmp(Ops.basicaxestype,'X-Y-Z') && ~strcmp(Ops.basicaxestype,'X-Y-Val'))
    setzcoord(hNewVec,Level)
end
for i=1:length(hNewVec)
    a = get(hNewVec(i),'parent');
    if strcmp(get(a,'type'),'axes')
        cp = get(a,'cameraposition');
        if cp(3)<Level
            cp(3) = 1.1*Level;
            set(a,'cameraposition',cp)
        end
    end
    %set(a,'zlim',limits(a,'zlim')+[-1 +1])
    %set(a,'CameraViewAngle',get(a,'CameraViewAngle'))
end
setappdata(hNewVec(1),'Level',Level)
