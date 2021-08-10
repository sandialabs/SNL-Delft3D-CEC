function [hNew,Thresholds,Param,Parent]=qp_plot_default(hNew,Parent,Param,data,Ops,Props)
%QP_PLOT_DEFAULT Plot function of QuickPlot for structured data sets.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_plot_default.m $
%   $Id: qp_plot_default.m 65778 2020-01-14 14:07:42Z mourits $

T_=1; ST_=2; M_=3; N_=4; K_=5;

FirstFrame=Param.FirstFrame;
Quant=Param.Quant;
Units=Param.Units;
if ~isempty(Units)
    PName=sprintf('%s (%s)',Quant,Units);
else
    PName=Quant;
end
TStr=Param.TStr;
Selected=Param.Selected;
multiple=Param.multiple;
NVal=Param.NVal;
quivopt=Param.quivopt;
stats=Param.stats;
stn=Param.stn;
s=Param.s;
compat7=Param.compat7;

DimFlag=Props.DimFlag;
Thresholds=Ops.Thresholds;
axestype=Ops.basicaxestype;

switch NVal
    
    case {0,0.5}
        switch axestype
            case {'X-Y','Lon-Lat','X-Y-Val','Lon-Lat-Val'}
                
                if isfield(data,'TRI')
                    if FirstFrame
                        if isempty(data.TRI)
                            data.TRI = 1:size(data.XYZ,2);
                            if length(data.TRI)<3
                                data.TRI(length(data.TRI)+1:3) = 1;
                            end
                            Ops.linestyle = 'none';
                        end
                        hNew=patch('vertices',reshape(data.XYZ(1,:,1,:),[size(data.XYZ,2) size(data.XYZ,4)]),'faces',data.TRI, ...
                            'facecolor','none','edgecolor',Ops.colour, ...
                            'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour, ...
                            'parent',Parent);
                    else
                        set(hNew,'vertices',data.XYZ,'faces',data.TRI);
                    end
                elseif isfield(data,'XDam')
                    if ~FirstFrame
                        delete(hNew)
                    end
                    if strcmp(Ops.presentationtype,'edge m')
                        data.XDamVal(:) = NaN;
                    elseif strcmp(Ops.presentationtype,'edge n')
                        data.YDamVal(:) = NaN;
                    end
                    if strcmp(Ops.presentationtype,'values')
                        xx = (data.X(:,1:end-1) + data.X(:,2:end))/2;
                        yy = (data.Y(:,1:end-1) + data.Y(:,2:end))/2;
                        vv = data.XDamVal(:,2:end);
                        hNew = qp_scalarfield(Parent,[],Ops.presentationtype,'QUAD',xx,yy,[],vv,Ops);
                        %
                        xx = (data.X(1:end-1,:) + data.X(2:end,:))/2;
                        yy = (data.Y(1:end-1,:) + data.Y(2:end,:))/2;
                        vv = data.YDamVal(2:end,:);
                        hNew2 = qp_scalarfield(Parent,[],Ops.presentationtype,'QUAD',xx,yy,[],vv,Ops);
                        %
                        hNew = cat(2,hNew,hNew2);
                    elseif isfield(data,'XDamVal') && Ops.colourdams
                        hNew=thindam(data.X,data.Y,data.XDam,data.YDam,'color',data.XDamVal,data.YDamVal,'parent',Parent);
                        set(hNew,'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'edgecolor','flat', ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour);
                    else
                        hNew=thindam(data.X,data.Y,data.XDam,data.YDam,'parent',Parent);
                        set(hNew,Ops.LineParams{:});
                    end
                elseif isfield(data,'X') && sum(size(data.X)>1)>=2
                    if ndims(data.X)>2
                        data.X = data.X(:,:,1);
                        data.Y = data.Y(:,:,1);
                    end
                    if FirstFrame
                        hNew=surface(data.X,data.Y,zeros(size(data.X)), ...
                            'cdata',[], ...
                            'parent',Parent, ...
                            'edgecolor',Ops.colour, ...
                            'linewidth',Ops.linewidth, ...
                            'linestyle',Ops.linestyle, ...
                            'marker',Ops.marker, ...
                            'markersize',Ops.markersize, ...
                            'markeredgecolor',Ops.markercolour, ...
                            'markerfacecolor',Ops.markerfillcolour, ...
                            'facecolor','none');
                    else
                        set(hNew,'xdata',data.X,'ydata',data.Y);
                    end
                else
                    if isfield(data,'X')
                        X = data.X;
                        Y = data.Y;
                    elseif isfield(data,'XY')
                        X = data.XY(:,1);
                        Y = data.XY(:,2);
                    end
                    if FirstFrame
                        hNew=line(X,Y, ...
                            'parent',Parent, ...
                            Ops.LineParams{:});
                    else
                        set(hNew,'xdata',X,'ydata',Y);
                    end
                end
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                end
            case {'X-Val'}
                if 1
                    s = data.X;
                    nX = length(s);
                    x = [s s NaN(nX,1)]';
                    y = repmat([get(Parent,'ylim')';NaN],1,nX);
                    x = x(:);
                    y = y(:);
                else
                    x = data.X;
                    y = zeros(size(data.X));
                end
                if FirstFrame
                    hNew=line(x,y, ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                else
                    set(hNew,'xdata',x);
                end
                qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
            case {'X-Z'}
                % dummy values
                if ~isequal(size(data.Z),size(data.X))
                    Ops.presentationtype = 'grid';
                    data.Val = repmat(NaN,size(data.Z)-[0 1]);
                else
                    Ops.presentationtype = 'old grid';
                    data.Val = repmat(NaN,size(data.Z));
                end
                hNew = plotslice(hNew,Parent,data,Ops,multiple,DimFlag,Props,Thresholds);
                qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
            case {'Val-Z'}
                z=squeeze(data.Z);
                if FirstFrame
                    hNew=line(zeros(size(z)),z, ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                else
                    set(hNew,'ydata',z);
                end
                qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
            case 'unknown?'
                if FirstFrame
                    hNew=line(data.X,data.Y, ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                    if isfield(data,'Z')
                        if length(hNew)==1
                            set(hNew,'zdata',data.Z);
                        else
                            for i=1:size(data.Z,2)
                                set(hNew(i),'zdata',data.Z(:,i));
                            end
                        end
                        %set(get(Parent,'zlabel'),'string','elevation (m) \rightarrow')
                    end
                else
                    if isfield(data,'Z')
                        set(hNew,'xdata',data.X,'ydata',data.Y,'zdata',data.Z);
                    else
                        set(hNew,'xdata',data.X,'ydata',data.Y);
                    end
                end
                if ~isempty(stn)
                    Str={PName,stn};
                else
                    Str=PName;
                end
                qp_title(Parent,Str,'quantity',Quant,'unit',Units)
            otherwise
                hNew=gentext(hNew,Ops,Parent,'Plot not defined');
        end
        
    case {1,5,6}
        switch axestype
            case {'X-Y','Lon-Lat','X-Y-Val','X-Y-Z','Lon-Lat-Val','Lon-Lat-Z'}
                if isfield(data,'TRI')
                    set(Parent,'NextPlot','add');
                    switch Ops.presentationtype
                        case {'values','markers'}
                            if isfield(data,'Z') && 0
                                hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,data.Z,data.Val,Ops);
                            else
                                hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,[],data.Val,Ops);
                            end
                        otherwise
                            hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'TRI',data.TRI,data.XYZ,data.Val,Ops);
                    end
                    if strcmp(Ops.colourbar,'none')
                        qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                    else
                        qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                    end
                else
                    data = qp_dimsqueeze(data,Ops.axestype,multiple,DimFlag,Props);
                    if strcmp(Ops.presentationtype,'labels') && isfield(data,'Classes')
                        miss = isnan(data.Val);
                        data.Val(miss) = 1;
                        data.Classes(data.Val);
                        data.Val = data.Classes(data.Val);
                        data.Val(miss) = {''};
                    end
                    if isfield(data,'Z') && 0
                        hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,data.Z,data.Val,Ops);
                    elseif isfield(data,'X')
                        hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.X,data.Y,[],data.Val,Ops);
                    else
                        hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'QUAD',data.XY(:,1),data.XY(:,2),[],data.Val,Ops);
                    end
                    if isempty(Selected{K_})
                        str=PName;
                        lyr={};
                    else
                        lyr = qp_layer(Selected{K_});
                        str = sprintf('%s in %s',PName,lyr);
                        lyr = {lyr};
                    end
                    %
                    if strcmp(Ops.colourbar,'none')
                        tit = {str};
                    else
                        tit = lyr;
                    end
                    if ~isempty(stn)
                        tit{end+1}=stn;
                    end
                    if ~isempty(TStr)
                        tit{end+1}=TStr;
                    end
                    if length(tit)>2
                        tit{1}=[tit{1} ' at ' tit{2}];
                        tit(2)=[];
                    end
                    qp_title(Parent,tit,'quantity',Quant,'unit',Units)
                end
                
            case {'Distance-Val','X-Val','X-Z','X-Time','Time-X','Time-Z','Time-Val'}
                if ~isfield(Ops,'plotcoordinate')
                    Ops.plotcoordinate = 'time';
                    data.X = data.Time;
                end
                if ~isempty(strfind(axestype,'Z')) && isfield(data,'Z') && multiple(K_)
                    hNew = plotslice(hNew,Parent,data,Ops,multiple,DimFlag,Props,Thresholds);
                    if FirstFrame
                        set(Parent,'view',[0 90],'layer','top');
                        %set(get(Parent,'ylabel'),'string','elevation (m) \rightarrow')
                    end
                    if strcmp(Ops.colourbar,'none')
                        tit = {PName};
                    else
                        tit = {};
                    end
                    if ~isempty(TStr)
                        tit{end+1} = TStr;
                    end
                    if ~isempty(stn)
                        tit{end+1}=stn;
                    end
                    qp_title(Parent,tit,'quantity',Quant,'unit',Units,'time',TStr)
                elseif strcmp(Ops.plotcoordinate,'time')
                    if multiple(T_)
                        if FirstFrame
                            hNew=line(data.Time,data.Val, ...
                                'parent',Parent, ...
                                Ops.LineParams{:});
                            if Props.DimFlag(T_)~=5
                                tick(Parent,'x','autodate')
                            end
                        else
                            set(hNew,'xdata',data.Time,'ydata',data.Val);
                        end
                        if ~isempty(stn)
                            Str=stn;
                        else
                            Str='';
                        end
                        qp_title(Parent,Str,'quantity',Quant,'unit',Units,'time',TStr)
                    else
                        strval=sprintf(Ops.numformat,data.Val);
                        if isfield(Ops,'axestype') && ...
                                (isequal(strtok(Ops.axestype),'Time-Val') || ...
                                isequal(strtok(Ops.axestype),'Time-Z'))
                            ylim = get(Parent,'ylim');
                            yval = min(ylim(2),max(ylim(1),data.Val));
                            if isempty(hNew)
                                hNew(2)=line(data.Time*[1 1],ylim,'parent',Parent,'color',Ops.colour);
                                hNew(1)=text('position',[data.Time yval 0],'string',strval,'parent',Parent,Ops.FontParams{:});
                            else
                                i1 = strmatch('text',get(hNew,'type')); % 1 or 2
                                i2 = 3-i1; % consequently, 2 or 1
                                set(hNew(i2),'xdata',data.Time*[1 1],'ydata',ylim);
                                set(hNew(i1),'position',[data.Time yval 0],'string',strval);
                            end
                        else
                            unit = '';
                            if ~isempty(Ops.units)
                                unit = [' ' Ops.units];
                            end
                            hNew=gentext(hNew,Ops,Parent,['Val = ',strval,unit]);
                        end
                    end
                else % distance-Val, X-Val, X-Time, Time-X
                    %Ops.plotcoordinate='(x,y)';
                    if isfield(data,'Time') && length(data.Time)>1
                        mask = all(isnan(data.Val(:,:)),1);
                    else
                        mask = isnan(data.Val);
                    end
                    if size(data.Val,3)==1 && size(data.X,3)>1
                        if size(data.X,3)==2
                            data.X = mean(data.X,3);
                            data.Y = mean(data.Y,3);
                            data.Z = mean(data.Z,3);
                        end
                    end
                    switch Ops.plotcoordinate
                        case '(x,y)'
                            data.X(mask)=NaN;
                            data.Y(mask)=NaN;
                            x=data.X;
                            y=data.Y;
                            z=data.Val;
                        otherwise
                            x=data.X;
                            y=data.Val;
                            z=zeros(size(x));
                    end
                    if isfield(data,'Time') && length(data.Time)>1
                        nx = numel(x);
                        nt = numel(data.Time);
                        if strcmp(Ops.axestype,'X-Time')
                            c1 = repmat(reshape(x, [1 nx]), [nt 1]);
                            c2 = repmat(reshape(data.Time, [nt 1]), [1 nx]);
                            v = squeeze(data.Val);
                        else
                            c1 = repmat(reshape(data.Time, [nt 1]), [1 nx]);
                            c2 = repmat(reshape(x, [1 nx]), [nt 1]);
                            v = squeeze(data.Val);
                        end
                        set(Parent,'NextPlot','add');
                        switch Ops.presentationtype
                            case 'values'
                                I=~isnan(data.Val);
                                hNew=gentextfld(hNew,Ops,Parent,v,c1,c2);
                                
                            case 'continuous shades'
                                hNew=gensurface(hNew,Ops,Parent,v,c1,c2,v);
                                
                            case 'markers'
                                hNew=genmarkers(hNew,Ops,Parent,v,c1,c2);
                                
                            case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
                                if isequal(size(c1),size(v)+1)
                                    [c1,c2,v]=face2surf(c1,c2,v);
                                end
                                v(isnan(c1) | isnan(c2))=NaN;
                                ms=max(c1(:));
                                mz=max(c2(:));
                                c1(isnan(c1))=ms;
                                c2(isnan(c2))=mz;
                                hNew=gencontour(hNew,Ops,Parent,c1,c2,v,Thresholds);
                                
                        end
                        if FirstFrame
                            set(Parent,'view',[0 90],'layer','top');
                        end
                        if strcmp(Ops.colourbar,'none')
                            qp_title(Parent,PName,'quantity',Quant,'unit',Units)
                        else
                            qp_title(Parent,'','quantity',Quant,'unit',Units)
                        end
                    else
                        if strcmp(Ops.facecolour,'none')
                            if length(y)==length(x)-1
                                if isfield(Ops,'presentationtype') && strcmp(Ops.presentationtype,'linear')
                                    x = (x(1:end-1)+x(2:end))/2;
                                    if length(z)>length(y)
                                        z = (z(1:end-1)+z(2:end))/2;
                                    end
                                else % stepwise
                                    x = x(ceil(1:.5:length(x)-0.5));
                                    y = y(ceil(.5:.5:length(y)));
                                    z = z(ceil(1:.5:length(z)-0.5));
                                end
                            end
                            if FirstFrame
                                hNew=line(x,y,z, ...
                                    'parent',Parent, ...
                                    Ops.LineParams{:});
                                set(Parent,'layer','top')
                            elseif ishandle(hNew)
                                set(hNew,'xdata',x, ...
                                    'ydata',y, ...
                                    'zdata',z);
                            else
                                return
                            end
                        else
                            if ~FirstFrame
                                delete(hNew)
                            end
                            vNaN=isnan(y);
                            if any(vNaN)
                                bs=findseries(~vNaN);
                            else
                                bs=[1 length(vNaN)];
                            end
                            for i=1:size(bs,1)
                                if x(bs(i,1))==x(bs(i,2)) && ...
                                        y(bs(i,1))==y(bs(i,2))
                                    % this patch should not influence color scaling.
                                    % however, the default "1" cdata will do so
                                    % we cannot set the cdata to [] immediately
                                    % so, we change it after having set all color options
                                    hNew(i)=patch(x(bs(i,1):bs(i,2)), ...
                                        y(bs(i,1):bs(i,2)), ...
                                        1, ...
                                        'edgecolor',Ops.colour, ...
                                        'facecolor',Ops.facecolour, ...
                                        'linestyle',Ops.linestyle, ...
                                        'linewidth',Ops.linewidth, ...
                                        'marker',Ops.marker, ...
                                        'markersize',Ops.markersize, ...
                                        'markeredgecolor',Ops.markercolour, ...
                                        'markerfacecolor',Ops.markerfillcolour, ...
                                        'cdata',[], ...
                                        'parent',Parent);
                                else
                                    hNew(i)=line(x(bs(i,1):bs(i,2)), ...
                                        y(bs(i,1):bs(i,2)), ...
                                        'parent',Parent, ...
                                        Ops.LineParams{:});
                                end
                            end
                            set(Parent,'layer','top')
                        end
                        tit = {};
                        if ~isempty(stn)
                            tit{end+1}=stn;
                        end
                        if ~isempty(TStr)
                            tit{end+1}=TStr;
                        end
                        qp_title(Parent,tit,'quantity',Quant,'unit',Units)
                    end
                end
                
            case 'Val-Z'
                
                if length(data.Time)>1 % Time-Z
                    c2 = squeeze(data.Z);
                    c1 = repmat(data.Time,1,size(c2,2));
                    v = squeeze(data.Val);
                    set(Parent,'NextPlot','add');
                    switch Ops.presentationtype
                        case 'values'
                            I=~isnan(data.Val);
                            hNew=gentextfld(hNew,Ops,Parent,v,c1,c2);
                            
                        case 'continuous shades'
                            hNew=gensurface(hNew,Ops,Parent,v,c1,c2,v);
                            
                        case 'markers'
                            hNew=genmarkers(hNew,Ops,Parent,v,c1,c2);
                            
                        case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
                            if isequal(size(c1),size(v)+1)
                                [c1,c2,v]=face2surf(c1,c2,v);
                            end
                            v(isnan(c1) | isnan(c2))=NaN;
                            ms=max(c1(:));
                            mz=max(c2(:));
                            c1(isnan(c1))=ms;
                            c2(isnan(c2))=mz;
                            hNew=gencontour(hNew,Ops,Parent,c1,c2,v,Thresholds);
                            
                    end
                    if FirstFrame
                        set(Parent,'view',[0 90],'layer','top');
                    end
                    qp_title(Parent,PName,'quantity',Quant,'unit',Units)
                else
                    if FirstFrame
                        hNew=line(squeeze(data.Val),squeeze(data.Z), ...
                            'parent',Parent, ...
                            Ops.LineParams{:});
                    elseif ishandle(hNew)
                        set(hNew,'xdata',squeeze(data.Val), ...
                            'ydata',squeeze(data.Z));
                    else
                        return
                    end
                    if ~isempty(stn)
                        Str={stn,TStr};
                    else
                        Str={TStr};
                    end
                    qp_title(Parent,Str,'quantity',Quant,'unit',Units,'time',TStr)
                end

            otherwise % Text
                strval = sprintf(Ops.numformat,data.Val);
                hNew=gentext(hNew,Ops,Parent,['Val=',strval]);
                
        end
        
    case {2,3}
        
        switch axestype
            case {'Time-Val','Time-Z'}
                if multiple(T_)
                    
                    if ~isempty(hNew)
                        delete(hNew)
                    end
                    
                    if length(Parent)==1
                        relaxpos = [ ...
                            0         0         0.2754    1.0000
                            0.3623    0.5814    0.6377    0.4186
                            0.3623    0         0.6377    0.4186];
                        Parent = getlinkedparents(Parent,relaxpos);
                    end
                    
                    Qc1 = [Quant ' comp.1'];
                    Qc2 = [Quant ' comp.2'];
                    ax=Parent(1);
                    if isfield(data,'YComp')
                        Y=data.YComp;
                    else
                        Y=data.ZComp;
                    end
                    hNew(1)=line(data.XComp,Y,'parent',ax);
                    setaxesprops(ax,'Val-Val',{Qc1 Qc2},{Units Units});
                    set(ax,'dataAspectRatio',[1 1 1], ...
                        'plotboxAspectRatio',[1 1 1e30])
                    
                    ax=Parent(2);
                    hNew(2)=line(data.Time,data.XComp,'parent',ax);
                    setaxesprops(ax,'Time-Val',{'' Qc1},{'' Units});
                    if ~isempty(stn)
                        qp_title(ax,stn)
                    end
                    
                    ax=Parent(3);
                    hNew(3)=line(data.Time,Y,'parent',ax);
                    setaxesprops(ax,'Time-Val',{'' Qc2},{'' Units});
                    set(hNew,Ops.LineParams{:});
                end
            case 'X-Z'
                s = min(data.X,[],3);
                if strcmp(Ops.plotcoordinate,'reverse path distance')
                    xsign=-1;
                else
                    xsign=1;
                end
                %
                if isequal(size(s),size(data.XComp))
                    %
                    % data provided at (X,Y,Z) locations
                    %
                    % (1) determine sign and size correction for horizontal
                    % component in case of x/y coordinate projection
                    %
                    if xsign==0
                        if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                            t=pathdistance(x,y,'geographic');
                        else
                            t=pathdistance(x,y);
                        end
                        dt1 = t-t([1 1:end-1]);
                        dt1(dt1==0)=NaN;
                        dx1 = (x-x([1 1:end-1]))./dt1;
                        dy1 = (y-y([1 1:end-1]))./dt1;
                        dt2 = t([2:end end])-t;
                        dt2(dt2==0)=NaN;
                        dx2 = (x([2:end end])-x)./dt2;
                        dy2 = (y([2:end end])-y)./dt2;
                        nan1 = isnan(dx1) | isnan(dy1);
                        nan2 = isnan(dx2) | isnan(dy2);
                        dx1(nan1) = 0;
                        dy1(nan1) = 0;
                        dx2(nan2) = 0;
                        dy2(nan2) = 0;
                        dx = (dx1+dx2)./max(~nan1+~nan2,1);
                        dy = (dy1+dy2)./max(~nan1+~nan2,1);
                        dx(nan1+nan2==2)=NaN;
                        dy(nan1+nan2==2)=NaN;
                        switch Ops.plotcoordinate
                            case 'x coordinate'
                                xsign = dx./sqrt(dx.^2+dy.^2);
                            case 'y coordinate'
                                xsign = dy./sqrt(dx.^2+dy.^2);
                        end
                        xsign = reshape(repmat(xsign,[1 1 size(data.XComp,3)]),size(data.XComp));
                    end
                    %
                    % (2) set horizontal coordinate of vector points
                    %
                    % s has already been set correctly
                    %
                    % (3) set vertical coordinate of vector points
                    %
                    Zvector=data.Z;
                    %
                else
                    %
                    % data provided in cell centres
                    %
                    % (1) determine sign and size correction for horizontal
                    % component in case of x/y coordinate projection
                    %
                    if xsign==0
                        xsign = diff(s(:,:,1))./sqrt(diff(x).^2+diff(y).^2);
                        xsign = reshape(repmat(xsign,[1 1 size(data.XComp,3)]),size(data.XComp));
                    end
                    %
                    % (2) determine horizontal coordinate of cell centres
                    %
                    s=squeeze(s);
                    s=(s(1:end-1,1:end-1)+s(2:end,1:end-1)+s(1:end-1,2:end)+s(2:end,2:end))/4;
                    s=reshape(s,size(data.XComp));
                    %
                    % (3) determine vertical coordinate of cell centres
                    %
                    Zvector=squeeze(data.Z);
                    data.Z=[];
                    Zvector=(Zvector(:,1:end-1)+Zvector(:,2:end))/2;
                    Zvector=reshape(Zvector,size(data.XComp));
                end
                
                %
                % get right component to plot: select the component in the plane
                % to be plotted.
                %
                if isfield(data,'dX_tangential')
                    % arbcross
                    ex=data.dX_tangential([1:end end]);
                    ey=data.dY_tangential([1:end end]);
                    ex(data.dX_tangential([1:end end])~=data.dX_tangential([1 1:end]))=NaN;
                    ey(isnan(ex))=NaN;
                    ex = repmat(ex,[1 1 size(data.XComp,3)]);
                    ey = repmat(ey,[1 1 size(data.XComp,3)]);
                    planecomp=ex.*data.XComp + ey.*data.YComp;
                elseif multiple(M_)
                    planecomp=data.XComp;
                else % multiple(N_)
                    planecomp=data.YComp;
                end
                planecomp=xsign.*planecomp;
                planecomp((planecomp==0) & (data.ZComp==0))=NaN;
                
                hold on
                delete(hNew);
                if any(~isnan(data.XComp(:)))
                    
                    switch Ops.verticalscalingmode
                        case 'manual'
                            ScaleFacZ=Ops.verticalscalefactor;
                            set(gca,'dataaspectratio',[1 1/ScaleFacZ 1]);
                        case 'automatic'
                            if FirstFrame
                                c1=max(max(Zvector(:))-min(Zvector(:)),1e-6);
                                c2=max(s(:))-min(s(:));
                                ScaleFacZ=c2/c1/10;
                                set(gca,'dataaspectratio',[1 1/ScaleFacZ 1]);
                            else
                                da=get(gca,'dataaspectratio');
                                ScaleFacZ=da(1)/da(2);
                            end
                        otherwise % unrestricted, same as automatic per timestep without actually setting dataaspectratio
                            
                            c1=max(max(Zvector(:))-min(Zvector(:)),1e-6);
                            c2=max(s(:))-min(s(:));
                            ScaleFacZ=c2/c1/10;
                            if ScaleFacZ==0
                                ScaleFacZ = 1;
                            end
                    end
                    if ScaleFacZ==1
                        hNew=qp_vector(Parent,Ops.vectorstyle,s,Zvector,[],planecomp,data.ZComp,[],quivopt{:});
                    else
                        
                        %       ----------
                        %        When the following lines are used, the lengths and the directions of
                        %        the vectors can be compared. The standard implementation allows for
                        %        the comparison of the individual components and the direction. The
                        %        standard implementation is furthermore consistent with the inter-
                        %        pretation of the vector as a particle displacement in a given period.
                        %
                        %        mag1=sqrt(planecomp.^2+data.ZComp.^2);
                        %        mag2=sqrt(planecomp.^2+(ScaleFacZ*data.ZComp).^2); mag2(mag2==0)=1;
                        %        mfac=mag1./mag2;
                        %        hNew=qp_vector(Parent,Ops.vectorstyle,s,ScaleFacZ*Zvector,[],mfac.*planecomp,ScaleFacZ*mfac.*data.ZComp,[],quivopt{:});
                        %       ----------
                        hNew=qp_vector(Parent,Ops.vectorstyle,s,ScaleFacZ*Zvector,[],planecomp,ScaleFacZ*data.ZComp,[],quivopt{:});
                        for i=1:length(hNew)
                            set(hNew(i),'ydata',get(hNew(i),'ydata')/ScaleFacZ)
                        end
                        
                    end
                    if ~isempty(Ops.vectorcolour)
                        hNew=colquiver(hNew,data.Val);
                    else                        
                        set(hNew,'color',Ops.colour)
                    end
                    set(hNew,'linewidth',Ops.linewidth)
                else
                    hNew=line(1,1,'xdata',[],'ydata',[]);
                end
                set(gca,'layer','top')
                %ylabel('elevation (m) \rightarrow')
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                end
                
            case {'X-Y','Lon-Lat','X-Y-Val','Lon-Lat-Val'}
                data.XComp((data.XComp==0) & (data.YComp==0))=NaN;
                I=~isnan(data.XComp(:));
                %
                minx=min(data.X(:));
                maxx=max(data.X(:));
                miny=min(data.Y(:));
                maxy=max(data.Y(:));
                %
                hold on
                delete(hNew);
                if any(I)
                    %
                    data.X=data.X(I);
                    data.Y=data.Y(I);
                    data.XComp=data.XComp(I);
                    data.YComp=data.YComp(I);
                    if isfield(data,'Z')
                        data.Z=data.Z(I);
                    end
                    if isfield(data,'ZComp')
                        data.ZComp=data.ZComp(I);
                    end
                    if isfield(data,'Val')
                        data.Val=data.Val(I);
                    end
                    %
                    if isfield(data,'ZComp')
                        hNew=qp_vector(Parent,Ops.vectorstyle,data.X,data.Y,data.Z,data.XComp,data.YComp,data.ZComp,quivopt{:});
                    else
                        hNew=qp_vector(Parent,Ops.vectorstyle,data.X,data.Y,[],data.XComp,data.YComp,[],quivopt{:});
                    end
                    
                    if ~isempty(Ops.vectorcolour)
                        if ~strcmp(Ops.Thresholds,'none')
                            vc = zeros(size(data.Val));
                            for i=1:length(Thresholds)
                                vc(data.Val>=Thresholds(i))=i;
                            end
                            data.Val=vc;
                            set(Parent,'clim',[1 length(Thresholds)]);
                        end
                        hNew=colquiver(hNew,data.Val);
                    else
                        set(hNew,'color',Ops.colour)
                    end
                    set(hNew,'linewidth',Ops.linewidth)
                    hNew(end+1)=line([minx maxx],[miny maxy],'linestyle','none','marker','none');
                else
                    hNew=line(1,1,'xdata',[],'ydata',[]);
                end
                if isempty(Selected{K_})
                    str=PName;
                    lyr={};
                else
                    lyr = qp_layer(Selected{K_});
                    str = sprintf('%s in %s',PName,lyr);
                    lyr = {lyr};
                end
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{str,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,[{TStr} lyr],'quantity',Quant,'unit',Units,'time',TStr)
                end
                
            case 'Val-Z'
                
                if ~isempty(hNew)
                    delete(hNew)
                end
                
                if length(Parent)==1
                    relaxpos = [ ...
                        0         0         0.4318    1.0000
                        0.5682    0         0.4318    1.0000];
                    Parent = getlinkedparents(Parent,relaxpos);
                end
                
                if isfield(data,'ZUnits') && ~isempty(data.ZUnits)
                    ZUnits = data.ZUnits;
                else
                    ZUnits = '';
                end
                
                if isfield(data,'Z')
                    Z = data.Z;
                elseif isfield(data,'XYZ')
                    Z = data.XYZ(:,:,:,3);
                end
                Qc1 = [Quant ' comp.1'];
                Qc2 = [Quant ' comp.2'];
                ax=Parent(1);
                hNew(1)=line(squeeze(data.XComp),squeeze(Z),'parent',ax);
                setaxesprops(ax,'Val-Z',{Qc1 'elevation'},{Units ZUnits});
                if ~isempty(stn)
                    qp_title(ax,stn,'quantity',Qc1,'unit',Units,'time',TStr)
                end
                
                ax=Parent(2);
                if isfield(data,'YComp')
                    hNew(2)=line(squeeze(data.YComp),squeeze(Z),'parent',ax);
                else
                    hNew(2)=line(squeeze(data.ZComp),squeeze(Z),'parent',ax);
                end
                setaxesprops(ax,'Val-Z',{Qc2 'elevation'},{Units ZUnits});
                qp_title(ax,TStr,'quantity',Qc2,'unit',Units,'time',TStr)
                
                set(hNew,Ops.LineParams{:});
                
            otherwise
                strxcomp = 'n/a';
                strycomp = 'n/a';
                strzcomp = 'n/a';
                if isfield(data,'XComp')
                    strxcomp = sprintf(Ops.numformat,data.XComp);
                end
                if isfield(data,'YComp')
                    strycomp = sprintf(Ops.numformat,data.YComp);
                end
                if isfield(data,'ZComp')
                    strzcomp = sprintf(Ops.numformat,data.ZComp);
                end
                if NVal==2
                    if isfield(data,'YComp')
                        strval=['[' strxcomp ' ' strycomp ']'];
                    else
                        strval=['[' strxcomp ' ' strzcomp ']'];
                    end
                else
                    strval=['[' strxcomp ' ' strycomp ' ' strzcomp ']'];
                end
                if isfield(Ops,'axestype') && isequal(strtok(Ops.axestype),'Time-Val')
                    ylim = get(Parent,'ylim');
                    yval = min(ylim(2),max(ylim(1),inf)); % XComp, YComp, ZComp, Magnitude?
                    if isempty(hNew)
                        hNew=line(data.Time*[1 1],ylim,'parent',Parent,'color',Ops.colour);
                        hNew(2)=text('position',[data.Time yval 0],'string',strval,'parent',Parent,Ops.FontParams{:});
                    else
                        set(hNew(1),'xdata',data.Time*[1 1],'ydata',ylim);
                        set(hNew(2),'position',[data.Time yval 0],'string',strval);
                    end
                else
                    hNew=gentext(hNew,Ops,Parent,['Val=',strval]);
                end
                
        end
    case {4}
        switch Ops.presentationtype
            case {'markers'}
                hNew=genmarkers(hNew,Ops,Parent,[],data.X,data.Y);
            case {'labels'}
                hNew=gentextfld(hNew,Ops,Parent,data.Val,data.X,data.Y);
        end
end

function Parent = getlinkedparents(Parent,relaxpos)
if isappdata(Parent,'linkedaxes')
    Parent = getappdata(Parent,'linkedaxes');
else
    fg = get(Parent,'parent');
    ps = get(Parent,'position');
    pu = get(Parent,'units');
    tg = get(Parent,'tag');
    delete(Parent);
    Parent = qp_createaxes(fg,'relative',ps,pu,relaxpos);
    for i = 1:length(Parent)
        set(Parent(i),'tag',sprintf('%s [%i]',tg,i))
        setappdata(Parent(i),'linkedaxes',Parent)
    end
end


function hNew = plotslice(hNew,Parent,data,Ops,multiple,DimFlag,Props,Thresholds)
data.X=squeeze(data.X);
data.Z=squeeze(data.Z);
data.Val=squeeze(data.Val);
%
if size(data.Z,2)>1
    Mask=repmat(min(data.Z,[],2)==max(data.Z,[],2),[1 size(data.Z,2)]);
    if isequal(size(Mask),size(data.X))
        data.X(Mask)=NaN;
    end
end
%
s = data.X;
set(Parent,'NextPlot','add');
if size(s,2)==1 && size(data.Z,2)~=1
    s = repmat(s,[1 size(data.Z,2)]);
end
switch Ops.presentationtype
    case {'patches','patches with lines','grid'}
        if isfield(Props,'ThreeD')
            hNew=genfaces(hNew,Ops,Parent,data.Val,data.X,data.Y,data.Z);
        else
            hNew=genfaces(hNew,Ops,Parent,data.Val,s,data.Z);
        end
        
    case 'old grid'
        if isempty(hNew)
            hNew=surface(s,data.Z,zeros(size(s)), ...
                'cdata',[], ...
                'parent',Parent, ...
                'edgecolor',Ops.colour, ...
                'linewidth',Ops.linewidth, ...
                'linestyle',Ops.linestyle, ...
                'marker',Ops.marker, ...
                'markersize',Ops.markersize, ...
                'markeredgecolor',Ops.markercolour, ...
                'markerfacecolor',Ops.markerfillcolour, ...
                'facecolor','none');
        else
            set(hNew,'xdata',s,'ydata',data.Z,'zdata',zeros(size(s)))
        end

    case 'values'
        I=~isnan(data.Val);
        hNew=gentextfld(hNew,Ops,Parent,data.Val(I),s(I),data.Z(I));
        
    case 'continuous shades'
        [val,s,z] = resize2data(data.Val,s,data.Z,Ops);
        hNew=gensurface(hNew,Ops,Parent,val,s,z,val);
        
    case 'markers'
        [val,s,z] = resize2data(data.Val,s,data.Z,Ops);
        hNew=genmarkers(hNew,Ops,Parent,val,s,z);
        
    case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
        [val,s,z] = resize2data(data.Val,s,data.Z,Ops);
        val(isnan(s) | isnan(z))=NaN;
        ms=max(s(:));
        mz=max(z(:));
        s(isnan(s))=ms;
        z(isnan(z))=mz;
        hNew=gencontour(hNew,Ops,Parent,s,z,val,Thresholds);
        
end

function [val,s,z] = resize2data(val,s,z,Ops)
if isfield(Ops,'extend2edge') && Ops.extend2edge
    [s,z,val] = face2surf(s,z,val);
    return
end
nH = size(val,1);
nV = size(val,2);
if size(s,1)==nH+1
    s = (s(1:end-1,:)+s(2:end,:))/2;
end
if size(s,2)==nV+1
    s = (s(:,1:end-1)+s(:,2:end))/2;
end
if size(z,1)==nH+1
    z = (z(1:end-1,:)+z(2:end,:))/2;
elseif size(z,1)==nH-1
    z = (z([1 1:end],:)+z([1:end end],:))/2;
end
if size(z,2)==nV+1
    z = (z(:,1:end-1)+z(:,2:end))/2;
end
