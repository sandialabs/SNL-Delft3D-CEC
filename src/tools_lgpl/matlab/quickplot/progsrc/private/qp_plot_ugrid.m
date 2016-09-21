function [hNew,Thresholds,Param,Parent]=qp_plot_ugrid(hNew,Parent,Param,data,Ops,Props)
%QP_PLOT_UGRID Plot function of QuickPlot for unstructured data sets.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_plot_ugrid.m $
%   $Id: qp_plot_ugrid.m 5564 2015-11-04 19:37:12Z jagers $

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

if isfield(data,'TRI')
    FaceNodeConnect = data.TRI;
elseif isfield(data,'FaceNodeConnect')
    FaceNodeConnect = data.FaceNodeConnect;
elseif isfield(data,'Connect')
    FaceNodeConnect = data.Connect;
else
    FaceNodeConnect = [];
end
nc = size(FaceNodeConnect,2);
if isfield(data,'XYZ')
    data.X = data.XYZ(:,:,:,1);
    data.Y = data.XYZ(:,:,:,2);
end

%data = qp_dimsqueeze(data,Ops.axestype,multiple,DimFlag,Props);
switch NVal
    
    case {0,0.5}
        switch axestype
            case {'X-Y','Lon-Lat'}
                %
                % edges
                %
                if isfield(data,'EdgeNodeConnect')
                    EdgeNodeConnect = data.EdgeNodeConnect;
                else
                    iConnect = ceil(([0 0:2*nc-2])/2+0.1);
                    EdgeNodeConnect = FaceNodeConnect(:,iConnect);
                    ncP = sum(~isnan(FaceNodeConnect),2);
                    EdgeNodeConnect(:,1) = FaceNodeConnect(sub2ind(size(FaceNodeConnect),(1:size(FaceNodeConnect,1))',ncP));
                    EdgeNodeConnect = unique(sort(reshape(EdgeNodeConnect',[2 numel(FaceNodeConnect)]),1)','rows');
                    EdgeNodeConnect(any(isnan(EdgeNodeConnect),2),:) = [];
                end
                %
                xy = EdgeNodeConnect(:,[1 2 2])';
                xy = xy(:);
                X = data.X(xy);
                Y = data.Y(xy);
                X(3:3:end) = NaN;
                Y(3:3:end) = NaN;
                %
                % points without edge
                %
                ip = find(~ismember(1:length(data.X),xy));
                Xp = data.X(ip);
                Yp = data.Y(ip);
                if FirstFrame
                    hNew=line(1,1, ...
                        'color',Ops.colour, ...
                        'linewidth',Ops.linewidth, ...
                        'linestyle',Ops.linestyle, ...
                        'marker',Ops.marker, ...
                        'markersize',Ops.markersize, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour, ...
                        'parent',Parent);
                    pMarker = Ops.marker;
                    if strcmp(pMarker,'none')
                        pMarker = '.';
                    end
                    hNew(2)=line(1,1, ...
                        'color',Ops.colour, ...
                        'linestyle','none', ...
                        'marker',pMarker, ...
                        'markersize',Ops.markersize, ...
                        'markeredgecolor',Ops.markercolour, ...
                        'markerfacecolor',Ops.markerfillcolour, ...
                        'parent',Parent);
                end
                set(hNew,'xdata',X,'ydata',Y);
                set(hNew(2),'xdata',Xp,'ydata',Yp);
                %
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                end
            case {'X-Val'}
                if FirstFrame
                    hNew=line(data.X,zeros(size(data.X)), ...
                        'parent',Parent, ...
                        Ops.LineParams{:});
                else
                    set(hNew,'xdata',data.X);
                end
                qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
            case {'X-Z'}
                switch Ops.plotcoordinate
                    case {'path distance','reverse path distance'}
                        xx=data.X(:,:,1);
                        if isfield(data,'Y')
                            yy=data.Y(:,:,1);
                        else
                            yy=0*xx;
                        end
                        if strcmp(Ops.plotcoordinate,'reverse path distance')
                            xx=flipud(fliplr(xx));
                            yy=flipud(fliplr(yy));
                        end
                        if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                            x=pathdistance(xx,yy,'geographic');
                        else
                            x=pathdistance(xx,yy);
                        end
                        if strcmp(Ops.plotcoordinate,'reverse path distance')
                            x=flipud(fliplr(x));
                        end
                        x = squeeze(reshape(repmat(x,[1 1 size(data.X,3)]),size(data.X)));
                        y = squeeze(data.Z);
                        z = zeros(size(y));
                    case 'x coordinate'
                        x = squeeze(data.X);
                        y = squeeze(data.Z);
                        z = zeros(size(y));
                    case 'y coordinate'
                        x = squeeze(data.Y);
                        y = squeeze(data.Z);
                        z = zeros(size(y));
                    case '(x,y)'
                        x = squeeze(data.X);
                        y = squeeze(data.Y);
                        z = squeeze(data.Z);
                end
                if FirstFrame
                    hNew=surface(x,y,z, ...
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
                    set(hNew,'xdata',x,'ydata',y,'zdata',z)
                end
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
        
    case {1,5}
        switch axestype
            case {'X-Y','Lon-Lat'}
                hNew = qp_scalarfield(Parent,hNew,Ops.presentationtype,'UGRID',data,Ops);
                if strcmp(Ops.colourbar,'none')
                    qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                else
                    qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                end
                
            case {'Distance-Val','X-Val','X-Z','X-Time','Time-X'}
                if multiple(K_)
                    data = qp_dimsqueeze(data,Ops.axestype,multiple,DimFlag,Props);
                    Mask=repmat(min(data.Z,[],3)==max(data.Z,[],3),[1 1 size(data.Z,3)]);
                    if isequal(size(Mask),size(data.X))
                        data.X(Mask)=NaN;
                    end
                    switch Ops.plotcoordinate
                        case {'path distance','reverse path distance'}
                            x=data.X(:,:,1);
                            if isfield(data,'Y')
                                y=data.Y(:,:,1);
                            else
                                y=0*x;
                            end
                            if strcmp(Ops.plotcoordinate,'reverse path distance')
                                x=flipud(fliplr(x));
                                y=flipud(fliplr(y));
                            end
                            if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                                s=pathdistance(x,y,'geographic');
                            else
                                s=pathdistance(x,y);
                            end
                            if ~isequal(size(data.X),size(data.Val))
                                ds=s(min(find(s>0)))/2;
                                if ~isempty(ds)
                                    s=s-ds;
                                end
                            end
                            if strcmp(Ops.plotcoordinate,'reverse path distance')
                                s=flipud(fliplr(s));
                            end
                            s=reshape(repmat(s,[1 1 size(data.X,3)]),size(data.X));
                        case 'x coordinate'
                            s=data.X;
                        case 'y coordinate'
                            s=data.Y;
                    end
                    s=squeeze(s);
                    data.X=squeeze(data.X);
                    if isfield(data,'Y')
                        data.Y=squeeze(data.Y);
                    end
                    data.Z=squeeze(data.Z);
                    data.Val=squeeze(data.Val);
                    %
                    set(Parent,'NextPlot','add');
                    switch Ops.presentationtype
                        case {'patches','patches with lines'}
                            if isfield(Props,'ThreeD')
                                hNew=genfaces(hNew,Ops,Parent,data.Val,data.X,data.Y,data.Z);
                            else
                                hNew=genfaces(hNew,Ops,Parent,data.Val,s,data.Z);
                            end
                            
                        case 'values'
                            I=~isnan(data.Val);
                            hNew=gentextfld(hNew,Ops,Parent,data.Val(I),s(I),data.Z(I));
                            
                        case 'continuous shades'
                            hNew=gensurface(hNew,Ops,Parent,data.Val,s,data.Z,data.Val);
                            
                        case 'markers'
                            hNew=genmarkers(hNew,Ops,Parent,data.Val,s,data.Z);
                            
                        case {'contour lines','coloured contour lines','contour patches','contour patches with lines'}
                            if isequal(size(s),size(data.Val)+1)
                                [s,data.Z,data.Val]=face2surf(s,data.Z,data.Val);
                            end
                            data.Val(isnan(s) | isnan(data.Z))=NaN;
                            ms=max(s(:));
                            mz=max(data.Z(:));
                            s(isnan(s))=ms;
                            data.Z(isnan(data.Z))=mz;
                            hNew=gencontour(hNew,Ops,Parent,s,data.Z,data.Val,Thresholds);
                            
                    end
                    if FirstFrame
                        set(Parent,'view',[0 90],'layer','top');
                        %set(get(Parent,'ylabel'),'string','elevation (m) \rightarrow')
                    end
                    if strcmp(Ops.colourbar,'none')
                        qp_title(Parent,{PName,TStr},'quantity',Quant,'unit',Units,'time',TStr)
                    else
                        qp_title(Parent,{TStr},'quantity',Quant,'unit',Units,'time',TStr)
                    end
                else
                    %Ops.plotcoordinate='(x,y)';
                    switch Ops.plotcoordinate
                        case 'x coordinate'
                            data.X(isnan(data.Val))=NaN;
                            x=data.X;
                            y=data.Val;
                            z=zeros(size(x));
                        case 'y coordinate'
                            data.Y(isnan(data.Val))=NaN;
                            x=data.Y;
                            y=data.Val;
                            z=zeros(size(x));
                        case '(x,y)'
                            data.X(isnan(data.Val))=NaN;
                            data.Y(isnan(data.Val))=NaN;
                            x=data.X;
                            y=data.Y;
                            z=data.Val;
                        otherwise %case {'path distance','reverse path distance'}
                            %data.X(isnan(data.Val))=NaN;
                            xx=data.X;
                            if isfield(data,'Y')
                                %data.Y(isnan(data.Val))=NaN;
                                yy=data.Y;
                            else
                                yy=0*xx;
                            end
                            if isfield(data,'Z')
                                %data.Z(isnan(data.Val))=NaN;
                                zz=data.Z;
                            else
                                zz=0*xx;
                            end
                            if strcmp(Ops.plotcoordinate,'reverse path distance')
                                xx=flipud(fliplr(xx));
                                yy=flipud(fliplr(yy));
                                zz=flipud(fliplr(zz));
                            end
                            if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                                x=pathdistance(xx,yy,'geographic');
                            else
                                x=pathdistance(xx,yy);
                            end
                            if strcmp(Ops.plotcoordinate,'reverse path distance')
                                x=flipud(fliplr(x));
                            end
                            y=data.Val;
                            z=zeros(size(x));
                    end
                    if length(data.Time)>1
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
                
            case {'Time-Val','Time-Z'}
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
                x=min(data.X,[],3);
                if isfield(data,'Y')
                    y=min(data.Y,[],3);
                else
                    y=0*x;
                end
                %
                xsign=0;
                switch Ops.plotcoordinate
                    case {'path distance','reverse path distance'}
                        if strcmp(Ops.plotcoordinate,'reverse path distance')
                            x=flipud(fliplr(x));
                            y=flipud(fliplr(y));
                            xsign=-1;
                        else
                            xsign=1;
                        end
                        if isfield(data,'XUnits') && strcmp(data.XUnits,'deg')
                            s=pathdistance(x,y,'geographic');
                        else
                            s=pathdistance(x,y);
                        end
                        if strcmp(Ops.plotcoordinate,'reverse path distance')
                            s=flipud(fliplr(s));
                        end
                        s=reshape(repmat(s,[1 1 size(data.X,3)]),size(data.X));
                    case 'x coordinate'
                        s=data.X;
                    case 'y coordinate'
                        s=data.Y;
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
                if multiple(M_)
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
                
            case {'X-Y','Lon-Lat'}
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
                    
                    hNew(end+1)=line([minx maxx],[miny maxy],'linestyle','none','marker','none');
                else
                    hNew=line(1,1,'xdata',[],'ydata',[]);
                end
                if isempty(Selected{K_})
                    str=PName;
                    lyr={};
                else
                    lyr=sprintf('layer %i',Selected{K_});
                    str=sprintf('%s in %s',PName,lyr);
                    lyr={lyr};
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
                
                Qc1 = [Quant ' comp.1'];
                Qc2 = [Quant ' comp.2'];
                ax=Parent(1);
                hNew(1)=line(squeeze(data.XComp),squeeze(data.Z),'parent',ax);
                setaxesprops(ax,'Val-Z',{Qc1 'elevation'},{Units ZUnits});
                if ~isempty(stn)
                    qp_title(ax,stn,'quantity',Qc1,'unit',Units,'time',TStr)
                end
                
                ax=Parent(2);
                if isfield(data,'YComp')
                    hNew(2)=line(squeeze(data.YComp),squeeze(data.Z),'parent',ax);
                else
                    hNew(2)=line(squeeze(data.ZComp),squeeze(data.Z),'parent',ax);
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
