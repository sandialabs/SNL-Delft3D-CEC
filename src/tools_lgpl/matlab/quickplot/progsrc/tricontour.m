function H=tricontour(tri,x,y,z,v,levels,color)
%TRICONTOUR Contour plot for triangulated data.
%   TRICONTOUR(TRI,X,Y,Z,Levels) displays the contour plot based on
%   triangles defined in the M-by-3 face matrix TRI as a surface. A
%   row of TRI contains indexes into the X,Y, and Z vertex vectors
%   to define a single triangular face.
%
%   TRICONTOUR(TRI,X,Y,Z,V,Levels) displays the contour plot based on
%   triangles defined in the M-by-3 face matrix TRI as a surface. A
%   row of TRI contains indexes into the X,Y, and Z vertex vectors
%   to define a single triangular face. The contours are determined
%   by the values V and the Levels.
%
%   H = TRICONTOUR(...) a vector H of handles to LINE or PATCH objects,
%   one handle per line. TRICONTOUR is not compatible with CLABEL.
%
%   The contours are normally colored based on the current colormap
%   and are drawn as PATCH objects. You can override this behavior
%   with the syntax CONTOUR(...,'LINESPEC') to draw the contours as
%   LINE objects with the color and linetype specified.
%
%   Example:
%      x=rand(20); y=rand(20); z=rand(20); tri=delaunay(x,y);
%      tricontour(tri,x,y,z,.3:.1:1); colorbar
%
%   See also CONTOUR, CONTOURF, TRICONTOURF

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/progsrc/tricontour.m $
%   $Id: tricontour.m 65778 2020-01-14 14:07:42Z mourits $

if nargin<5
    error('Not enough input arguments.')
elseif nargin>7
    error('Too many input arguments.')
end
getdata=0;
if nargin==7
    if strcmp(color,'getdata')
        getdata=1;
        lin = '*';
        col = '*';
    else
        [lin,col,mark,msg] = colstyle(color);
        if ~isempty(msg)
            error(msg)
        end
    end
elseif nargin==6
    if ischar(levels)
        color=levels;
        levels=v;
        v=z;
        z=[];
        if strcmp(color,'getdata')
            getdata=1;
            lin = '*';
            col = '*';
        else
            [lin,col,mark,msg] = colstyle(color);
            if ~isempty(msg)
                error(msg)
            end
        end
    else
        lin = '';
        col = '';
    end
else
    levels=v;
    v=z;
    z=[];
    lin = '';
    col = '';
end

if ~getdata
    ax = newplot;
    if isempty(col) % no color spec was given
        colortab = get(ax,'colororder');
        ncol = size(colortab,1);
    end
end

x=transpose(x(:));
y=transpose(y(:));
zdef=~isempty(z);
if zdef
    z=transpose(z(:));
end
v=transpose(v(:));

Patches=1:size(tri,1);
nlevels = length(levels);
if getdata
    H=cell(1,nlevels);
else
    H=zeros(1,nlevels);
end
NonEmptyLevel=zeros(1,nlevels);
for LevelNr=1:nlevels
    level=levels(LevelNr);
    Nlarger=sum(v(tri)>level,2);
    Nsmaller=sum(v(tri)<level,2);
    NaN_tri=any(isnan(v(tri)),2);
    Nsmaller(NaN_tri)=3;
    Nlarger(NaN_tri)=0;
    CLIndex=1+4*Nlarger+Nsmaller;
    %Cutslevel=[0 3 2 0 3 3 3 0 2 3 0 0 0 0 0]; % deactivate cases 3 and 9 for process_polylines
    Cutslevel=[0 3 0 0 3 3 3 0 0 3 0 0 0 0 0];
    NLevel=sum(Cutslevel(CLIndex));
    XLevel=repmat(NaN,[NLevel 1]);
    YLevel=XLevel;
    if zdef
        ZLevel=XLevel;
    end
    LvlOffset=0;
    
    % patches with three equal
    % Patch=Patches(CLIndex==1);
    
    % patches with two equal
    Patch=Patches((CLIndex==2) | (CLIndex==5));
    if ~isempty(Patch)
        LvlIndex=LvlOffset+(1:3:(3*length(Patch)));
        LvlIndex=[LvlIndex;LvlIndex+1];
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(v(Index)==level,2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        % first element has elevation different from level
        Index=Index(:,[2 3]);
        XPoint=transpose(x(Index));
        XLevel(LvlIndex(:))=XPoint(:);
        YPoint=transpose(y(Index));
        YLevel(LvlIndex(:))=YPoint(:);
        if zdef
            ZPoint=transpose(z(Index));
            ZLevel(LvlIndex(:))=ZPoint(:);
        end
        LvlOffset=LvlOffset+3*length(Patch);
    end
    
    % patches with one equal, one larger and one smaller
    Patch=Patches(CLIndex==6);
    if ~isempty(Patch)
        LvlIndex=LvlOffset+(1:3:(3*length(Patch)));
        LvlIndex=[LvlIndex;LvlIndex+1];
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(v(Index),2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        % second element has elevation equal to level
        VTemp=v(Index);
        Lambda=min(1,(level-VTemp(:,1))./(VTemp(:,3)-VTemp(:,1)));
        XPoint=transpose([transpose(x(Index(:,1)))+Lambda.*transpose(x(Index(:,3))-x(Index(:,1))) transpose(x(Index(:,2)))]);
        XLevel(LvlIndex(:))=XPoint(:);
        YPoint=transpose([transpose(y(Index(:,1)))+Lambda.*transpose(y(Index(:,3))-y(Index(:,1))) transpose(y(Index(:,2)))]);
        YLevel(LvlIndex(:))=YPoint(:);
        if zdef
            ZPoint=transpose([transpose(z(Index(:,1)))+Lambda.*transpose(z(Index(:,3))-z(Index(:,1))) transpose(z(Index(:,2)))]);
            ZLevel(LvlIndex(:))=ZPoint(:);
        end
        LvlOffset=LvlOffset+3*length(Patch);
    end
    
    % patches with two larger and one smaller
    Patch=Patches(CLIndex==10);
    if ~isempty(Patch)
        LvlIndex=LvlOffset+(1:3:(3*length(Patch)));
        LvlIndex=[LvlIndex;LvlIndex+1];
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(v(Index)<level,2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        % last element has elevation less than level
        VTemp=v(Index);
        Lambda=min(1,(level-VTemp(:,3)*ones(1,2))./(VTemp(:,[1 2])-VTemp(:,3)*ones(1,2)));
        XPoint=transpose(transpose(x(Index(:,3)))*ones(1,2)+Lambda.*(x(Index(:,[1 2]))-transpose(x(Index(:,3)))*ones(1,2)));
        XLevel(LvlIndex(:))=XPoint(:);
        YPoint=transpose(transpose(y(Index(:,3)))*ones(1,2)+Lambda.*(y(Index(:,[1 2]))-transpose(y(Index(:,3)))*ones(1,2)));
        YLevel(LvlIndex(:))=YPoint(:);
        if zdef
            ZPoint=transpose(transpose(z(Index(:,3)))*ones(1,2)+Lambda.*(z(Index(:,[1 2]))-transpose(z(Index(:,3)))*ones(1,2)));
            ZLevel(LvlIndex(:))=ZPoint(:);
        end
        LvlOffset=LvlOffset+3*length(Patch);
    end
    
    % patches with two smaller and one larger
    Patch=Patches(CLIndex==7);
    if ~isempty(Patch)
        LvlIndex=LvlOffset+(1:3:(3*length(Patch)));
        LvlIndex=[LvlIndex;LvlIndex+1];
        Index=tri(Patch,:);
        [Dummy,Permutation]=sort(v(Index)>level,2);
        Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
        % last element has elevation larger than level
        VTemp=v(Index);
        Lambda=min(1,(level-VTemp(:,[1 2]))./(VTemp(:,3)*ones(1,2)-VTemp(:,[1 2])));
        XPoint=transpose(x(Index(:,[1 2]))+Lambda.*(transpose(x(Index(:,3)))*ones(1,2)-x(Index(:,[1 2]))));
        XLevel(LvlIndex(:))=XPoint(:);
        YPoint=transpose(y(Index(:,[1 2]))+Lambda.*(transpose(y(Index(:,3)))*ones(1,2)-y(Index(:,[1 2]))));
        YLevel(LvlIndex(:))=YPoint(:);
        if zdef
            ZPoint=transpose(z(Index(:,[1 2]))+Lambda.*(transpose(z(Index(:,3)))*ones(1,2)-z(Index(:,[1 2]))));
            ZLevel(LvlIndex(:))=ZPoint(:);
        end
        LvlOffset=LvlOffset+3*length(Patch);
    end
    
    % patches with one equal and two larger or two smaller
    % Needed to capture peaks that coincide with contour.
    % Deactivated for compatibility with process_polylines
%     Patch=Patches((CLIndex==3) | (CLIndex==9));
%     if ~isempty(Patch)
%         LvlIndex=LvlOffset+(1:2:(2*length(Patch)));
%         Index=tri(Patch,:);
%         [Dummy,Permutation]=sort(v(Index)==level,2);
%         Index=Index((Permutation-1)*size(Index,1)+transpose(1:size(Index,1))*[1 1 1]);
%         % third element has elevation equal to level
%         Index=Index(:,3);
%         XPoint=x(Index);
%         XLevel(LvlIndex(:))=XPoint(:);
%         YPoint=y(Index);
%         YLevel(LvlIndex(:))=YPoint(:);
%         if zdef
%             ZPoint=z(Index);
%             ZLevel(LvlIndex(:))=ZPoint(:);
%         end
%         LvlOffset=LvlOffset+2*length(Patch);
%     end
    
    NonEmptyLevel(LevelNr)=1;
    if 1 % the processing of the polylines adds a performance penalty, but improves the quality of the plot in HG2. 
    %if isempty(XLevel)
        % no line, so no reason to concatenate lines
    elseif zdef
        [XLevel,YLevel,ZLevel] = process_polylines(XLevel,YLevel,ZLevel);
    else
        [XLevel,YLevel] = process_polylines(XLevel,YLevel);
    end
    
    VLevel=level*ones(size(XLevel));
    
    if getdata
        if zdef
            H{LevelNr}={XLevel YLevel ZLevel LevelNr};
        else
            H{LevelNr}={XLevel YLevel LevelNr};
        end
    elseif isempty(col) && isempty(lin)
        if ~zdef
            ZLevel=VLevel;
        end
        NewH = patch('XData',transpose([XLevel;XLevel]), ...
            'YData',transpose([YLevel;YLevel]), ...
            'ZData',transpose([ZLevel;ZLevel]), ...
            'CData',transpose([VLevel;VLevel]), ...
            'facecolor','none', ...
            'edgecolor','flat',...
            'userdata',level, ...
            'parent',ax);
        H(LevelNr)=NewH;
    else
        if ~zdef
            ZLevel=VLevel;
        end
        NewH = line('XData',XLevel, ...
            'YData',YLevel, ...
            'ZData',ZLevel, ...
            'userdata',level, ...
            'parent',ax);
        H(LevelNr)=NewH;
    end
end

levels=levels(logical(NonEmptyLevel));

if getdata
    % do nothing
elseif isempty(col) && ~isempty(lin)
    nlvl = length(levels);
    colortab = colortab(mod(1:nlvl,ncol)+1,:);
    for i = 1:length(H)
        set(H(i),'linestyle',lin,'color',colortab(i,:));
    end
else
    if ~isempty(lin)
        set(H,'linestyle',lin);
    end
    if ~isempty(col)
        set(H,'color',col);
    end
end


function [X,Y,Z] = process_polylines(X,Y,Z)
valid = ~isnan(X);
if nargin==2 || isempty(Z)
    xyz = [X(valid) Y(valid)];
else
    xyz = [X(valid) Y(valid) Z(valid)];
end
[uxyz,ia,ic] = unique(xyz,'rows');
edp = reshape(ic,[2 length(ic)/2])';
sedp = sort(edp,2);
[uedp,edgenr,isorted] = unique(sedp,'rows');
edges = edp(edgenr,:);
%
contour = NaN(1,2*size(edges,1));
i1 = 1;
contour(i1:i1+1) = edges(1,:);
n1 = contour(i1);
n2 = contour(i1+1);
i2 = i1+1;
%
[snodes,irow] = sort(edges(:));
irow = mod(irow-1,size(edges,1))+1;
istart = find(diff([0;snodes;0]));
%
% mark edge as traversed
edges(1,:) = NaN;
%
ntodo = size(edges,1)-1;
%
flipped = false;
while 1
    %
    % search for an edge that connects to node n2.
    % these edges are numbered irow(istart(n2):istart(n2+1)-1)
    % they may already be traversed, so let's check which one is available
    %
    found = false;
    for r2 = istart(n2):istart(n2+1)-1
        l2 = irow(r2);
        if edges(l2,1)==n2
            found = true;
            n3 = edges(l2,2);
            break
        elseif edges(l2,2)==n2
            found = true;
            n3 = edges(l2,1);
            break
        end
    end
    %
    if ~found
        if flipped
            if ntodo>0
                % start new polyline using first unused edge
                todo = find(~isnan(edges(:,1)));
                todo = todo(1);
                %
                i1 = i2+2;
                %
                contour(i1:i1+1) = edges(todo,:);
                n1 = contour(i1);
                n2 = contour(i1+1);
                i2 = i1+1;
                %
                % mark edge as traversed
                edges(todo,:) = NaN;
                ntodo = ntodo-1;
                %
                flipped = false;
            else
                % all edges done
                break
            end
        else
            % no edge to continue ... try flipping
            contour(i1:i2) = contour(i2:-1:i1);
            n1 = contour(i1);
            n2 = contour(i2);
            flipped = true;
        end
        continue
    end
    %
    % n3 is the number of the other node
    % add it to the contour
    i2=i2+1;
    contour(i2) = n3;
    %
    % mark edge as traversed
    edges(l2,1:2) = NaN;
    ntodo = ntodo-1;
    %
    % continue the search from the latest node
    n2 = n3;
end
%
% TODO: check here how many polylines have been created
%
% put contour back into X,Y,Z
contour = contour(1:i2);
bpoint = isnan(contour);
contour(bpoint) = 1;
X = uxyz(contour,1);
X(bpoint) = NaN;
Y = uxyz(contour,2);
Y(bpoint) = NaN;
if nargout==3
    Z = uxyz(contour,3);
    Z(bpoint) = NaN;
end

