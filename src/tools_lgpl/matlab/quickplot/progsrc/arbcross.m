function [varargout]=arbcross(varargin)
%ARBCROSS Arbitrary cross-section through grid.
%   [X,Y,V1,V2, ...]=ARBCROSS(TRI,XTRI,YTRI,VTRI1,VTRI2, ... ,XB,YB)
%   Intersects a triangular mesh defined by TRI, XTRI and YTRI with an
%   arbitrary line defined by base points XB, YB. The output vectors X
%   and Y contain the co-ordinates at which the line crosses the grid
%   lines of the mesh. The vector Vi contains interpolated values at
%   these locations given the values VTRIi at the mesh points.
%
%   [X,Y,V1,V2, ...]=ARBCROSS(XGRID,YGRID,VGRID1,VGRID2, ... ,XB,YB)
%   Intersects a curvilinear mesh defined by XGRID and YGRID with an
%   arbitrary line.
%
%   Computing the locations of the intersections of the mesh and the line
%   can take a significant amount of time. It can be more efficient to
%   compute these intersections and the associated coefficients for the
%   interpolation only once. The necessary intermediate information can
%   be stored in a structure by using the following syntax:
%   STRUCT=ARBCROSS(TRI,XTRI,YTRI,XB,YB)
%   [X,Y,STRUCT]=ARBCROSS(TRI,XTRI,YTRI,XB,YB)
%   STRUCT=ARBCROSS(XGRID,YGRID,XB,YB)
%   [X,Y,STRUCT]=ARBCROSS(XGRID,YGRID,XB,YB)
%
%   Subsequently, the interpolation of data to that line can be carried
%   out efficiently by providing the structure as a first argument
%   instead of the original coordinates:
%   [V1,V2, ...] = ARBCROSS(STRUCT,VGRID1,VGRID2, ...)
%   [X,Y,V1,V2, ...] = ARBCROSS(STRUCT,VGRID1,VGRID2, ...)

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/arbcross.m $
%   $Id: arbcross.m 5507 2015-10-20 09:50:46Z jagers $

varargout=cell(1,nargout);
if (nargout+1==nargin || nargout-1==nargin) && nargin>1 && isstruct(varargin{1})
    %
    % One input argument more than there are output arguments: besides the
    % fields that match between input and output (VGRIDi - Vi), the list of
    % input arguments contains also a structure. The structure contains all
    % information necessary for creating the cross-section; this structure
    % should be created using a previous ARBCROSS call.
    %
    input_offset = 1;
    if nargout+1==nargin
        output_offset = 0;
    else
        output_offset = 2;
    end
    input_skip_end = 0;
    %
    Keep = varargin{1};
    x = Keep.x;
    y = Keep.y;
    wght = Keep.wght;
    iNode = Keep.iNode;
    iFace = Keep.iFace;
    iEdge = Keep.iEdge;
    outside = Keep.outside;
    %dxt = Keep.dxt;
    %dyt = Keep.dyt;
    VGRIDStr = Keep.VGRIDStr;
    szXGRID = Keep.szXGRID;
    nFaces = Keep.nFaces;
else
    %
    % No structure containing all necessary information.
    %
    input_skip_end = 2;
    layeredxy = 0;
    if nargout+2==nargin || ((nargout==1 || nargout==3) && nargin==4)
        %
        % CURVILINEAR GRID:
        % (1) Two input arguments more than there are output arguments:
        % besides the fields that match between input and output (VGRIDi -
        % Vi), the list of output arguments also contains X and Y (the
        % coordinates of the intersections of the grid and the line) whereas
        % the list of input arguments also contains XGRID, YGRID (first
        % two arguments) and XB, YB (last two arguments). The first two
        % arguments define the curvilinear grid on which the data are
        % defined, and the last two arguments define the line of the
        % cross-section.
        % (2) When you create a structure, there are two options:
        % -a- one output argument (the structure) and four input arguments,
        % namely XGRID, YGRID, XB and YB. See description above.
        % -b- three output arguments: the structure, X and Y. Same input
        % arguments as in case -a-.
        %
        structure_out = nargout+2~=nargin;
        %
        input_offset = 2;
        if nargout==1
            output_offset = 0;
        else
            output_offset = 2;
        end
        XGRID = varargin{1}(:,:,1);
        YGRID = varargin{2}(:,:,1);
        [FaceNodeConnect,QUADTRI] = grid2tri(XGRID,YGRID);
        EdgeNodeConnect = [];
        layeredxy = size(varargin{1},3)>1;
        VGRIDStr = 'VGRID';
        nFaces = 0;
    elseif nargout+3==nargin || nargout+4==nargin || ...
            ((nargout==1 || nargout==3) && (nargin==5 || nargin==6))
        %
        % TRIANGULAR MESH / UGRID MESH
        % (1) Three input arguments more than there are output arguments:
        % besides the fields that match between input and output (VGRIDi -
        % Vi), the list of output arguments also contains X and Y (the
        % coordinates of the intersections of the grid and the line) whereas
        % the list of input arguments also contains TRI, XTRI, YTRI (first
        % two arguments) and XB, YB (last two arguments). The first three
        % arguments define the triangular mesh on which the data are defined,
        % and the last two arguments define the line of the cross-section.
        % (2) When you create a structure, there are two options:
        % -a- one output argument (the structure) and five input arguments,
        % namely TRI, XTRI, YTRI, XB and YB. See description above.
        % -b- three output arguments: the structure, X and Y. Same input
        % arguments as in case -a-.
        %
        structure_out = nargout+3~=nargin;
        %
        if nargin==6
            input_offset = 4;
        else
            input_offset = 3;
        end
        if nargout==1
            output_offset = 0;
        else
            output_offset = 2;
        end
        FaceNodeConnect = varargin{1};
        if input_offset==4
            EdgeNodeConnect = varargin{4};
        else
            EdgeNodeConnect = [];
        end
        QUADTRI = [];
        XGRID = varargin{2}(:,:,1);
        YGRID = varargin{3}(:,:,1);
        VGRIDStr = 'VTRI';
        nFaces = size(FaceNodeConnect,1);
    else
        error('Number of input arguments does not match number of output arguments.');
    end

    %
    % Determine intersection points of cross-section line and curvilinear
    % grid or triangular mesh.
    %
    XB=varargin{end-1};
    YB=varargin{end};
    [x,y,iNode,wght,iFace,fracudist,dxt,dyt,outside]=int_lntri(XB,YB,FaceNodeConnect,XGRID,YGRID);
    iEdge = [];

    %
    % Add dummy points where the slice goes out of the computational domain
    % such that there will appear a break in the plots.
    %
    for i=length(outside):-1:1
        if outside(i)
            ii = [1:i i i+1:length(x)];
            x=x(ii); x(i+1)=NaN;
            y=y(ii); y(i+1)=NaN;
            iNode=iNode(ii,:);
            wght=wght(ii,:); wght(i+1,:)=NaN;
            fracudist=fracudist(ii);
            ii = [1:i i i+1:length(iFace)];
            iFace=iFace(ii);
            outside=outside(ii);
            dxt=dxt(ii);
            dyt=dyt(ii);
        end
    end
    %
    % Remove diagonals ... maybe it would be faster to not put them in in
    % the first place, but maybe I need them again in the future for
    % consistency.
    %
    if ~isempty(QUADTRI)
        iFace = QUADTRI(iFace);
        rm = find((iFace(1:end-1)==iFace(2:end)) & ~isnan(x(2:end-1,1)));
        x(rm+1,:)        =[];
        y(rm+1,:)        =[];
        wght(rm+1,:)     =[];
        iFace(rm+1,:)    =[];
        iNode(rm+1,:)    =[];
        fracudist(rm+1,:)=[];
        outside(rm,:)    =[];
        dxt(rm,:)        =[];
        dyt(rm,:)        =[];
    end
    %
    % Expand coordinates to 3D if original x/y arrays were 3D.
    %
    if layeredxy
        x = repmat(x,[1 1 size(varargin{1},3)]);
        y = repmat(y,[1 1 size(varargin{2},3)]);
    end
    %
    %
    %
    if ~isempty(EdgeNodeConnect)
        edgeCrossing = sum(~isnan(wght) & wght~=0,2)==2;
        edgeNodes = sort(iNode(:,1:2),2);
        EdgeNodeConnect = sort(EdgeNodeConnect,2);
        [isEdge,edgeNr] = ismember(edgeNodes,EdgeNodeConnect,'rows');
        iEdge = NaN(size(wght,1),1);
        iEdge(isEdge & edgeCrossing) = edgeNr(isEdge & edgeCrossing);
    end
    %
    % Define structure for future use
    %
    szXGRID = size(XGRID);
    if structure_out
        Keep.x = x;
        Keep.y = y;
        Keep.wght = wght;
        Keep.iNode = iNode;
        Keep.fracudist = fracudist;
        Keep.iFace = iFace;
        Keep.iEdge = iEdge;
        Keep.outside = outside;
        Keep.dxt = dxt;
        Keep.dyt = dyt;
        Keep.VGRIDStr = VGRIDStr;
        Keep.szXGRID = szXGRID;
        Keep.nFaces = nFaces;
        varargout{nargout} = Keep;
    end
end

%
% Define output
%
if output_offset>0
    varargout{1}=x;
    varargout{2}=y;
end
%
% For each data field VGRIDi in the list of input arguments, i.e. those
% starting after the grid information (unless it is a 3D grid) and stopping
% before the last two arguments.
%
for i=1:nargin-input_offset-input_skip_end
    VLOC = '?';
    VGRID = varargin{input_offset+i};
    if iscell(VGRID)
        VLOC  = VGRID{1};
        VGRID = VGRID{2};
    end
    szVGRID = size(VGRID);
    if strcmp(VLOC,'NODE') || ...
            (strcmp(VLOC,'?') && ...
             (isequal(szVGRID(1:2),szXGRID) || ...
              (isequal(szVGRID([2 1]),szXGRID) && szXGRID(2)==1)))
        %
        % Values defined at mesh nodes
        %
        v=[];
        for k = size(VGRID,3):-1:1
            vgrid = VGRID(:,:,k);
            v(:,1,k) = sum(wght.*vgrid(iNode),2);
        end
    elseif strcmp(VLOC,'FACE') || ...
            (strcmp(VLOC,'?') && ...
             (isequal(szVGRID(1:2),szXGRID-1) || ...
              numel(VGRID)==nFaces))
        %
        % Values defined on mesh patches
        %
        v=[];
        for k = size(VGRID,3):-1:1
            vgrid = VGRID(:,:,k);
            v(:,1,k) = vgrid(iFace);
            v(outside,1,k) = NaN;
        end
    elseif strcmp(VLOC,'EDGE')
        noEdge = isnan(iEdge);
        iEdge(noEdge) = 1;
        for k = size(VGRID,3):-1:1
            vgrid = VGRID(:,:,k);
            v(:,1,k) = vgrid(iEdge);
        end
        v(noEdge,:) = NaN;
    else
        error('Invalid size of %s%i',VGRIDStr,i)
    end
    varargout{output_offset+i} = v;
end


function [tri,quadtri]=grid2tri(X,Y)
% GRID2TRI converts a curvilinear grid into a triangular grid
%       [TRI,QUADTRI]=GRID2TRI(XGRID,YGRID)
%       Splits the quadrangles of the curvilinear grid along the main
%       diagonal and returns the triangle definition table TRI (indicating
%       the corner points of the triangles as indices into XGRID, YGRID)
%       and an array QUADTRI that contains for every triangle the index of
%       the quadrangle to which the triangle belongs (index into an array
%       of size SIZE(XGRID)-1).

szX=size(X);
% [m,n]=ndgrid(1:szX(1),1:szX(2));
I=reshape(1:prod(szX),szX);
I=I(1:end-1,1:end-1);
I=I(:);
quad=(1:prod(szX-1))';

tri= [I I+1 I+szX(1)+1; I I+szX(1) I+szX(1)+1];
quadtri= [quad;quad];
% mtri= [m(I);m(I)];
% ntri= [n(I);n(I)];

k=any(isnan(X(tri)) | isnan(Y(tri)),2);
tri(k,:)=[];
quadtri(k)=[];
