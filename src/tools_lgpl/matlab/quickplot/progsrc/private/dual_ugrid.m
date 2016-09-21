function data = dual_ugrid(data)
%DUAL_UGRID Create dual unstructured grid

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/dual_ugrid.m $
%   $Id: dual_ugrid.m 5507 2015-10-20 09:50:46Z jagers $ 

X = data.X;
Y = data.Y;
FaceNodeConnect = data.FaceNodeConnect;
if isfield(data,'EdgeNodeConnect')
    EdgeNodeConnect = data.EdgeNodeConnect;
else
    EdgeNodeConnect = [];
end
%-----
FNCe = FaceNodeConnect(:,[ceil(1:0.5:size(FaceNodeConnect,2)) 1])';
FNCe(isnan(FNCe)) = [];
FNCe = reshape(FNCe,[2 numel(FNCe)/2])';
if isempty(EdgeNodeConnect)
    EdgeNodeConnect = unique(sort(FNCe,2),'rows');
end
%-----
nEdges = size(EdgeNodeConnect,1);
nNodes = length(X);
nFaces = size(FaceNodeConnect,1);
%-----
missing = isnan(FaceNodeConnect);
FaceNodeConnect(missing) = 1;
nc=sum(~missing,2);
%-----
if isfield(data,'XFace')
    xFace = data.XFace;
    yFace = data.YFace;
    data = rmfield(data,{'XFace','YFace'});
else
    xFace = X(FaceNodeConnect);
    xFace(missing)=0;
    xFace = sum(xFace,2)./nc;
    yFace = Y(FaceNodeConnect);
    yFace(missing)=0;
    yFace=sum(yFace,2)./nc;
end
%-----
nFacesPerNode = accumarray(FaceNodeConnect(:),1,[nNodes 1]);
nFacesPerNode(1) = nFacesPerNode(1)-sum(missing(:));
NFC = [FaceNodeConnect(:) repmat((1:nFaces)',size(FaceNodeConnect,2),1)];
NFC(missing,:) = [];
NFC = sortrows(NFC);
j = ones(max(nFacesPerNode)+1,nNodes);
j(nFacesPerNode+1+(0:nNodes-1)'*size(j,1)) = NaN;
j = cumsum(j);
j = j(~isnan(j));
NodeFaceConnect = NaN(nNodes,max(nFacesPerNode));
NodeFaceConnect(NFC(:,1)+(j-1)*nNodes) = NFC(:,2); % not yet oriented clockwise/counterclockwise!
%-----
nEdgesPerNode = accumarray(EdgeNodeConnect(:),1,[nNodes 1]);
NEC = sortrows([EdgeNodeConnect(:) repmat((1:nEdges)',2,1)]);
j = ones(max(nEdgesPerNode)+1,nNodes);
j(nEdgesPerNode+1+(0:nNodes-1)'*size(j,1)) = NaN;
j = cumsum(j);
j = j(~isnan(j));
NodeEdgeConnect = NaN(nNodes,max(nEdgesPerNode));
NodeEdgeConnect(NEC(:,1)+(j-1)*nNodes) = NEC(:,2); % not yet oriented clockwise/counterclockwise!
%-----
iBoundaryNodes = find(nEdgesPerNode>nFacesPerNode);
%-----
FCe = repmat(1:nFaces,size(FaceNodeConnect,2),1);
FCe = FCe(~missing');
[lia,edgeNr] = ismember(FNCe,EdgeNodeConnect,'rows');
[lia2,edgeNr2] = ismember(FNCe(:,[2 1]),EdgeNodeConnect,'rows');
EdgeFaceConnect = NaN(nEdges,2);
EdgeFaceConnect(edgeNr(lia),1)   = FCe(lia);
EdgeFaceConnect(edgeNr2(lia2),2) = FCe(lia2);
%-----
iBoundaryEdges = find(any(isnan(EdgeFaceConnect),2));
%-----
for i=1:nNodes
    nEdges_i = nEdgesPerNode(i);
    nFaces_i = nFacesPerNode(i);
    if nFaces_i==0 && nEdges_i==0
        continue
    end
    %
    Faces_i = NodeFaceConnect(i,1:nFaces_i);
    Edges_i = NodeEdgeConnect(i,1:nEdges_i);
    Edges_i_Faces = EdgeFaceConnect(Edges_i,:);
    %
    reversed = EdgeNodeConnect(Edges_i,2)==i;
    Edges_i_Faces(reversed,:) = Edges_i_Faces(reversed,[2 1]);
    %
    newFaces_i = Faces_i;
    newEdges_i = Edges_i;
    %
    if nEdges_i>nFaces_i % boundary node
        jE = find(isnan(Edges_i_Faces(:,1)));
    else
        jE = 1;
    end
    newEdges_i(1) = jE;
    %
    for k=1:nFaces_i
        jF = Edges_i_Faces(jE,2);
        newFaces_i(k) = jF;
        jE = find(Edges_i_Faces(:,1)==jF);
        if k<nEdges_i
            newEdges_i(k+1) = jE;
        end
    end
    %
    NodeFaceConnect(i,1:nFaces_i) = newFaces_i;
    NodeEdgeConnect(i,1:nEdges_i) = Edges_i(newEdges_i);
end
%-----
iBE(iBoundaryEdges,1) = 1:length(iBoundaryEdges);
nBoundEdges = length(iBoundaryEdges);
nBoundNodes = length(iBoundaryNodes);
xBoundEdge = sum(X(EdgeNodeConnect(iBoundaryEdges,:)),2)/2;
yBoundEdge = sum(Y(EdgeNodeConnect(iBoundaryEdges,:)),2)/2;
xBoundNode = X(iBoundaryNodes);
yBoundNode = Y(iBoundaryNodes);
%-----
iBoundEdge1 = iBE(NodeEdgeConnect(iBoundaryNodes,1));
iBoundEdge2 = iBE(NodeEdgeConnect(iBoundaryNodes + nNodes*(nEdgesPerNode(iBoundaryNodes,:)-1)));
%-----
data.X = [xFace;xBoundEdge;xBoundNode];
data.Y = [yFace;yBoundEdge;yBoundNode];
NodeFaceDual = NodeFaceConnect;
nFacesPerBoundNode_max = max(nFacesPerNode(iBoundaryNodes));
NodeFaceDual(:,end+1:nFacesPerBoundNode_max+3) = NaN;
for i = 1:nBoundNodes
    nFpN = nFacesPerNode(iBoundaryNodes(i));
    NodeFaceDual(iBoundaryNodes(i),1:nFpN+3) = [nFaces+nBoundEdges+i nFaces+iBoundEdge1(i) NodeFaceConnect(iBoundaryNodes(i),1:nFpN) nFaces+iBoundEdge2(i)];
end
data.FaceNodeConnect = NodeFaceDual;
% WARNING: The boundary faces will not be convex if the model domain at the
% boundary node is not convex! However this is not a major problem since in
% qp_scalarfield we will subdivide the face into triangles for contours and
% continuous shades plotting and those triangles will be generated based by
% combining node 1 with nodes j and j+1. Since we use the central boundary
% node as node 1 all triangles generated will be located inside the domain.
%-----
bEdgeVal = data.Val(max(EdgeFaceConnect(iBoundaryEdges,:),[],2));
bNodeVal = (bEdgeVal(iBoundEdge1) + bEdgeVal(iBoundEdge2))/2;
data.Val = [data.Val;bEdgeVal;bNodeVal];
data.ValLocation = 'NODE';