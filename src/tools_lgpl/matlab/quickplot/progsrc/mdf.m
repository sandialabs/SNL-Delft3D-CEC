function varargout = mdf(cmd,varargin)
%MDF Manipulate Delft3D-FLOW mdf files.
%   MDF_STRUCT = MDF('read',FILENAME) read mdf file and various attribute
%   files into structure.
%
%   MDF_OUT = MDF('rotate',MDF_IN,ANGLE) rotates the model administration
%   by ANGLE degrees; ANGLE can be 0, 90 (default), 180 or 270 degrees.
%
%   MDF('write',MDF_STRUCT,CASENAME,PATH) stores the data of the MDF_STRUCT
%   in an mdf file named CASENAME.mdf and all files are stored using the
%   same CASENAME combined with their default extension.
%
%   See also WLGRID, WLDEP, D3D_ATTRIB.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/progsrc/mdf.m $
%   $Id: mdf.m 65778 2020-01-14 14:07:42Z mourits $

switch lower(cmd)
    case 'read'
        varargout{1} = masterread(varargin{:});
    case 'rotate'
        varargout{1} = mdfrotate(varargin{:});
    case 'clip'
        [varargout{1:max(1,nargout)}] = mdfclip(varargin{:});
    case 'write'
        mdfwrite(varargin{:});
    otherwise
        error('Unknown command: %s',var2str(cmd)) 
end


function varargout = mdfclip(MDF1,varargin)
% MDFCLIP(MDF,MASK)
% MDFCLIP(MDF,MMIN,MMAX,NMIN,NMAX
% MDFCLIP(MDF,MLIM,NLIM)
%
mnkmax = propgetval(MDF1.mdf,'','MNKmax');
switch nargin
    case 2
        % MDFCLIP(MDF,MASK)
        mask = varargin{1};
        if ~isnumeric(mask) && ~islogical(mask)
            error('MASK array should be logical or numeric')
        elseif ~isequal(size(mask),mnkmax(1:2))
            error('MASK argument should be size %i x %i.',mnkmax(1:2))
        end
    case 3
        % MDFCLIP(MDF,MLIM,NLIM)
        mask = zeros(mnkmax(1:2));
        mlim = varargin{1};
        if ~isnumeric(mlim) || ~isequal(size(mlim),[1 2])
            error('Invalid MLIM argument.')
        end
        nlim = varargin{2};
        if ~isnumeric(nlim) || ~isequal(size(nlim),[1 2])
            error('Invalid NLIM argument.')
        end
        mask(mlim(1):mlim(2),nlim(1):nlim(2))=1;
    case 5
        % MDFCLIP(MDF,MMIN,MMAX,NMIN,NMAX
        mask = zeros(mnkmax(1:2));
        mmin = varargin{1};
        if ~isnumeric(mmin) || ~isequal(size(mmin),[1 1])
            error('Invalid MMIN argument.')
        end
        mmax = varargin{2};
        if ~isnumeric(mmax) || ~isequal(size(mmax),[1 1])
            error('Invalid MMAX argument.')
        end
        nmin = varargin{3};
        if ~isnumeric(nmin) || ~isequal(size(nmin),[1 1])
            error('Invalid NMIN argument.')
        end
        nmax = varargin{4};
        if ~isnumeric(nmax) || ~isequal(size(nmax),[1 1])
            error('Invalid NMAX argument.')
        end
        mask(mmin:mmax,nmin:nmax)=1;
    otherwise
        error('Invalid input arguments for mdf(''clip'',...)')
end
%
% find mask numbers; replace NaNs by 0; exclude 0 from domain number list
%
mask(isnan(mask)) = 0;
mask([1 end],:) = 0;
mask(:,[1 end]) = 0;
domains = unique(mask(:));
domains(domains==0)=[];
%
if nargout<length(domains)
    warning('More domains indicated in MASK array than output arguments.')
    domains = domains(1:nargout);
end
varargout = cell(1,length(domains));
for i = 1:length(domains)
    mask1 = mask==domains(i);
    MDF2 = MDF1;
    %
    MAct = find(any(mask1,2));
    mmin = MAct(1)-1;
    mmax = MAct(end)+1;
    NAct = find(any(mask1,1));
    nmin = NAct(1)-1;
    nmax = NAct(end)+1;
    %
    cmask = ~mask1(mmin:mmax,nmin:nmax);
    gmask = cmask([2:end end],[2:end end])&cmask(:,[2:end end])&cmask([2:end end],:)&cmask;
    %
    MDF2.mdf = inifile('seti',MDF2.mdf,'','MNKmax',[mmax-mmin+1 nmax-nmin+1 mnkmax(3)]);
    MDF2.grd.X = MDF2.grd.X(mmin:mmax-1,nmin:nmax-1);
    MDF2.grd.X(gmask(1:end-1,1:end-1))=MDF2.grd.MissingValue;
    MDF2.grd.Y = MDF2.grd.Y(mmin:mmax-1,nmin:nmax-1);
    MDF2.grd.Y(gmask(1:end-1,1:end-1))=MDF2.grd.MissingValue;
    MDF2.grd.Enclosure = enclosure('extract',MDF2.grd.X,MDF2.grd.Y);
    %
    if isfield(MDF2,'dep')
        MDF2.dep = MDF2.dep(mmin:mmax,nmin:nmax);
        %
        dpsopt = propget(MDF2.mdf,'','Dpsopt','DEFAULT');
        if strcmpi(dpsopt,'DP')
            MDF2.dep(cmask)=-999;
        else
            MDF2.dep(gmask)=-999;
        end
    end
    %
    varargout{i} = MDF2;
end


function [M2,N2] = rotate(M1,N1,MMAX)
if nargin==2
    MMAX = N1;
    N1 = M1(:,2);
    M1 = M1(:,1);
end
M2 = N1;
N2 = MMAX-M1+1;
if nargout<=1
    M2 = [M2 N2];
end


function varargout = fldrotateX(loc,varargin)
for i=1:nargin-1
    szA = size(varargin{i});
    varargin{i} = reshape(varargin{i},szA(2:end));
end
[varargout{1:nargout}] = fldrotate(loc,varargin{:});
for i=1:nargout
    szA = size(varargout{i});
    varargout{i} = reshape(varargout{i},[1 szA]);
end


function [U1,V1] = fldrotate(loc,U,V)
szU = size(U);
switch loc
    case 'center'
        if length(szU)>2
            U1 = repmat(U(1),szU([2 1 3:end]));
            for k = 1:prod(szU(3:end))
                U1(:,:,k) = rot90(U(:,:,k),-1);
            end
        else
            U1 = rot90(U,-1);
        end
    case 'corner'
        szM = szU(1);
        szN = szU(2);
        if length(szU)>2
            U1 = repmat(U(1),szU([2 1 3:end]));
            U1(:,end,:) = -999;
            U1(end,:,:) = -999;
            for k = 1:prod(szU(3:end))
                U1(1:end-1,1:end-1,k) = rot90(U(1:end-1,1:end-1,k),-1);
            end
        else
            U1 = [rot90(U(1:end-1,1:end-1),-1) repmat(-999,szN-1,1); repmat(-999,1,szM)];
        end
    case 'edges'
        if length(szU)>2
            U1 = repmat(U(1),szU([2 1 3:end]));
            V1 = repmat(V(1),szU([2 1 3:end]));
            for k = 1:prod(szU(3:end))
                U1(:,:,k) = rot90(V([end 1:end-1],:,k),-1);
                V1(:,:,k) = rot90(U(:,:,k),-1);
            end
        else
            U1 = rot90(V([end 1:end-1],:),-1);
            V1 = rot90(U,-1);
        end
    case 'veloc'
        if length(szU)>2
            U1 = repmat(U(1),szU([2 1 3:end]));
            V1 = repmat(V(1),szU([2 1 3:end]));
            for k = 1:prod(szU(3:end))
                U1(:,:,k) = rot90(V([end 1:end-1],:,k),-1);
                V1(:,:,k) = rot90(-U(:,:,k),-1);
            end
        else
            U1 = rot90(V([end 1:end-1],:),-1);
            V1 = rot90(-U,-1);
        end
end

function MDF2 = mdfrotate(MDF1,angle)
if nargin>1
    switch angle
        case 0
            MDF2 = MDF1;
            return
        case 90
            % going to do here ...
        case 180
            % rotate first 90 degrees
            MDF1 = mdfrotate(MDF1);
            % other 90 degrees going to do here ...
        case 270
            % rotate first 90 degrees
            MDF1 = mdfrotate(MDF1);
            % rotate second 90 degrees
            MDF1 = mdfrotate(MDF1);
            % final 90 degrees going to do here ...
        otherwise
            error('Can only rotate 0, 90, 180 and 270 degrees.')
    end
end
MDF2 = MDF1;
%
mnkmax = propgetval(MDF2.mdf,'','MNKmax');
MMAX = mnkmax(1);
MDF2.mdf = inifile('seti',MDF2.mdf,'','MNKmax',mnkmax([2 1 3]));
%
ccofu = propget(MDF2.mdf,'','Ccofu');
ccofv = propget(MDF2.mdf,'','Ccofv');
MDF2.mdf = inifile('seti',MDF2.mdf,'','Ccofu',ccofv);
MDF2.mdf = inifile('seti',MDF2.mdf,'','Ccofv',ccofu);
%
MDF2.grd.X = fliplr(MDF2.grd.X');
MDF2.grd.Y = fliplr(MDF2.grd.Y');
MDF2.grd.Enclosure = rotate(MDF2.grd.Enclosure,MMAX);
%
if isfield(MDF2,'dep')
    dpsopt = propget(MDF2.mdf,'','Dpsopt');
    if strcmpi(dpsopt,'DP')
        % data in cell centres: dummy row along all sides
        MDF2.dep = fldrotate('center',MDF2.dep);
    else
        % data at grid points: dummy row only at high M and N
        MDF2.dep = fldrotate('corner',MDF2.dep);
    end
end
%
if isfield(MDF2,'rgh')
    % data at velocity points
    [MDF2.rgh(1).Data,MDF2.rgh(2).Data] = fldrotate('edges',MDF2.rgh(1).Data,MDF2.rgh(2).Data);
end
%
if isfield(MDF2,'ini')
    if ~isfield(MDF2.ini,'FileType')
        % plain binary restart file (flow only)
        % 
        % water level: data at cell centres
        MDF2.ini(1).Data = fldrotate('center',MDF2.ini(1).Data);
        % velocities: data at velocity points
        for k = 1:mnkmax(3)
            iu = 1+k;
            iv = iu + mnkmax(3);
            [MDF2.ini(iu).Data,MDF2.ini(iv).Data] = fldrotate('veloc',MDF2.ini(iu).Data,MDF2.ini(iv).Data);
        end
        % constituents and turbulent quantities: data at cell centres
        for f = 2+2*mnkmax(3):length(MDF2.ini)-2
            MDF2.ini(f).Data = fldrotate('center',MDF2.ini(f).Data);
        end
        % u/v mnldf: data at velocity points
        iu = length(MDF2.ini)-1;
        iv = length(MDF2.ini);
        [MDF2.ini(iu).Data,MDF2.ini(iv).Data] = fldrotate('veloc',MDF2.ini(iu).Data,MDF2.ini(iv).Data);
    else
        % NEFIS map-file
        oldsz = [MDF2.ini.Data.map_const.NMAX MDF2.ini.Data.map_const.MMAX];
        newsz = oldsz([2 1]);
        for i = 1:length(MDF2.ini.ElmDef)
            if length(MDF2.ini.ElmDef(i).Size)>2 && isequal(MDF2.ini.ElmDef(i).Size(2:3),oldsz)
                MDF2.ini.ElmDef(i).Size(2:3) = newsz;
            end
        end
        %
        MDF2.ini.Data.map_const.NMAX = newsz(1);
        MDF2.ini.Data.map_const.MMAX = newsz(2);
        %
        for gc = fieldnames(MDF2.ini.Data)'
            g = gc{1};
            for ec = fieldnames(MDF2.ini.Data.(g))'
                e = ec{1};
                sz = size(MDF2.ini.Data.(g).(e));
                if length(sz)>2 && isequal(sz(2:3),oldsz)
                    switch e
                        case {'XCOR','YCOR','DP0','CODB'} % DP0 only if not dpsopt=DP
                            % data at cell corners
                            MDF2.ini.Data.(g).(e) = fldrotateX('corner',MDF2.ini.Data.(g).(e));
                        case {'KFU','KCU'}
                            % data at cell edges
                            e2 = strrep(e,'U','V');
                            [MDF2.ini.Data.(g).(e),MDF2.ini.Data.(g).(e2)] = fldrotateX('edges',MDF2.ini.Data.(g).(e),MDF2.ini.Data.(g).(e2));
                        case {'U1','TAUKSI','UMNLDF','SBUU','SSUU','SBUUA','SSUUA'}
                            % data at cell edges - velocity components
                            if strcmp(e,'TAUKSI')
                                e2 = 'TAUETA';
                            else
                                e2 = strrep(e,'U','V');
                            end
                            [MDF2.ini.Data.(g).(e),MDF2.ini.Data.(g).(e2)] = fldrotateX('veloc',MDF2.ini.Data.(g).(e),MDF2.ini.Data.(g).(e2));
                        case {'KFV','KCV','V1','TAUETA','VMNLDF','SBVV','SSVV','SBVVA','SSVVA'}
                            % skip - treated with U component
                        otherwise
                            % data at cell centres
                            MDF2.ini.Data.(g).(e) = fldrotateX('center',MDF2.ini.Data.(g).(e));
                    end
                end
            end
        end
    end
end
%
if isfield(MDF2,'dry')
    MDF2.dry.MN(:,1:2) = rotate(MDF2.dry.MN(:,1:2),MMAX);
    MDF2.dry.MN(:,3:4) = rotate(MDF2.dry.MN(:,3:4),MMAX);
end
%
for fldc = {'thd','bar','gat','cdw','w2d','lwl','ppl','rgs'}
    fld = fldc{1};
    if isfield(MDF2,fld)
        FLD = MDF2.(fld);
        %
        if isfield(FLD,'MNu')
            MNu = 'MNu';
            MNv = 'MNv';
        else
            MNu = 'MNKu';
            MNv = 'MNKv';
        end
        FLD.(MNu)(:,1:2) = rotate(FLD.(MNu)(:,1:2),MMAX);
        FLD.(MNu)(:,3:4) = rotate(FLD.(MNu)(:,3:4),MMAX);
        FLD.(MNv)(:,1:2) = rotate(FLD.(MNv)(:,1:2),MMAX);
        FLD.(MNv)(:,3:4) = rotate(FLD.(MNv)(:,3:4),MMAX);
        TMP_MN = FLD.(MNu);
        TMP_CHAR = FLD.CHARu;
        FLD.(MNu) = FLD.(MNv);
        FLD.CHARu = FLD.CHARv;
        FLD.(MNv) = TMP_MN;
        FLD.CHARv = TMP_CHAR;
        %
        MDF2.(fld) = FLD;
    end
end
%
if isfield(MDF2,'fls')
    MDF2.fls = fldrotate('center',MDF2.fls);
end
%
if isfield(MDF2,'bnd')
    MDF2.bnd.MN(:,1:2) = rotate(MDF2.bnd.MN(:,1:2),MMAX);
    MDF2.bnd.MN(:,3:4) = rotate(MDF2.bnd.MN(:,3:4),MMAX);
    %
    for i = 1:length(MDF2.bnd.Name)
        if any('CQTRN'==MDF2.bnd.BndType(i))
            if MDF2.bnd.BndType(i)=='R'
                warning('Riemann data may need adjustment: sign of velocity component changes')
            end
            % if along N axis, then change sign of flux
            if MDF2.bnd.MN(i,1)~=MDF2.bnd.MN(i,3)
                switch MDF2.bnd.Forcing(i)
                    case 'T'
                        for j = 1:length(MDF2.bct.Table)
                            if strcmp(MDF2.bct.Table(j).Location,MDF2.bnd.Name{i})
                                MDF2.bct.Table(j).Data(:,2:end) = -MDF2.bct.Table(j).Data(:,2:end);
                                break
                            end
                        end
                end
            end
        end
    end
end
%
if isfield(MDF2,'morini') && isfield(MDF2.morini,'field')
    for f = 1:length(MDF2.morini.field)
        % all fields in cell centres
        MDF2.morini.field(f).data = fldrotate('center',MDF2.morini.field(f).data);
    end
end
%
if isfield(MDF2,'sta')
    MDF2.sta.MN = rotate(MDF2.sta.MN,MMAX);
end
%
if isfield(MDF2,'crs')
    MDF2.crs.MNMN(:,1:2) = rotate(MDF2.crs.MNMN(:,1:2),MMAX);
    MDF2.crs.MNMN(:,3:4) = rotate(MDF2.crs.MNMN(:,3:4),MMAX);
end


function mdfwrite(MDF,caseid,path)
if nargin<3
    path = '';
end
if isfield(MDF,'grd')
    filename = [caseid '.grd'];
    wlgrid('write',fullfile(path,filename),MDF.grd);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filcco',['#' filename '#']);
    %
    filename = [caseid '.enc'];
    MDF.mdf = inifile('seti',MDF.mdf,'','Filgrd',['#' filename '#']);
end
%
if isfield(MDF,'dep')
    filename = [caseid '.dep'];
    wldep('write',fullfile(path,filename),'',MDF.dep);
    MDF.mdf = inifile('seti',MDF.mdf,'','Fildep',['#' filename '#']);
end
%
if isfield(MDF,'rgh')
    filename = [caseid '.rgh'];
    wldep('write',fullfile(path,filename),MDF.rgh);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filrgh',['#' filename '#']);
end
%
if isfield(MDF,'ini')
    if ~isfield(MDF.ini,'FileType')
        % plain binary restart file (flow only)
        filename = ['tri-rst.' caseid];
        trirst('write',fullfile(path,filename),MDF.ini);
        MDF.mdf = inifile('seti',MDF.mdf,'','Restid',['#' caseid '#']);
    else
        % NEFIS map-file
        filename = ['trim-restart-for-' caseid];
        TRIMnew = vs_ini(fullfile(path,[filename '.dat']),fullfile(path,[filename '.def']));
        vs_copy(MDF.ini,TRIMnew);
        MDF.mdf = inifile('seti',MDF.mdf,'','Restid',['#' filename '#']);
    end
end
%
if isfield(MDF,'dry')
    filename = [caseid '.dry'];
    d3d_attrib('write',fullfile(path,filename),MDF.dry);
    MDF.mdf = inifile('seti',MDF.mdf,'','Fildry',['#' filename '#']);
end
%
if isfield(MDF,'thd')
    filename = [caseid '.thd'];
    d3d_attrib('write',fullfile(path,filename),MDF.thd);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filtd',['#' filename '#']);
end
%
if isfield(MDF,'bnd')
    filename = [caseid '.bnd'];
    d3d_attrib('write',fullfile(path,filename),MDF.bnd);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filbnd',['#' filename '#']);
end
%
if isfield(MDF,'bct')
    filename = [caseid '.bct'];
    bct_io('write',fullfile(path,filename),MDF.bct);
    MDF.mdf = inifile('seti',MDF.mdf,'','FilbcT',['#' filename '#']);
end
%
if isfield(MDF,'bch')
    filename = [caseid '.bch'];
    bch_io('write',fullfile(path,filename),MDF.bch);
    MDF.mdf = inifile('seti',MDF.mdf,'','FilbcH',['#' filename '#']);
end
%
if isfield(MDF,'bcc')
    filename = [caseid '.bcc'];
    bct_io('write',fullfile(path,filename),MDF.bcc);
    MDF.mdf = inifile('seti',MDF.mdf,'','FilbcC',['#' filename '#']);
end
%
if isfield(MDF,'sed')
    filename = [caseid '.sed'];
    inifile('write',fullfile(path,filename),MDF.sed);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filsed',['#' filename '#']);
end
%
if isfield(MDF,'morini')
    %
    if isfield(MDF.morini,'field')
        for f = 1:length(MDF.morini.field)
            Fld = MDF.morini.field(f);
            filename = sprintf('%s_layer%i_key%i.frc',caseid,Fld.lyr,Fld.key);
            wldep('write',fullfile(path,filename),'',Fld.data);
            MDF.morini.inb = inifile('seti',MDF.morini.inb,Fld.chp,Fld.key,['#' filename '#']);
        end
    end
    %
    filename = [caseid '.inb'];
    inifile('write',fullfile(path,filename),MDF.morini.inb);
    MDF.mor = inifile('seti',MDF.mor,'Underlayer','IniComp',['#' filename '#']);
end
%
if isfield(MDF,'mor')
    filename = [caseid '.mor'];
    inifile('write',fullfile(path,filename),MDF.mor);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filmor',['#' filename '#']);
end
%
if isfield(MDF,'tra')
    filename = [caseid '.tra'];
    inifile('write',fullfile(path,filename),MDF.tra);
    MDF.mdf = inifile('seti',MDF.mdf,'','TraFrm',['#' filename '#']);
end
%
if isfield(MDF,'sta')
    filename = [caseid '.obs'];
    d3d_attrib('write',fullfile(path,filename),MDF.sta);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filsta',['#' filename '#']);
end
%
if isfield(MDF,'crs')
    filename = [caseid '.crs'];
    d3d_attrib('write',fullfile(path,filename),MDF.crs);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filcrs',['#' filename '#']);
end
%
keys = {'Filbar' 'bar' 'bar'
    'Filgat' 'gat' 'gat'
    'Filcdw' 'cdw' 'cdw'
    'Fil2dw' 'w2d' '2dw'
    'Fillwl' 'lwl' 'lwl'
    'Filppl' 'ppl' 'ppl'
    'Filrgs' 'rgs' 'rgs'};
for i = 1:size(keys,1)
    key = keys{i,1};
    fld = keys{i,2};
    ext = keys{i,3};
    %
    if isfield(MDF,fld)
        filename = [caseid '.' ext];
        d3d_attrib('write',fullfile(path,filename),MDF.(fld));
        MDF.mdf = inifile('seti',MDF.mdf,'',key,['#' filename '#']);
    end
end
%
if isfield(MDF,'fls')
    filename = [caseid '.fls'];
    wldep('write',fullfile(path,filename),'',MDF.fls);
    MDF.mdf = inifile('seti',MDF.mdf,'','Filfls',['#' filename '#']);
end
%
filename = [caseid '.mdf'];
inifile('write',fullfile(path,filename),MDF.mdf);


function Val = propget(varargin)
Val = rmhash(inifile('geti',varargin{:}));


function MFile = masterread(filename)
%MFile = ddbread(filename);
master = inifile('open',filename);
master_path = fileparts(filename);
%
UNSPECIFIED = 'UNSPECIFIED';
block = 'model';
Program = propget(master,block,'Program',UNSPECIFIED);
if isequal(Program,UNSPECIFIED)
    block = 'general';
    Program = propget(master,block,'Program',UNSPECIFIED);
end
%
if isequal(Program,UNSPECIFIED)
    if inifile('existsi',master,'WaveFileInformation')
        MFile.FileType = 'Delft3D D-Wave';
        MFile.mdw = master;
        MFile = mdwread(MFile,master_path);
    elseif inifile('existsi',master,'General','fileType')
        fileType = propget(master,'General','fileType');
        fversion = inifile('getstringi',master,'General','fileVersion','');
        if isempty(fversion)
            mversion = inifile('getstringi',master,'General','majorVersion','');
            if isempty(mversion)
                fversion = '1.0';
            else
                iversion = inifile('getstringi',master,'General','minorVeriosn','');
                fversion = [mversion '.' iversion];
            end
        end
        switch fileType
            case 'modelDef'
                MFile.FileType = 'Delft3D D-Flow1D';
                MFile.md1d = master;
                MFile = md1dread(MFile,master_path);
            case '1D2D'
                MFile.FileType = 'Delft3D Coupled Model';
                MFile.config = master;
                %
                typeModel = inifile('geti',master,'Model','type');
                nameModel = inifile('geti',master,'Model','name');
                dirModel  = inifile('geti',master,'Model','directory');
                mdfModel  = inifile('geti',master,'Model','modelDefinitionFile');
                nModels = length(typeModel);
                %
                MFile.Domains = cell(nModels,3);
                MFile.Domains(:,1) = typeModel;
                MFile.Domains(:,2) = nameModel;
                for i = 1:length(typeModel)
                    fName = relpath(master_path,[dirModel{i} filesep mdfModel{i}]);
                    MFile.Domains{i,3} = masterread(fName);
                end
                %
                mappingFile = inifile('geti',master,'Files','mappingFile');
                Domain.FileType = 'Delft3D 1D2D mapping';
                Domain.mapping = inifile('open',relpath(master_path,mappingFile));
                MFile.Domains(end+1,:) = {'1D2D','1D2Dmapping',Domain};
            otherwise
                MFile.FileType = fileType;
                MFile.FileVersion = fversion;
                MFile.file = master;
        end
    elseif inifile('existsi',master,'','MNKmax')
        MFile.FileType = 'Delft3D D-Flow2D3D';
        MFile.mdf = master;
        MFile = mdfread(MFile,master_path);
    else
        error('Unable to determine type of simulation for %s',filename)
    end
else
    switch Program
        case 'D-Flow FM'
            MFile.FileType = 'Delft3D D-Flow FM';
            MFile.mdu = master;
            MFile.general = block;
            MFile = mduread(MFile,master_path);
        otherwise
            error('Unknown program type %s in %s',Program,filename)
    end
end


function DDB = ddbread(filename)
fid = fopen(filename,'r');
DDB.DomainNames = {};
iLine = 0;
iBound = 0;
while ~feof(fid)
    iLine = iLine+1;
    Line = fgetl(fid);
    [Dom1,Rem] = strtok(Line);
    [val,n] = sscanf(Rem,' %d %d %d %d %s %d %d %d %d');
    if n<7
        fclose(fid);
        error('Invalid DD syntax in line %i of %s',iLine,filename)
    end
    Dom2 = char(val(5:end-4)');
    Idx = val([1:4 end-3:end])';
    iBound = iBound+1;
    DDB.DomainNrs(iBound,1:2) = {Dom1 Dom2};
    DDB.DomainMN(iBound,1:8)  = Idx;
end
fclose(fid);
[DDB.DomainNames,dummy,DDB.DomainNrs]=unique(DDB.DomainNrs);
DDB.DomainNrs = reshape(DDB.DomainNrs,[iBound 2]);

function MF = md1dread(MF,md_path)
ntwname = propget(MF.md1d,'Files','networkFile');
if ~isempty(ntwname)
    ntwname = relpath(md_path,ntwname);
    MF.ntw = inifile('open',ntwname);
else
    error('Unable to locate networkFile keyword in [Files] chapter.');
end
%
crsname = propget(MF.md1d,'Files','crossLocFile');
if ~isempty(crsname)
    crsname = relpath(md_path,crsname);
    MF.crsLoc = inifile('open',crsname);
else
    error('Unable to locate crossLocFile keyword in [Files] chapter.');
end
%
bndname = propget(MF.md1d,'Files','boundLocFile');
if ~isempty(bndname)
    bndname = relpath(md_path,bndname);
    MF.bndLoc = inifile('open',bndname);
else
    error('Unable to locate boundLocFile keyword in [Files] chapter.');
end
%
bndname = propget(MF.md1d,'Files','latDischargeLocFile');
if ~isempty(bndname)
    bndname = relpath(md_path,bndname);
    MF.latLoc = inifile('open',bndname);
else
    error('Unable to locate latDischargeLocFile keyword in [Files] chapter.');
end
%
crsname = propget(MF.md1d,'Files','crossDefFile');
if ~isempty(crsname)
    crsname = relpath(md_path,crsname);
    MF.crsDef = inifile('open',crsname);
else
    error('Unable to locate crossDefFile keyword in [Files] chapter.');
end
%
CT=inifile('geti',MF.crsDef,'Definition','type');
if ~iscell(CT)
    CT = {CT};
end
CID=inifile('cgetstringi',MF.crsDef,'Definition','id');
CDF=inifile('cgetstringi',MF.crsLoc,'CrossSection','definition');
[lDF,iDF]=ismember(CDF,CID);
if ~all(lDF)
    missingDF = unique(CDF(~lDF));
    error('Missing cross section definitions: %s',sprintf('%s ',missingDF{:}))
else
    % copy CrossSection type to CrossSection location data structure
    MF.crsLoc=inifile('set',MF.crsLoc,'CrossSection','type',CT(iDF));
end
%
strname = propget(MF.md1d,'Files','structureFile');
if ~isempty(strname)
    strname = relpath(md_path,strname);
    MF.strucLoc = inifile('open',strname);
else
    error('Unable to locate structureFile keyword in [Files] chapter.');
end
%
obsname = propget(MF.md1d,'Files','obsPointsFile');
if ~isempty(obsname)
    obsname = relpath(md_path,obsname);
    MF.obs = inifile('open',obsname);
else
    error('Unable to locate obsPointsFile keyword in [Files] chapter.');
end
%


function MF = mduread(MF,md_path)
mshname = propget(MF.mdu,'geometry','NetFile');
if ~isempty(mshname)
    mshname = relpath(md_path,mshname);
    [F,Q] = getmesh(mshname);
    MF.mesh.nc_file = F;
    MF.mesh.quant = Q(1);
    %
    FirstPoint = netcdffil(F,1,Q(1),'grid',1);
    MF.mesh.XYUnits = FirstPoint.XUnits;
else
    error('Unable to locate NetFile keyword in [geometry] chapter.');
end
%
attfiles = {...
   'geometry','BedlevelFile','BedLevel'
   'geometry','DryPointsFile','DryPoints'
   'geometry','WaterLevIniFile','WaterLevIni'
   'geometry','LandBoundaryFile','LandBoundary'
   'geometry','ThinDamFile','ThinDam'
   'geometry','FixedWeirFile','FixedWeir'
   'geometry','ThindykeFile','Thindyke'
   'geometry','StructureFile','Structure'
   'geometry','VertplizFile','Vertpliz'
   'geometry','ProflocFile','Profloc'
   'geometry','ProfdefFile','Profdef'
   'geometry','ProfdefxyzFile','Profdefxyz'
   'geometry','ManholeFile','Manhole'
   'geometry','PartitionFile','Partition'
   'restart','RestartFile','Restart'
   'external forcing','ExtForceFile','ExtForce'
   'external forcing','ExtForceFileNew','ExtForceNew'
   'output','ObsFile','Obs'
   'output','CrsFile','Crs'
   'sediment','MorFile','Mor'
   'sediment','SedFile','Sed'};
for i = 1:size(attfiles,1)
    grp = attfiles{i,1};
    fld = attfiles{i,2};
    key = attfiles{i,3};
    %
    if strcmp(key,'BedLevel')
        Missing = -999;
        bltyp = propgetval(MF.mdu,grp,'BedlevType',Missing);
        if bltyp == Missing
            bltyp = propgetval(MF.mdu,grp,'BotlevType',Missing);
            if bltyp == Missing
                bltyp = 3;
            end
        end
        MF.BedLevelType = bltyp;
        %
        zkuni = -5;
        zkuni = propgetval(MF.mdu,grp,'BotLevUni',zkuni);
        zkuni = propgetval(MF.mdu,grp,'BedLevUni',zkuni);
        VNames = {F.Dataset.Name}';
        %
        if bltyp == 1
            % bed levels specified at faces: from samples or net file
            % ... data from Bathymetry/BedlevelFile
            filename = propget(MF.mdu,grp,fld,'');
            if isempty(filename)
                filename = propget(MF.mdu,grp,'BathymetryFile','');
            end
            if isempty(filename) % in FM: if file not exists ...
                % ... variable with standard name "altitude" at cell centres from mesh file
                % ... variable with name "mesh2d_flowelem_bl" from mesh file
                SNames = get_standard_names(F);
                ibl2d = strcmp('altitude',SNames);
                if none(ibl2d)
                    ibl2d = strcmp('mesh2d_flowelem_bl',VNames);
                end
                ibl2d = find(ibl2d)-1;
                if ~isempty(ibl2d)
                    % use bed level from mesh file
                    iq = cellfun(@(x) isequal(x,ibl2d),{Q.varid}');
                    MF.BedLevel = Q(iq);
                else
                    % use uniform bed level
                    MF.BedLevel = zkuni;
                end
                continue
            end
        elseif bltyp == 2
            % bed levels specified at edges: always from samples
        else
            % bed levels specified at nodes: always from mesh file
            ibl2d = strcmp('NetNode_z',VNames);
            if none(ibl2d)
                ibl2d = strcmp('node_z',VNames);
            end
            ibl2d = find(ibl2d)-1;
            if ~isempty(ibl2d)
                % use bed level from mesh file
                iq = cellfun(@(x) isequal(x,ibl2d),{Q.varid}');
                MF.BedLevel = Q(iq);
            else
                % use uniform bed level
                MF.BedLevel = zkuni;
            end
            continue
        end
    else
        filename = propget(MF.mdu,grp,fld,'');
    end
    if ~isempty(filename)
        filenames = strsplit(filename,';');
        Files = [];
        for ifile = length(filenames):-1:1
            filename = relpath(md_path,filenames{ifile});
            switch key
                case 'BedLevel'
                    F = samples('read',filename);
                case {'Mor','Sed'}
                    F = inifile('open',filename);
                case 'Crs'
                    try
                        F = tekal('open',filename,'loaddata');
                    catch
                        F = inifile('open',filename);
                    end
                case 'Obs'
                    try
                        F = samples('read',filename);
                    catch
                        F = inifile('open',filename);
                    end
                case 'Structure'
                    F = inifile('open',filename);
                case 'ExtForce'
                    ext_path = fileparts(filename);
                    %
                    F1 = inifile('open',filename); % open external forcings file as ini-file
                    K1 = inifile('keywords',F1,1); % keywords of first (and only) section
                    nK = length(K1);
                    Ks = false(1,nK);
                    for k = 1:nK
                        if isempty(K1{k}) % no =-sign in line
                            continue
                        elseif K1{k}(1)=='*' % comment line
                            continue
                        else
                            Ks(k) = true;
                        end
                    end
                    K1 = K1(Ks);
                    nQ = sum(strcmpi(K1,'quantity'));
                    F = [];
                    %
                    Ks = find(Ks);
                    q = 0;
                    F(nQ).Quantity = '';
                    for k = 1:length(K1)
                        val = inifile('get',F1,1,Ks(k));
                        switch lower(K1{k})
                            case 'quantity'
                                q = q+1;
                                F(q).Quantity = val;
                            case 'filename'
                                F(q).FileName = relpath(ext_path,val);
                            case 'filetype'
                                if isscalar(val) && val>=1 && val<=12
                                    FileTypeList = {'uniform','unimagdir','svwp','arcinfo','spiderweb','curvi','triangulation','triangulation_magdir','polyline','inside_polygon','ncgrid','ncflow'};
                                    val = FileTypeList{val};
                                end
                                F(q).FileType = val;
                            case 'method'
                                if isscalar(val) && val>=0 && val<=9
                                    MethodList = {'provider','intp_space_and_time','intp_space_then_intp_time','save_weights','spatial_inside_polygon','spatial_triangulation','spatial_averaging','spatial_index_triangulation','spatial_smoothing','spatial_internal_diffusion'};
                                    val = MethodList{val+1};
                                end
                                F(q).Method = val;
                            case 'operand'
                                switch lower(val)
                                    case 'o'
                                        val = 'override';
                                    case '+'
                                        val = 'add';
                                    case '*'
                                        val = 'multiply';
                                    case 'a'
                                        val = 'apply_when_undefined';
                                end
                                F(q).Operand = val;
                            case 'value'
                                F(q).Value = val;
                            case 'factor'
                                F(q).Value = val;
                            otherwise
                                % unknown keyword - skip it or warn?
                        end
                    end
                    %
                    for q = 1:length(F)
                        switch F(q).FileType
                            case 'polyline'
                                % ... read pli with optional 3rd column ...
                        end
                    end
                case 'ExtForceNew'
                    F = [];
                    F.File = inifile('open',filename);
                    ext_path = fileparts(filename);
                    if inifile('exists',F.File,'boundary')>0
                        BndQuant = inifile('cgetstring',F.File,'boundary','quantity');
                        %
                        [BndLines,pliBnd]  = inifile('cgetstring',F.File,'boundary','locationfile',{});
                        [BndNodes,nodBnd] = inifile('cgetstring',F.File,'boundary','nodeId',{});
                        BndLocs = cell(size(BndQuant));
                        BndType = BndLocs;
                        BndLocs(pliBnd) = BndLines;
                        BndType(pliBnd) = {'line'};
                        BndLocs(nodBnd) = BndNodes;
                        BndType(nodBnd) = {'node'};
                        %
                        BndForce = inifile('cgetstring',F.File,'boundary','forcingfile');
                        uBndQuant = unique(BndQuant);
                        F.Bnd.Types = uBndQuant;
                        BndInd = cellfun(@(f)find(strcmp(f,BndQuant)),F.Bnd.Types,'uniformoutput',false);
                        %
                        [BndLocs,~,ic] = unique(BndLocs);
                        for iBL = length(BndLocs):-1:1
                            if strcmp(BndType{ic(iBL)},'line')
                                bndfilename = relpath(ext_path,BndLocs{iBL});
                                F.BndLoc.Files{iBL} = tekal('open',bndfilename,'loaddata');
                                [p,BndLocs{iBL}] = fileparts(BndLocs{iBL});
                            end
                        end
                        F.BndLoc.Names = BndLocs;
                        BndLocs = BndLocs(ic);
                        F.Bnd.Locs = cellfun(@(f)BndLocs(f),BndInd,'uniformoutput',false);
                        %
                        [uBndForce,~,ic] = unique(BndForce);
                        for iBF = length(uBndForce):-1:1
                            bndfilename = uBndForce{iBF};
                            if strcmp(bndfilename,'REALTIME')
                                % REALTIME in memory data exchange
                                F.BndForce.Files{iBF} = 'REALTIME';
                            else
                                bndfilename = relpath(ext_path,bndfilename);
                                F.BndForce.Files{iBF} = inifile('open',bndfilename);
                            end
                        end
                        BndForce = F.BndForce.Files(ic);
                        F.Bnd.Forcing = cellfun(@(f)BndForce(f),BndInd,'uniformoutput',false);
                        %
                        for iBT = 1:length(F.Bnd.Types)
                            BTp  = F.Bnd.Types{iBT};
                            Locs = F.Bnd.Locs{iBT};
                            for iBL = 1:length(Locs)
                                Loc = Locs{iBL};
                                ForceFile = F.Bnd.Forcing{iBT}{iBL};
                                %fprintf('Searching for %s at %s in %s\n',BTp,Loc,ForceFile.FileName);
                                if strcmp(ForceFile,'REALTIME')
                                    continue
                                end
                                [nForcings,iQ]=inifile('exists',ForceFile,'forcing');
                                forcesThisLocAndType = false(size(ForceFile.Data,1));
                                for iFQ = 1:nForcings
                                    Name = inifile('getstringi',ForceFile,iQ(iFQ),'Name');
                                    if ~strncmp(Name,Loc,length(Loc))
                                        continue
                                    end
                                    Qnts = inifile('cgetstringi',ForceFile,iQ(iFQ),'Quantity');
                                    if ~strncmp(Qnts{end},BTp,length(BTp))
                                        continue
                                    end
                                    forcesThisLocAndType(iQ(iFQ)) = true;
                                end
                                ForceFile.Data = ForceFile.Data(forcesThisLocAndType,:);
                                F.Bnd.Forcing{iBT}{iBL} = ForceFile;
                            end
                        end
                    else
                        F.Bnd.Types = {};
                    end
                otherwise
                    F = filename;
            end
            switch key
                case {'Obs','Crs'}
                    Files{ifile} = F;
                otherwise
                    Files = F;
            end
        end
        MF.(key) = Files;
    end
end


function SNames = get_standard_names(F)
N = length(F.Dataset);
SNames = cell(N,1);
for i = 1:N
    if ~isempty(F.Dataset(i).Attribute)
        Att = {F.Dataset(i).Attribute.Name};
        SN = strcmp(Att,'standard_name');
        if any(SN)
            SNames{i} = F.Dataset(i).Attribute(SN).Value;
        end
    end
end


function [F,Q] = getmesh(mshname)
F = nc_interpret(mshname);
F.FileType = 'NetCDF';
Q = qpread(F);
if ~strcmp(Q(1).Geom,'UGRID1D-NODE') && ~strcmp(Q(1).Geom,'UGRID2D-NODE')
    % old mesh file: modify data structures such that it behaves like a
    % new ugrid file.
    %
    grdid = length(F.Dataset)+1;
    %
    F.Dataset(grdid).Name = 'Mesh2D';
    F.Dataset(grdid).Attribute(1).Name = 'edge_node_connectivity';
    F.Dataset(grdid).Attribute(1).Value = 'NetLink';
    F.Dataset(grdid).Mesh = {'ugrid' 2 grdid -1 'nNetNode' 'nNetLink' ''};
    F.Dataset(grdid).X = ustrcmpi({F.Dataset.Name},'NetNode_x');
    F.Dataset(grdid).Y = ustrcmpi({F.Dataset.Name},'NetNode_y');
    NL = ustrcmpi({F.Dataset.Name},'NetLink');
    F.Dataset(NL).Attribute(end+1).Name = 'start_index';
    F.Dataset(NL).Attribute(end).Value = 1;
    %
    Q = [];
    Q.Name = 'Mesh2D';
    Q.Units = '';
    Q.TemperatureType = 'unspecified';
    Q.Geom = 'UGRID1D-NODE';
    Q.Coords = 'xy';
    Q.DimFlag = [0 0 6 0 0];
    Q.DataInCell = 0;
    Q.NVal = 0;
    Q.SubFld = [];
    Q.MNK = 0;
    Q.varid = {'node_index' grdid-1};
    Q.DimName = {[]  []  'nNetNode'  []  []};
    Q.hasCoords = 1;
    Q.VectorDef = 0;
    Q.ClosedPoly = 0;
    Q.UseGrid = 1;
end


function MF = mdwread(MF,md_path)
grdname = inifile('geti',MF.mdw,'Domain','Grid');
if ~isempty(grdname)
    if iscell(grdname)
        numDomains = length(grdname);
        for idom = 1:numDomains
            grdname_loc = relpath(md_path,grdname{idom});
            [f,p] = fileparts(grdname_loc);
            MF.domain(idom).name = p;
            MF.domain(idom).grd  = wlgrid('read',grdname_loc);
        end
    else
        % numDomains = 1;
        idom = 1;
        grdname_loc = relpath(md_path,grdname);
        [f,p] = fileparts(grdname_loc);
        MF.domain(idom).name = p;
        MF.domain(idom).grd = wlgrid('read',grdname_loc);
    end
else
    error('Unable to locate Grid keyword in [Domain] chapter.');
end


function MF = mdfread(MF,md_path)
mnkmax = propgetval(MF.mdf,'','MNKmax');
SUB1   = propget(MF.mdf,'','Sub1','');
salin  = ~isempty(strfind(lower(SUB1),'s'));
tempa  = ~isempty(strfind(lower(SUB1),'t'));
secfl  = ~isempty(strfind(lower(SUB1),'i'));
SUB2   = propget(MF.mdf,'','Sub2','');
consti = ~isempty(strfind(lower(SUB2),'c'));
if consti
    consti = 0;
    for i = 1:99
        Namc = propget(MF.mdf,'',sprintf('Namc%i',i),'');
        if isempty(Namc)
            break
        else
            consti = consti+1;
        end
    end
end
lstsci = salin + tempa + secfl + consti;
nturb  = 0;
if mnkmax(3)>1
    %TODO: determine number of turbulent state variables
end
%
grdname = propget(MF.mdf,'','Filcco','');
if ~isempty(grdname)
    grdname = relpath(md_path,grdname);
    MF.grd = wlgrid('read',grdname);
else
    error('Filcco is empty: grid in mdf file not yet supported.');
end
%
depname = propget(MF.mdf,'','Fildep','');
if ~isempty(depname)
    depname = relpath(md_path,depname);
    MF.dep = wldep('read',depname,MF.grd);
end
%
rghname = propget(MF.mdf,'','Filrgh','');
if ~isempty(rghname)
    rghname = relpath(md_path,rghname);
    MF.rgh = wldep('read',rghname,MF.grd,'multiple');
    if length(MF.rgh)~=2
        error('Unexpected length of roughness file');
    end
end
%
ininame = propget(MF.mdf,'','Restid','');
if ~isempty(ininame)
    idate = propget(MF.mdf,'','Itdate');
    idate = idate([1:4 6:7 9:10]);
    %
    tunit = propgetval(MF.mdf,'','Tunit');
    switch lower(tunit)
        case 'w'
            tunit = 7;
        case 'd'
            tunit = 1;
        case 'h'
            tunit = 1/24;
        case 'm'
            tunit = 1/1440;
        case 's'
            tunit = 1/86400;
    end
    itime = propgetval(MF.mdf,'','TStart')*tunit;
    rdate = datenum(idate,'yyyymmdd')+itime;
    if itime>=1
        itime = datestr(rdate,'HHMMSS');
        idate = datestr(rdate,'yyyymmdd');
    else
        itime = datestr(itime,'HHMMSS');
    end
    %
    % try tri-rst.restid.YYYYMMDD.HHMMSS
    inicond = relpath(md_path,['tri-rst.' ininame '.' idate '.' itime]);
    try
        MF.ini = trirst('read',inicond,MF.grd,'all');
    catch
        %
        % try tri-rst.restid
        inicond = relpath(md_path,['tri-rst.' ininame]);
        try
            MF.ini = trirst('read',inicond,MF.grd,'all');
        catch
            %
            % try restid as trim-dat/def
            inicond = relpath(md_path,ininame);
            MF.ini = vs_use(inicond,'quiet');
            %
            times = qpread(MF.ini,'water level','times');
            iMAP  = find(times==rdate);
            %
            for ig = 1:length(MF.ini.GrpDat)
                g = MF.ini.GrpDat(ig).Name;
                g_ = strrep(g,'-','_');
                if MF.ini.GrpDat(ig).SizeDim>1
                    MF.ini.Data.(g_) = vs_let(MF.ini,g,{iMAP},'*','quiet');
                    MF.ini.GrpDat(ig).SizeDim=1;
                else
                    MF.ini.Data.(g_) = vs_let(MF.ini,g,{1},'*','quiet');
                end
            end
            %
            MF.ini.FileName = 'IN MEMORY';
            MF.ini.DatExt = '';
            MF.ini.DefExt = '';
        end
    end
    %
    if ~isfield(MF.ini,'FileType')
        % plain binary restart file (flow only)
        nfields = length(MF.ini);
        % water level, velocity, constituents, turbulent quantities, u/v mnldf
        nf_req  = 1 + 2*mnkmax(3) + lstsci*mnkmax(3) + nturb*(mnkmax(3)+1) + 2;
        if nfields ~= nf_req
            error('Number of fields in restart file (%i) does not match expect number of fields (%i)',nfields,nf_req)
        end
    end
end
%
dryname = propget(MF.mdf,'','Fildry','');
if ~isempty(dryname)
    dryname = relpath(md_path,dryname);
    MF.dry = d3d_attrib('read',dryname);
end
%
thdname = propget(MF.mdf,'','Filtd','');
if ~isempty(thdname)
    thdname = relpath(md_path,thdname);
    MF.thd = d3d_attrib('read',thdname);
end
%
wndname = propget(MF.mdf,'','Filwnd','');
if ~isempty(wndname)
    warning('Support for Filwnd not yet implemented.')
end
%
wndname = propget(MF.mdf,'','Filwp','');
if ~isempty(wndname)
    warning('Support for Filwp not yet implemented.')
end
%
wndname = propget(MF.mdf,'','Filwu','');
if ~isempty(wndname)
    warning('Support for Filwu not yet implemented.')
end
%
wndname = propget(MF.mdf,'','Filwv','');
if ~isempty(wndname)
    warning('Support for Filwv not yet implemented.')
end
%
bndname = propget(MF.mdf,'','Filbnd','');
if ~isempty(bndname)
    bndname = relpath(md_path,bndname);
    MF.bnd = d3d_attrib('read',bndname);
    %
    bctname = propget(MF.mdf,'','FilbcT','');
    if ~isempty(bctname)
        bctname = relpath(md_path,bctname);
        MF.bct = bct_io('read',bctname);
    end
    %
    bcaname = propget(MF.mdf,'','Filana','');
    if ~isempty(bcaname)
        warning('Support for Filana not yet implemented.')
    end
    %
    bchname = propget(MF.mdf,'','FilbcH','');
    if ~isempty(bchname)
        bchname = relpath(md_path,bchname);
        MF.bch = bch_io('read',bchname);
    end
    %
    bccname = propget(MF.mdf,'','FilbcC','');
    if ~isempty(bccname)
        bccname = relpath(md_path,bccname);
        MF.bcc = bct_io('read',bccname);
    end
end
%
sedname = propget(MF.mdf,'','Filsed','');
if ~isempty(sedname)
    sedname = relpath(md_path,sedname);
    MF.sed = inifile('open',sedname);
end
%
morname = propget(MF.mdf,'','Filmor','');
if ~isempty(morname)
    morname = relpath(md_path,morname);
    MF.mor = inifile('open',morname);
end
%
if isfield(MF,'mor')
    morininame = propget(MF.mor,'Underlayer','IniComp','');
    if ~isempty(morininame)
        morininame = relpath(md_path,morininame);
        MF.morini.inb = inifile('open',morininame);
        Chaps = inifile('chapters',MF.morini.inb);
        f = 0;
        l = 0;
        for c = 1:length(Chaps)
            if strcmpi(Chaps{c},'layer')
                l = l+1;
                Keys = inifile('keywords',MF.morini.inb,c);
                for k = 1:length(Keys)
                    if ~strcmpi(Keys{k},'Type')
                        val = propget(MF.morini.inb,c,k);
                        if ischar(val)
                            f = f+1;
                            filename = relpath(md_path,val);
                            MF.morini.field(f).chp  = c;
                            MF.morini.field(f).lyr  = l;
                            MF.morini.field(f).key  = k;
                            MF.morini.field(f).data = wldep('read',filename,MF.grd);
                        end
                    end
                end
            end
        end
    end
end
%
traname = propget(MF.mdf,'','TraFrm','');
if ~isempty(traname)
    traname = relpath(md_path,traname);
    MF.tra = readtra(traname);
end
%
staname = propget(MF.mdf,'','Filsta','');
if ~isempty(staname)
    staname = relpath(md_path,staname);
    MF.sta = d3d_attrib('read',staname);
end
%
crsname = propget(MF.mdf,'','Filcrs','');
if ~isempty(crsname)
    crsname = relpath(md_path,crsname);
    MF.crs = d3d_attrib('read',crsname);
end
%
keys = {'Filbar' 'bar'
    'Filgat' 'gat'
    'Filcdw' 'cdw'
    'Fil2dw' 'w2d'
    'Fillwl' 'lwl'
    'Filppl' 'ppl'
    'Filrgs' 'rgs'};
for i = 1:size(keys,1)
    key = keys{i,1};
    fld = keys{i,2};
    fldname = propget(MF.mdf,'',key,'');
    if ~isempty(fldname)
        fldname = relpath(md_path,fldname);
        MF.(fld) = d3d_attrib('read',fldname);
    end
end
%
flsname = propget(MF.mdf,'','Filfls','');
if ~isempty(flsname)
    flsname = relpath(md_path,flsname);
    MF.fls = wldep('read',flsname,MF.grd);
end


function val = propgetval(varargin)
str = propget(varargin{:});
if iscell(str)
    val = cell(size(str));
    for i = 1:length(str)
        val{i} = getval(str{i});
    end
elseif isnumeric(str)
    val = str;
else
    val = sscanf(str,'%f',[1 inf]);
end


function str = rmhash(str)
if iscell(str)
    for i = 1:length(str)
        str{i} = rmhash(str{i});
    end
elseif ischar(str)
    hashes = strfind(str,'#');
    if length(hashes)>1
        str1 = deblank(str(1:hashes(1)-1));
        if isempty(str1)
            str = str(hashes(1)+1:hashes(2)-1);
        else
            str = str1;
        end
    elseif length(hashes)==1
        str = str(1:hashes(1)-1);
    end
    str = deblank(str);
end


function pf = relpath(path,file)
if length(file)>1 && file(2)==':'
    pf = file;
else
    pf = fullfile(path,file);
end


function varargout = bch_io(cmd,varargin)
switch lower(cmd)
    case 'read'
        O = readbch(varargin{:});
        if nargout>0
            varargout{1} = O;
        end
    case 'write'
        writebch(varargin{:})
    otherwise
        error('Unknown command: %s',cmd)
end

function writebch(filename,S)
fid = fopen(filename,'wt');
Format = [repmat(' %15.7e',1,length(S.Freq)) '\n'];
fprintf(fid,Format,S.Freq);
fprintf(fid,'\n');
fprintf(fid,Format,S.Amplitudes');
fprintf(fid,'\n');
Format = [repmat(' ',1,16) repmat(' %15.7e',1,length(S.Freq)-1) '\n'];
fprintf(fid,Format,S.Phases(:,2:end)');
fclose(fid);


function S = readbch(filename)
S.FileName = filename;
S.FileType = 'Delft3D-FLOW BCH-file';
fid = fopen(filename,'r');
%
Line = fgetl(fid);
[S.Freq,nFreq,err] = sscanf(Line,'%f');
if ~isempty(err)
    fclose(fid);
    error('Only values expected in first line of BCH file: "%s"',Line)
end
%
Line = fgetl(fid);
if ~isempty(Line)
    fclose(fid);
    error('Unexpected data on second line of BCH file: "%s". This line should be empty.',Line)
end
%
Line = fgetl(fid);
lNr = 3;
Data = zeros(0,nFreq);
while ~isempty(Line)
    [DataRow,nVal2,err] = sscanf(Line,'%f');
    if ~isempty(err) || nVal2~=nFreq
        fclose(fid);
        error('%i values expected in line %i "%s"',nFreq,lNr,Line)
    end
    Data(end+1,:) = DataRow;
    Line = fgetl(fid);
    lNr = lNr+1;
end
nLines = size(Data,1);
S.Amplitudes = Data;
%
Data(:) = NaN;
Line = fgetl(fid);
lNr = lNr+1;
for i = 1:nLines
    [DataRow,nVal2,err] = sscanf(Line,'%f');
    if ~isempty(err) || nVal2~=nFreq-1
        fclose(fid);
        error('%i values expected in line %i "%s"',nFreq-1,lNr,Line)
    end
    Data(i,2:end) = DataRow;
    Line = fgetl(fid);
    lNr = lNr+1;
end
S.Phases = Data;
%
if ~feof(fid) || (ischar(Line) && ~isempty(Line))
    fclose(fid);
    error('More data lines in file than expected.')
end
fclose(fid);

function I = readtra(filename)
I =inifile('open',filename);