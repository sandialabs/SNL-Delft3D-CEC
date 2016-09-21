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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/mdf.m $
%   $Id: mdf.m 5600 2015-11-20 09:10:36Z jagers $

switch lower(cmd)
    case 'read'
        varargout{1} = mdfread(varargin{:});
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
mnkmax = propget(MDF1.mdf,'','MNKmax');
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
mnkmax = propget(MDF2.mdf,'','MNKmax');
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
if isfield(MDF2,'thd')
    MDF2.thd.MNu(:,1:2) = rotate(MDF2.thd.MNu(:,1:2),MMAX);
    MDF2.thd.MNu(:,3:4) = rotate(MDF2.thd.MNu(:,3:4),MMAX);
    MDF2.thd.MNv(:,1:2) = rotate(MDF2.thd.MNv(:,1:2),MMAX);
    MDF2.thd.MNv(:,3:4) = rotate(MDF2.thd.MNv(:,3:4),MMAX);
    TMP_MN = MDF2.thd.MNu;
    TMP_CHAR = MDF2.thd.CHARu;
    MDF2.thd.MNu = MDF2.thd.MNv;
    MDF2.thd.CHARu = MDF2.thd.CHARv;
    MDF2.thd.MNv = TMP_MN;
    MDF2.thd.CHARv = TMP_CHAR;
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
filename = [caseid '.mdf'];
inifile('write',fullfile(path,filename),MDF.mdf);


function Val = propget(FILE,varargin)
Val = rmhash(inifile('geti',FILE,varargin{:}));


function MDF = mdfread(filename)
MDF.mdf = inifile('open',filename);
mdfpath = fileparts(filename);
%
mnkmax = propget(MDF.mdf,'','MNKmax');
SUB1   = propget(MDF.mdf,'','Sub1','');
salin  = ~isempty(strfind(lower(SUB1),'s'));
tempa  = ~isempty(strfind(lower(SUB1),'t'));
secfl  = ~isempty(strfind(lower(SUB1),'i'));
SUB2   = propget(MDF.mdf,'','Sub2','');
consti = ~isempty(strfind(lower(SUB2),'c'));
if consti
    consti = 0;
    for i = 1:99
        Namc = propget(MDF.mdf,'',sprintf('Namc%i',i),'');
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
grdname = propget(MDF.mdf,'','Filcco','');
grdname = rmhash(grdname);
if ~isempty(grdname)
    grdname = relpath(mdfpath,grdname);
    MDF.grd = wlgrid('read',grdname);
else
    error('Filcco is empty: grid in mdf file not yet supported.');
end
%
depname = propget(MDF.mdf,'','Fildep','');
depname = rmhash(depname);
if ~isempty(depname)
    depname = relpath(mdfpath,depname);
    MDF.dep = wldep('read',depname,MDF.grd);
end
%
rghname = propget(MDF.mdf,'','Filrgh','');
rghname = rmhash(rghname);
if ~isempty(rghname)
    rghname = relpath(mdfpath,rghname);
    MDF.rgh = wldep('read',rghname,MDF.grd,'multiple');
    if length(MDF.rgh)~=2
        error('Unexpected length of roughness file');
    end
end
%
ininame = propget(MDF.mdf,'','Restid','');
ininame = rmhash(ininame);
if ~isempty(ininame)
    idate = propget(MDF.mdf,'','Itdate');
    idate = idate([1:4 6:7 9:10]);
    %
    tunit = propget(MDF.mdf,'','Tunit');
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
    itime = propget(MDF.mdf,'','TStart')*tunit;
    rdate = datenum(idate,'yyyymmdd')+itime;
    if itime>=1
        itime = datestr(rdate,'HHMMSS');
        idate = datestr(rdate,'yyyymmdd');
    else
        itime = datestr(itime,'HHMMSS');
    end
    %
    % try tri-rst.restid.YYYYMMDD.HHMMSS
    inicond = relpath(mdfpath,['tri-rst.' ininame '.' idate '.' itime]);
    try
        MDF.ini = trirst('read',inicond,MDF.grd,'all');
    catch
        %
        % try tri-rst.restid
        inicond = relpath(mdfpath,['tri-rst.' ininame]);
        try
            MDF.ini = trirst('read',inicond,MDF.grd,'all');
        catch
            %
            % try restid as trim-dat/def
            inicond = relpath(mdfpath,ininame);
            MDF.ini = vs_use(inicond,'quiet');
            %
            times = qpread(MDF.ini,'water level','times');
            iMAP  = find(times==rdate);
            %
            for ig = 1:length(MDF.ini.GrpDat)
                g = MDF.ini.GrpDat(ig).Name;
                g_ = strrep(g,'-','_');
                if MDF.ini.GrpDat(ig).SizeDim>1
                    MDF.ini.Data.(g_) = vs_let(MDF.ini,g,{iMAP},'*','quiet');
                    MDF.ini.GrpDat(ig).SizeDim=1;
                else
                    MDF.ini.Data.(g_) = vs_let(MDF.ini,g,{1},'*','quiet');
                end
            end
            %
            MDF.ini.FileName = 'IN MEMORY';
            MDF.ini.DatExt = '';
            MDF.ini.DefExt = '';
        end
    end
    %
    if ~isfield(MDF.ini,'FileType')
        % plain binary restart file (flow only)
        nfields = length(MDF.ini);
        % water level, velocity, constituents, turbulent quantities, u/v mnldf
        nf_req  = 1 + 2*mnkmax(3) + lstsci*mnkmax(3) + nturb*(mnkmax(3)+1) + 2;
        if nfields ~= nf_req
            error('Number of fields in restart file (%i) does not match expect number of fields (%i)',nfields,nf_req)
        end
    end
end
%
dryname = propget(MDF.mdf,'','Fildry','');
if ~isempty(dryname)
    dryname = relpath(mdfpath,dryname);
    MDF.dry = d3d_attrib('read',dryname);
end
%
thdname = propget(MDF.mdf,'','Filtd','');
if ~isempty(thdname)
    thdname = relpath(mdfpath,thdname);
    MDF.thd = d3d_attrib('read',thdname);
end
%
wndname = propget(MDF.mdf,'','Filwnd','');
if ~isempty(wndname)
    warning('Support for Filwnd not yet implemented.')
end
%
wndname = propget(MDF.mdf,'','Filwp','');
if ~isempty(wndname)
    warning('Support for Filwp not yet implemented.')
end
%
wndname = propget(MDF.mdf,'','Filwu','');
if ~isempty(wndname)
    warning('Support for Filwu not yet implemented.')
end
%
wndname = propget(MDF.mdf,'','Filwv','');
if ~isempty(wndname)
    warning('Support for Filwv not yet implemented.')
end
%
bndname = propget(MDF.mdf,'','Filbnd','');
if ~isempty(bndname)
    bndname = relpath(mdfpath,bndname);
    MDF.bnd = d3d_attrib('read',bndname);
    %
    bctname = propget(MDF.mdf,'','FilbcT','');
    if ~isempty(bctname)
        bctname = relpath(mdfpath,bctname);
        MDF.bct = bct_io('read',bctname);
    end
    %
    bcaname = propget(MDF.mdf,'','Filana','');
    if ~isempty(bcaname)
        warning('Support for Filana not yet implemented.')
    end
    %
    bchname = propget(MDF.mdf,'','FilbcH','');
    if ~isempty(bchname)
        bchname = relpath(mdfpath,bchname);
        MDF.bch = bch_io('read',bchname);
    end
    %
    bccname = propget(MDF.mdf,'','FilbcC','');
    if ~isempty(bccname)
        bccname = relpath(mdfpath,bccname);
        MDF.bcc = bct_io('read',bccname);
    end
end
%
sedname = propget(MDF.mdf,'','Filsed','');
if ~isempty(sedname)
    sedname = relpath(mdfpath,sedname);
    MDF.sed = inifile('open',sedname);
end
%
morname = propget(MDF.mdf,'','Filmor','');
if ~isempty(morname)
    morname = relpath(mdfpath,morname);
    MDF.mor = inifile('open',morname);
end
%
morininame = propget(MDF.mor,'Underlayer','IniComp','');
if ~isempty(morininame)
    morininame = relpath(mdfpath,morininame);
    MDF.morini.inb = inifile('open',morininame);
    Chaps = inifile('chapters',MDF.morini.inb);
    f = 0;
    l = 0;
    for c = 1:length(Chaps)
        if strcmpi(Chaps{c},'layer')
            l = l+1;
            Keys = inifile('keywords',MDF.morini.inb,c);
            for k = 1:length(Keys)
                if ~strcmpi(Keys{k},'Type')
                    val = propget(MDF.morini.inb,c,k);
                    if ischar(val)
                        f = f+1;
                        filename = relpath(mdfpath,val);
                        MDF.morini.field(f).chp  = c;
                        MDF.morini.field(f).lyr  = l;
                        MDF.morini.field(f).key  = k;
                        MDF.morini.field(f).data = wldep('read',filename,MDF.grd);
                    end
                end
            end
        end
    end
end
%
traname = propget(MDF.mdf,'','TraFrm','');
if ~isempty(traname)
    traname = relpath(mdfpath,traname);
    MDF.tra = readtra(traname);
end
%
staname = propget(MDF.mdf,'','Filsta','');
if ~isempty(staname)
    staname = relpath(mdfpath,staname);
    MDF.sta = d3d_attrib('read',staname);
end
%
crsname = propget(MDF.mdf,'','Filcrs','');
if ~isempty(crsname)
    crsname = relpath(mdfpath,crsname);
    MDF.crs = d3d_attrib('read',crsname);
end


function val = getval(str)
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
        str = str(hashes(1)+1:hashes(2)-1);
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