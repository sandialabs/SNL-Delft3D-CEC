function make_d3dmatlab(basedir,varargin)
%MAKE_D3DMATLAB Pre-compile Delft3D-MATLAB toolbox
%   Pre-compile MATLAB m-code to p-code for distribution as Delft3D-MATLAB toolbox.
%
%   MAKE_D3DMATLAB(BASEDIR)
%   Use specified directory instead of current directory as base directory

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/make_d3dmatlab.m $
%   $Id: make_d3dmatlab.m 65778 2020-01-14 14:07:42Z mourits $

curdir=pwd;
addpath(curdir)
%
% comment lines for
% consistency with
% make_quickplot and
% make_ecoplot
%
if nargin>0
    cd(basedir);
end
try
    err=localmake(varargin{:});
catch
    err=lasterr;
end
if nargin>0
    cd(curdir);
end
rmpath(curdir)
if ~isempty(err)
    error(err)
end


function err=localmake(qpversion,T)
err='';
if ~exist('progsrc','dir')
    err='Cannot locate source'; return
end
V=version; V=str2num(V(1));

tdir = 'delft3d_matlab';
sourcedir=[pwd,filesep,'progsrc'];
targetdir=[pwd,filesep,tdir];
if nargin<2
    qpversion=read_identification(sourcedir,'d3d_qp.m');
    T=now;
end
qpversion = deblank(sscanf(qpversion,'%[^(]')); % strip off the 32/64 bit flag (the toolbox is platform independent)
fprintf('\nBuilding Delft3D-MATLAB interface version %s (all platforms)\n\n',qpversion);
TStr=datestr(T);
fprintf('Current date and time           : %s\n',TStr);

fprintf('Creating %s directory ...\n',tdir);
if ~exist(tdir,'dir')
    [success,message] = mkdir(tdir);
    if ~success
        err=message;
        return
    end
end

fprintf('Copying files ...\n');
exportsrc(sourcedir,targetdir)

fprintf('Modifying files ...\n');
fstrrep([targetdir,filesep,'d3d_qp.m'],'<VERSION>',qpversion)
fstrrep([targetdir,filesep,'d3d_qp.m'],'<CREATIONDATE>',TStr)
fstrrep([targetdir,filesep,'Contents.m'],'<VERSION>',qpversion)
fstrrep([targetdir,filesep,'Contents.m'],'<CREATIONDATE>',TStr)

fprintf('Stripping files ...\n');
svnstripfile(targetdir)

%fprintf('Pcoding files ...\n');
%pmfile('dir',targetdir,targetdir,'-verbose')

fprintf('Cleaning up directory ...\n');
cd(tdir)
X={ '*.asv'
    '*.bak'
    '*.scc'
    'bin'
    'compileonly'};
cleanup(X)

fprintf('Removing unneeded subdirectories ...\n');
X={'org'};
cleanup(X)

cd ..
fprintf('Finished.\n');


function exportsrc(sourcedir,targetdir)
d = dir(sourcedir);
for i = 1:length(d)
    source = [sourcedir filesep d(i).name];
    target = [targetdir filesep d(i).name];
    if d(i).isdir
        switch d(i).name
            case {'.','..','.svn'}
                % skip
            otherwise
                mkdir(target);
                exportsrc(source,target)
        end
    else
        copyfile(source,target)
    end
end