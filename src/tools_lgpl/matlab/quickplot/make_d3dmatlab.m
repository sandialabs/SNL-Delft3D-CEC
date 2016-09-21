function make_d3dmatlab(basedir,varargin)
%MAKE_D3DMATLAB Pre-compile Delft3D-MATLAB toolbox
%   Pre-compile MATLAB m-code to p-code for distribution as Delft3D-MATLAB toolbox.
%
%   MAKE_D3DMATLAB(BASEDIR)
%   Use specified directory instead of current directory as base directory

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/make_d3dmatlab.m $
%   $Id: make_d3dmatlab.m 4612 2015-01-21 08:48:09Z mourits $

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
disp(['Delft3D-MATLAB interface version: ' qpversion]);
TStr=datestr(T);
disp(['Current date and time           : ' TStr]);

disp(['Creating ',tdir,' directory ...']);
if ~exist(tdir,'dir')
    [success,message] = mkdir(tdir);
    if ~success
        err=message;
        return
    end
end

disp('Copying files ...');
exportsrc(sourcedir,targetdir)

disp('Modifying files ...');
fstrrep([targetdir,filesep,'d3d_qp.m'],'<VERSION>',qpversion)
fstrrep([targetdir,filesep,'d3d_qp.m'],'<CREATIONDATE>',TStr)
fstrrep([targetdir,filesep,'Contents.m'],'<VERSION>',qpversion)
fstrrep([targetdir,filesep,'Contents.m'],'<CREATIONDATE>',TStr)

disp('Stripping files ...');
svnstripfile(targetdir)

%disp('Pcoding files ...');
%pmfile('dir',targetdir,targetdir,'-verbose')

disp('Cleaning up directory ...');
cd(tdir)
X={ '*.asv'
    '*.bak'
    '*.scc'
    'bin'
    'compileonly'};
cleanup(X)

disp('Removing unneeded subdirectories ...');
X={'org'};
cleanup(X)

cd ..
disp('Finished.');


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