function make_quickplot(basedir,varargin)
%MAKE_QUICKPLOT Compile QUICKPLOT executable
%   Compile MATLAB code to QUICKPLOT executable
%
%   MAKE_QUICKPLOT(BASEDIR)
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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/make_quickplot.m $
%   $Id: make_quickplot.m 65778 2020-01-14 14:07:42Z mourits $

curdir=pwd;
addpath(curdir)
if matlabversionnumber<7.09
    error('Invalid MATLAB version. Use MATLAB R2009b (7.9) or higher for compiling Delft3D-QUICKPLOT!')
end
if ~exist('mcc')
    error('Cannot find MATLAB compiler. Use another MATLAB installation!')
end
if nargin>0
    cd(basedir);
end
err=[];
try
    localmake(varargin{:});
catch err
end
if nargin>0
    cd(curdir);
end
rmpath(curdir)
if ~isempty(err)
    rethrow(err)
end


function localmake(qpversion,T)
if ~exist('progsrc','dir')
    error('Cannot locate source folder "progsrc".')
end
sourcedir=[pwd,filesep,'progsrc'];
disp('Copying files ...')
if strncmp(fliplr(computer),'46',2)
    qpdir = 'quickplot64';
else
    qpdir = 'quickplot32';
end
if ~exist([pwd,filesep,qpdir])
    [success,message] = mkdir(qpdir);
    if ~success
        error(message)
    end
end
cd(qpdir);
diary make_quickplot_diary
if isunix
    unix('cp -rf ../progsrc/* .');
    unix('mv compileonly/* .');
else
    [s,msg]=dos('xcopy "..\progsrc\*.*" "." /E /Y');
    if s==0
        [s,msg]=dos('move compileonly\*.*  .');
    end
    if s~=0
        error(msg)
    end
end
%
copyfile('../../../../third_party_open/netcdf/matlab/netcdfAll-4.1.jar','.')
addpath ../../../../third_party_open/netcdf/matlab/mexnc
addpath ../../../../third_party_open/netcdf/matlab/snctools
%
if nargin<2
    qpversion=read_identification(sourcedir,'d3d_qp.m');
    T=now;
end
fprintf('\nBuilding Delft3D-QUICKPLOT version %s\n\n',qpversion);
TStr = datestr(T);
fstrrep('d3d_qp.m','<VERSION>',qpversion)
fstrrep('d3d_qp.m','<CREATIONDATE>',TStr)
fstrrep('wl_identification.c','<VERSION>',qpversion)
fstrrep('wl_identification.c','<CREATIONDATE>',TStr)
if ~d3d_qp('version',qpversion,TStr)
    error('Unable to write correct d3d_qp.version file.')
end
g = which('-all','gscript');
if ~isempty(g)
    copyfile(g{1},'.')
end
make_exe
if ispc
   movefile('d3d_qp.exe','d3d_qp.exec');
end
X={'*.asv'
    '*.bak'
    '*.m'
    '*.c'
    '*.cpp'
    '*.h'
    '*.o'
    '*.obj'
    '*.a'
    '*.lib'
    '*.scc'
    'private'
    'compileonly'
    '@qp_data'
    '@qp_data_resource'};
if isunix
    X=cat(1,X,{'*.dll'
        '*.mexw*'});
else
    X=cat(1,X,{'*.mexglx'
        '*.mexa64'
        '*.exp'});
end
cleanup(X)
diary off
cd ..