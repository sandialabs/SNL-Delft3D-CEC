function make_diocore
%MAKE_DIOCORE  Build statements for dio_core.dll

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/delftio/dio_core/make_diocore.m $
%   $Id: make_diocore.m 65778 2020-01-14 14:07:42Z mourits $

fprintf('Initializing...\n');
cppfiles0=dir('*.cpp');
cppfiles0 = {cppfiles0.name};
copycppfiles

fprintf('Compiling and linking...\n');
files = {'dio_core.cpp','dio_shm.cpp','dio_shm_datablock.cpp','dio_shm_f2c_c.cpp','dio_shm_handle.cpp','dio_shm_sync.cpp'};
mex('-DWIN32','-I../../../../utils_lgpl/delftio/packages/delftio_shm/include',files{:})
pause(1)

fprintf('Cleaning up ...\n');
cppfiles1=dir('*.cpp');
cppfiles1 = {cppfiles1.name};
cppfiles1 = setdiff(cppfiles1,cppfiles0);
for i = 1:length(cppfiles1)
    delete(cppfiles1{i})
end

installdir = '../progsrc/private';
fprintf('Installing library in %s ...\n',installdir);
if exist('dio_core.dll')
    movefile('dio_core.dll',installdir)
elseif exist('dio_core.mexw32')
    movefile('dio_core.mexw32',installdir)
elseif exist('dio_core.mexw64')
    movefile('dio_core.mexw64',installdir)
end

fprintf('Finished.');


function copycppfiles
srcpath = '../../../../utils_lgpl/delftio/packages/delftio_shm/src/diof90/';
if sscanf(version,'%f',1)<6
    srcpath = strrep(srcpath,'/',filesep);
    d = dir([srcpath '*.cpp']);
    for i = 1:length(d)
        copyfile([srcpath d(i).name],pwd)
    end
else
    copyfile([srcpath '*.cpp'],pwd)
end
