function [x,y,z]=samples(cmd,varargin)
%SAMPLES Read/write sample data from file.
%     XYZ = SAMPLES('read',FILENAME) read the specified file and return the
%     data contained in it. In the classic case of a simple plain Delft3D
%     samples file contain three data columns, the function returns an Nx3
%     array with the data. Due to the algorithm used, the file may contain
%     any number of comments in MATLAB style, i.e. starting with %. As a
%     generalization beyond the classic samples file, the number of columns
%     may differ from 3.
%
%     [X,Y,Z] = SAMPLES('read',FILENAME) read the specified file and return
%     the samples in three separate Nx1 arrays.
%
%     SAMPLES('write',FILENAME,XYZ) write samples given in a Nx3 (or 3xN)
%     array to a samples file. Because of the automatic transpose option,
%     this function does not support any other number of data columns.
%
%     SAMPLES('write',FILENAME,X,Y,Z) write samples given in three Nx1 (or
%     1xN) arrays to a samples file. Because of the automatic transpose
%     option, this function does not support any other number of data
%     columns.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/progsrc/samples.m $
%   $Id: samples.m 65778 2020-01-14 14:07:42Z mourits $

switch lower(cmd)
    case 'read'
        xyz=Local_read_samples(varargin{:});
        if nargout>1
            x=xyz(:,1);
            y=xyz(:,2);
            z=xyz(:,3);
        else
            x=xyz;
        end
    case 'write'
        if ~ischar(varargin{1})
            Local_write_samples('?',varargin{:});
        else
            Local_write_samples(varargin{:});
        end
    otherwise
        error('Unknown command: %s',var2str(cmd)) 
end

function xyz=Local_read_samples(filename,opt)
if (nargin==0) || strcmp(filename,'?')
    [fname,fpath]=uigetfile('*.xyz','Select sample file');
    if ~ischar(fname)
        xyz=zeros(0,3);
        return
    end
    filename=[fpath,fname];
end

readtype='default';
if nargin>1
    opt = lower(opt);
    switch opt
        case {'simple','struct'}
            readtype = opt;
        otherwise
            error('Invalid samples read option: %s',opt)
    end
end

if exist(filename)~=2
    error('Cannot open %s.',filename)
end
try
    xyz0 = load(filename);
    simplexyz = 1;
catch
    try
        xyz0 = asciiload(filename);
        simplexyz = 1;
    catch
        simplexyz = 0;
    end
end

if simplexyz
    switch readtype
        case {'simple','default'}
            xyz = xyz0;
        otherwise
            xyz.XYZ = xyz0;
            if iscell(xyz0)
                npar = 0;
                for c = 1:length(xyz0)
                    npar = npar+size(xyz0{c},2);
                end
            else
                npar = size(xyz0,2);
            end
            xyz.Params = cell(1,npar);
            for i=1:npar
                xyz.Params{i}=sprintf('Parameter %i',i);
            end
            xyz.FileType = 'samples';
            xyz.FileName = filename;
            %
            fid=fopen(filename,'r');
            Line=fgetl(fid);
            xyz.Header = {};
            while ~isempty(Line) && Line(1)=='%'
                xyz.Header{end+1} = Line;
                Line=fgetl(fid);
            end
            fclose(fid);
            %
            if ~isempty(xyz.Header)
                for i = length(xyz.Header):-1:1
                    j = strfind(xyz.Header{i},'Run:');
                    if ~isempty(j)
                        xyz.Run = strtok(xyz.Header{i}(j+4:end));
                    end
                    j = strfind(xyz.Header{i},'Table:');
                    if ~isempty(j)
                        xyz.Table = strtok(xyz.Header{i}(j+6:end));
                    end
                    j = strfind(xyz.Header{i},'SWAN version:');
                    if ~isempty(j)
                        xyz.SWAN_version = strtok(xyz.Header{i}(j+13:end));
                    end
                    j1 = strfind(xyz.Header{i},'[');
                    j2 = strfind(xyz.Header{i},']');
                    if length(j1) == npar && length(j2) == npar
                        xyz.ParamUnits = repmat({''},1,npar);
                        for j = 1:npar
                            xyz.ParamUnits{j} = strtrim(xyz.Header{i}(j1(j)+1:j2(j)-1));
                        end
                        par = textscan(xyz.Header{i-1}(2:end),'%s');
                        xyz.Params = par{1};
                    end
                end
            end
    end
else % readtype always forced to 'struct'
    fid=fopen(filename,'r');
    try
        skiplines=0;
        Line=fgetl(fid);
        xyz.Header={};
        while ischar(Line) && ~isempty(Line) && (Line(1)=='*' || Line(1)=='#')
            skiplines=skiplines+1;
            xyz.Header{skiplines} = strtrim(Line(2:end));
            Line=fgetl(fid);
        end
        if ~ischar(Line)
            fclose(fid);
            error('%s does not contain samples.',filename);
        end
        %
        Params = {};
        firstitem = strtok(Line);
        while firstitem(1)=='"'
            try
                X = textscan(Line,' "%[^"]"','returnonerror',0);
                Params = [Params X{1}'];
            catch
                error('Reading line: %s\nAll parameter names should be enclosed by double quotes.',Line);
            end
            skiplines=skiplines+1;
            Line=fgetl(fid);
            firstitem = strtok(Line);
        end
        %
        X = textscan(Line,' %[^ \t]','returnonerror',0);
        n = length(X{1});
        if n<3
            error('Not enough values for sample data (X,Y,Value1,...)')
        end
        Line=fgetl(fid);
        X  = textscan(Line,' %[^ \t]','returnonerror',0);
        n2 = length(X{1});
        if n2~=n && ~feof(fid)
            error('Number of values per line should be consistent.')
        end
        if length(Params)<n
            for i=n:-1:(length(Params)+1)
                Params{i}=sprintf('Parameter %i',i);
            end
        elseif length(Params)>n
            Params=Params(1:n);
        end
        fclose(fid);
    catch
        fclose(fid);
        rethrow(lasterror)
    end
    
    xyz.XYZ=asciiload(filename,'skiplines',skiplines,'comment','*');
    xyz.Params=Params;
    xyz.FileType='samples';
    xyz.FileName=filename;
    
    if strcmp(readtype,'simple')
        xyz = xyz.XYZ;
    end
end
%
if isstruct(xyz)
    xyz.X = [];
    xyz.Y = [];
    xyz.Time = [];
    for i = 1:length(xyz.Params)
        switch lower(xyz.Params{i})
            case {'longitude','lon','x','xp','x-coordinate','x coordinate','x_coordinate','x_gpp','distance'}
                xyz.X = i;
            case {'latitude' ,'lat','y','yp','y-coordinate','y coordinate','y_coordinate','y_gpp'}
                xyz.Y = i;
            case 'time'
                if i>1 && strcmpi(xyz.Params{i-1},'date')
                    xyz.Time = [i-1 i];
                else
                    xyz.Time = i;
                end
            case 'datetime'
                xyz.Time = i;
        end
    end
    if isempty(xyz.X) && isempty(xyz.Y)
        xyz.X = find(strcmp(xyz.Params,'Parameter 1'));
        if ~isempty(xyz.X)
           xyz.Y = find(strcmp(xyz.Params,'Parameter 2'));
        end
    end
    %
    if isempty(xyz.Time)
        if iscell(xyz.XYZ)
            xyz.nLoc  = size(xyz.XYZ{1},1);
        else
            xyz.nLoc  = size(xyz.XYZ,1);
        end
        xyz.Times = gettimestamp(xyz.Header);
    else
        crds = [xyz.X xyz.Y];
        iTime = [];
        %
        % Usually times will be sorted in the data file. If this is not the
        % case, we might want to sort them, but we haven't implemented that
        % yet.
        %
        if ~isempty(crds)
            sortloc = unique(xyz.XYZ(:,crds),'rows');
            nLoc = size(sortloc,1);
            nVal = size(xyz.XYZ,1);
            locations = xyz.XYZ(1:nLoc,crds); % alternatively use unique(...,'stable')
            if nVal/nLoc == floor(nVal/nLoc) && isequal(xyz.XYZ(:,crds),repmat(locations,[nVal/nLoc 1]))
                Times = gettimes(xyz.XYZ(1:nLoc:nVal,xyz.Time));
                iTime = cumsum(repmat((1:nLoc)'==1,[nVal/nLoc 1]));
            end
        end
        if isempty(iTime)
            [Times,idum,iTime] = unique(gettimes(xyz.XYZ(:,xyz.Time)),'stable');
            nLoc = hist(iTime,max(iTime));
        end
        %
        xyz.Times = Times;
        xyz.iTime = iTime;
        %
        if all(nLoc==nLoc(1))
            xyz.nLoc = nLoc(1);
        else
            xyz.nLoc = nLoc;
        end
    end
end


function Local_write_samples(filename,x,y,z)
if strcmp(filename,'?')
    [fn,fp]=uiputfile('*.xyz');
    if ~ischar(fn)
        return
    end
    filename=[fp fn];
end
fid=fopen(filename,'wt');
if fid<0
    error(['Could not create or open: ',filename])
end

if nargin==4
    if size(x,2)==1 % column vectors
        xyz=transpose([x y z]);
    else % row vectors
        xyz=[x;y;z];
    end
    fprintf(fid,'%f %f %f\n',xyz);
else
    if size(x,2)==3 % column vector (3x)
        x=transpose(x);
    end
    fprintf(fid,'%f %f %f\n',x);
end
fclose(fid);


function Times = gettimes(Times)
if size(Times,2)==2 % gpp => 20140118 105120
    d = Times(:,1);
    s = Times(:,2);
else % swan => 20140118.105120
    d = floor(Times);
    s = round((Times - d)*1000000);
end
m = floor(d/100);
d = d - 100*m;
y = floor(m/100);
m = m - 100*y;
%
mn = floor(s/100);
s  = s - mn*100;
h = floor(mn/100);
mn = mn - h*100;
%
Times = datenum(y,m,d,h,mn,s);


function Time=gettimestamp(Cmnt)
Time=[];
if ~isempty(Cmnt)
    for i=1:length(Cmnt)
        [Tk,Rm]=strtok(Cmnt{i});
        if (length(Cmnt{i})>10) && strcmpi(Tk,'time')
            [a,c,err,idx]=sscanf(Rm,'%*[ :=]%i %i',2);
            if c==2
                yr = floor(a(1)/10000);
                mo = floor((a(1)-yr*10000)/100);
                dy = a(1)-yr*10000-mo*100;
                hr = floor(a(2)/10000);
                mn = floor((a(2)-hr*10000)/100);
                sc = a(2)-hr*10000-mn*100;
                Time=datenum(yr,mo,dy,hr,mn,sc);
            else
                try
                    [a,c,err,idx]=sscanf(Rm,'%*[ :=] %1c',1);
                    Time=datenum(Rm(idx-1:end));
                catch
                end
            end
        end
    end
end