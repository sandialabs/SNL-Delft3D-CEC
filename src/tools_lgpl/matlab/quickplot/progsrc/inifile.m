function varargout=inifile(cmd,varargin)
%INIFILE Read/write INI files.
%   Info=INIFILE('open',FileName)
%   Open and read the INI file; return the data to the workspace in a
%   set of nested cell arrays.
%
%   Info=INIFILE('new')
%   Create a new INI file structure.
%
%   Info=INIFILE('write',FileName,Info)
%   Open and write the INI file; the data in the file is overwritten
%   without asking.
%
%   ListOfChapters=INIFILE('chapters',Info)
%   Retrieve list of Chapter names (cell array of strings).
%
%   IndexChapter=INIFILE('chapters',Info,Chapter)
%   Retrieve the chapter indices that match the specified chapter name.
%
%   ListOfKeywords=INIFILE('keywords',Info,Chapter)
%   Retrieve list of Keywords in specified Chapter (cell array of strings).
%
%   BOOL = INIFILE('exists',Info,Chapter,Keyword)
%   Check whether a Chapter/Keyword exists in the the Info data set.
%
%   Val=INIFILE('get',Info,Chapter,Keyword,Default)
%   Retrieve Chapter/Keyword from the Info data set. The Default value is
%   optional. If the Chapter ID is '*', the Keyword is searched for in
%   all chapters in the file.
%
%   Info=INIFILE('set',Info,Chapter,Keyword,Value)
%   Set Chapter/Keyword in the data set to the indicated value. The
%   updated data set is returned. Data is not written to file. If the
%   chapter and/or keyword do not exist, they are created. If Value equals
%   [], the keyword is deleted (see below). Use the 'write' option to
%   write the data to file.
%
%   Info=INIFILE('delete',Info,Chapter,Keyword)
%   Info=INIFILE('set',Info,Chapter,Keyword,[])
%   Delete Chapter/Keyword from the data set. The updated data set is
%   returned. Data is not written to file. Use the 'write' option to
%   write the data to file.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/progsrc/inifile.m $
%   $Id: inifile.m 65778 2020-01-14 14:07:42Z mourits $

lcmd = lower(cmd);
switch lcmd
    case 'open'
        varargout{1} = readfile(varargin{:});
    case {'chapters','chaptersi'}
        varargout{1} = chapfile(lcmd,varargin{:});
    case {'keywords','keywordsi'}
        varargout{1} = chapkeys(lcmd,varargin{:});
    case {'exists','existsi'}
        try
            if nargin==3
                % only Chapter
                if strcmp(lcmd,'exists')
                    lcmd = 'chapters';
                    chap = varargin{2};
                else
                    lcmd = 'chaptersi';
                    chap = lower(varargin{2});
                end
                A = chapfile(lcmd,varargin{1});
                Amatch = strcmp(chap,A);
                varargout{1} = sum(Amatch);
                if nargout>1
                    varargout{2} = find(Amatch);
                end
                return
            else
                % Chapter/Keyword pair
                [A,iChap] = inifile(lcmd,varargin{1:2});
                if strcmp(lcmd,'exists')
                    lcmd = 'keywords';
                    keyw = varargin{3};
                else
                    lcmd = 'keywordsi';
                    keyw = lower(varargin{3});
                end
                %
                M = zeros(size(iChap));
                for i = 1:length(iChap)
                    A = chapkeys(lcmd,varargin{1},iChap(i));
                    M(i) = sum(strcmp(keyw,A));
                end
                varargout{1} = M;
                return
            end
        catch
            varargout{1} = false;
        end
    case {'get','getstring','geti','getstringi','cget','cgetstring','cgeti','cgetstringi'}
        [varargout{1:max(nargout,1)}] = getfield(lcmd,varargin{:});
    case {'set','seti'}
        varargout{1} = setfield(lcmd,varargin{:});
    case {'delete','remove','deletei','removei'}
        varargout{1} = setfield(lcmd,varargin{:},[]);
    case 'write'
        FI = writefile(varargin{:});
        if nargout>0
            varargout = {FI};
        end
    case 'new'
        varargout{1} = newfile;
    otherwise
        error('Unknown command: %s',cmd)
end


function FI=newfile
S=cell(0,2);
FI.FileName='new file';
FI.FileType='INI file';
FI.Data=S;


function FI=readfile(filename)
fid=fopen(filename,'rt');
if fid<0
    error('Error opening %s.',filename)
end
Line = textscan(fid,'%s','delimiter','\n','whitespace','');
Line = Line{1};
fclose(fid);
Line = strtrim(Line);
Line(cellfun('isempty',Line))=[];
%
ichp = 0;
% preallocate space for 1000 chapters
PreAllocChap = 1000;
S = cell(PreAllocChap,2);
for i = 1:length(Line)
    ln = Line{i};
    if ln(1)=='['
        % remove unused preallocated key fields
        if ichp>0
            S{ichp,2} = strtrim(K(1:ikey,:));
        end
        % if we reach the preallocated chapter array length, double its length
        if ichp==PreAllocChap
            PreAllocChap = 2*PreAllocChap;
            S{PreAllocChap,1} = [];
        end
        % create a new chapter and preallocate array space for keys
        ichp = ichp+1;
        PreAllocKey = 1000;
        K = cell(PreAllocKey,2);
        S{ichp,1} = ln(2:end-1);
        ikey = 0;
    else
        % if we find lines before a chapter, add a dumy chapter
        if ichp==0
            ichp = ichp+1;
            PreAllocKey = 1000;
            K = cell(PreAllocKey,2);
            S{ichp,1} = '';
            ikey = 0;
        end
        % if we reach the preallocated key array length, double its length
        if ikey==PreAllocKey
            PreAllocKey = 2*PreAllocKey;
            K{PreAllocKey,1} = [];
        end
        ikey = ikey+1;
        % process the key
        eq = strfind(ln,'=');
        if ~isempty(eq)
            K{ikey,1} = ln(1:eq(1)-1);
            K{ikey,2} = ln(eq(1)+1:end);
        else
            K{ikey,1} = '';
            K{ikey,2} = ln;
        end
    end
end
% remove any superfluous preallocated cells for keys and chapters
if ichp>0
    S{ichp,2} = strtrim(K(1:ikey,:));
end
S = S(1:ichp,:);
%
FI.FileName=filename;
FI.FileType='INI file';
FI.Data=S;


function FI = writefile(filename,FI,formatStyle)
FI.FileName = filename;
S = FI.Data;
fid = fopen(filename,'wt');
if fid<0
    error('Error opening %s.',filename)
end
if nargin<3
    formatStyle = 'standard';
end
compact = false;
pretty  = false;
switch formatStyle
    case 'compact'
        compact = true;
    case 'standard'
    case 'pretty'
        pretty = true;
end
if pretty
    indent = '    ';
else
    indent = '';
end
%
% Keywords without a Chapter title should be written first.
%
for i = 1:size(S,1)
    if isempty(S{i,1})
        S = cat(1,S(i,:),S(1:i-1,:),S(i+1:end,:));
    end
end
if compact
    format        = '%s=%s\n';
    format_spaces = '%s\n';
else
    maxkeywordlength = 0;
    for i = 1:size(S,1)
        SF = S{i,2};
        for j = 1:size(SF,1)
            maxkeywordlength = max(maxkeywordlength,length(SF{j,1}));
        end
    end
    if pretty
        maxkeywordlength = maxkeywordlength+1;
    end
    format        = [indent,'%-',num2str(maxkeywordlength),'s= %s\n'];
    format_spaces = [indent,repmat(' ',1,maxkeywordlength),'  %s\n'];
end
for i = 1:size(S,1)
    if ~isempty(S{i,1})
        fprintf(fid,'[%s]\n',S{i,1});
    end
    SF = S{i,2};
    for j = 1:size(SF,1)
        Str = SF{j,2};
        if ~ischar(Str)
            Str = sprintf('%g ',Str);
            Str(end) = [];
        end
        if isempty(SF{j,1})
            fprintf(fid,format_spaces,Str);
        else
            fprintf(fid,format,SF{j,1},Str);
        end
    end
    if pretty
        fprintf(fid,'\n');
    end
end
fclose(fid);


function Chapters = chapfile(cmd,FI,grpS)
CaseInsensitive = cmd(end)=='i';
Chapters = FI.Data(:,1);
if nargin>2
    if CaseInsensitive
        Chapters = find(strcmpi(Chapters,grpS));
    else
        Chapters = find(strcmp(Chapters,grpS));
    end
elseif CaseInsensitive
    Chapters = lower(Chapters);
end


function Keywords = chapkeys(cmd,FI,grpS)
S = FI.Data;
CaseInsensitive = cmd(end)=='i';
if ischar(grpS)
    if isequal(grpS,'*')
        grp = 1:size(S,1);
    else
        if CaseInsensitive
            grp = strcmpi(grpS,S(:,1));
        else
            grp = strcmp(grpS,S(:,1));
        end
        grp = find(grp);
    end
elseif isnumeric(grpS) && all(grpS(:)<=size(S,1))
    grp = grpS(:)';
else
    grp = [];
end
if isempty(grp)
    error('Chapter ''%s'' does not exist.',var2str(grpS))
elseif length(grp)>1
    error('Can''t retrieve keywords for multiple chapters at once.')
end
Keywords = S{grp,2}(:,1);
if CaseInsensitive
    Keywords = lower(Keywords);
end


function [val,iGRP]=getfield(cmd,FI,grpS,keyS,def)
S = FI.Data;
CaseInsensitive = cmd(end)=='i';
if CaseInsensitive
    cmd = cmd(1:end-1);
end
CellOutput = cmd(1)=='c';
if CellOutput
    cmd = cmd(2:end);
end
if ischar(grpS)
    if isequal(grpS,'*')
        grp = 1:size(S,1);
    else
        if CaseInsensitive
            grp = strcmpi(grpS,S(:,1));
        else
            grp = strcmp(grpS,S(:,1));
        end
        grp = find(grp);
    end
elseif isnumeric(grpS) && all(grpS(:)<=size(S,1))
    grp = grpS;
    grpS = sprintf('group#%i',grp);
else
    grp = [];
end
if isempty(grp)
    if CellOutput
        val = {};
        return
    elseif nargin>=5
        val = def;
        return
    end
    error('Chapter ''%s'' does not exist',var2str(grpS))
end
Keywords = cat(1,S{grp,2});
if nargout>1
    iGRP = zeros(length(Keywords),1);
    o = 0;
    for i = 1:length(grp)
        nKeyw = size(S{grp(i),2},1);
        iGRP(o+(1:nKeyw)) = grp(i);
        o = o+nKeyw;
    end
else
    iGRP = [];
end
if ischar(keyS)
    keyS = deblank(keyS);
    if CaseInsensitive
        key = strcmpi(keyS,Keywords(:,1));
    else
        key = strcmp(keyS,Keywords(:,1));
    end
    key = find(key);
else
    key = keyS;
    if length(grp)>1
        error('Keyword indexing not supported for multiple chapters at once.')
    end
end
if isempty(key) && nargin>=5
    val=def;
elseif isequal(size(key),[1 1]) && ~CellOutput
    val=Keywords{key,2};
    if ischar(val) && ~strcmp(cmd,'getstring')
        [lni,n,err,SF2i]=sscanf(val,'%f',[1 inf]);
        if isempty(err) && SF2i>length(val)
            val=lni;
        end
    end
elseif ~isempty(key)
    val=Keywords(key,2);
    if ~strcmp(cmd,'getstring')
        for i=1:length(val)
            [lni,n,err,SF2i]=sscanf(val{i},'%f',[1 inf]);
            if isempty(err) && SF2i>length(val{i})
                val{i} = lni;
            end
        end
    end
else
    error('Keyword ''%s'' not found in Chapter ''%s''.',keyS,grpS)
end
if ~isempty(iGRP)
    iGRP = iGRP(key);
end


function FI=setfield(cmd,FI,grpS,varargin)
S = FI.Data;
CaseInsensitive = cmd(end)=='i';
if nargin<4
    error('Not enough input arguments.')
end
if ischar(grpS)
    % find a group by name
    if isequal(grpS,'*')
        grp = 1:size(S,1);
    else
        if CaseInsensitive
            grp = strcmpi(grpS,S(:,1));
        else
            grp = strcmp(grpS,S(:,1));
        end
        grp = find(grp);
    end
elseif isnumeric(grpS)
    % find a group by index
    if any(grpS(:)>size(S,1))
        error('Invalid group index: index larger than number of groups in file.')
    elseif any(grpS(:)<1)
        error('Invalid group index: index less than 1.')
    end
    grp = grpS;
else
    % unrecognized group identifier
    error('Unrecognized group indentifier specified as 3rd argument to INIFILE call.')
end
if isempty(grp)
    if isempty(varargin{1}) && isnumeric(varargin{1})
        % remove a non-existing group. Done!
        return
    end
    S(end+1,1:2) = {grpS cell(0,2)};
    grp = size(S,1);
end
if ischar(grpS) && isnumeric(varargin{1})
    % group index
    igrp = varargin{1};
    if isequal(igrp,length(grp)+1)
        S(end+1,1:2) = {grpS cell(0,2)};
        grp = size(S,1);
    else
        grp = grp(igrp);
    end
    kS = 2;
else
    kS = 1;
end
%
if length(varargin)>=kS
    keyS = varargin{kS};
end
if ischar(keyS)
    keyS = deblank(keyS);
end
%
if length(varargin)>=kS+1
    val = varargin{kS+1};
    DeleteKey = isempty(val) && isnumeric(val);
    iv = 0;
    for i=1:length(grp)
        Keywords=S{grp(i),2};
        if isnumeric(keyS)
            key = keyS;
        elseif CaseInsensitive
            key = strcmpi(keyS,Keywords(:,1));
        else
            key = strcmp(keyS,Keywords(:,1));
        end
        if any(key)
            % key exists
            ingrp(i)=1;
            if DeleteKey
                S{grp,2}(key,:)=[];
            elseif iscell(val)
                % assign different value per record
                key=find(key);
                iv = iv+1;
                if iv<=length(val)
                    S{grp,2}{key(1),2}=val{iv};
                    if length(key)>1
                        S{grp,2}(key(2:end),:)=[];
                    end
                end
            else
                % assign same value to all records
                key=find(key);
                S{grp,2}{key(1),2}=val;
                if length(key)>1
                    S{grp,2}(key(2:end),:)=[];
                end
            end
        else
            % key doesn't exist
            if DeleteKey
                % nothing to do
            elseif iscell(val)
                % assign different value per record
                iv = iv+1;
                if iv<=length(val)
                    S{grp(i),2}(end+1,1:2)={keyS val{iv}};
                end
            else
                % assign same value to all records
                S{grp(i),2}(end+1,1:2)={keyS val};
            end
        end
    end
    %
    if iscell(val) && iv~=numel(val)
        error('Mismatch between the number of matching chapters (%i) and number of values given (%i)',iv,numel(val))
    end
elseif isempty(keyS) && isnumeric(keyS)
    % remove group
    S(grp,:) = [];
else
    error('No value specified during INIFILE SET call.')
end
FI.Data=S;
