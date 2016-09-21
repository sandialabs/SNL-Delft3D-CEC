function [DataFI,FileName,Tp,Otherargs] = grid_fopen(cmd,varargin)
%GRID_FOPEN Routine for opening attribute files on grid.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/grid_fopen.m $
%   $Id: grid_fopen.m 5295 2015-07-25 05:45:18Z jagers $

DataFI   = [];
FileName = '';
Tp       = '';
%
filterspec={...
    '*.dep;*.qin' 'QuickIn file'                         'wldep'     'wldep'
    '*.*'         'SIMONA box file'                      'boxfile'   'boxfile'
    '*.*'         'Delft3D-MOR field file'               'wlfdep'    'wlfdep'
    'tri-rst.*'   'Delft3D-FLOW restart file'            'trirst'    'trirst'
    '*.enc'       'Delft3D/SIMONA enclosure file'        'enclosure' 'enclosure'
    '*.aru;*.arv' 'Delft3D/SIMONA roughness area file'   'trtarea'   'trtarea'
    'swanout'     'SWAN map output file'                 'swanout'   'swanout'
    '*.bnd'       'Delft3D-FLOW boundary file'           'attrib'    'attrib openboundary'
    '*.thd'       'Delft3D-FLOW thin dam file'           'attrib'    'attrib thindam'
    '*.*'         'SIMONA thin dam file'                 'attrib'    'attrib thindam-waqua'
    '*.dry'       'Delft3D-FLOW dry points file'         'attrib'    'attrib drypoint'
    '*.rgs'       'Delft3D-FLOW rigid sheet file'        'attrib'    'attrib rigidsheet'
    '*.3dg'       'Delft3D-FLOW 3D gate file'            'attrib'    'attrib 3dgate'
    '*.obs'       'Delft3D-FLOW observation point file'  'attrib'    'attrib observation points'
    '*.crs'       'Delft3D-FLOW cross-section file'      'attrib'    'attrib cross-sections'
    '*.src'       'Delft3D-FLOW discharge station file'  'attrib'    'attrib discharge stations'
    '*.2dw;*.wr'  'Delft3D-FLOW weir file'               'attrib'    'attrib weir'
    '*.*'         'SIMONA weir file'                     'attrib'    'attrib weir-waqua'
    'bag*.*'      'Delft3D-MOR dredge map output file'   'bagmap'    'bagmap'
    '*.inc'       'Incremental file'                     'fls'       'fls'};
%
switch cmd
    case 'opennew'
        FileName  = '';
        targetdir = varargin{1};
    case 'open'
        FileName  = deblank(varargin{1});
        targetdir = fileparts(FileName);
end
gridsize  = varargin{2};
Otherargs = {gridsize};
if isempty(FileName)
    currentdir=pwd;
    cd(targetdir);
    [fn,pn]=uigetfile(cat(1,{'*.*','All Files'},filterspec(:,1:2)),'Select data file to open ...');
    cd(currentdir);
    if ~ischar(fn)
        return
    end
    FileName=[pn fn];
end
%autodetect intelligence ...
try_next='trtarea';
[pn,fn,en]=fileparts(FileName);
if strncmpi('bag',fn,3)
    try_next='bagmap';
elseif strncmpi('tri-rst',fn,7)
    try_next='trirst';
elseif strncmpi('swanout',fn,7)
    try_next='swanout';
else
    switch lower(deblank(en))
        case {'.aru','.arv'}
            try_next='trtarea';
        case {'.dep','.qin'}
            try_next='wldep';
        case {'.inc'}
            try_next='fls';
        case {'.bnd','.thd','.wr','.obs','.crs','.src','.dry'}
            try_next='attrib';
        case '.enc'
            try_next='enclosure';
    end
end
FileName = absfullfile(FileName);

types_to_check = setdiff(filterspec(:,3),try_next);

%try opening the file ...
userasked=0;
usertrytp='';
while isempty(DataFI)
    try
        switch try_next
            case 'trtarea'
                DataFI=trtarea('read',FileName);
            case 'swanout'
                DataFI=swan('read',FileName,gridsize);
            case 'wldep'
                try
                    DataFI=wldep('read',FileName,gridsize,'multiple');
                catch err1
                    try
                        DataFI=wldep('read',FileName,gridsize-1,'multiple');
                    catch
                        rethrow(err1)
                    end
                end
                if ~isempty(DataFI)
                    if ~isstruct(DataFI)
                        Tmp.Data={DataFI};
                        DataFI=Tmp; Tmp=[];
                    else
                        Tmp.Data={DataFI.Data};
                        DataFI=Tmp; Tmp=[];
                    end
                end
            case 'wlfdep'
                DataFI=wlfdep('read',FileName);
                if ~isequal(size(DataFI),gridsize)
                    error('Size of datafield does not match size of grid')
                else
                    Tmp.Data={DataFI};
                    DataFI=Tmp; Tmp=[];
                end
            case 'boxfile'
                DataFI=boxfile('read',FileName,gridsize);
                if ~isempty(DataFI)
                    if ~isequal(size(DataFI),gridsize)
                        %          ui_message('error','Size of datafield does not match size of grid');
                        %          DataFI=[]; return
                        DataFI=[];
                    else
                        Tmp.Data={DataFI};
                        DataFI=Tmp; Tmp=[];
                    end
                end
            case 'fls'
                DataFI=incremental('open',FileName,gridsize);
                DataFI=incremental('readtimes',DataFI);
                if ~isfield(DataFI,'Check')
                    DataFI=[];
                elseif strcmp(DataFI.Check,'NotOK')
                    DataFI=[];
                elseif ~strcmp(DataFI.FileType,'FLS-inc')
                    error('Don''t know how to relate %s file to grid.',DataFI.FileType)
                elseif length(DataFI.Domain)~=1
                    error('Multi-domain incremental file not supported on grid.')
                elseif ~isequal([DataFI.Domain.NRows DataFI.Domain.NCols],gridsize)
                    error('Size of datafield does not match size of grid.')
                end
            case 'trirst'
                DataFI=trirst('read',FileName,gridsize,'all');
                if ~isempty(DataFI)
                    Tmp.Data={DataFI.Data};
                    Tmp.NLyr=1;
                    DataFI=Tmp; Tmp=[];
                end
            case 'bagmap'
                DataFI=bagmap('open',FileName);
                if ~isequal([DataFI.M DataFI.N],gridsize)
                    error('Size of datafield does not match size of grid.')
                end
            case 'enclosure'
                DataTmp=enclosure('read',FileName);
                if any(max(DataTmp)>gridsize)
                    error('Enclosure extends beyond grid')
                else
                    DataFI.Data=DataTmp;
                end
            otherwise
                if strcmp(try_next(1:min(6,end)),'attrib')
                    DataFI=d3d_attrib('read',FileName,try_next(8:end));
                    if ~isempty(DataFI) && strcmp(DataFI.Check,'OK');
                        try_next=DataFI.Type;
                        if isfield(DataFI,'MNu')
                            if (max(max(DataFI.MNu(:,[1 3])))>gridsize(1)) || (max(max(DataFI.MNv(:,[1 3])))>gridsize(1)) || ...
                                    (max(max(DataFI.MNu(:,[2 4])))>gridsize(2)) || (max(max(DataFI.MNv(:,[2 4])))>gridsize(2))
                                error('Weirs/dams outside grid encountered.')
                            end
                        elseif isfield(DataFI,'MNKu')
                            if (max(max(DataFI.MNKu(:,[1 3])))>gridsize(1)) || (max(max(DataFI.MNKv(:,[1 3])))>gridsize(1)) || ...
                                    (max(max(DataFI.MNKu(:,[2 4])))>gridsize(2)) || (max(max(DataFI.MNKv(:,[2 4])))>gridsize(2))
                                error('Gates/sheets outside grid encountered.')
                            end
                        elseif isequal(DataFI.Type,'drypoint')
                            if (max(max(DataFI.MN(:,[1 3])))>gridsize(1)) || (max(max(DataFI.MN(:,[2 4])))>gridsize(2))
                                error('Dry points outside grid encountered.')
                            end
                        elseif isequal(DataFI.Type,'observation points')
                            if (max(DataFI.MN(:,1))>gridsize(1)) || (max(DataFI.MN(:,2))>gridsize(2))
                                error('Observation points outside grid encountered.')
                            end
                        elseif isequal(DataFI.Type,'openboundary')
                            if (max(DataFI.MN(:,1))>gridsize(1)) || (max(DataFI.MN(:,2))>gridsize(2))
                                error('Boundary locations outside grid encountered.')
                            end
                        elseif isequal(DataFI.Type,'discharge stations')
                            if (max(DataFI.MNK(:,1))>gridsize(1)) || (max(DataFI.MNK(:,2))>gridsize(2))
                                error('Discharge stations outside grid encountered.')
                            end
                        elseif isequal(DataFI.Type,'cross-sections')
                            if (max(max(DataFI.MNMN(:,[1 3])))>gridsize(1)) || ...
                                    (max(max(DataFI.MNMN(:,[2 4])))>gridsize(2))
                                error('Cross-sections outside grid encountered.')
                            end
                        else
                            error('%s not yet supported.',DataFI.Type)
                        end
                    else
                        DataFI=[];
                    end
                else
                    break
                end
        end
        if ~isempty(DataFI)
            DataFI.FileType=try_next;
        end
    catch err
        DataFI=[];
    end
    if isempty(DataFI)
        % opening of file was not successful
        if ~isempty(types_to_check)
            % other file type to check
            try_next = types_to_check{1};
            types_to_check = types_to_check(2:end);
        else
            % I have tried all file types
            if userasked
                % and I have asked the user
                qp_error(sprintf('Unable to load attribute file\n%s\nonto grid as %s:',FileName,usertrytp),err)
                break
            else
                [usertrytp,try_i]=ui_type(filterspec(:,2),'windowtitle','Specify file format');
                if isempty(usertrytp)
                    break
                end
                try_next=filterspec{try_i,4};
                userasked=1;
                err=[];
                err.message='No specific error message generated';
                err.stack=[];
            end
        end
    end
end
if ~isempty(DataFI)
    Tp=DataFI.FileType;
end
