function file = absfullfile(varargin)
%ABSFULLFILE Build full absolute file name from parts.
%   F = ABSFULLFILE(FOLDERNAME1, FOLDERNAME2, ..., FILENAME) uses the
%   standard MATLAB function FULLFILE to build a full file specification F
%   from the folders and file name specified. In addition to FULLFILE,
%   ABSFULLFILE will verify whether the resulting file specification F is
%   an absolute path. If not, it will prepend the current path PWD.
%
%   See also FULLFILE, PWD.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/absfullfile.m $
%   $Id: absfullfile.m 4612 2015-01-21 08:48:09Z mourits $

file = varargin{1};
if isequal(file(1),'/') || isequal(file(1),'\')
    % assume OK \\OtherComputer\folder\file or /folder/file
    % use fullfile to combine and simplify file expresssion
    file = fullfile(varargin{:});
else
    cl = strfind(file,':');
    if ~isempty(cl)
        % assume OK d:\folder\file or http://server/folder/file
        if isequal(cl,2)
            % use fullfile to combine and simplify file expresssion
            file = fullfile(varargin{:});
        else
            % don't use fullfile to concat webserver path because it also
            % converts forward slashes to backward slashes on Windows
            if nargin>1
                error('Multiple arguments to absfullfile not yet supported for web addresses')
            end
        end
    else
        % just simply file or folder\file
        % attach pwd to create absolute path
        % use fullfile to combine and simplify file expresssion
        file = fullfile(pwd,varargin{:});
    end
end