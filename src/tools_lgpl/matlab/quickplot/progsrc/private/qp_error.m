function qp_error(msg,Ex,varargin)
%QP_ERROR QUICKPLOT error handler
%   QP_ERROR(CodeMessage,Exception) prints the CodeMessage followed by the
%   message and stack of the Exception to the ui_message dialog as an "error".
%
%   See also UI_MESSAGE.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_error.m $
%   $Id: qp_error.m 5227 2015-06-23 20:35:51Z jagers $

stacklist = stack2str(Ex.stack,varargin{:});
message = Ex.message;
if isequal(message(1:min(20,end)),'Error using <a href=')
    newline = find(message==char(10));
    message = message(newline(1)+1:end);
end
ui_message('error',{msg,message,stacklist{:}})