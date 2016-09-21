function qp_update_options(OH,UD,Ops)
%QP_UPDATE_OPTIONS Update QuickPlot user interface for plot options.

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/branches/research/Deltares/20160119_tidal_turbines/src/tools_lgpl/matlab/quickplot/progsrc/private/qp_update_options.m $
%   $Id: qp_update_options.m 5632 2015-12-09 08:50:03Z jagers $

set(OH,'enable','off','visible','off')

if isempty(Ops)
    return
end

OTHER = {'other option 1','other option 2','other option 3'};
Active=UD.Active;

if isfield(Ops,'axestimezone_shift') && ~isnan(Ops.axestimezone_shift)
    atz = findobj(OH,'tag','axestimezone=?');
    set(findobj(OH,'tag','axestimezone'),'enable','on');
    set(atz,'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'plotcoordinate')
    coords = {Ops.plotcoordinate,OTHER{:}};
    i = 1;
    set(findobj(OH,'tag','plotcoordinate'),'enable','on');
    pd=findobj(OH,'tag','plotcoordinate=?');
    set(pd,'string',coords,'value',i,'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'vectorcomponent')
    set(findobj(OH,'tag','component'),'enable','on');
    compon=findobj(OH,'tag','component=?');
    set(compon,'enable','on','backgroundcolor',Active)
    compList = {Ops.vectorcomponent,OTHER{:}};
    comp = 1;
    set(compon,'value',1,'string',compList,'value',comp)
end

%    switch Ops.vectorcomponent
%        case {'vector','patch centred vector','vector (split x,y)','vector (split m,n)'}
%            Ops.presentationtype=Ops.vectorcomponent;
%        case 'edge'
%            Ops.presentationtype=Ops.vectorcomponent;
%    end

if isfield(Ops,'presentationtype')
    set(findobj(OH,'tag','presenttype'),'enable','on')
    pt=findobj(OH,'tag','presenttype=?');
    PrsTps = {Ops.presentationtype,OTHER{:}};
    p = 1;
    set(pt,'enable','on','value',1,'string',PrsTps,'value',p,'backgroundcolor',Active)
%    switch Ops.presentationtype
%        case 'vector'
%            Ops.vectorcomponent='edge';
%    end
end

if isfield(Ops,'vectorcolour')
    colvect=findobj(OH,'tag','colourvectors');
    set(colvect,'enable','on')
    colvecm=findobj(OH,'tag','vectorcolour=?');
    vecCLR = {Ops.vectorcolour,OTHER{:}};
    colveci = 1;
    set(colvecm,'value',1,'string',vecCLR,'value',colveci)
end

if isfield(Ops,'units')
    set(findobj(OH,'tag','dataunits'),'enable','on')
    dunit=findobj(OH,'tag','dataunits=?');
    if strcmp(Ops.units,'**Hide**')
        ival = 8;
    else
        ival = 7;
        dunitset=findobj(OH,'tag','dataunits=!');
        set(dunitset,'enable','on','backgroundcolor',Active,'string',Ops.units)
    end
    set(dunit,'enable','on','backgroundcolor',Active,'value',ival)
end

if isfield(Ops,'angleconvention')
    pd=findobj(OH,'tag','angleconvention=?');
    conventions = {Ops.angleconvention,OTHER{:}};
    i = 1;
    set(pd,'value',1,'string',conventions,'value',i)
    set(findobj(OH,'tag','angleconvention'),'enable','on');
    set(pd,'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'colourdams')
    coldams=findobj(OH,'tag','colourdams');
    set(coldams,'enable','on','value',Ops.colourdams)
end

if isfield(Ops,'operator')
    oper=findobj(OH,'tag','operator');
    set(oper,'enable','on')
    oper=findobj(OH,'tag','operator=?');
    operstr = {Ops.operator,OTHER{:}};
    operi   = 1;
    set(oper,'enable','on','backgroundcolor',Active)
end

if isfield(Ops,'vectorstyle')
    set(findobj(OH,'tag','vectorstyle'),'enable','on')
    vstyle=findobj(OH,'tag','vectorstyle=?');
    vstyles={Ops.vectorstyle,OTHER{:}};
    i = 1;
    set(vstyle,'enable','on','backgroundcolor',Active,'string',vstyles,'value',i)
end

if isfield(Ops,'vectorscalingmode')
    set(findobj(OH,'tag','vecscalem'),'enable','on')
    vsmode=findobj(OH,'tag','vecscalem=?');
    vsmodes={Ops.vectorscalingmode,OTHER{:}};
    i=1;
    set(vsmode,'enable','on','backgroundcolor',Active,'string',vsmodes,'value',i)
end

if isfield(Ops,'vectorscale')
    oneunitis=findobj(OH,'tag','1vecunit=?');
    set(findobj(OH,'tag','1vecunit'),'enable','on')
    set(oneunitis,'enable','on','backgroundcolor',Active,'string',num2str(Ops.vectorscale))
end

if isfield(Ops,'verticalscalingmode')
    set(findobj(OH,'tag','vertscalem'),'enable','on')
    vsm=findobj(OH,'tag','vertscalem=?');
    VsMeths = {Ops.verticalscalingmode,OTHER{:}};
    vsi = 1;
    set(vsm,'enable','on','backgroundcolor',Active,'string',VsMeths,'value',vsi)
end

if isfield(Ops,'verticalscalefactor')
    set(findobj(OH,'tag','vscale'),'enable','on')
    enl=findobj(OH,'tag','vscale=?');
    set(enl,'enable','on','backgroundcolor',Active,'string',num2str(Ops.verticalscalefactor))
end

if isfield(Ops,'extend2edge')
    h = findobj(OH,'tag','extend2edge');
    set(h,'enable','on','value',Ops.extend2edge)
end

if isfield(Ops,'numformat')
    set(findobj(OH,'tag','numformat'),'enable','on');
    numform=findobj(OH,'tag','numformat=?');
    set(numform,'enable','on','backgroundcolor',Active,'string',Ops.numformat)
end

if isfield(Ops,'fontsize')
    set(findobj(OH,'tag','fontsize'),'enable','on');
    hFontsize=findobj(OH,'tag','fontsize=?');
    set(hFontsize,'enable','on','backgroundcolor',Active,'string',num2str(Ops.fontsize))
end

if isfield(Ops,'horizontalalignment')
    set(findobj(OH,'tag','alignment'),'enable','on');
    set(findobj(OH,'tag','horizontalalignment'),'enable','on');
    set(findobj(OH,'tag','verticalalignment'),'enable','on');
    hHorAlign=findobj(OH,'tag','horizontalalignment=?');
    strHorAlign={Ops.horizontalalignment,OTHER{:}};
    iHorAlign=1;
    set(hHorAlign,'enable','on','backgroundcolor',Active,'string',strHorAlign,'value',iHorAlign)
    %
    hVerAlign=findobj(OH,'tag','verticalalignment=?');
    strVerAlign={Ops.verticalalignment,OTHER{:}};
    iVerAlign=1;
    set(hVerAlign,'enable','on','backgroundcolor',Active,'string',strVerAlign,'value',iVerAlign)
end

if isfield(Ops,'thinningmode')
    set(findobj(OH,'tag','thinfld'),'enable','on');
    thinfld=findobj(OH,'tag','thinfld=?');
    thinmodes = {Ops.thinningmode,OTHER{:}};
    i = 1;
    set(thinfld,'enable','on','backgroundcolor',Active,'string',thinmodes,'value',i)
end

if isfield(Ops,'thinningfactors')
    set(findobj(OH,'tag','thinfact'),'enable','on');
    thinfact=findobj(OH,'tag','thinfact=?');
    set(thinfact,'enable','on','backgroundcolor',Active,'string',num2str(Ops.thinningfactors))
end

if isfield(Ops,'thinningdistance')
    set(findobj(OH,'tag','thindist'),'enable','on');
    thindist=findobj(OH,'tag','thindist=?');
    set(thindist,'enable','on','backgroundcolor',Active,'string',num2str(Ops.thinningdistance))
end

if isfield(Ops,'colour')
    set(findobj(OH,'tag','colour'),'enable','on')
    clrh=findobj(OH,'tag','colour=?');
    set(clrh,'enable','on','backgroundcolor',Ops.colour)
end

if isfield(Ops,'facecolour')
    if ~ischar(Ops.facecolour)
        clrh=findobj(OH,'tag','facecolour=?');
        set(clrh,'enable','on','backgroundcolor',Ops.facecolour)
    elseif strcmp(Ops.facecolour,'yes')
        fpoly=findobj(OH,'tag','fillpolygons');
        set(fpoly,'enable','on','value',1)
    end
end

if isfield(Ops,'textboxfacecolour')
    hTextbox=findobj(OH,'tag','textbox=?');
    set(hTextbox,'enable','on','enable','on','backgroundcolor',Ops.textboxfacecolour)
end

if isfield(Ops,'linestyle')
    set(findobj(OH,'tag','linestyle'),'enable','on')
    lns=findobj(OH,'tag','linestyle=?');
    lnstls = {Ops.linestyle,OTHER{:}};
    i = 1;
    set(lns,'enable','on','backgroundcolor',Active,'string',lnstls,'value',i)
end

if isfield(Ops,'linewidth')
    set(findobj(OH,'tag','linewidth'),'enable','on')
    lnw=findobj(OH,'tag','linewidth=?');
    set(lnw,'enable','on','backgroundcolor',Active,'string',num2str(Ops.linewidth))
end

if isfield(Ops,'marker')
    set(findobj(OH,'tag','marker'),'enable','on')
    mrk=findobj(OH,'tag','marker=?');
    mrkrs = {Ops.marker,OTHER{:}};
    imrk = 1;
    set(mrk,'enable','on','backgroundcolor',Active,'string',mrkrs,'value',imrk)
end

if isfield(Ops,'markersize') && ~isequal(Ops.marker,'none')
    set(findobj(OH,'tag','markersize'),'enable','on')
    mrk=findobj(OH,'tag','markersize=?');
    set(mrk,'enable','on','backgroundcolor',Active,'string',num2str(Ops.markersize))
end

if isfield(Ops,'markercolour') && ~ischar(Ops.markercolour)
    mc=findobj(OH,'tag','usemarkercolour');
    set(mc,'enable','on')
    mc=findobj(OH,'tag','markercolour=?');
    set(mc,'enable','on','backgroundcolor',Ops.markercolour)
end

if isfield(Ops,'markerfillcolour') && ~ischar(Ops.markerfillcolour)
    mc=findobj(OH,'tag','usemarkerfillcolour');
    set(mc,'enable','on','value',1)
    %
    mc=findobj(OH,'tag','markerfillcolour=?');
    set(mc,'enable','on','backgroundcolor',Ops.markerfillcolour)
end

if isfield(Ops,'presentationtype') && isfield(Ops,'thresholds') && ...
        ismember(Ops.presentationtype,{'vector','patches','patches with lines','markers'})
    cclass=findobj(OH,'tag','colclassify');
    set(cclass,'enable','on','value',1)
end

if isfield(Ops,'thresholds')
    c = Ops.thresholds;
    set(findobj(OH,'tag','thresholds'),'enable','on')
    set(findobj(OH,'tag','thresholds=?'),'enable','on','backgroundcolor',Active,'string',vec2str(c,'noones','nobrackets'),'userdata',c)
end

if isfield(Ops,'thresholddistribution')
    thrd=findobj(OH,'tag','threshdistr=?');
    thrdStr = {Ops.thresholddistribution,OTHER{:}};
    i = 1;
    set(thrd,'enable','on','backgroundcolor',Active,'string',thrdStr,'value',i)
end

if isfield(Ops,'colourlimits')
    set(findobj(OH,'tag','climmode'),'enable','on')
    climmode=findobj(OH,'tag','climmode=?');
    if isequal(size(Ops.colourlimits),[1 2])
        set(climmode,'enable','on','backgroundcolor',Active,'value',2)
        set(findobj(OH,'tag','climmax'),'enable','on')
        set(findobj(OH,'tag','climmax=?'),'enable','on','backgroundcolor',Active,'string',Ops.colourlimits(2))
        set(findobj(OH,'tag','climmin'),'enable','on')
        set(findobj(OH,'tag','climmin=?'),'enable','on','backgroundcolor',Active,'string',Ops.colourlimits(1))
        Min=get(findobj(OH,'tag','climmin=?'),'userdata');
        Max=get(findobj(OH,'tag','climmax=?'),'userdata');
    else
        set(climmode,'enable','on','backgroundcolor',Active,'value',1)
    end
end

if isfield(Ops,'symmetriccolourlimits')
    climsymm=findobj(OH,'tag','climsymm');
    set(climsymm,'enable','on','value',Ops.symmetriccolourlimits)
end

if isfield(Ops,'colourmap')
    set(findobj(OH,'tag','colourmap'),'enable','on')
    set(findobj(OH,'tag','colourmapbutton'),'enable','on')
    cmaps = {Ops.colourmap,OTHER{:}};
    imap = 1;
    cmap=findobj(OH,'tag','colourmap=?');
    set(cmap,'enable','on','backgroundcolor',Active,'string',cmaps,'value',imap)
end

if isfield(Ops,'colourbar')
    cbar=findobj(OH,'tag','colourbar');
    set(cbar,'enable','on')
    cbarh=findobj(OH,'tag','colbarhorz');
    set(cbarh,'enable','on')
end

if isfield(Ops,'axestype')
    % do something
end

if isfield(Ops,'clippingvalues')
    c = Ops.clippingvalues;
    set(findobj(OH,'tag','clippingvals'),'enable','on')
    set(findobj(OH,'tag','clippingvals=?'),'enable','on','backgroundcolor',Active,'string',clip2str(c),'userdata',c)
end

if isfield(Ops,'xclipping')
    set(findobj(OH,'tag','clippingvals'),'enable','on')
    c = Ops.xclipping;
    set(findobj(OH,'tag','xclipping'),'enable','on')
    set(findobj(OH,'tag','xclipping=?'),'enable','on','backgroundcolor',Active,'string',clip2str(c),'userdata',c)
    c = Ops.yclipping;
    set(findobj(OH,'tag','yclipping'),'enable','on')
    set(findobj(OH,'tag','yclipping=?'),'enable','on','backgroundcolor',Active,'string',clip2str(c),'userdata',c)
end

set(findall(OH,'enable','on'),'enable','inactive')

function Str = clip2str(c)
if isstruct(c)
    Str=realset(c);
else
    Str=vec2str(c,'noones','nobrackets');
end
