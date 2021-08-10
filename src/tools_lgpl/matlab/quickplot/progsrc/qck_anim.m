function qck_anim(cmd,afig,ANISteps)
%QCK_ANIM Helper function for QuickPlot Animations.

%QCK_ANIM(Cmd,Figure,CmdArgs)

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
%   $HeadURL: https://svn.oss.deltares.nl/repos/delft3d/tags/delft3d4/65936/src/tools_lgpl/matlab/quickplot/progsrc/qck_anim.m $
%   $Id: qck_anim.m 65778 2020-01-14 14:07:42Z mourits $

persistent savedir
if ~ischar(savedir)
    savedir='';
elseif ~isempty(savedir)
    if savedir(end)~=filesep
        savedir(end+1)=filesep;
    end
end

if nargin<2
    afig=gcbf;
    if isempty(afig)
        error('This function should not be called manually.')
    end
end

if strcmp(cmd,'animpush')
    pos=get(gcbo,'position');
    uicm=findobj(gcbf,'tag','animpushuicontextmenu');
    set(uicm,'position',pos(1:2)+pos(3:4)/2,'visible','on')
    return
end
%
T_=1; ST_=2; M_=3; N_=4; K_=5;
AnimSlid=findobj(afig,'tag','animslid');
AS=get(AnimSlid,'userdata');
if strcmp(cmd,'animselect')
    h = [];
    par_fig = gcbo;
    while ~isempty(par_fig) && ~isequal(get(par_fig,'type'),'figure')
        par_fig=get(par_fig,'parent');
    end
elseif isempty(AS)
    set(AnimSlid,'enable','off')
    return
else
    t_=AS(1).Fld(1);
    mask=logical(ones(1,length(AS)));
    for iobj=1:length(AS)
        h{iobj}=findall(0,'tag',AS(iobj).Tag);
        if isempty(h{iobj})
            mask(iobj)=0;
        end
    end
    h=h(mask);
    AS=AS(mask);
    set(AnimSlid,'userdata',AS)
    if isempty(AS)
        set(AnimSlid,'enable','off')
        return
    end
    h=cat(1,h{:});
    %
    % get figures in which the items to be animated are located
    %
    par_ax=get(h,'parent');
    if iscell(par_ax)
        par_ax=unique(cat(1,par_ax{:}));
    end
    par_fig=get(par_ax,'parent');
    if iscell(par_fig)
        par_fig=unique(cat(1,par_fig{:}));
    end
    %
    UDh=get(h,'userdata');
    if iscell(UDh)
        UDh=UDh(~cellfun('isempty',UDh));
    else
        UDh={UDh};
    end
    AnimObj=UDh{1};
    %
    % Backward compatible with cell version of PlotState ...
    %
    if iscell(AnimObj.PlotState)
        AnimObj.PlotState=plotstatestruct(AnimObj.PlotState);
    end
    %
    if t_==0
        t = AnimObj.PlotState.SubField{1};
    else
        t=AnimObj.PlotState.Selected{t_};
        if isnumeric(AS(1).Values)
            t=find(AS(1).Values==t);
        end
    end
end
%
switch cmd
    case {'start','startanim'}
        asld=findobj(afig,'tag','animslid');
        sld=findobj(par_fig,'tag','animslid');
        psh=findobj(par_fig,'tag','animpush');
        animstop=findall(par_fig,'tag','stopanim');
        set(animstop,'userdata',0)
        i0=getappdata(asld,'minival');
        i1=getappdata(asld,'maxival');
        background=0;
        animloop=0;
        maxfps=25;
        scriptname='';
        if nargin<3
            [ANISteps,output,Cancel,background,animloop,maxfps,scriptname]=local_ui(i0,i1);
            if Cancel
                return
            end
        else
            output='';
        end
        if strcmp(output,'print/export')
            I=md_print([]);
            if isequal(I.PrtID,0)
                return
            end
            if strcmp(I.PrtID(max(1,end-4):end),' file')
                ext=lower(strtok(I.PrtID(max(1,end-8):end)));
                [fn,pn]=uiputfile([savedir '*.' ext],'Specify location and base ...');
                if ~ischar(fn)
                    return
                end
                savedir=pn;
                [p,f,e]=fileparts(fn);
                if isempty(e)
                    e=['.' ext];
                end
                n='';
                while length(f)>0 & ismember(f(end),'0123456789')
                    n=[f(end),n];
                    f=f(1:end-1);
                end
                if isempty(n)
                    n=0;
                    ndig=3;
                else
                    ndig=length(n);
                    n=str2num(n);
                end
                I.BaseStr=fullfile(pn,f);
                I.NextNr=n;
                ndigstr=num2str(ndig);
                I.FrmtNr=strcat('%',ndigstr,'.',ndigstr,'i');
                I.ExtStr=e;
            end
        elseif strcmp(output,'avi file')
            ext=strtok(output);
            [fn,pn]=uiputfile([savedir '*.' ext],'Specify output file ...');
            if ~ischar(fn)
                return
            end
            savedir=pn;
            [p,f,e]=fileparts(fn);
            if isempty(e)
                fn=[fn '.avi'];
            end
            drawnow;
            AVIHandle = avi('initialize');
            AVIHandle = avi('open',AVIHandle,[pn fn]);
        elseif ~strcmp(output,'')
            ext=strtok(output);
            [fn,pn]=uiputfile([savedir '*.' ext],'Specify location and base ...');
            if ~ischar(fn)
                return
            end
            savedir=pn;
            [p,f,e]=fileparts(fn);
            n='';
            while length(f)>0 & ismember(f(end),'0123456789')
                n=[f(end),n];
                f=f(1:end-1);
            end
            if isempty(n)
                n=0;
                ndig=3;
            else
                ndig=length(n);
                n=str2num(n);
            end
            if ~isempty(strmatch(lower(e),{'.tif','.tiff','.jpg','.jpeg','.png','.bmp'},'exact'))
                e=[e(2:end) '_'];
            elseif ~isempty(strmatch(lower(ext),{'tif','jpg','png','bmp'},'exact'))
                e=[ext '_'];
            else
                e=ext;
            end
            opsarg={};
            for afgi=1:length(par_fig)
                if length(par_fig)>1
                    %
                    % add some figure identification. Now: A,B,C, ...
                    % thus limited to 26 figures parallel but should be sufficient.
                    %
                    opsarg={'subcase' char(64+afgi)};
                end
                SERIES{afgi}=series_init(fullfile(pn,f),n,'digits',ndig,opsarg{:},e);
            end
        end
        set(sld,'vis','off')
        set(psh,'vis','off')
        if background
            set(par_fig,'vis','off')
            pbfig=progressbar('cancel','closereq');
        else
            pbfig=-1;
        end
        vuim=findall(par_fig,'type','uimenu','visible','on');
        %      vtb=findall(par_fig,'type','uitoolbar','visible','on');
        set(vuim,'vis','off')
        %      set(vtb,'vis','off')
        set(findall(par_fig,'tag','startanim'),'enable','off');
        set(findall(par_fig,'tag','stopanim'),'enable','on');
        %
        mversion = matlabversionnumber;
        for fg = par_fig(:)'
            disable_listeners(fg,mversion)
            set(fg,'keypressfcn','qck_anim stopanimkey')
            enable_listeners(fg,mversion)
        end
        try
            if strcmp(output,'avi file')
                Fig=getframe(par_fig(1));
                %A=avi('options');
                [AVIHandle,OK] = avi('addvideo',AVIHandle,maxfps,Fig.cdata);
                if ~OK
                    error('Cannot add video stream to output file.')
                end
            end
            NSteps=length(ANISteps);
            doloop=1;
            min_seconds_per_frame=1/maxfps;
            user_time_new=now*24*3600;
            while doloop
                if ~animloop
                    doloop=0;
                end
                for i0=1:NSteps,
                    user_time_prev = user_time_new;
                    i=ANISteps(i0);
                    for iobj=1:length(UDh)
                        AnimObj=UDh{iobj};
                        %
                        % Backward compatible with cell version of PlotState ...
                        %
                        if iscell(AnimObj.PlotState)
                            AnimObj.PlotState=plotstatestruct(AnimObj.PlotState);
                        end
                        if iscellstr(AS(1).Values)
                            tval = i;
                        else
                            tval = AS(1).Values(i);
                        end
                        if t_==0
                            AnimObj.PlotState.SubField{1} = tval;
                        else
                            AnimObj.PlotState.Selected{t_} = tval;
                        end
                        [hNew,Error,FileInfo]=qp_plot(AnimObj.PlotState);
                        UDh{iobj}=get(hNew(1),'userdata');
                    end
                    if ~isempty(scriptname)
                        local_eval(scriptname,i0);
                    end
                    drawnow
                    if background
                        H=progressbar(i0/NSteps,pbfig);
                        ish=ishandle(animstop);
                        if (H<0) & any(ish)
                            set(animstop(ish),'userdata',1)
                        end
                    else
                        user_time_new=now*24*3600;
                        user_time_diff = user_time_new-user_time_prev;
                        if user_time_diff<min_seconds_per_frame
                            pause(min_seconds_per_frame-user_time_diff);
                            user_time_new=now*24*3600;
                        end
                    end
                    switch output
                        case ''
                        case 'print/export'
                            if isfield(I,'BaseStr') % to file ...
                                SubCase='';
                                for afgi=1:length(par_fig)
                                    if length(par_fig)>1
                                        SubCase=char(64+afgi);
                                    end
                                    filename=[I.BaseStr sprintf(I.FrmtNr,I.NextNr) SubCase I.ExtStr];
                                    md_print(par_fig(afgi),I,filename);
                                end
                                I.NextNr=I.NextNr+1;
                            else % to printer ...
                                md_print(par_fig,I);
                            end
                        case 'avi file'
                            Fig=getframe(par_fig(1));
                            AVIHandle = avi('addframe',AVIHandle,Fig.cdata);
                        otherwise
                            for afgi=1:length(par_fig)
                                SERIES{afgi}=series_frame(par_fig(afgi),SERIES{afgi});
                            end
                    end
                    ish=ishandle(animstop);
                    stop=get(animstop(ish),'userdata');
                    if iscell(stop)
                        stop=cat(1,stop{:});
                        stop=any(stop);
                    end
                    if any(~ish) | stop
                        doloop=0;
                        break
                    end
                end
            end
            switch output
                case 'avi file'
                    avi('finalize',AVIHandle);
            end
        catch
            ui_message('error',lasterr)
            try
                switch output
                    case 'avi file'
                        avi('finalize',AVIHandle);
                end
            catch
            end
        end
        try
            if length(vuim)>1 & any(ishandle(vuim))
                set(vuim(ishandle(vuim)),'vis','on')
                %         set(vtb,'vis','on')
                par_fig = par_fig(ishandle(par_fig));
                set(findall(par_fig,'tag','startanim'),'enable','on');
                set(findall(par_fig,'tag','stopanim'),'enable','off');
            end
            if ishandle(pbfig)
                delete(pbfig);
            end
            ish=ishandle(par_fig);
            if ish
                for fg = par_fig(ish)'
                    disable_listeners(fg,mversion)
                    set(fg,'keypressfcn','','vis','on')
                    enable_listeners(fg,mversion)
                end
            end
            for i=1:length(sld)
                if ishandle(sld(i))
                    %tt=get(sld(i),'value');
                    %
                    if iscellstr(AS(1).Values)
                        Str=AS(1).Values{t};
                    else
                        Str=sprintf('%i',AS(1).Values(t));
                    end
                    uislider(sld(i),'value',t);
                    set(sld(i),'tooltip',sprintf('%s(%i)=%s',AS(1).Label,t,Str));
                end
            end
            ish=ishandle(sld);
            set(sld(ish),'vis','on')
            ish=ishandle(psh);
            set(psh(ish),'vis','on')
        catch
        end
    case 'slider'
        sld=findobj(afig,'tag','animslid');
        if nargin>2
            t=ANISteps;
        else
            t=getappdata(sld,'currentvalue');
        end
        selected=AnimObj.PlotState.Selected;
        if t_==0
            current_t = AnimObj.PlotState.SubField{1};
        else
            current_t = selected{t_};
            if isnumeric(AS(1).Values)
                current_t = find(AS(1).Values==current_t);
            end
        end
        if t>current_t
            t=min(max(round(t),current_t+1),getappdata(sld,'maxival'));
        elseif t<current_t
            t=max(min(round(t),current_t-1),1);
        end
        %
        sld=findall(par_fig,'tag','animslid');
        uislider(sld,'value',t)
        %
        if iscellstr(AS(1).Values)
            Str=AS(1).Values{t};
        else
            Str=sprintf('%i',AS(1).Values(t));
        end
        set(sld,'tooltip',sprintf('%s(%i)=%s',AS(1).Label,t,Str));
    case 'animselect'
        AnimOpt=get(gcbo,'userdata');
        uicm=findobj(par_fig,'tag','animpushuicontextmenu');
        hChecked=findall(uicm,'checked','on');
        if strcmp(get(gcbo,'checked'),'on')
            if length(hChecked)>1
                animslid     = findobj(afig,'tag','animslid');
                animslid_all = findobj(par_fig,'tag','animslid');
                Ans=questdlg('Unlink object(s)?','','This object','Other objects','None','None');
                AnimTag=get(get(gcbo,'parent'),'userdata');
                for i=1:length(AS)
                    if isequal(AS(i).Tag,AnimTag)
                        break
                    end
                end
                switch Ans
                    case 'This object'
                        set(gcbo,'checked','off');
                        h(strmatch(AS(i).Tag,get(h,'tag')))=[];
                        AS(i)=[];
                        set(animslid_all,'userdata',AS);
                    case 'Other objects'
                        set(hChecked,'checked','off');
                        set(gcbo,'checked','on');
                        AS=AS(i);
                        h=h(strmatch(AS.Tag,get(h,'tag')));
                        set(animslid_all,'userdata',AS);
                    case 'None'
                end
                %
                % get figures in which the items to be animated are located
                %
                par_ax1=get(h,'parent');
                if iscell(par_ax1)
                    par_ax1=unique(cat(1,par_ax1{:}));
                end
                par_fig1=get(par_ax1,'parent');
                if iscell(par_fig1)
                    par_fig1=unique(cat(1,par_fig1{:}));
                end
                %
                notsel_fig = setdiff(par_fig,par_fig1);
                animslid_notsel = findobj(notsel_fig,'tag','animslid');
                set(animslid_notsel,'enable','off','userdata',[])
            end
            return
        end
        set(gcbo,'checked','on')
        AnimTag=get(get(gcbo,'parent'),'userdata');
        %
        animslid     = findobj(afig,'tag','animslid');
        animslid_all = findobj(par_fig,'tag','animslid');
        %
        AnimMax=length(AnimOpt.Values);
        sstep=[min(1/(AnimMax-1),.1) min(10/(AnimMax-1),.9)];
        AS=[];
        AS.Fld=AnimOpt.Dim;
        AS.Tag=AnimTag;
        AS.Label=AnimOpt.Label;
        AS.Values=AnimOpt.Values;
        h=findall(afig,'tag',AS.Tag);
        UDh=get(h,'userdata');
        if iscell(UDh)
            UDh=UDh(~cellfun('isempty',UDh));
            if length(UDh)>1
                error('Problem encountered while searching for object information.');
            end
        else
            UDh={UDh};
        end
        AnimObj=UDh{1};
        %
        % Backward compatible with cell version of PlotState ...
        %
        if iscell(AnimObj.PlotState)
            AnimObj.PlotState=plotstatestruct(AnimObj.PlotState);
        end
        t_=AnimOpt.Dim;
        if t_==0
            t=AnimObj.PlotState.SubField{1};
        else
            t=AnimObj.PlotState.Selected{t_};
            if isnumeric(AS.Values)
                t=find(AS.Values==t);
            end
        end
        %
        NStep=getappdata(animslid,'maxival');
        NoUpdateNec=1;
        ASold=get(animslid,'userdata');
        if isstruct(ASold) && ~isempty(ASold)
            ASoldFld=ASold(1).Fld;
        else
            ASoldFld=0;
        end
        if (NStep==AnimMax) & (AS.Fld==ASoldFld)
            Ans=questdlg('Link with previous object(s)?','','Yes','No','No');
            if strcmp(Ans,'Yes')
                t1=getappdata(animslid,'currentvalue');
                if t~=t1
                    NoUpdateNec=0;
                    t=t1;
                end
                AS=[ASold AS];
            else
                set(hChecked,'checked','off');
            end
        else
            set(hChecked,'checked','off');
        end
        %set(animslid,'userdata',AS,'value',1,'sliderstep',sstep,'Max',AnimMax,'enable','on','value',t)
        uislider(animslid,'value',t,'max',AnimMax)
        set(animslid,'userdata',AS,'enable','on')
        %
        if iscellstr(AS(1).Values)
            Str=AS(1).Values{t};
        else
            Str=sprintf('%i',AS(1).Values(t));
        end
        set(animslid,'tooltip',sprintf('%s(%i)=%s',AS(1).Label,t,Str));
        if NoUpdateNec
            return
        end
    case {'stopanimkey','stopanim'}
        if strcmp(cmd,'stopanimkey')
            if ~isequal(get(afig,'currentcharacter'),8) %Ctrl+H
                return
            end
        end
        animstop=findall(afig,'tag','stopanim');
        set(animstop,'userdata',1)
        return
end

existpar = ishandle(par_fig);
set(par_fig(existpar),'pointer','watch')
for iobj=1:length(UDh)
    AnimObj=UDh{iobj};
    %
    % Backward compatible with cell version of PlotState ...
    %
    if iscell(AnimObj.PlotState)
        AnimObj.PlotState=plotstatestruct(AnimObj.PlotState);
    end
    %
    if iscellstr(AS(1).Values)
        tval = t;
    else
        tval = AS(1).Values(t);
    end
    if t_==0
        AnimObj.PlotState.SubField{1} = tval;
    else
        AnimObj.PlotState.Selected{t_} = tval;
    end
    if ~isempty(findall(0,'tag',AS(iobj).Tag))
        try
            [hNew,Error,FileInfo]=qp_plot(AnimObj.PlotState);
        catch Ex
            qp_error('Catch in qck_anim:',Ex,'qck_anim')
        end
    end
end
set(par_fig(existpar),'pointer','arrow')


function local_eval(scriptname,i)
eval(scriptname,'')


function [ANISteps,output,Cancel,background,animloop,maxfps,scriptname]=local_ui(MinT,MaxT)
Inactive = qp_settings('UIInActiveColor');
Active   = qp_settings('UIActiveColor');

outputtypes={'no output','tif files','jpg files','png files','bmp files','print/export'};
NoBackRend=1;
OptionsFnc={'','','','','',''};
if strncmp(computer,'PC',2) & matlabversionnumber>=6
    outputtypes{end+1}='avi file';
    NoBackRend(1,end+1)=length(outputtypes);
    %TODO: Try to render AVI in background by means of hardcopy
    OptionsFnc{end+1}='avi options';
end
Cancel=1;
background=0;
animloop=0;
maxfps=25;
scriptname='';
uifig = local_draw_ui;

Houtp=findobj(uifig,'tag','animation output');
set(Houtp,'string',outputtypes);

Hrendback=findobj(uifig,'tag','renderback');
Hoptions=findobj(uifig,'tag','options');
Hanimloop=findobj(uifig,'tag','animloop');

Hscr=findobj(uifig,'tag','script','style','edit');
if isstandalone
    Hscripts=findobj(uifig,'tag','script');
    set(Hscripts,'enable','off')
    set(Hscr,'backgroundcolor',Inactive)
end
Hfps=findobj(uifig,'tag','max_fps');
set(Hfps,'string',num2str(maxfps));

Htim=findobj(uifig,'tag','time steps');
ANISteps=MinT:MaxT;
set(Htim,'string',vec2str(ANISteps,'noones','nobrackets'))
Hnoan=findobj(uifig,'tag','noanim list');
Han=findobj(uifig,'tag','anim list');

set(uifig,'userdata',3);
anim=1;
animdone=0;
output='';
while ~animdone,
    if isempty(get(uifig,'userdata')),
        waitfor(uifig,'userdata');
    end;
    if ishandle(uifig),
        cmd=get(uifig,'userdata');
        set(uifig,'userdata',[]);
    else, % figure deleted equivalent with cancel
        % animdone=1; cmd=-1;
        return;
    end;
    
    switch cmd,
        case -1 % cancel
            if ishandle(uifig)
                delete(uifig);
            end;
            return;
        case 0 % continue
            i=get(Han,'userdata');
            output=get(Houtp,'value');
            if ismember(output,NoBackRend)
                background=0;
            else
                background=get(Hrendback,'value');
            end
            if output==1
                output='';
                animloop=get(Hanimloop,'value');
            else
                output=outputtypes{output};
                animloop=0;
            end
            animdone=1;
        case 3 % simsteps
            ANISteps=str2vec(get(Htim,'string'),'range',[MinT MaxT],'applylimit');
            set(Htim,'string',vec2str(ANISteps,'noones','nobrackets'));
        case 4 % output options
            output=get(Houtp,'value');
            if ismember(output,NoBackRend)
                set(Hrendback,'enable','off')
            else
                set(Hrendback,'enable','on')
            end
            if ~isempty(OptionsFnc{output})
                set(Hoptions,'enable','on')
            else
                set(Hoptions,'enable','off')
            end
            if output==1
                set(Hanimloop,'enable','on')
            else
                set(Hanimloop,'enable','off')
            end
        case 5 % max_fps
            newfps=str2num(get(Hfps,'string'));
            if ~isempty(newfps)
                maxfps = newfps(1);
            end
            set(Hfps,'string',num2str(maxfps),'userdata',maxfps);
        case 6 % scriptname
            scriptname=get(Hscr,'string');
        case 7 % options
            output=get(Houtp,'value');
            eval(OptionsFnc{output});
    end
end
if ishandle(uifig), delete(uifig); end;
Cancel=0;


function a=local_draw_ui
Inactive=get(0,'defaultuicontrolbackgroundcolor');
Active=[1 1 1];

ss=qp_getscreen;
figsize=[320 45+6*25];
VOffset=figsize(2)-5;

a=qp_uifigure('Specify Animation Parameters','','QuickPlot animate items',[ss(1:2)+(ss(3:4)-figsize)/2 figsize]);
%
% Output format ...
%
VOffset=VOffset-25;
b = uicontrol('Parent',a, ...
    'BackgroundColor',Inactive, ...
    'Position',[10 VOffset 50 18], ...
    'HorizontalAlignment','left', ...
    'String','Output', ...
    'Style','text', ...
    'Tag','');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[60 VOffset 250 20], ...
    'HorizontalAlignment','right', ...
    'callback','set(gcbf,''userdata'',4)', ...
    'String','Animation Output', ...
    'Style','popupmenu', ...
    'Tag','animation output');
%
% Render in background ...
%
VOffset=VOffset-25;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset 150 18], ...
    'HorizontalAlignment','left', ...
    'String','Render in Background', ...
    'Style','checkbox', ...
    'Enable','off', ...
    'Value',0, ...
    'Tag','renderback');
%
% Render options ...
%
b = uicontrol('Parent',a, ...
    'Position',[210 VOffset 100 20], ...
    'HorizontalAlignment','left', ...
    'callback','set(gcbf,''userdata'',7)', ...
    'String','Options', ...
    'Enable','off', ...
    'Tag','options');
%
% Simulation steps ...
%
VOffset=VOffset-25;
b = uicontrol('Parent',a, ...
    'BackgroundColor',Inactive, ...
    'Position',[10 VOffset 50 18], ...
    'HorizontalAlignment','left', ...
    'String','Steps', ...
    'Style','text', ...
    'Tag','');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[60 VOffset 250 20], ...
    'HorizontalAlignment','left', ...
    'callback','set(gcbf,''userdata'',3)', ...
    'String','1', ...
    'Style','edit', ...
    'Tag','time steps');
%
% Render in background ...
%
VOffset=VOffset-25;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset 250 18], ...
    'HorizontalAlignment','left', ...
    'String','Loop until Stopped', ...
    'Style','checkbox', ...
    'Value',0, ...
    'Tag','animloop');
%
% Maximum frame rate ...
%
VOffset=VOffset-25;
b = uicontrol('Parent',a, ...
    'Position',[60 VOffset 150 18], ...
    'HorizontalAlignment','left', ...
    'String','Maximum Frame Rate (fps)', ...
    'Style','text');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[210 VOffset 100 20], ...
    'HorizontalAlignment','right', ...
    'callback','set(gcbf,''userdata'',5)', ...
    'String','', ...
    'Style','edit', ...
    'Tag','max_fps');
%
% Script name ...
%
VOffset=VOffset-25;
b = uicontrol('Parent',a, ...
    'BackgroundColor',Inactive, ...
    'Position',[10 VOffset 50 18], ...
    'HorizontalAlignment','left', ...
    'String','Script', ...
    'Style','text', ...
    'Tag','script');

b = uicontrol('Parent',a, ...
    'BackgroundColor',Active, ...
    'Position',[60 VOffset 250 20], ...
    'HorizontalAlignment','left', ...
    'callback','set(gcbf,''userdata'',6)', ...
    'String','', ...
    'Style','edit', ...
    'Tag','script');
%
% Cancel or continue  ...
%
b = uicontrol('Parent',a, ...
    'callback','set(gcbf,''userdata'',0)', ...
    'Position',[165 10 145 20], ...
    'String','Continue', ...
    'Tag','continue');
b = uicontrol('Parent',a, ...
    'callback','set(gcbf,''userdata'',-1)', ...
    'Position',[10 10 145 20], ...
    'String','Cancel', ...
    'Tag','cancel');
%
% Show figure ...
%
set(a,'visible','on')

function disable_listeners(fg,mversion)
%Disable listeners
if mversion>=8.04
    mmgr = uigetmodemanager(fg);
    [mmgr.WindowListenerHandles(:).Enabled] = deal(0);
elseif mversion>=7.02
    mmgr = uigetmodemanager(fg);
    set(mmgr.WindowListenerHandles,'Enable','off');
end

function enable_listeners(fg,mversion)
%Enable listeners
if mversion>=8.04
    mmgr = uigetmodemanager(fg);
    [mmgr.WindowListenerHandles(:).Enabled] = deal(1);
elseif mversion>=7.02
    mmgr = uigetmodemanager(fg);
    set(mmgr.WindowListenerHandles,'Enable','on');
end
