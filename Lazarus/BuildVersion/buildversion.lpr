program buildversion;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp
  { you can add units after this };

type

  { TBuildVersion }

  TBuildVersion = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TBuildVersion }

procedure TBuildVersion.DoRun;
var
  ErrorMsg: String;
  Year,Month,Day, Hour, Minute, Second, MilliSecond : Word;
  VersionFile: TextFile;
  VersionFileName: string;

begin

    ErrorMsg := CheckOptions('hf:', 'help file:');

    if ErrorMsg <> '' then begin
      ShowException(Exception.Create(ErrorMsg));
      Terminate;
      Exit;
    end;

    // parse parameters
    if HasOption('h', 'help') then
    begin
      WriteHelp;
      Terminate;
      Exit;
    end;

    if HasOption('f', 'file') then
    begin
      WriteLn('file: ' + getOptionValue('f', 'file'));
      VersionFileName := getOptionValue('f', 'file');
    end else
    begin
      WriteLn('A filename must be supplied -f {path+filename}');
      Terminate;
      Exit;
    end;

  DecodeTime(Now(), Hour, Minute, Second, MilliSecond);
  DecodeDate(Date(), Year, Month, Day);

  WriteLn('FileName: ' + VersionFileName);

  if FileExists(VersionFileName) then
  begin
    WriteLn('Found file: ' + VersionFileName);
    DeleteFile(VersionFileName);

    if FileExists(VersionFileName) then
      WriteLn('File Not Deleted')
    else
      WriteLn('File Deleted');
  end;

  AssignFile(VersionFile, VersionFileName);
  try
    try
      rewrite(VersionFile);

      WriteLn('File Created: ' + VersionFileName);
      WriteLn('Generating version "h" file: ' + Format('%.04d%.02d%.02d.%.02d%.02d%.02d', [Year, Month, Day, Hour, Minute, Second]));

      WriteLn(VersionFile, '/** \copyright');
      WriteLn(VersionFile, '* Copyright (c) ' + Format('%.04d%', [Year]) +', Jim Kueneman');
      WriteLn(VersionFile, '* All rights reserved.');
      WriteLn(VersionFile, '* ');
      WriteLn(VersionFile, '* Redistribution and use in source and binary forms, with or without ');
      WriteLn(VersionFile, '* modification, are permitted provided that the following conditions are met:');
      WriteLn(VersionFile, '*');
      WriteLn(VersionFile, '*  - Redistributions of source code must retain the above copyright notice,');
      WriteLn(VersionFile, '*    this list of conditions and the following disclaimer.');
      WriteLn(VersionFile, '* ');
      WriteLn(VersionFile, '*  - Redistributions in binary form must reproduce the above copyright notice,');
      WriteLn(VersionFile, '*    this list of conditions and the following disclaimer in the documentation');
      WriteLn(VersionFile, '*    and/or other materials provided with the distribution.');
      WriteLn(VersionFile, '*');
      WriteLn(VersionFile, '* THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"');
      WriteLn(VersionFile, '* AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE');
      WriteLn(VersionFile, '* IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE');
      WriteLn(VersionFile, '* ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE');
      WriteLn(VersionFile, '* LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR');
      WriteLn(VersionFile, '* CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF');
      WriteLn(VersionFile, '* SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS');
      WriteLn(VersionFile, '* INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN');
      WriteLn(VersionFile, '* CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)');
      WriteLn(VersionFile, '* ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE');
      WriteLn(VersionFile, '* POSSIBILITY OF SUCH DAMAGE.');
      WriteLn(VersionFile, '*');
      WriteLn(VersionFile, '* \file turnoutboss_version.h');
      WriteLn(VersionFile, '*');
      WriteLn(VersionFile, '*');
      WriteLn(VersionFile, '* @author Jim Kueneman');
      WriteLn(VersionFile, '* @date ' + Format('%.02d', [Month]) + ' ' + Format('%.02d', [Day]) + ' ' + Format('%.04d', [Year]));
      WriteLn(VersionFile, '*/');
      WriteLn(VersionFile, ' ');
      WriteLn(VersionFile, '// This is a guard condition so that contents of this file are not included');
      WriteLn(VersionFile, '// more than once.');
      WriteLn(VersionFile, '#ifndef __TURNOUTBOSS_VERSION__');
      WriteLn(VersionFile, '#define	__TURNOUTBOSS_VERSION__');
      WriteLn(VersionFile, ' ');
      WriteLn(VersionFile, '#define TURNOUTBOSS_VERSION "' + Format('%.04d%.02d%.02d.%.02d%.02d%.02d', [Year, Month, Day, Hour, Minute, Second]) + '"');
      WriteLn(VersionFile, ' ');
      WriteLn(VersionFile, '#ifdef	__cplusplus');
      WriteLn(VersionFile, 'extern "C" {');
      WriteLn(VersionFile, '#endif /* __cplusplus */');
      WriteLn(VersionFile, '');
      WriteLn(VersionFile, '    // TODO If C++ is being used, regular C code needs function names to have C ');
      WriteLn(VersionFile, '    // linkage so the functions can be used by the c code.');
      WriteLn(VersionFile, ' ');
      WriteLn(VersionFile, '#ifdef	__cplusplus');
      WriteLn(VersionFile, '}');
      WriteLn(VersionFile, '#endif /* __cplusplus */');
      WriteLn(VersionFile, ' ');
      WriteLn(VersionFile, '#endif	/* __TURNOUTBOSS_VERSION__ */');

    finally
      CloseFile(VersionFile);
    end;

  except

    WriteLn('Failed');
  end;

  // stop program loop
  Terminate;
end;

constructor TBuildVersion.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TBuildVersion.Destroy;
begin
  inherited Destroy;
end;

procedure TBuildVersion.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
  writeln('-f {filename}');
end;

var
  Application: TBuildVersion;
begin
  Application:=TBuildVersion.Create(nil);
  Application.Title:='BuildVersion App';
  Application.Run;
  Application.Free;
end.

