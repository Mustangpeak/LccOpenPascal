program TrainCommander;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Interfaces, // this includes the LCL widgetset
  Forms, memdslaz, TrainCommanderUnit, servervisualunit, 
lcc_alias_server_thread;

{$R *.res}

begin
  RequireDerivedFormResource := True;
  Application.Scaled:=True;
  Application.Initialize;
  Application.CreateForm(TFormTrainCommander, FormTrainCommander);
  Application.CreateForm(TFormServerInfo, FormServerInfo);
  Application.Run;
end.

