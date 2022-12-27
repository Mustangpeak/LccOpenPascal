program LccThrottleApp;

uses
  System.StartUpCopy,
  FMX.Forms,
  Skia.FMX,
  FormLccThrottleApp in 'FormLccThrottleApp.pas' {LccThrottleAppForm};

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TLccThrottleAppForm, LccThrottleAppForm);
  Application.Run;
end.
