program FMX_UserInterfaces;

uses
  System.StartUpCopy,
  FMX.Forms,
  Skia.FMX,
  FMX_UserInterfaceApp in 'FMX_UserInterfaceApp.pas' {FMX_UserInterfaceForm},
  Frame_LccNodeEditor in 'Frame_LccNodeEditor.pas' {FrameLccNodeEditor: TFrame},
  Frame_LccNodeEditorGroup in 'Frame_LccNodeEditorGroup.pas' {FrameLccNodeEditorGroup: TFrame};

{$R *.res}

begin
  GlobalUseSkia := True;
  Application.Initialize;
  Application.CreateForm(TFMX_UserInterfaceForm, FMX_UserInterfaceForm);
  Application.Run;
end.
