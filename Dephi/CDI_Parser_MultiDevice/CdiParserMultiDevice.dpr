program CdiParserMultiDevice;

uses
  System.StartUpCopy,
  FMX.Forms,
  Cdi_Parser in 'Cdi_Parser.pas' {FormCdiParser},
  unit_custom_fmx_listview in 'unit_custom_fmx_listview.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCdiParser, FormCdiParser);
  Application.Run;
end.
