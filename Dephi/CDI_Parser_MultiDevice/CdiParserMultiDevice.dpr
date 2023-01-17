program CdiParserMultiDevice;

uses
  System.StartUpCopy,
  FMX.Forms,
  Cdi_Parser in 'Cdi_Parser.pas' {FormCdiParser},
  lcc_cdi_xml_reader in 'lcc_cdi_xml_reader.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormCdiParser, FormCdiParser);
  Application.Run;
end.
