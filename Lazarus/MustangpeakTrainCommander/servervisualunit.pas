unit servervisualunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, lcc_alias_server
, lcc_utilities;

type

  { TFormServerInfo }

  TFormServerInfo = class(TForm)
    ButtonAliasMappingRefresh: TButton;
    MemoMappings: TMemo;
    PanelAliasMappingMain: TPanel;
    PanelAliasMappingHeader: TPanel;
    procedure ButtonAliasMappingRefreshClick(Sender: TObject);
  private

  public

  end;

var
  FormServerInfo: TFormServerInfo;

implementation

{$R *.lfm}

{ TFormServerInfo }

procedure TFormServerInfo.ButtonAliasMappingRefreshClick(Sender: TObject);
var
  StringList: TStringList;
begin
  MemoMappings.Lines.BeginUpdate;
  try
    StringList := TStringList.Create;
    MemoMappings.Lines.Clear;

    AliasServer.ReadMappingsToStringList(StringList);
    MemoMappings.Lines.Assign(StringList);

  finally
    StringList.Free;
    MemoMappings.Lines.EndUpdate;
  end;
end;


end.

