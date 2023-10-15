unit servervisualunit;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, ExtCtrls,
  StdCtrls, lcc_alias_server, lcc_train_server, lcc_utilities;

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

    procedure AddTrainObject(ATrain: TLccTractionObject);
    procedure RemoveTrainObject(ATrain: TLccTractionObject);
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


procedure TFormServerInfo.AddTrainObject(ATrain: TLccTractionObject);
var
  TreeNode: TTreeNode;
  CaptionStr: String;
begin
 { TreeViewTrains.Items.BeginUpdate;
  try
    CaptionStr := NodeIDToString(ATrain.NodeID, False) + '; ' + NodeAliasToString(ATrain.NodeAlias);

    TreeNode := TreeViewTrains.Items.FindNodeWithText( CaptionStr);
    if not Assigned(TreeNode) then
    begin
      TreeNode := TreeViewTrains.Items.Add(nil, CaptionStr);
     end;
  finally
    TreeViewTrains.Items.EndUpdate;
  end;  }
end;

procedure TFormServerInfo.RemoveTrainObject(ATrain: TLccTractionObject);
var
  TreeNode: TTreeNode;
  CaptionStr: String;
begin
 { TreeViewTrains.Items.BeginUpdate;
  try
    CaptionStr := NodeIDToString(ATrain.NodeID, False) + '; ' + NodeAliasToString(ATrain.NodeAlias);

    TreeNode := TreeViewTrains.Items.FindNodeWithText( CaptionStr);
    if Assigned(TreeNode) then
      TreeViewTrains.Items.Delete(TreeNode);
  finally
    TreeViewTrains.Items.EndUpdate;
  end;
       }
end;

end.

