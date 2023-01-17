unit Cdi_Parser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation,
  lcc_cdi_xml_reader, FMX.Layouts, FMX.TreeView;

type
  TFormCdiParser = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    TreeView1: TTreeView;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  private
    { Private declarations }
  public
    { Public declarations }
    CDI: TLccCdiRoot;
    procedure BuildTreeview(Element: TLccCidCore; Node: TTreeViewItem);
  end;

var
  FormCdiParser: TFormCdiParser;

implementation

{$R *.fmx}

procedure TFormCdiParser.BuildTreeview(Element: TLccCidCore; Node: TTreeViewItem);
var
  LocalTreeItem, MapTreeItem, MapRelationTreeItem: TTreeViewItem;
  LocalElement: TLccCidCore;
  LocalElementCore: TLccCdiElementCore;
  LocalMapRelations: TLccCdiMapRelation;
  i, j: Integer;
begin

  for i := 0 to Element.ChildElementCount - 1 do
  begin
    LocalElement := Element.ChildElement[i];
    LocalTreeItem := TTreeViewItem.Create(Treeview1);
    LocalTreeItem.Text := LocalElement.ClassName;
    LocalTreeItem.Parent := Node;

    BuildTreeview(LocalElement, LocalTreeItem);

    if LocalElement is TLccCdiElementCore then
    begin
      LocalElementCore := LocalElement as TLccCdiElementCore;
      for j := 0 to LocalElementCore.MapRelations.Count - 1 do
      begin
        LocalMapRelations := LocalElementCore.MapRelations[j];
        MapRelationTreeItem := TTreeViewItem.Create(Treeview1);
        MapRelationTreeItem.Text := LocalMapRelations.ClassName;
        MapRelationTreeItem.Parent := LocalTreeItem;
      end;
    end;
  end;
end;

procedure TFormCdiParser.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FreeAndNil(CDI);
end;

procedure TFormCdiParser.SpeedButton1Click(Sender: TObject);
var
  TreeItem: TTreeViewItem;
begin
  FreeAndNil(CDI);

  if OpenDialog1.Execute then
  begin
    CDI := TLccCdiRoot.Create;
    CDI.LoadFromFile(OpenDialog1.FileName);
    CDI.BuildTree;


    Treeview1.Clear;

    TreeItem := TTreeViewItem.Create(Treeview1);
    TreeItem.Text := CDI.ClassName;
    TreeItem.Parent := Treeview1;
    BuildTreeview(CDI, TreeItem);
  end;

end;

end.
