unit Cdi_Parser;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation,
  lcc_cdi_xml_reader,
  lcc_defines,
  FMX.Layouts, FMX.TreeView, Data.Bind.GenData, Fmx.Bind.GenData,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  Data.Bind.EngExt, Fmx.Bind.DBEngExt, System.Rtti, System.Bindings.Outputs,
  Fmx.Bind.Editors, Data.Bind.Components, FMX.ListView, Data.Bind.ObjectScope,
  FMX.Edit, FMX.ListBox, FMX.TabControl, FMX.TextLayout, System.ImageList,
  FMX.ImgList, FMX.Objects, unit_custom_fmx_listview, System.Actions,
  FMX.ActnList;

type
  TFormCdiParser = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    SpeedButton1: TSpeedButton;
    OpenDialog1: TOpenDialog;
    SpeedButton2: TSpeedButton;
    ImageList1: TImageList;
    ActionList1: TActionList;
    TabControlCdiView: TTabControl;
    PreviousTabAction1: TPreviousTabAction;
    NextTabAction1: TNextTabAction;
    procedure SpeedButton1Click(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  protected
    procedure OnListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
  public
    { Public declarations }
    CDI: TLccCdiRoot;

    procedure BuildUI;
    function AddTabAndListview: TCustomLayoutListview;
  end;

var
  FormCdiParser: TFormCdiParser;

implementation

{$R *.fmx}

function TFormCdiParser.AddTabAndListview: TCustomLayoutListview;
var
  Tab: TTabItem;
begin
  Tab := TabControlCdiView.Add;
  Tab.Text := 'Tab ' + IntToStr( TabControlCdiView.TabCount);
  Result := TCustomLayoutListview.Create(Tab);
  Result.Align := TAlignLayout.Client;
  Result.Parent := Tab;
  Result.Images := ImageList1;
  Result.OnItemClick := OnListViewItemClick;
end;

procedure TFormCdiParser.BuildUI;
var
  i: Integer;
  ChildCdiNode: TLccCidCore;
  ListviewItem: TListviewItem;
  LocalListview: TCustomLayoutListview;

begin

  for i := TabControlCdiView.TabCount - 1 downto 0 do
    TabControlCdiView.Delete(i);

  LocalListview := AddTabAndListview;

  LocalListview.Items.BeginUpdate;
  try

     // Show the Segment Name/Description Somewhere
    for i := 0 to CDI.ChildElementCount - 1 do
    begin
      ChildCdiNode := CDI.ChildElement[i];
      if ChildCdiNode is TLccCdiSegment then
      begin
        ListviewItem := LocalListview.Items.Add;
        ListviewItem.Objects.ImageObject.ImageIndex := 0;
        ListviewItem.Objects.TextObject.Text := ChildCdiNode.Name;
        ListviewItem.Objects.DetailObject.Text := ChildCdiNode.Description;
        ListviewItem.TagObject := ChildCdiNode;

     //   RunNode(CDI.ChildElement[i], 1);
      end else
      if ChildCdiNode is TLccCdiAcdi then
      begin
      end else
      if ChildCdiNode is TLccCdiIdentification then
      begin
      end;
    end;
  finally
    LocalListview.Items.EndUpdate;
    // Hack to make it resize
    LocalListview.Align := TAlignLayout.None;
    LocalListview.Width := LocalListview.Width - 1;
    LocalListview.Align := TAlignLayout.Client;
  end;
end;

procedure TFormCdiParser.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
begin
  FreeAndNil(CDI);
end;


procedure TFormCdiParser.OnListViewItemClick(const Sender: TObject; const AItem: TListViewItem);
var
  Segment: TLccCdiSegment;
  Group: TLccCdiGroup;
  i, iReplication: Integer;
  LocalListview: TCustomLayoutListview;
  LocalListviewItem: TListviewItem;
  ChildNode: TLccCidCore;
  ReplicationCount: Integer;
  ReplicationName: string;
begin
  if AItem.TagObject is TLccCdiSegment then
  begin
    Segment := AItem.TagObject as TLccCdiSegment;   // this works for a Segment and Group as it is a decendant of Segment
    LocalListview := AddTabAndListview;

    if Segment is TLccCdiGroup then
    begin
      Group := Segment as TLccCdiGroup;
      ReplicationCount := Group.ReplicationCount;
      ReplicationName := Group.RepeatName;
    end else
    begin
      ReplicationCount := 1;
      ReplicationName := '';
    end;

    for iReplication := 0 to ReplicationCount - 1 do
    begin
      if ReplicationCount > 1 then
      begin
        LocalListviewItem := LocalListview.Items.Add;
        LocalListviewItem.Purpose := TListItemPurpose.Header;
        LocalListviewItem.Text := ReplicationName +  ' ' + IntToStr(iReplication + 1);
  //      LocalListviewItem.Objects.TextObject.Text := ReplicationName;
      end;

      for i := 0 to Segment.ChildElementCount - 1 do
      begin
        ChildNode := Segment.ChildElement[i];
        LocalListviewItem := LocalListview.Items.Add;
        LocalListviewItem.Objects.ImageObject.ImageIndex := 0;
        LocalListviewItem.Objects.TextObject.Text := ChildNode.Name;
        LocalListviewItem.Objects.DetailObject.Text := ChildNode.Description;
        LocalListviewItem.TagObject := ChildNode;
      end;
    end;
    NextTabAction1.Execute;
  end;
end;

procedure TFormCdiParser.SpeedButton1Click(Sender: TObject);
begin
  if TabControlCdiView.TabCount > 1 then
  begin
    PreviousTabAction1.Execute;
    TabControlCdiView.Delete(TabControlCdiView.TabCount - 1)
  end;
end;

procedure TFormCdiParser.SpeedButton2Click(Sender: TObject);
begin
  FreeAndNil(CDI);

  if OpenDialog1.Execute then
  begin
    CDI := TLccCdiRoot.Create(nil);
    CDI.LoadFromFile(OpenDialog1.FileName);
    CDI.SetTargetNode(NULL_NODE_ID, 0);
    CDI.BuildTree(False);

    BuildUI;
  end;

end;

end.
