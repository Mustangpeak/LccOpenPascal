unit Frame_LccNodeEditorControl;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, Frame_LccNodeEditorGroup, Frame_LccNodeEditor;

type
  TFrameNodeEditorControl = class(TFrame)
    FramedVertScrollBox: TFramedVertScrollBox;
    procedure FramedVertScrollBoxResize(Sender: TObject);
  private
    FUniqueEditCounter: UInt64;
    FGroupList: TList;
    function GetGroupCount: Integer;
    { Private declarations }
  protected
    property UniqueEditCounter: UInt64 read FUniqueEditCounter write FUniqueEditCounter;
    property GroupList: TList read FGroupList write FGroupList;

    function GenerateUniqueEditCounter: Integer;
    procedure LinkToLastGroup(AGroup: TFrameLccNodeEditorGroup);
  public
    { Public declarations }
    property GroupCount: Integer read GetGroupCount;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddGroup(AName, ADescription: String; IsCollapsed: Boolean): TFrameLccNodeEditorGroup;
    procedure Clear;
    function LastGroupTop: single;
    function LastGroupBottom: single;
    function LastGroup: TFrameLccNodeEditorGroup;
    function FirstGroup: TFrameLccNodeEditorGroup;
  end;

implementation

{$R *.fmx}

{ TFrame1 }

function TFrameNodeEditorControl.AddGroup(AName, ADescription: String; IsCollapsed: Boolean): TFrameLccNodeEditorGroup;
begin
  Result := TFrameLccNodeEditorGroup.Create(FramedVertScrollBox);
  Result.Collapsed := IsCollapsed;
  Result.Parent := FramedVertScrollBox;

  Result.Name := 'GroupEditor' + IntToStr(GenerateUniqueEditCounter);;
  if AName <> '' then
    Result.LabelName.Text := AName
  else
    Result.LabelName.Visible := False;
  if ADescription <> '' then
    Result.LabelDescription.Text := ADescription
  else
    Result.LabelDescription.Visible := False;

  Result.Position.x := 0;
  Result.Position.y := LastGroupBottom;

  LinkToLastGroup(Result);

  // Now officially in the list
  GroupList.Add(Result);
end;

procedure TFrameNodeEditorControl.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to GroupCount - 1 do
      TObject( GroupList[i]).Free;
  finally
    GroupList.Clear;
  end;
end;

constructor TFrameNodeEditorControl.Create(AOwner: TComponent);
begin
  // The Create call will call events like Resize.. crazy but true so make sure things are ready for that
  FGroupList := TList.Create;
  inherited;
end;

destructor TFrameNodeEditorControl.Destroy;
begin
  Clear;
  GroupList.Free;
  inherited;
end;

function TFrameNodeEditorControl.FirstGroup: TFrameLccNodeEditorGroup;
begin
  Result := nil;
  if GroupList.Count > 0 then
    Result := TFrameLccNodeEditorGroup( GroupList[0]);
end;

procedure TFrameNodeEditorControl.FramedVertScrollBoxResize(Sender: TObject);
var
  i: Integer;
begin
  // The Groups are not Aligned since you can't do that in a ScrollBox so we need to
  // readjust the widths manually
  for i := GroupList.Count - 1 downto 0 do
     TFrameLccNodeEditorGroup( GroupList[i]).Width := FramedVertScrollBox.ClientWidth;
end;

function TFrameNodeEditorControl.GenerateUniqueEditCounter: Integer;
begin
  Result := FUniqueEditCounter;
  Inc(FUniqueEditCounter);
end;

function TFrameNodeEditorControl.GetGroupCount: Integer;
begin
  Result := GroupList.Count;
end;

function TFrameNodeEditorControl.LastGroup: TFrameLccNodeEditorGroup;
begin
   Result := nil;
  if GroupList.Count > 0 then
    Result := TFrameLccNodeEditorGroup( GroupList[GroupList.Count -1]);
end;

function TFrameNodeEditorControl.LastGroupBottom: single;
begin
  Result := 0;
  if GroupList.Count > 0 then
    Result := TFrameLccNodeEditorGroup( GroupList[GroupList.Count - 1]).Position.y +
      TFrameLccNodeEditorGroup( GroupList[GroupList.Count - 1]).Height;
end;

function TFrameNodeEditorControl.LastGroupTop: single;
begin
  Result := 0;
  if GroupList.Count > 0 then
    Result := TFrameLccNodeEditorGroup( GroupList[GroupList.Count - 1]).Position.y;
end;

procedure TFrameNodeEditorControl.LinkToLastGroup(AGroup: TFrameLccNodeEditorGroup);
begin
  if GroupList.Count > 0 then
  begin
    TFrameLccNodeEditorGroup( GroupList[GroupList.Count - 1]).NextGroup := AGroup;
    AGroup.PrevGroup :=  TFrameLccNodeEditorGroup( GroupList[GroupList.Count - 1]);
  end;
end;

end.
