unit Frame_LccNodeEditorControl;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, Frame_LccNodeEditorGroup, Frame_LccNodeEditor, FMX.Objects;

type
  TFrameNodeEditorControl = class(TFrame)
    FramedVertScrollBox: TFramedVertScrollBox;
    procedure FramedVertScrollBoxResized(Sender: TObject);
  private
    FRootGroups: TList;
    FUniqueEditCounter: UInt64;
    FRootGroup: TFrameLccNodeEditorGroup;
    FLockCount: Integer;
    { Private declarations }
  protected
    property RootGroups: TList read FRootGroups write FRootGroups;
    property UniqueEditCounter: UInt64 read FUniqueEditCounter write FUniqueEditCounter;
  public
    { Public declarations }
    property LockCount: Integer read FLockCount write FLockCount;
    property RootGroup: TFrameLccNodeEditorGroup read FRootGroup write FRootGroup;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Clear;

    procedure Lock;
    procedure UnLock;
    function GenerateUniqueEditCounter: Integer;
    procedure Rebuild;
    procedure ResizeGroupsAndEditorsWidths;
  end;

implementation

{$R *.fmx}

{ TFrame1 }

procedure TFrameNodeEditorControl.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to RootGroups.Count - 1 do
      TObject(RootGroups[i]).Free;
  finally
    RootGroups.Clear;
  end;
end;

constructor TFrameNodeEditorControl.Create(AOwner: TComponent);
begin
  // The Create call will call events like Resize.. crazy but true so make sure things are ready for that
  inherited;

  RootGroups := TList.Create;

  RootGroup := TFrameLccNodeEditorGroup.Create(Self);
  RootGroup.Name := 'GroupRoot' + IntToStr(GenerateUniqueEditCounter);
  RootGroup.LabelName.Text := 'OpenLCB';
  RootGroup.LabelDescription.Text := 'CDI Editor';
  RootGroup.Parent := FramedVertScrollBox;
  RootGroup.OwnerControl := Self;
  RootGroup.Collapsed := False;
  RootGroup.Visible := True;
end;

destructor TFrameNodeEditorControl.Destroy;
begin
  Clear;
  FreeAndNil(FRootGroups);
  RootGroup.Clear;
  FreeAndNil(FRootGroup);
  inherited;
end;

procedure TFrameNodeEditorControl.FramedVertScrollBoxResized(Sender: TObject);
begin
  if LockCount <> 0 then Exit;
  ResizeGroupsAndEditorsWidths;
end;

function TFrameNodeEditorControl.GenerateUniqueEditCounter: Integer;
begin
  Result := FUniqueEditCounter;
  Inc(FUniqueEditCounter);
end;

procedure TFrameNodeEditorControl.Lock;
begin
  Inc(FLockCount);
end;

procedure TFrameNodeEditorControl.Rebuild;
begin
  if LockCount <> 0 then Exit;

  ResizeGroupsAndEditorsWidths;
  RootGroup.ReCalculateGroupLayout;
end;

procedure TFrameNodeEditorControl.ResizeGroupsAndEditorsWidths;
begin
  if LockCount <> 0 then Exit;

  if Assigned(RootGroup) then
  begin
    RootGroup.Width := FramedVertScrollBox.ClientWidth;
    RootGroup.ReCalculateChildWidths(FramedVertScrollBox.ClientWidth)
  end;
end;

procedure TFrameNodeEditorControl.UnLock;
begin
  Dec(FLockCount);
  if LockCount < 0 then
    LockCount := 0;
  if LockCount = 0 then
    Rebuild;
end;

end.
