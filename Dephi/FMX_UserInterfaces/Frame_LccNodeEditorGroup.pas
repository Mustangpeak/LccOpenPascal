unit Frame_LccNodeEditorGroup;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Controls.Presentation, FMX.Layouts, Frame_LccNodeEditor;

type
  TFrameLccNodeEditorGroup = class(TFrame)
    LayoutGroup: TLayout;
    ToolBarGroup: TToolBar;
    LayoutGroupHeaderText: TLayout;
    LabelGroupName: TLabel;
    LabelGroupDescription: TLabel;
    SpeedButtonGroupExpand: TSpeedButton;
    LayoutGroupContents: TLayout;
    FloatAnimationGroupExpand: TFloatAnimation;
    procedure SpeedButtonGroupExpandClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure FloatAnimationGroupExpandFinish(Sender: TObject);
    procedure LabelGroupNameClick(Sender: TObject);
  private
    FUniqueEditCounter: UInt64;
    function GetEditorCount: Integer;
    { Private declarations }
  protected
     property UniqueEditCounter: UInt64 read FUniqueEditCounter write FUniqueEditCounter;
     function GenerateUniqueEditCounter: Integer;

     procedure AddEditorCommon(AnEditor: TFrameLccNodeEditor; AName, ADescription: String);
  public
    { Public declarations }
    property EditorCount: Integer read GetEditorCount;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
    function AddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
    function AddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;

    function LastEditorTop: single;
    function LastEditorBottom: single;
    procedure ReSizeEditorsWidth;
  end;

implementation

{$R *.fmx}

function TFrameLccNodeEditorGroup.AddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
begin
  LayoutGroup.BeginUpdate;
  try
    Result := TFrameLccNodeEditor.Create(Self);
    Result.EnableComboBox;
    Result.LoadComboBox(AStringList, AnItemIndex);
    AddEditorCommon(Result, AName, ADescription);
  finally
    LayoutGroup.EndUpdate;
  end;
end;

function TFrameLccNodeEditorGroup.AddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
begin
  LayoutGroup.BeginUpdate;
  try
    Result := TFrameLccNodeEditor.Create(Self);
    Result.EnableEditBox;
    Result.LoadEditBox(AString);
    AddEditorCommon(Result, AName, ADescription)
  finally
    LayoutGroup.EndUpdate;
  end;
end;

procedure TFrameLccNodeEditorGroup.AddEditorCommon(AnEditor: TFrameLccNodeEditor; AName, ADescription: String);
begin

  // This won't stick... once parent is set the FMX frame work force it to be a few
  // pixels shorter than the LayoutEditor in it
  // Use Editor.RealHeight to get the correct value
  // Editor.Height := Editor.LayoutEditor.Height;

  // Manually setup the position in the Group... faster and less frustrating than alTop annoyances
  AnEditor.Position.x := 0;
  AnEditor.Position.y := LastEditorTop;
  AnEditor.Width := Width;
  AnEditor.Name := 'FrameEditor' + IntToStr(GenerateUniqueEditCounter);
  if AName <> '' then
    AnEditor.LabelName.Text := AName
  else
    AnEditor.LabelName.Visible := False;
  if ADescription <> '' then
    AnEditor.LabelDescription.Text := ADescription
  else
    AnEditor.LabelDescription.Visible := False;
  AnEditor.Parent := LayoutGroupContents;
  LayoutGroup.Height := ToolBarGroup.Height + AnEditor.Position.y + AnEditor.RealHeight;
  Self.Height := AnEditor.Position.y + AnEditor.RealHeight + ToolBarGroup.Height;
  LayoutGroupContents.Height := AnEditor.Position.y + AnEditor.RealHeight;
end;

function TFrameLccNodeEditorGroup.AddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;
begin
  LayoutGroup.BeginUpdate;
  try
    Result := TFrameLccNodeEditor.Create(Self);
    Result.EnableSpinBox;
    Result.LoadSpinEdit(AValue, AMin, AMax);
    AddEditorCommon(Result, AName, ADescription)
  finally
    LayoutGroup.EndUpdate;
  end;
end;

constructor TFrameLccNodeEditorGroup.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TFrameLccNodeEditorGroup.Destroy;
begin
  inherited;
end;

procedure TFrameLccNodeEditorGroup.FloatAnimationGroupExpandFinish(Sender: TObject);
begin
  FloatAnimationGroupExpand.Enabled := False;
end;

procedure TFrameLccNodeEditorGroup.FrameResize(Sender: TObject);
begin
  ReSizeEditorsWidth;
end;

function TFrameLccNodeEditorGroup.GenerateUniqueEditCounter: Integer;
begin
  Result := UniqueEditCounter;
  Inc(FUniqueEditCounter);
end;

function TFrameLccNodeEditorGroup.GetEditorCount: Integer;
begin
  Result := LayoutGroupContents.Controls.Count
end;

procedure TFrameLccNodeEditorGroup.LabelGroupNameClick(Sender: TObject);
begin

beep
end;

function TFrameLccNodeEditorGroup.LastEditorBottom: single;
begin
  Result := LastEditorTop + (LayoutGroupContents.Controls[EditorCount - 1] as TFrameLccNodeEditor).RealHeight;
end;

function TFrameLccNodeEditorGroup.LastEditorTop: single;
var
  i: Integer;
begin
  Result := 0;
  for i := 0 to LayoutGroupContents.Controls.Count - 1 do
    Result := Result + (LayoutGroupContents.Controls[i] as TFrameLccNodeEditor).RealHeight;
end;

procedure TFrameLccNodeEditorGroup.ReSizeEditorsWidth;
var
  i: Integer;
begin
  // Resize called before
  if Assigned(LayoutGroupContents) then
  begin
    for i := 0 to LayoutGroupContents.Controls.Count - 1 do
      (LayoutGroupContents.Controls[i] as TFrameLccNodeEditor).Width := Width;
  end;

end;

procedure TFrameLccNodeEditorGroup.SpeedButtonGroupExpandClick(Sender: TObject);
begin
  if SpeedButtonGroupExpand.StyleLookup = 'arrowdowntoolbutton' then
  begin
    SpeedButtonGroupExpand.StyleLookup := 'arrowrighttoolbutton';
    FloatAnimationGroupExpand.Inverse := True;
    FloatAnimationGroupExpand.StartValue := ToolBarGroup.Height;
    FloatAnimationGroupExpand.StopValue := Height;
    FloatAnimationGroupExpand.AnimationType := TAnimationType.In;
    FloatAnimationGroupExpand.Enabled := True;
    FloatAnimationGroupExpand.Start;
  end else
  begin
    SpeedButtonGroupExpand.StyleLookup := 'arrowdowntoolbutton';
    FloatAnimationGroupExpand.Inverse := False;
    FloatAnimationGroupExpand.StartValue := Height;
    FloatAnimationGroupExpand.AnimationType := TAnimationType.Out;
    FloatAnimationGroupExpand.Enabled := True;
    FloatAnimationGroupExpand.Start;
  end;
end;

end.
