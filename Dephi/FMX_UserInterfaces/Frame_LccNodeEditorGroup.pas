unit Frame_LccNodeEditorGroup;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Controls.Presentation, FMX.Layouts, Frame_LccNodeEditor;

type

  TDelayedCreateEditors = class
  private
    FName: String;
    FDescription: String;
    FStrValue: String;
    FIntMax: Integer;
    FIntMin: Integer;
    FDataType: Word;
    FIntValue: Integer;
    FComboStrings: TStringList;
    FComboItemIndex: Integer;
  public
    property Name: String read FName write FName;
    property Description: String read FDescription write FDescription;
    property DataType: Word read FDataType write FDataType;
    property IntMin: Integer read FIntMin write FIntMin;
    property IntMax: Integer read FIntMax write FIntMax;
    property IntValue: Integer read FIntValue write FIntValue;
    property StrValue: String read FStrValue write FStrValue;
    property ComboStrings: TStringList read FComboStrings write FComboStrings;
    property ComboItemIndex: Integer read FComboItemIndex write FComboItemIndex;

    constructor Create;
    destructor Destroy; override;
  end;

  TFrameLccNodeEditorGroup = class(TFrame)
    LayoutGroup: TLayout;
    ToolBarGroup: TToolBar;
    LayoutGroupHeaderText: TLayout;
    LabelName: TLabel;
    LabelDescription: TLabel;
    SpeedButtonGroupExpand: TSpeedButton;
    LayoutGroupContents: TLayout;
    FloatAnimationGroupExpand: TFloatAnimation;
    procedure SpeedButtonGroupExpandClick(Sender: TObject);
    procedure FrameResize(Sender: TObject);
    procedure FloatAnimationGroupExpandFinish(Sender: TObject);
    procedure FloatAnimationGroupExpandProcess(Sender: TObject);
  private
    FUniqueEditCounter: UInt64;
    FPrevGroup: TFrameLccNodeEditorGroup;
    FNextGroup: TFrameLccNodeEditorGroup;
    FEditorList: TList;
    FCollapsed: Boolean;
    FCollapsing: Boolean;
    FExpanding: Boolean;
    FExpandedOnce: Boolean;
    FDelayedEditorList: TList;
    FDelayEditorBuild: Boolean;
    FDelayedEditorAnimation: single;
    function GetEditorCount: Integer;
    procedure SetCollapsed(const Value: Boolean);
    procedure SetDelayedEditorAnimation(const Value: single);
    { Private declarations }
  protected
     property UniqueEditCounter: UInt64 read FUniqueEditCounter write FUniqueEditCounter;
     property EditorList: TList read FEditorList write FEditorList;
     property ExpandedOnce: Boolean read FExpandedOnce write FExpandedOnce;
     property DelayedEditorList: TList read FDelayedEditorList write FDelayedEditorList;
     property DelayedEditorAnimation: single read FDelayedEditorAnimation write SetDelayedEditorAnimation;

     function GenerateUniqueEditCounter: Integer;
     procedure FreeDelayedEditors;

     procedure AddEditorCommon(AnEditor: TFrameLccNodeEditor; AName, ADescription: String);
     function InternalAddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
     function InternalAddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
     function InternalAddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;
  public

    { Public declarations }
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property Collapsing: Boolean read FCollapsing;
    property Expanding: Boolean read FExpanding;
    property EditorCount: Integer read GetEditorCount;
    property NextGroup: TFrameLccNodeEditorGroup read FNextGroup write FNextGroup;
    property PrevGroup: TFrameLccNodeEditorGroup read FPrevGroup write FPRevGroup;
    property DelayEditorBuild: Boolean read FDelayEditorBuild write FDelayEditorBuild;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
    function AddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
    function AddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;
    procedure CreateDelayedEditors;

    function LastEditorTop: single;
    function LastEditorBottom: single;
    procedure ReSizeChildEditorsWidth;
  end;

implementation

{$R *.fmx}

function TFrameLccNodeEditorGroup.AddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
var
  DelayedEditor: TDelayedCreateEditors;
begin
  Result := nil;
  if DelayEditorBuild then
  begin
    DelayedEditor := TDelayedCreateEditors.Create;
    DelayedEditor.Name := AName;
    DelayedEditor.Description := ADescription;
    DelayedEditor.DataType := 0;
    DelayedEditor.ComboStrings.Assign(AStringList);
    DelayedEditor.ComboItemIndex := AnItemIndex;
    DelayedEditorList.Add(DelayedEditor);
  end else
  begin
    Result := InternalAddComboBoxEditor(AName, ADescription, AStringList, AnItemIndex);
  end;
end;

function TFrameLccNodeEditorGroup.AddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
var
  DelayedEditor: TDelayedCreateEditors;
begin
  Result := nil;
  if DelayEditorBuild then
  begin
    DelayedEditor := TDelayedCreateEditors.Create;
    DelayedEditor.Name := AName;
    DelayedEditor.Description := ADescription;
    DelayedEditor.DataType := 1;
    DelayedEditor.StrValue := AString;
    DelayedEditorList.Add(DelayedEditor)
  end else
    Result := InternalAddEditBoxEditor(AName, ADescription, AString)
end;

procedure TFrameLccNodeEditorGroup.AddEditorCommon(AnEditor: TFrameLccNodeEditor; AName, ADescription: String);
begin

  // This won't stick... once parent is set the FMX frame work force it to be a few
  // pixels shorter than the LayoutEditor in it
  // Use Editor.RealHeight to get the correct value
  // Editor.Height := Editor.LayoutEditor.Height;

  // Manually setup the position in the Group... faster and less frustrating than alTop annoyances
  AnEditor.Position.x := 0;
  AnEditor.Position.y := LastEditorBottom;
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

  if not Collapsed then
    Self.Height := AnEditor.Position.y + AnEditor.RealHeight + ToolBarGroup.Height;

  // Always keep the Content Layout Height the same size as the Editors in it
  LayoutGroupContents.Height := AnEditor.Position.y + AnEditor.RealHeight;

  // Now officially in the list
  EditorList.Add(AnEditor);
end;

function TFrameLccNodeEditorGroup.AddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;
var
  DelayedEditor: TDelayedCreateEditors;
begin
  Result := nil;
  if DelayEditorBuild then
  begin
    DelayedEditor := TDelayedCreateEditors.Create;
    DelayedEditor.Name := AName;
    DelayedEditor.Description := ADescription;
    DelayedEditor.DataType := 2;
    DelayedEditor.IntMin := AMin;
    DelayedEditor.IntMax := AMax;
    DelayedEditor.IntValue := AValue;
    DelayedEditorList.Add(DelayedEditor);
  end else
    InternalAddSpinBoxEditor(AName, ADescription, AValue, AMin, AMax)
end;

constructor TFrameLccNodeEditorGroup.Create(AOwner: TComponent);
begin
  // The Create call will call events like Resize.. crazy but true so make sure things are ready for that
  FEditorList := TList.Create;
  FDelayedEditorList := TList.Create;
  inherited;
end;

procedure TFrameLccNodeEditorGroup.CreateDelayedEditors;
var
  i: Integer;
  DelayedEditor: TDelayedCreateEditors;
begin
  try
    for i := 0 to DelayedEditorList.Count - 1 do
    begin
      DelayedEditor := TDelayedCreateEditors( DelayedEditorList[i]);
      case DelayedEditor.DataType of
        0 : InternalAddComboBoxEditor(DelayedEditor.Name, DelayedEditor.Description, DelayedEditor.ComboStrings, DelayedEditor.ComboItemIndex);
        1 : InternalAddEditBoxEditor(DelayedEditor.Name, DelayedEditor.Description, DelayedEditor.StrValue);
        2 : InternalAddSpinBoxEditor(DelayedEditor.Name, DelayedEditor.Description, DelayedEditor.IntValue, DelayedEditor.IntMin, DelayedEditor.IntMax);
      end;
    end;
  finally
    FreeDelayedEditors
  end;
end;

destructor TFrameLccNodeEditorGroup.Destroy;
begin
  FEditorList.Free;
  FreeDelayedEditors;
  FDelayedEditorList.Free;
  inherited;
end;

procedure TFrameLccNodeEditorGroup.FloatAnimationGroupExpandFinish(Sender: TObject);
begin
  FCollapsing := False;
  FExpanding := False;
  FloatAnimationGroupExpand.Enabled := False;
end;

procedure TFrameLccNodeEditorGroup.FloatAnimationGroupExpandProcess(Sender: TObject);
var
  ANextGroup: TFrameLccNodeEditorGroup;
  ANextY: single;
begin
  ANextGroup := NextGroup;
  ANextY := Position.y + Height;
  while Assigned(ANextGroup) do
  begin
    ANextGroup.Position.y := ANextY;
    ANextY := ANextGroup.Position.y + ANextGroup.Height;
    ANextGroup := ANextGroup.NextGroup;
  end;
end;

procedure TFrameLccNodeEditorGroup.FrameResize(Sender: TObject);
begin
  ReSizeChildEditorsWidth;
end;

procedure TFrameLccNodeEditorGroup.FreeDelayedEditors;
var
  i: Integer;
begin
  for i := 0 to DelayedEditorList.Count - 1 do
    TObject( DelayedEditorList[i]).Free;
  DelayedEditorList.Clear;
end;

function TFrameLccNodeEditorGroup.GenerateUniqueEditCounter: Integer;
begin
  Result := FUniqueEditCounter;
  Inc(FUniqueEditCounter);
end;

function TFrameLccNodeEditorGroup.GetEditorCount: Integer;
begin
  Result := LayoutGroupContents.Controls.Count
end;

function TFrameLccNodeEditorGroup.InternalAddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
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

function TFrameLccNodeEditorGroup.InternalAddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
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

function TFrameLccNodeEditorGroup.InternalAddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;
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

function TFrameLccNodeEditorGroup.LastEditorBottom: single;
begin
  Result := 0;
  if EditorList.Count > 0 then
    Result := TFrameLccNodeEditor( EditorList[EditorList.Count - 1]).Position.y +
      TFrameLccNodeEditor( EditorList[EditorList.Count - 1]).RealHeight;
end;

function TFrameLccNodeEditorGroup.LastEditorTop: single;
begin
  Result := 0;
  if EditorList.Count > 0 then
    Result := TFrameLccNodeEditor( EditorList[EditorList.Count - 1]).Position.y;
end;

procedure TFrameLccNodeEditorGroup.ReSizeChildEditorsWidth;
var
  i: Integer;
begin
  // Resize called before
  if Assigned(LayoutGroupContents) then
  begin
    for i := EditorList.Count - 1 downto 0 do
     TFrameLccNodeEditor( EditorList[i]).Width := Width;
  end;
end;

procedure TFrameLccNodeEditorGroup.SetCollapsed(const Value: Boolean);
begin
  if not ExpandedOnce and (Value = False) then
  begin
    CreateDelayedEditors;
    ExpandedOnce := True;
  end;

  // many need to think about this... maybe queue calls?
  if (FCollapsed <> Value) and not Collapsing and not Expanding then
  begin
    FCollapsed := Value;
    if Value then      // new desire is to be Collapsed and we must be expanded so time to collapse
    begin
      FCollapsing := True;
      SpeedButtonGroupExpand.StyleLookup := 'arrowrighttoolbutton';
      FloatAnimationGroupExpand.StartValue := Self.Height;
      FloatAnimationGroupExpand.StopValue := ToolBarGroup.Height;
      FloatAnimationGroupExpand.Enabled := True;
      FloatAnimationGroupExpand.Start;
    end else
    begin  // new desire is to be Expanded and we must be collapsed so time to expand
      FExpanding := True;
      SpeedButtonGroupExpand.StyleLookup := 'arrowdowntoolbutton';
      FloatAnimationGroupExpand.StartValue := ToolBarGroup.Height;
      FloatAnimationGroupExpand.StopValue := LastEditorBottom + ToolBarGroup.Height;
      FloatAnimationGroupExpand.Enabled := True;
      FloatAnimationGroupExpand.Start;
    end;

  end;
end;

procedure TFrameLccNodeEditorGroup.SetDelayedEditorAnimation(const Value: single);
begin
  FDelayedEditorAnimation := Value;
  if DelayedEditorList.Count > 0 then
  begin
 //   beep;
  end;
end;

procedure TFrameLccNodeEditorGroup.SpeedButtonGroupExpandClick(Sender: TObject);
begin
  Collapsed := not Collapsed;
end;


{ TDelayedCreateEditors }

constructor TDelayedCreateEditors.Create;
begin
  FComboStrings := TStringList.Create;
end;

destructor TDelayedCreateEditors.Destroy;
begin
  FComboStrings.Free;
  inherited;
end;

end.
