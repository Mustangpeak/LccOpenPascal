unit Frame_LccNodeEditorGroup;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Ani, FMX.Controls.Presentation, FMX.Layouts, Frame_LccNodeEditor, FMX.TextLayout;

type

  TFrameLccNodeEditorGroup = class;

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
    FIsCollapsed: Boolean;
    FOwnerControl: TObject;
    FParentGroup: TFrameLccNodeEditorGroup;   // TFrameNodeEditorControl
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
    property IsCollapsed: Boolean read FIsCollapsed write FIsCollapsed;    // Group
    property OwnerControl: TObject read FOwnerControl write FOwnerControl;     // TFrameNodeEditorControl
    property ParentGroup: TFrameLccNodeEditorGroup read FParentGroup write FParentGroup;

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
    procedure FloatAnimationGroupExpandFinish(Sender: TObject);
    procedure FloatAnimationGroupExpandProcess(Sender: TObject);
    procedure FrameResized(Sender: TObject);
  private
    FChildItemList: TList;
    FCollapsed: Boolean;
    FCollapsing: Boolean;
    FExpanding: Boolean;
    FExpandedOnce: Boolean;
    FDelayedEditorList: TList;
    FDelayEditorBuild: Boolean;
    FDelayedEditorAnimation: single;
    FOwnerControl: TObject;
    FParentGroup: TFrameLccNodeEditorGroup;
    FUpdateCount: Integer;
    FHeaderHeight: single;
    FContentsHeight: single;
    function GetEditorCount: Integer;
    procedure SetCollapsed(const Value: Boolean);
    procedure SetDelayedEditorAnimation(const Value: single);
    function GetCurrentHeight: single;
    procedure SetCurrentHeight(const Value: single);
    { Private declarations }
  protected
    property ExpandedOnce: Boolean read FExpandedOnce write FExpandedOnce;
    property DelayedEditorList: TList read FDelayedEditorList write FDelayedEditorList;
    property DelayedEditorAnimation: single read FDelayedEditorAnimation write SetDelayedEditorAnimation;
    property UpdateCount: Integer read FUpdateCount;

    procedure FreeDelayedEditors;
    function GetTextHeight(const T: TTextSettings; const AWidth: Single; const Text: string): Integer;

    procedure AddEditorCommon(AnEditor: TFrameLccNodeEditor; AName, ADescription: String);
    function InternalAddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
    function InternalAddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
    function InternalAddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;
    function InternalAddSubGroup(AnOwnerControl: TObject; AParentGroup: TFrameLccNodeEditorGroup; AName, ADescription: String; IsCollapsed: Boolean): TFrameLccNodeEditorGroup;

    procedure ReAdjustFrameHeight;
    function ReCalculateHeader: single;
    function ReCalculateGroupHeight: single;
  public

    { Public declarations }
    property Collapsed: Boolean read FCollapsed write SetCollapsed;
    property Collapsing: Boolean read FCollapsing;
    property Expanding: Boolean read FExpanding;
    property EditorCount: Integer read GetEditorCount;
    property DelayEditorBuild: Boolean read FDelayEditorBuild write FDelayEditorBuild;
    property OwnerControl: TObject read FOwnerControl write FOwnerControl;  // TFrameNodeEditorControl object
    property ParentGroup: TFrameLccNodeEditorGroup read FParentGroup;
    property HeaderHeight: single read FHeaderHeight write FHeaderHeight;
    property ContentsHeight: single read FContentsHeight write FContentsHeight;
    property CurrentHeight: single read GetCurrentHeight write SetCurrentHeight;

    property ChildItemList: TList read FChildItemList write FChildItemList;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddSubGroup(AnOwnerControl: TObject; AParentGroup: TFrameLccNodeEditorGroup; AName, ADescription: String; IsCollapsed: Boolean): TFrameLccNodeEditorGroup;
    function AddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
    function AddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
    function AddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;
    procedure Clear;
    procedure CreateDelayedEditors;

    procedure ReCalculateChildWidths(AWidth: single);
    procedure ReCalculateGroupLayout;
  end;

implementation

{$R *.fmx}

uses
  Frame_LccNodeEditorControl;

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
  AnEditor.Name := 'FrameEditor' + IntToStr((OwnerControl as TFrameNodeEditorControl).GenerateUniqueEditCounter);
  if AName <> '' then
    AnEditor.LabelName.Text := AName
  else
    AnEditor.LabelName.Visible := False;

  if ADescription <> '' then
    AnEditor.LabelDescription.Text := ADescription
  else
    AnEditor.LabelDescription.Visible := False;

  AnEditor.Parent := LayoutGroupContents;

  // Now officially in the list
  ChildItemList.Add(AnEditor);
end;

function TFrameLccNodeEditorGroup.AddSubGroup(AnOwnerControl: TObject; AParentGroup: TFrameLccNodeEditorGroup; AName, ADescription: String; IsCollapsed: Boolean): TFrameLccNodeEditorGroup;
var
  DelayedEditor: TDelayedCreateEditors;
begin
  Result := nil;
  if DelayEditorBuild and not IsCollapsed then
  begin
    DelayedEditor := TDelayedCreateEditors.Create;
    DelayedEditor.Name := AName;
    DelayedEditor.Description := ADescription;
    DelayedEditor.DataType := 3;
    DelayedEditor.IsCollapsed := IsCollapsed;
    DelayedEditor.OwnerControl := AnOwnerControl;
    DelayedEditor.ParentGroup := AParentGroup;
    DelayedEditorList.Add(DelayedEditor);
  end else
    Result := InternalAddSubGroup(AnOwnerControl, AParentGroup, AName, ADescription, IsCollapsed)
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
    Result := InternalAddSpinBoxEditor(AName, ADescription, AValue, AMin, AMax)
end;

procedure TFrameLccNodeEditorGroup.ReAdjustFrameHeight;
begin
  if Collapsed then
    Height := HeaderHeight
  else
    Height := HeaderHeight + ContentsHeight;
end;

procedure TFrameLccNodeEditorGroup.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to EditorCount - 1 do
      TObject( ChildItemList[i]).Free;
  finally
    ChildItemList.Clear;
  end;

end;

constructor TFrameLccNodeEditorGroup.Create(AOwner: TComponent);
begin
  // The Create call will call events like Resize.. crazy but true so make sure things are ready for that
  FChildItemList := TList.Create;
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
        3 : InternalAddSubGroup(DelayedEditor.OwnerControl, DelayedEditor.ParentGroup, DelayedEditor.Name, DelayedEditor.Description, DelayedEditor.IsCollapsed);
      end;
    end;
  finally
    FreeDelayedEditors
  end;
end;

destructor TFrameLccNodeEditorGroup.Destroy;
begin
  Clear;
  FChildItemList.Free;
  FreeDelayedEditors;
  FDelayedEditorList.Free;
  inherited;
end;


procedure TFrameLccNodeEditorGroup.FloatAnimationGroupExpandFinish(Sender: TObject);
var
  TestGroup: TFrameLccNodeEditorGroup;
begin
  FCollapsing := False;
  FExpanding := False;
  FloatAnimationGroupExpand.Enabled := False;

  (OwnerControl as TFrameNodeEditorControl).beginUpdate;

  if Assigned(ParentGroup) then
  begin
    TestGroup := Self;
    repeat
      TestGroup := TestGroup.ParentGroup;
    until not Assigned(TestGroup.ParentGroup);
  end else
    TestGroup := Self;

  if Assigned(TestGroup) then
    TestGroup.ReCalculateGroupLayout;

   (OwnerControl as TFrameNodeEditorControl).EndUpdate;
end;

procedure TFrameLccNodeEditorGroup.FloatAnimationGroupExpandProcess(Sender: TObject);

  procedure FindInParentAndSlideUpLaterSibling(TargetGroup: TFrameLccNodeEditorGroup; OffsetY: single);
  var
    i: Integer;
    TestGroup: TFrameLccNodeEditorGroup;
    IsLaterSibling: Boolean;
  begin
    IsLaterSibling := False;
    if Assigned(TargetGroup.ParentGroup) then
    begin
      for i := 0 to TargetGroup.ParentGroup.ChildItemList.Count - 1 do
      begin
        if TFrame( TargetGroup.ParentGroup.ChildItemList[i]) is TFrameLccNodeEditorGroup then
        begin
          TestGroup := TargetGroup.ParentGroup.ChildItemList[i];
          if IsLaterSibling then
          begin
            TestGroup.Position.Y := TestGroup.Position.Y + OffsetY;
            FindInParentAndSlideUpLaterSibling(TestGroup, OffsetY);
          end;
          if not IsLaterSibling and (TestGroup = TargetGroup) then
            IsLaterSibling := True;
        end;
      end;
    end;
  end;

var
  DeltaH: single;
begin

Exit;

  DeltaH := CurrentHeight - (HeaderHeight + ContentsHeight);
  FindInParentAndSlideUpLaterSibling(Self, DeltaH);
end;

procedure TFrameLccNodeEditorGroup.FrameResized(Sender: TObject);
begin
  if Assigned(OwnerControl) then
  begin
    if (OwnerControl as TFrameNodeEditorControl).LockCount <> 0 then Exit;

    ReCalculateChildWidths(Width);
    ReCalculateGroupLayout;
  end;
end;

procedure TFrameLccNodeEditorGroup.FreeDelayedEditors;
var
  i: Integer;
begin
  for i := 0 to DelayedEditorList.Count - 1 do
    TObject( DelayedEditorList[i]).Free;
  DelayedEditorList.Clear;
end;

function TFrameLccNodeEditorGroup.GetCurrentHeight: single;
begin
  Result := Height
end;

function TFrameLccNodeEditorGroup.GetEditorCount: Integer;
begin
  Result := ChildItemList.Count;
end;

function TFrameLccNodeEditorGroup.GetTextHeight(const T: TTextSettings; const AWidth: Single; const Text: string): Integer;
var
  Layout: TTextLayout;
begin
  // Create a TTextLayout to measure text dimensions
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    try
      // Initialize layout parameters with those of the drawable
      Layout.Font.Assign(T.Font);
      Layout.VerticalAlign := T.VertAlign;
      Layout.HorizontalAlign := T.HorzAlign;
      Layout.WordWrap := T.WordWrap;
      Layout.Trimming := T.Trimming;
      Layout.MaxSize := TPointF.Create(AWidth, TTextLayout.MaxLayoutSize.Y);
      Layout.Text := Text;
    finally
      Layout.EndUpdate;
    end;
    Result := Round(Layout.Height * 1.1);   // 10% margin
  finally
    Layout.Free;
  end;
end;

function TFrameLccNodeEditorGroup.InternalAddComboBoxEditor(AName, ADescription: String; AStringList: TStringList; AnItemIndex: Integer): TFrameLccNodeEditor;
begin
  Result := TFrameLccNodeEditor.Create(Self);
  Result.EnableComboBox;
  Result.LoadComboBox(AStringList, AnItemIndex);
  AddEditorCommon(Result, AName, ADescription);
end;

function TFrameLccNodeEditorGroup.InternalAddEditBoxEditor(AName, ADescription, AString: String): TFrameLccNodeEditor;
begin
  Result := TFrameLccNodeEditor.Create(Self);
  Result.EnableEditBox;
  Result.LoadEditBox(AString);
  AddEditorCommon(Result, AName, ADescription)
end;

function TFrameLccNodeEditorGroup.InternalAddSpinBoxEditor(AName, ADescription: String; AValue, AMin, AMax: Integer): TFrameLccNodeEditor;
begin
  Result := TFrameLccNodeEditor.Create(Self);
  Result.EnableSpinBox;
  Result.LoadSpinEdit(AValue, AMin, AMax);
  AddEditorCommon(Result, AName, ADescription)
end;

function TFrameLccNodeEditorGroup.InternalAddSubGroup(AnOwnerControl: TObject; AParentGroup: TFrameLccNodeEditorGroup; AName, ADescription: String; IsCollapsed: Boolean): TFrameLccNodeEditorGroup;
begin
  // Do not call Begin/EndUpdate here as it will lock up changing the width of the group later... Don't know why it is something internal to the Firemonkey Begin/EndUpdate calls
  Result := TFrameLccNodeEditorGroup.Create(Self);
  Result.Name := 'GroupEditorSubGroup' + IntToStr((AnOwnerControl as TFrameNodeEditorControl).GenerateUniqueEditCounter);
  Result.FCollapsed := IsCollapsed;
  Result.FParentGroup := AParentGroup;
  Result.Parent := AParentGroup.LayoutGroupContents;
  Result.OwnerControl := AnOwnerControl;

  if AName <> '' then
    Result.LabelName.Text := AName
  else
    Result.LabelName.Visible := False;
  if ADescription <> '' then
    Result.LabelDescription.Text := ADescription
  else
    Result.LabelDescription.Visible := False;

  // Now officially in the list
  ChildItemList.Add(Result);
end;

function TFrameLccNodeEditorGroup.ReCalculateGroupHeight: single;
// This only repositions the contents of the group.. anything in the LayoutGroupContents control
// and resize that control, it does not resize the frame that is up to the collapase/expand logic
var
  HeaderH, CurrentTop, EditorH, GroupH: single;
  i: Integer;
  Editor: TFrameLccNodeEditor;
  Group: TFrameLccNodeEditorGroup;
begin
  HeaderH := ReCalculateHeader;

  // This is with respect to a container TLayout (LayoutGroupContents)
  CurrentTop := 0;

  if not Collapsed then
  begin
    for i := 0 to ChildItemList.Count - 1 do
    begin
      if TFrame( ChildItemList[i]) is TFrameLccNodeEditor then
      begin
        Editor := TFrameLccNodeEditor( ChildItemList[i]);
        EditorH := Editor.ReCalculateAndRepositionContentsHeight;
        Editor.Position.Y := CurrentTop;
        CurrentTop := CurrentTop + EditorH;
      end else
      if TFrame( ChildItemList[i]) is TFrameLccNodeEditorGroup then
      begin
        // Recursively go into subgroups to update them and get the get their current heights
        Group := TFrameLccNodeEditorGroup( ChildItemList[i]);
        GroupH := Group.ReCalculateGroupHeight;
        Group.Position.Y := CurrentTop;
        CurrentTop := CurrentTop + GroupH;
      end;
    end;
  end;

  Result := CurrentTop + HeaderH;

  LayoutGroupContents.Height := Result;
  LayoutGroup.Height := Result + HeaderH;
  ContentsHeight := Result;
end;

function TFrameLccNodeEditorGroup.ReCalculateHeader: single;
var
  NameH, DescriptionH: single;
begin
  // Assumes the width has been correctly set prior
  NameH := 0;
  DescriptionH := 0;

  if LabelName.Text <> '' then
    NameH := GetTextHeight(LabelName.TextSettings, Width -
                                                   SpeedButtonGroupExpand.Width -
                                                   LabelName.Margins.Left -
                                                   LabelName.Margins.Right,
                                                   LabelName.Text);
  if LabelDescription.Text <> '' then
    DescriptionH := GetTextHeight(LabelDescription.TextSettings, Width -
                                                                 SpeedButtonGroupExpand.Width -
                                                                 LabelDescription.Margins.Left -
                                                                 LabelDescription.Margins.Right,
                                                                 LabelDescription.Text);

  Result :=  NameH +
             DescriptionH +
             LabelName.Margins.Top +
             LabelName.Margins.Bottom +
             LabelDescription.Margins.Top -
             LabelDescription.Margins.Bottom ;

  if Result < 44 then
    Result := 44;

  LabelName.Height := NameH;
  LabelDescription.Height := DescriptionH;
  ToolBarGroup.Height := Result;
  HeaderHeight := Result;
  LayoutGroup.Height := Result + ContentsHeight;
end;

procedure TFrameLccNodeEditorGroup.ReCalculateGroupLayout;
begin
  if (OwnerControl as TFrameNodeEditorControl).LockCount <> 0 then Exit;

  ReCalculateGroupHeight;
  ReAdjustFrameHeight
end;

procedure TFrameLccNodeEditorGroup.ReCalculateChildWidths(AWidth: single);
var
  i: Integer;
  Group: TFrameLccNodeEditorGroup;
  Editor: TFrameLccNodeEditor;
begin
  if (OwnerControl as TFrameNodeEditorControl).LockCount <> 0 then Exit;

  // Resize called before inherited Create returns in the override.....
  if Assigned(LayoutGroupContents) then
  begin
    for i := 0 to ChildItemList.Count - 1 do
    begin
      if TFrame( ChildItemList[i]) is TFrameLccNodeEditorGroup then
      begin
        Group := TFrameLccNodeEditorGroup( ChildItemList[i]);
        Group.Width := AWidth;
        Group.ReCalculateChildWidths(AWidth);
      end else
      begin
        Editor := TFrameLccNodeEditor( ChildItemList[i]);
        Editor.Width := AWidth;
      end;
    end;
  end;
end;

procedure TFrameLccNodeEditorGroup.SetCollapsed(const Value: Boolean);
begin

  begin
    if Value <> FCollapsed then
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
          FloatAnimationGroupExpand.StartValue := ContentsHeight + HeaderHeight;
          FloatAnimationGroupExpand.StopValue := HeaderHeight;
          FloatAnimationGroupExpand.Enabled := True;
          FloatAnimationGroupExpand.Start;
        end else
        begin  // new desire is to be Expanded and we must be collapsed so time to expand
          FExpanding := True;
          SpeedButtonGroupExpand.StyleLookup := 'arrowdowntoolbutton';
          FloatAnimationGroupExpand.StartValue := HeaderHeight;
          FloatAnimationGroupExpand.StopValue := ContentsHeight + HeaderHeight;
          FloatAnimationGroupExpand.Enabled := True;
          FloatAnimationGroupExpand.Start;
        end;
      end;
    end;
  end;
end;

procedure TFrameLccNodeEditorGroup.SetCurrentHeight(const Value: single);
begin
  Height := Value;
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
