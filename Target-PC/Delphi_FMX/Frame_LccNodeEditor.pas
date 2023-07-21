unit Frame_LccNodeEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Effects, FMX.Edit, FMX.ComboEdit, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Ani, FMX.Colors, FMX.Filter.Effects, FMX.EditBox, FMX.SpinBox, FMX.Text,
  FMX.ListBox, FMX.TextLayout;

const
  ANIMATION_DURATION = 0.2;
  ANNIMATION_GLOW_COLOR_TRANSITION = 0.2;

type
  TFrameLccNodeEditor = class(TFrame)
    LabelName: TLabel;
    LabelDescription: TLabel;
    LayoutEditorControl: TLayout;
    SpeedButtonActionWrite: TSpeedButton;
    SpeedButtonActionRead: TSpeedButton;
    SpeedButtonActionCompare: TSpeedButton;
    InnerGlowEffectEditorComboBox: TInnerGlowEffect;
    LayoutActionButtons: TLayout;
    SpeedButtonShowActions: TSpeedButton;
    FloatAnimationShowActions: TFloatAnimation;
    LayoutEditorContainer: TLayout;
    EditBox: TEdit;
    SpinBox: TSpinBox;
    InnerGlowEffectEditorEditBox: TInnerGlowEffect;
    InnerGlowEffectEditorSpinBox: TInnerGlowEffect;
    ColorAnimationComboBoxInnerGlowEffect: TColorAnimation;
    ColorAnimationEditBoxInnerGlowEffect: TColorAnimation;
    ColorAnimationSpinBoxInnerGlowEffect: TColorAnimation;
    ComboBox: TComboBox;
    LayoutContainer: TLayout;
    procedure SpeedButtonShowActionsClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure EditBoxChange(Sender: TObject);
    procedure SpinBoxChange(Sender: TObject);
    procedure ColorAnimationComboBoxInnerGlowEffectFinish(Sender: TObject);
    procedure ColorAnimationEditBoxInnerGlowEffectFinish(Sender: TObject);
    procedure ColorAnimationSpinBoxInnerGlowEffectFinish(Sender: TObject);
  private
    FActionsExpanded: Boolean;
    FGlowColorChanged: TAlphaColor;
    FGlowColorUnknown: TAlphaColor;
    FGlowColorValid: TAlphaColor;
    FPreChangeSpinBoxValue: double;
    FPreChangeEditBoxValue: String;
    FPreChangeComboBoxValue: Integer;
    FBoundingHeight: single;
    function GetBoundingHeight: single;
    { Private declarations }
  protected
    property PreChangeSpinBoxValue: double read FPreChangeSpinBoxValue write FPreChangeSpinBoxValue;
    property PreChangeEditBoxValue: String read FPreChangeEditBoxValue write FPreChangeEditBoxValue;
    property PreChangeComboBoxValue: Integer read FPreChangeComboBoxValue write FPreChangeComboBoxValue;

    function GetTextHeight(const T: TTextSettings; const Width: Single; const Text: string): Integer;

  public
    { Public declarations }
    property ActionsExpanded: Boolean read FActionsExpanded write FActionsExpanded;
    property BoundingHeight: single read GetBoundingHeight write FBoundingHeight;


    property GlowColorValid: TAlphaColor read FGlowColorValid write FGlowColorValid;
    property GlowColorChanged: TAlphaColor read FGlowColorChanged write FGlowColorChanged;
    property GlowColorUnknown: TAlphaColor read FGlowColorUnknown write FGlowColorUnknown;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure EnableComboBox;
    procedure EnableEditBox;
    procedure EnableSpinBox;
    procedure EnableGlow(DoEnable: Boolean);
    procedure LoadComboBox(ComboBoxStrings: TStringList; AnItemIndex: Integer);
    procedure LoadEditBox(AString: String);
    procedure LoadSpinEdit(AValue: Integer; AMin, AMax: double); overload;
    procedure LoadSpinEdit(AValue: single; AMin, AMax: double); overload;
    procedure LoadSpinEditValueType(AValueType: TNumValueType);

    function ReCalculateAndRepositionContentsHeight: single;
  end;

implementation

{$R *.fmx}

function TFrameLccNodeEditor.ReCalculateAndRepositionContentsHeight: single;
// This repositions the contents of the editor and resize the frame to fit these items
const
  EDITOR_CONTROL_HEIGHT = 44;
var
  NameH, DescriptionH: single;
begin
  NameH := 0;
  DescriptionH := 0;

  if LabelName.Text <> '' then
    NameH := GetTextHeight(LabelName.TextSettings, Width -
                                                   LabelName.Margins.Left -
                                                   LabelName.Margins.Right,
                                                   LabelName.Text);
  if LabelDescription.Text <> '' then
    DescriptionH := GetTextHeight(LabelDescription.TextSettings, Width -
                                                                 LabelDescription.Margins.Left -
                                                                 LabelDescription.Margins.Right,
                                                                 LabelDescription.Text);

  Result := DescriptionH +
                     NameH +
                     EDITOR_CONTROL_HEIGHT +
                     LabelName.Margins.Top +
                     LabelName.Margins.Bottom +
                     LabelDescription.Margins.Top +
                     LabelDescription.Margins.Bottom;

  Height := Result;
  LayoutContainer.Height := Result;
  LabelName.Height := NameH;
  LabelDescription.Height := DescriptionH;
  FBoundingHeight := Result;
end;

procedure TFrameLccNodeEditor.ColorAnimationComboBoxInnerGlowEffectFinish(Sender: TObject);
begin
  ColorAnimationComboBoxInnerGlowEffect.Enabled := False;
end;

procedure TFrameLccNodeEditor.ColorAnimationEditBoxInnerGlowEffectFinish(Sender: TObject);
begin
  ColorAnimationEditBoxInnerGlowEffect.Enabled := False;
end;

procedure TFrameLccNodeEditor.ColorAnimationSpinBoxInnerGlowEffectFinish( Sender: TObject);
begin
  ColorAnimationSpinBoxInnerGlowEffect.Enabled := False;
end;

procedure TFrameLccNodeEditor.ComboBoxChange(Sender: TObject);
begin
  if PreChangeComboBoxValue = ComboBox.ItemIndex then
  begin
    if InnerGlowEffectEditorEditBox.GlowColor <> GlowColorValid then
    begin
     ColorAnimationComboBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
     ColorAnimationComboBoxInnerGlowEffect.StopValue := GlowColorValid;
     ColorAnimationComboBoxInnerGlowEffect.Start;
    end;
  end else
  begin
    if InnerGlowEffectEditorComboBox.GlowColor <> GlowColorChanged then
    begin
    ColorAnimationComboBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
     ColorAnimationComboBoxInnerGlowEffect.StopValue := GlowColorChanged;
     ColorAnimationComboBoxInnerGlowEffect.Start;
    end;
  end;
end;

constructor TFrameLccNodeEditor.Create(AOwner: TComponent);
begin
  inherited;
  LayoutActionButtons.Width := 0;

  GlowColorChanged := TAlphaColorRec.Red;
  GlowColorUnknown := TAlphaColorRec.Yellow;
  GlowColorValid := TAlphaColorRec.Green;
end;

destructor TFrameLccNodeEditor.Destroy;
begin

  inherited;
end;

procedure TFrameLccNodeEditor.EditBoxChange(Sender: TObject);
begin
  if PreChangeEditBoxValue = EditBox.Text then
  begin
    if InnerGlowEffectEditorEditBox.GlowColor <> GlowColorValid then
    begin
     ColorAnimationEditBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
     ColorAnimationEditBoxInnerGlowEffect.StopValue := GlowColorValid;
     ColorAnimationEditBoxInnerGlowEffect.Start;
    end;
  end else
  begin
    if InnerGlowEffectEditorEditBox.GlowColor <> GlowColorChanged then
    begin
     ColorAnimationEditBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
     ColorAnimationEditBoxInnerGlowEffect.StopValue := GlowColorChanged;
     ColorAnimationEditBoxInnerGlowEffect.Start;
    end;
  end;
end;

procedure TFrameLccNodeEditor.EnableComboBox;
begin
  ComboBox.Visible := True;
  ComboBox.Align := TAlignLayout.Client;
  EditBox.Visible := False;
  SpinBox.Visible := False;
end;

procedure TFrameLccNodeEditor.EnableEditBox;
begin
  EditBox.Visible := True;
  EditBox.Align := TAlignLayout.Client;
  ComboBox.Visible := False;
  SpinBox.Visible := False;
end;

procedure TFrameLccNodeEditor.EnableGlow(DoEnable: Boolean);
begin
  InnerGlowEffectEditorComboBox.Enabled := DoEnable;
  InnerGlowEffectEditorSpinBox.Enabled := DoEnable;
  InnerGlowEffectEditorEditBox.Enabled := DoEnable;
end;

procedure TFrameLccNodeEditor.EnableSpinBox;
begin
  SpinBox.Visible := True;
  SpinBox.Align := TAlignLayout.Client;
  ComboBox.Visible := False;
  EditBox.Visible := False;
end;

function TFrameLccNodeEditor.GetBoundingHeight: single;
begin
  Result := FBoundingHeight;
end;

function TFrameLccNodeEditor.GetTextHeight(const T: TTextSettings; const Width: Single; const Text: string): Integer;
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
      Layout.MaxSize := TPointF.Create(Width, TTextLayout.MaxLayoutSize.Y);
      Layout.Text := Text;
    finally
      Layout.EndUpdate;
    end;
    Result := Round(Layout.Height * 1.1);   // 10% margin
  finally
    Layout.Free;
  end;
end;

procedure TFrameLccNodeEditor.LoadComboBox(ComboBoxStrings: TStringList; AnItemIndex: Integer);
begin
  ComboBox.Items.Assign(ComboBoxStrings);
  ComboBox.ItemIndex := AnItemIndex;
  PreChangeComboBoxValue := ComboBox.ItemIndex;
  ColorAnimationComboBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
  ColorAnimationComboBoxInnerGlowEffect.StopValue := GlowColorValid;
  ColorAnimationComboBoxInnerGlowEffect.Start;
end;

procedure TFrameLccNodeEditor.LoadEditBox(AString: String);
begin
  EditBox.Text := AString;
  PreChangeEditBoxValue := AString;
  ColorAnimationEditBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
  ColorAnimationEditBoxInnerGlowEffect.StopValue := GlowColorValid;
  ColorAnimationEditBoxInnerGlowEffect.Start;
end;

procedure TFrameLccNodeEditor.LoadSpinEdit(AValue: Integer; AMin, AMax: double);
begin
  SpinBox.Value := AValue;
  SpinBox.Min := AMin;
  SpinBox.Max := AMax;
  PreChangeSpinBoxValue := AValue;
  ColorAnimationSpinBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
  ColorAnimationSpinBoxInnerGlowEffect.StopValue := GlowColorValid;
  ColorAnimationSpinBoxInnerGlowEffect.Start;
end;

procedure TFrameLccNodeEditor.LoadSpinEdit(AValue: single; AMin, AMax: double);
begin
  SpinBox.Value := AValue;
  SpinBox.Min := AMin;
  SpinBox.Max := AMax;
  PreChangeSpinBoxValue := AValue;
  ColorAnimationSpinBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
  ColorAnimationSpinBoxInnerGlowEffect.StopValue := GlowColorValid;
  ColorAnimationSpinBoxInnerGlowEffect.Start;
end;

procedure TFrameLccNodeEditor.LoadSpinEditValueType(AValueType: TNumValueType);
begin
  SpinBox.ValueType := AValueType;
end;

procedure TFrameLccNodeEditor.SpeedButtonShowActionsClick(Sender: TObject);
begin
  if ActionsExpanded then
  begin
    ActionsExpanded := False;

    FloatAnimationShowActions.AnimationType := TAnimationType.In;
    FloatAnimationShowActions.Duration := ANIMATION_DURATION;
    FloatAnimationShowActions.Inverse := True;
    FloatAnimationShowActions.Enabled := True;
    FloatAnimationShowActions.Start;

  end else
  begin
    ActionsExpanded := True;

    if SpeedButtonActionWrite.Visible then


    FloatAnimationShowActions.StartValue := LayoutActionButtons.Width;
    FloatAnimationShowActions.StopValue := 0;
    if SpeedButtonActionWrite.Visible then
      FloatAnimationShowActions.StopValue := FloatAnimationShowActions.StopValue +  SpeedButtonActionWrite.Width;
    if SpeedButtonActionRead.Visible then
      FloatAnimationShowActions.StopValue := FloatAnimationShowActions.StopValue +  SpeedButtonActionRead.Width;
    if SpeedButtonActionCompare.Visible then
      FloatAnimationShowActions.StopValue := FloatAnimationShowActions.StopValue +  SpeedButtonActionCompare.Width;
    FloatAnimationShowActions.Duration := ANIMATION_DURATION;
    FloatAnimationShowActions.AnimationType := TAnimationType.Out;
    FloatAnimationShowActions.Inverse := False;
    FloatAnimationShowActions.Enabled := True;
    FloatAnimationShowActions.Start;

  end;

end;

procedure TFrameLccNodeEditor.SpinBoxChange(Sender: TObject);
begin
  if PreChangeSpinBoxValue = SpinBox.Value then
  begin
    if InnerGlowEffectEditorSpinBox.GlowColor <> GlowColorValid then
    begin
     ColorAnimationSpinBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
     ColorAnimationSpinBoxInnerGlowEffect.StopValue := GlowColorValid;
     ColorAnimationSpinBoxInnerGlowEffect.Start;
    end;
  end else
  begin
    if InnerGlowEffectEditorSpinBox.GlowColor <> GlowColorChanged then
    begin
     ColorAnimationSpinBoxInnerGlowEffect.Duration := ANNIMATION_GLOW_COLOR_TRANSITION;
     ColorAnimationSpinBoxInnerGlowEffect.StopValue := GlowColorChanged;
     ColorAnimationSpinBoxInnerGlowEffect.Start;
    end;
  end;
end;

end.
