unit Frame_LccNodeEditor;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Effects, FMX.Edit, FMX.ComboEdit, FMX.Controls.Presentation, FMX.Layouts,
  FMX.Ani, FMX.Colors, FMX.Filter.Effects, FMX.EditBox, FMX.SpinBox, FMX.Text,
  FMX.ListBox;

const
  ANIMATION_DURATION = 0.2;
  ANNIMATION_GLOW_COLOR_TRANSITION = 0.2;

type
  TFrameLccNodeEditor = class(TFrame)
    FrameLayoutEditor: TLayout;
    LabelName: TLabel;
    LabelDescription: TLabel;
    LayoutEditorControl: TLayout;
    SpeedButtonActionWrite: TSpeedButton;
    SpeedButtonActionRead: TSpeedButton;
    SpeedButtonActionCompare: TSpeedButton;
    InnerGlowEffectEditorComboBox: TInnerGlowEffect;
    FloatAnimationDescription: TFloatAnimation;
    FloatAnimationName: TFloatAnimation;
    LayoutActionButtons: TLayout;
    SpeedButtonShowActions: TSpeedButton;
    FloatAnimationShowActions: TFloatAnimation;
    SpeedButtonTextFolding: TSpeedButton;
    LayoutEditor: TLayout;
    LayoutTextFolding: TLayout;
    FloatAnimationLayoutTextFolding: TFloatAnimation;
    LayoutEditorContainer: TLayout;
    EditBox: TEdit;
    SpinBox: TSpinBox;
    InnerGlowEffectEditorEditBox: TInnerGlowEffect;
    InnerGlowEffectEditorSpinBox: TInnerGlowEffect;
    ColorAnimationComboBoxInnerGlowEffect: TColorAnimation;
    ColorAnimationEditBoxInnerGlowEffect: TColorAnimation;
    ColorAnimationSpinBoxInnerGlowEffect: TColorAnimation;
    ComboBox: TComboBox;
    procedure LabelNameClick(Sender: TObject);
    procedure LabelDescriptionClick(Sender: TObject);
    procedure FloatAnimationNameFinish(Sender: TObject);
    procedure FloatAnimationDescriptionFinish(Sender: TObject);
    procedure SpeedButtonShowActionsClick(Sender: TObject);
    procedure FloatAnimationLayoutTextFoldingFinish(Sender: TObject);
    procedure SpeedButtonTextFoldingClick(Sender: TObject);
    procedure ComboBoxChange(Sender: TObject);
    procedure EditBoxChange(Sender: TObject);
    procedure SpinBoxChange(Sender: TObject);
    procedure ColorAnimationComboBoxInnerGlowEffectFinish(Sender: TObject);
    procedure ColorAnimationEditBoxInnerGlowEffectFinish(Sender: TObject);
    procedure ColorAnimationSpinBoxInnerGlowEffectFinish(Sender: TObject);
  private
    FDescriptionExpanded: Boolean;
    FNameExpanded: Boolean;
    FDescriptionShowing: Boolean;
    FOldDescriptionHeight: single;
    FActionsExpanded: Boolean;
    FGlowColorChanged: TAlphaColor;
    FGlowColorUnknown: TAlphaColor;
    FGlowColorValid: TAlphaColor;
    FPreChangeSpinBoxValue: double;
    FPreChangeEditBoxValue: String;
    FPreChangeComboBoxValue: Integer;
    function GetRealHeight: single;
    { Private declarations }
  protected
    property PreChangeSpinBoxValue: double read FPreChangeSpinBoxValue write FPreChangeSpinBoxValue;
    property PreChangeEditBoxValue: String read FPreChangeEditBoxValue write FPreChangeEditBoxValue;
    property PreChangeComboBoxValue: Integer read FPreChangeComboBoxValue write FPreChangeComboBoxValue;
  public
    { Public declarations }
    property ActionsExpanded: Boolean read FActionsExpanded write FActionsExpanded;
    property NameExpanded: Boolean read FNameExpanded write FNameExpanded;
    property DescriptionExpanding: Boolean read FDescriptionShowing write FDescriptionShowing;
    property DescriptionExpanded: Boolean read FDescriptionExpanded write FDescriptionExpanded;
    property OldDescriptionHeight: single read FOldDescriptionHeight write FOldDescriptionHeight;
    property RealHeight: single read GetRealHeight;

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
  end;

implementation

{$R *.fmx}

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
  LayoutTextFolding.Width := 0;

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

procedure TFrameLccNodeEditor.FloatAnimationDescriptionFinish(Sender: TObject);
begin
  FloatAnimationDescription.Enabled := False;

  // Is this the finish of the Description expanding?
  if DescriptionExpanded then
  begin
    // Yes so show the Folding button to give a hint to the user to restore it
    FloatAnimationLayoutTextFolding.Duration := ANIMATION_DURATION;
    FloatAnimationLayoutTextFolding.Inverse := False;
    FloatAnimationLayoutTextFolding.StartValue := 0;
    FloatAnimationLayoutTextFolding.StopValue := 44;
    FloatAnimationLayoutTextFolding.Enabled := True;
    FloatAnimationLayoutTextFolding.Start;
  end;
end;

procedure TFrameLccNodeEditor.FloatAnimationLayoutTextFoldingFinish(Sender: TObject);
begin
  FloatAnimationLayoutTextFolding.Enabled := False;
end;

procedure TFrameLccNodeEditor.FloatAnimationNameFinish(Sender: TObject);
begin
  FloatAnimationName.Enabled := False;

  // Is the finish of the Name height changing part of the description expanding/shrinking?
  if DescriptionExpanding then
  begin
    // Yes so finish up the sequence and expand or shrink the description
    if DescriptionExpanded then
    begin
      DescriptionExpanded := False;
      DescriptionExpanding := False;

      // Collapse Description back to normal
      FloatAnimationDescription.AnimationType := TAnimationType.Out;
      FloatAnimationDescription.StartValue := LabelDescription.Height;
      FloatAnimationDescription.StopValue := OldDescriptionHeight;
      FloatAnimationDescription.Duration := ANIMATION_DURATION;
      FloatAnimationDescription.Inverse := False;
      FloatAnimationDescription.Enabled := True;
      FloatAnimationDescription.Start;
    end else
    begin
      DescriptionExpanded := True;

      // Expand Description to full height
      FloatAnimationDescription.StartValue := LabelDescription.Height;
      FloatAnimationDescription.StopValue := Self.Height + 7;
      FloatAnimationDescription.AnimationType := TAnimationType.Out;
      FloatAnimationDescription.Duration := ANIMATION_DURATION;
      FloatAnimationDescription.Inverse := False;
      FloatAnimationDescription.Enabled := True;
      FloatAnimationDescription.Start;
    end;
  end else
  begin // Not the Description Expanding so must be the Name Expanding finishing

    // Is the Name expanded?
    if NameExpanded then
    begin
     // Yes so show the Folding button to give a hint to the user to restore it
      FloatAnimationLayoutTextFolding.Duration := ANIMATION_DURATION;
      FloatAnimationLayoutTextFolding.Inverse := False;
      FloatAnimationLayoutTextFolding.StartValue := 0;
      FloatAnimationLayoutTextFolding.StopValue := 44;
      FloatAnimationLayoutTextFolding.Enabled := True;
      FloatAnimationLayoutTextFolding.Start;
    end;
  end;
end;

function TFrameLccNodeEditor.GetRealHeight: single;
begin
  Result := LayoutEditor.Height;
end;

procedure TFrameLccNodeEditor.LabelDescriptionClick(Sender: TObject);
begin
  if not NameExpanded then
  begin  // Is the Description currently expanded?
    if DescriptionExpanded then
    begin

      // Yes so hide the Folding button
      FloatAnimationLayoutTextFolding.Duration := ANIMATION_DURATION;
      FloatAnimationLayoutTextFolding.Inverse := False;
      FloatAnimationLayoutTextFolding.StartValue := 44;
      FloatAnimationLayoutTextFolding.StopValue := 0;
      FloatAnimationLayoutTextFolding.Enabled := True;
      FloatAnimationLayoutTextFolding.Start;

      // Collapse the name first
      FloatAnimationName.AnimationType := TAnimationType.In;
      FloatAnimationName.Duration := ANIMATION_DURATION;
      FloatAnimationName.Inverse := True;
      FloatAnimationName.Enabled := True;
      FloatAnimationName.Start;

    end else
    begin
      DescriptionExpanding := True;

      OldDescriptionHeight := LabelDescription.Height;

      // Collapse the name first
      FloatAnimationName.StartValue := LabelName.Height;
      FloatAnimationName.StopValue := 0;
      FloatAnimationName.Duration := ANIMATION_DURATION;
      FloatAnimationName.AnimationType := TAnimationType.Out;
      FloatAnimationName.Inverse := False;
      FloatAnimationName.Enabled := True;
      FloatAnimationName.Start;

    end;
  end;
end;

procedure TFrameLccNodeEditor.LabelNameClick(Sender: TObject);
begin
  // If the Description is expanded then don't do anything
  if not DescriptionExpanded then
  begin
    // Is the Name currently expanded?
    if NameExpanded then
    begin
      NameExpanded := False;

       // Yes so hide the Folding button
      FloatAnimationLayoutTextFolding.Duration := ANIMATION_DURATION;
      FloatAnimationLayoutTextFolding.Inverse := False;
      FloatAnimationLayoutTextFolding.StartValue := LayoutTextFolding.Width;
      FloatAnimationLayoutTextFolding.StopValue := 0;
      FloatAnimationLayoutTextFolding.Enabled := True;
      FloatAnimationLayoutTextFolding.Start;

      // Now shrink the Name
      FloatAnimationName.Inverse := True;
      FloatAnimationName.AnimationType := TAnimationType.In;
      FloatAnimationName.Inverse := True;
      FloatAnimationName.Duration := ANIMATION_DURATION;
      FloatAnimationName.Enabled := True;
      FloatAnimationName.Start;
    end else
    begin
      NameExpanded := True;
      FloatAnimationName.StartValue := LabelName.Height;
      FloatAnimationName.StopValue := LayoutEditorControl.Height + LabelDescription.Height + LabelName.Height;
      FloatAnimationName.AnimationType := TAnimationType.Out;
      FloatAnimationName.Inverse := False;
      FloatAnimationName.Duration := ANIMATION_DURATION;
      FloatAnimationName.Enabled := True;
      FloatAnimationName.Start;
    end;
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

procedure TFrameLccNodeEditor.SpeedButtonTextFoldingClick(Sender: TObject);
begin
  if NameExpanded then
    LabelNameClick(Self)
  else
  if DescriptionExpanded then
    LabelDescriptionClick(Self)
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
