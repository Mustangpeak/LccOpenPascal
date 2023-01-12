unit FMX_UserInterfaceApp;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.Layouts, FMX.Edit, FMX.ComboEdit, FMX.Effects,
  FMX.TextLayout, FMX.Ani, FMX.Objects, FMX.EditBox, FMX.SpinBox,
  Frame_LccNodeEditor, Frame_LccNodeEditorGroup, Frame_LccNodeEditorControl;

type

  TFMX_LccConfigMemEditorButtonSizeManager = class
  private
    FLayout: TTextLayout;
    FButtonWidthFullText: single;

    FMaxTextWidth: single;
    FButtonWidthShortText: single;
    FTextWidthValid: Boolean;
    FIsShortText: Boolean;
  public
    property  Layout: TTextLayout read FLayout write FLayout;
     property ButtonWidthFullText: single read FButtonWidthFullText;
     property ButtonWidthShortText: single read FButtonWidthShortText;
     property MaxTextWidth: single read FMaxTextWidth;
     property TextWidthValid: Boolean read FTextWidthValid write FTextWidthValid;
     property IsShortText: Boolean read FIsShortText;

    constructor Create;
    destructor Destroy; override;
    procedure RecalculateTextExtents;
    procedure UpdateSpeedButtonWidth(SpeedButton1, SpeedButton2, SpeedButton3: TSpeedButton; ContainterWidth: single);
  end;


  TFMX_UserInterfaceForm = class(TForm)
    Header: TToolBar;
    Footer: TToolBar;
    HeaderLabel: TLabel;
    SpeedButton1: TSpeedButton;
    LayoutEditorControl: TLayout;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure SpeedButton1Click(Sender: TObject);
  private
    FButtonSizeManager: TFMX_LccConfigMemEditorButtonSizeManager;
    { Private declarations }
  public
    { Public declarations }

    Group: TFrameLccNodeEditorGroup;
    FrameNodeEditorControl: TFrameNodeEditorControl;


    property ButtonSizeManager: TFMX_LccConfigMemEditorButtonSizeManager read FButtonSizeManager write FButtonSizeManager;

  end;

var
  FMX_UserInterfaceForm: TFMX_UserInterfaceForm;

implementation

{$R *.fmx}
{$R *.iPhone4in.fmx IOS}
{$R *.iPhone47in.fmx IOS}

const
  MIN_BUTTON_WIDTH = 32;


procedure TFMX_UserInterfaceForm.FormCreate(Sender: TObject);
begin
  FButtonSizeManager := TFMX_LccConfigMemEditorButtonSizeManager.Create;

end;

procedure TFMX_UserInterfaceForm.FormDestroy(Sender: TObject);
begin
  FreeAndNil(FButtonSizeManager);

end;

procedure TFMX_UserInterfaceForm.SpeedButton1Click(Sender: TObject);
var
  i: Integer;
  Count, CountPerGroup: Integer;
  StringList: TStringList;
  j: Integer;
begin
  StringList := TSTringList.Create;
  StringList.Add('Bob');
  StringList.Add('Sue');
  StringList.Add('Frank');
  StringList.Add('Mary');
  StringList.Add('Sue');


  FrameNodeEditorControl := TFrameNodeEditorControl.Create(Self);
  FrameNodeEditorControl.Align := TAlignLayout.Client;



  FrameNodeEditorControl.Visible := False;
  FrameNodeEditorControl.BeginUpdate;
  for j := 0 to 4 do
  begin
    Group := FrameNodeEditorControl.AddGroup('New Group ' + IntToStr(j), 'Description', True);
    Group.DelayEditorBuild := True;

    CountPerGroup := 100;

    Count := CountPerGroup div 3;
    for i := 0 to Count - 1 do
    begin
      Group.AddComboBoxEditor('Combo Name ' + IntToStr(i), 'Combo Description', StringList, 0);
      Group.AddEditBoxEditor('Edit Name ' + IntToStr(i), 'Edit Description', 'Some Text');
      Group.AddSpinBoxEditor('Spin Name ' + IntToStr(i), 'Spin Description', 1000, 0, 2000);
    end;
  end;
  FrameNodeEditorControl.EndUpdate;
  FrameNodeEditorControl.Visible := True;


  FrameNodeEditorControl.Parent := LayoutEditorControl;

  StringList.Free
end;

{ TFMX_LccConfigMemEditorButtonSizeManager }

constructor TFMX_LccConfigMemEditorButtonSizeManager.Create;
begin
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
end;

destructor TFMX_LccConfigMemEditorButtonSizeManager.Destroy;
begin
  FreeAndNil(FLayout);
  inherited;
end;

procedure TFMX_LccConfigMemEditorButtonSizeManager.RecalculateTextExtents;
begin
  if Assigned(Layout) and not TextWidthValid then
  begin
    Layout.Text := 'Compare';        // The widest
    FButtonWidthFullText := Layout.TextWidth;
    FButtonWidthFullText := FButtonWidthFullText * 1.2;  // Make the button width 20% larger than text

    // Make sure the button does not get to small to click
    if FButtonWidthFullText < MIN_BUTTON_WIDTH then
      FButtonWidthFullText := MIN_BUTTON_WIDTH;

    Layout.Text := 'C';        // The widest
    FButtonWidthShortText := Layout.TextWidth;
    FButtonWidthShortText := FButtonWidthShortText * 1.2;  // Make the button width 20% larger than text

    // Make sure the button does not get to small to click
    if FButtonWidthShortText < MIN_BUTTON_WIDTH then
      FButtonWidthShortText := MIN_BUTTON_WIDTH;

    TextWidthValid := True;
  end
end;

procedure TFMX_LccConfigMemEditorButtonSizeManager.UpdateSpeedButtonWidth(
  SpeedButton1, SpeedButton2, SpeedButton3: TSpeedButton;
  ContainterWidth: single);
var
  AllButtonsW: single;
begin
  AllButtonsW := 0;
  if SpeedButton1.Visible then AllButtonsW := AllButtonsW + ButtonWidthFullText;
  if SpeedButton2.Visible then AllButtonsW := AllButtonsW + ButtonWidthFullText;
  if SpeedButton3.Visible then AllButtonsW := AllButtonsW + ButtonWidthFullText;

   // If buttons take up more than 40% of space drop back to letters
  if AllButtonsW < ContainterWidth * 0.40 then
  begin
    SpeedButton1.Width := ButtonWidthFullText;
    SpeedButton2.Width := ButtonWidthFullText;
    SpeedButton3.Width := ButtonWidthFullText;
    SpeedButton1.Text := 'Compare';
    SpeedButton2.Text := 'Read';
    SpeedButton3.Text := 'Write';
  end else
  begin
    SpeedButton1.Width := ButtonWidthShortText;
    SpeedButton2.Width := ButtonWidthShortText;
    SpeedButton3.Width := ButtonWidthShortText;
    SpeedButton1.Text := 'C';
    SpeedButton2.Text := 'R';
    SpeedButton3.Text := 'W';
  end;
end;

end.
