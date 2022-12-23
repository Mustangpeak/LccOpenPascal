unit UnitMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Objects,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Layouts,
  lcc_cdi_parser, lcc_xmlutilities;

type
  TForm1 = class(TForm)
    Layout1: TLayout;
    OpenDialog1: TOpenDialog;
    Button1: TButton;
    Layout2: TLayout;
    Button2: TButton;
    Button3: TButton;
    Label1: TLabel;
    Button4: TButton;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    Parser: TLccCdiParser;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.Button1Click(Sender: TObject);
var
  CDI: TLccXMLDocument;
begin
  if OpenDialog1.Execute then
  begin
    CDI := XmlLoadFromFile(OpenDialog1.FileName);
    Parser.Build_CDI_Interface(nil, Layout2, CDI);
    XmlFreeDocument(CDI);
  end;
end;


procedure TForm1.Button2Click(Sender: TObject);
var
  TabControl: TLccTabControl;
  TabItem:  TLccTabSheet;
  X: TRectangle;
begin
  TabControl := TLccTabControl.Create(Self);
  TabControl.Parent := Layout2;
  TabControl.Align := TAlignLayout.Client;

  TabItem := TLccTabSheet.Create(TabControl);
  TabItem.Text := 'Tab';
  TabItem.Parent := TabControl;

  X := TRectangle.Create(TabItem);
  X.Parent := TabItem;
  X.Align := TAlignLayout.Client;
  X.Fill.Color := TAlphaColorRec.Cadetblue;
end;

procedure TForm1.Button3Click(Sender: TObject);
begin
  Parser.Build_CDI_Interface(nil, Layout2, nil);
end;

procedure TForm1.Button4Click(Sender: TObject);
var
  L: TLabel;
begin
  Label1.TextSettings.Font.Style := [TFontStyle.fsBold];  // Desiger Dropped Label is Bold
  L := TLabel.Create(Self);
  L.Text := 'Testing';
  L.Align := TAlignLayout.Bottom;
  L.StyledSettings := L.StyledSettings-[TStyledSetting.Style];
  L.TextSettings.Font.Style := [TFontStyle.fsBold];  // Dynamically created Label is not Bold
  L.Parent := Layout1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Parser := TLccCdiParser.Create(Self);
end;

end.
