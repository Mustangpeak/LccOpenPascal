unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls, ComCtrls,
  lcc_cdi_parser, lcc_xmlutilities;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Label4: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Panel4Resize(Sender: TObject);
  private

  public
    Parser: TLccCdiParser;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  CDI: TLccXMLDocument;
begin
  if OpenDialog1.Execute then
  begin
    CDI := XmlLoadFromFile(OpenDialog1.FileName);
    Parser.Build_CDI_Interface(nil, Panel2, CDI);
    XmlFreeDocument(CDI);
  end;
end;

procedure TForm1.Button2Click(Sender: TObject);
var
  L: TLabel;
var
  i: Integer;
  Page: TPageControl;
  LocalTab: TTabSheet;
  LocalScrollBox: TScrollBox;
  ScrollPanel: TPanel;
begin

  Page := TPageControl.Create(Self);
  Page.Align := alClient;


  LocalTab := Page.AddTabSheet;
  LocalTab.Caption := 'Tab';

   // Create the ScrollBox and put it in the Tab
   LocalScrollBox := TScrollBox.Create(LocalTab);
   LocalScrollBox.Parent := LocalTab;
   LocalScrollBox.Align := alClient;
   LocalScrollBox.BorderSpacing.Around := 4;
   LocalScrollBox.AutoScroll := True;

   LocalScrollBox.VertScrollBar.Tracking := True;
   LocalScrollBox.HorzScrollBar.Tracking := True;

   ScrollPanel := TLccPanel.Create(LocalScrollBox);
   ScrollPanel.Anchors := [akLeft, akRight, akTop];
   ScrollPanel.Left := 0;
   ScrollPanel.Width := LocalScrollBox.Width;
   ScrollPanel.Height := 20000;                // TODO: TEMP
   ScrollPanel.Parent := LocalScrollBox;

   Page.Parent := Panel2;

  for i := 0 to 5 do
  begin
    L := TLabel.Create(Self);
    L.Caption := 'Caption: ' + IntToStr(i);
    L.Align := alTop;
    L.Top := 40000;
    L.Parent := ScrollPanel
  end;

end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Parser := TLccCdiParser.Create(Self);
end;

procedure TForm1.Panel4Resize(Sender: TObject);
begin

end;

end.

