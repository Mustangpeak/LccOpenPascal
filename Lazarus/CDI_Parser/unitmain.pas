unit unitMain;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  lcc_cdi_parser, lcc_xmlutilities;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    OpenDialog1: TOpenDialog;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    Panel4: TPanel;
    ScrollBox1: TScrollBox;
    Splitter1: TSplitter;
    procedure Button1Click(Sender: TObject);
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
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Parser := TLccCdiParser.Create(Self);
end;

procedure TForm1.Panel4Resize(Sender: TObject);
begin
  Label4.Caption := IntToSTr(Label3.Height);
end;

end.

