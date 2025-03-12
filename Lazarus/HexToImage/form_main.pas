unit form_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls ;

type

  { TForm1 }

  TForm1 = class(TForm)
    ButtonAnalyze: TButton;
    Button_open_hex: TButton;
    Memo1: TMemo;
    OpenDialog: TOpenDialog;
    procedure ButtonAnalyzeClick(Sender: TObject);
    procedure Button_open_hexClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    FHexFile: TStringList;

  public

    property HexFile: TStringList read FHexFile write FHexFile;

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Button_open_hexClick(Sender: TObject);
var
  TargetFile: Text;
  Line: String;

begin
  if OpenDialog.Execute = True then
  begin
    HexFile.BeginUpdate;
    try
      HexFile.Clear;
      AssignFile(TargetFile, OpenDialog.FileName);
      Reset(TargetFile);
      try
        while not EOF(TargetFile) do
        begin
          ReadLn(TargetFile, Line);
          HexFile.Add(Line);
        end;
      finally
        CloseFile(TargetFile);
      end;
    finally
      HexFile.EndUpdate;
    end;
  end;
end;

procedure TForm1.ButtonAnalyzeClick(Sender: TObject);
var
  i: Integer;
  HexCode: Byte;
  ByteCount: Byte;
  AddressLowBits, AddressHighBits: Word;
  Address, CalculatedNextAddress: DWord;
  ProgramSize: DWord;

  eofFound: Boolean;
begin
  Memo1.Lines.BeginUpdate;
  try
    Memo1.Clear;
 //   Memo1.Text := HexFile.Text;

    AddressHighBits := 0;
    AddressLowBits := 0;
    CalculatedNextAddress := 0;
    ProgramSize := 0;
    eofFound := False;

    for i := 0 to HexFile.Count - 1 do
    begin
      HexCode := StrToInt('$' + HexFile.Strings[i][8] + HexFile.Strings[i][9]);
      ByteCount := StrToInt('$' + HexFile.Strings[i][2] + HexFile.Strings[i][3]);
      AddressLowBits := StrToInt('$' + HexFile.Strings[i][4] + HexFile.Strings[i][5]+ HexFile.Strings[i][6] + HexFile.Strings[i][7]);

      case HexCode of
        00: begin
           Address := ((AddressHighBits << 16) or AddressLowBits) div 2; // because the dspic is 24 bits and they send it like 32 bits.
           if Address <> CalculatedNextAddress then
             Memo1.Lines.Add('Not a match: Address: 0x' + IntToHex(Address, 8));

           CalculatedNextAddress := Address + (ByteCount div 2);

           ProgramSize := ProgramSize + ByteCount;
     //      Memo1.Lines.Add('Address: 0x' + IntToHex(Address, 8)  + '  Data Count: ' + IntToStr(ByteCount));
        end;
        01: begin
          eofFound := True;
          Memo1.Lines.Add('Program Size: ' + IntToStr(ProgramSize));
        end;
        02: begin
          ShowMessage('02');
        end;
        03: begin
           ShowMessage('03');
        end;
        04: begin
           AddressHighBits := StrToInt('$' + HexFile.Strings[i][10] + HexFile.Strings[i][11]+ HexFile.Strings[i][12] + HexFile.Strings[i][13]);
        end;
        05: begin
           ShowMessage('05');
        end;
      end;
    end;

  finally
    if not eofFound then
      ShowMessage('Did not find the end of file flag in the hex');
    Memo1.Lines.EndUpdate;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
   HexFile := TStringList.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  HexFile.Free;
end;

end.

