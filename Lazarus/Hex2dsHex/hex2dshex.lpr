program hex2dshex;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}
  cthreads,
  {$ENDIF}
  Classes, SysUtils, CustApp,
  { you can add units after this }
  contnrs;


// Command Line for the Turnout Boss
//
// -f /Users/jimkueneman/Documents/OpenLcbCLib/src/applications/dsPIC/TurnoutBOSS.X/dist/default/production/TurnoutBOSS.X.production.hex -m 0xB000 -x 0x55000 -e 1024 -c 0x54800 -k UniqueIdentifierForLoad

type

  { TInstruction }

  TInstruction = class
  private
    FAddress: DWord;
    FInstruction: DWord;
    public
      property Address: DWord read FAddress write FAddress;
      property Instruction: DWord read FInstruction write FInstruction;

      constructor Create(AnAddress: DWord; AnInstruction: DWord);
  end;

  { TInstructionPage }

  TInstructionPage = class

  private
    FAddress: DWord;
    FInstructions: TObjectList;
    FValidInstructions: Integer;
  public
     property Address: DWord read FAddress write FAddress;
     property Instructions: TObjectList read FInstructions write FInstructions;
     property ValidInstructions: Integer read FValidInstructions write FValidInstructions;

     constructor Create; reintroduce;
     destructor Destroy; override;

  end;

  { TdsHexConverter }

  TdsHexConverter = class(TCustomApplication)
  private
    FCheckSumPageAddress: DWord;
    FFileName: string;
    FIncomingHexFile: TStringList;
    FInstructionErasePageList: TObjectList;
    FInstructionList: TObjectList;
    FMinAddress: DWord;
    FMaxAddress: DWord;
    FOutgoing_dsHexFile: TStringList;
    FPageSizeInAddresses: DWord;
    FUniqueID: string;
  protected
    procedure DoRun; override;
  public
    property FileName: string read FFileName write FFileName;

    // The string list that contains the original HEX file
    property IncomingHexFile: TStringList read FIncomingHexFile write FIncomingHexFile;

    // holds the instruction list that was pulled from the HEX file
    property InstructionList: TObjectList read FInstructionList write FInstructionList;

    // holds ErasePage blocks that contain the instructions (and addresses) for that particular block.  What the dsHEX is generated from
    property InstructionErasePageList: TObjectList read FInstructionErasePageList write FInstructionErasePageList;

    // The string list that contains the image of the text file that is the dsHEX file
    property Outgoing_dsHexFile: TStringList read FOutgoing_dsHexFile write FOutgoing_dsHexFile;

    property MinAddress: DWord read FMinAddress write FMinAddress;
    property MaxAddress: DWord read FMaxAddress write FMaxAddress;
    property PageSizeInAddresses: DWord read FPageSizeInAddresses write FPageSizeInAddresses;
    property CheckSumPageAddress: DWord read FCheckSumPageAddress write FCheckSumPageAddress;
    property UniqueID: string read FUniqueID write FUniqueID;

    procedure ProcessFile;
    procedure ParseTo_dsHexFile;
    procedure Generate_dsHexFile;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

constructor TInstructionPage.Create;
begin
  FInstructions := TObjectList.Create(True);
end;

destructor TInstructionPage.Destroy;
begin
  Instructions.Free;
end;

constructor TInstruction.Create(AnAddress: DWord; AnInstruction: DWord);
begin
  FAddress := AnAddress;
  FInstruction := AnInstruction;
end;

{ TdsHexConverter }

procedure TdsHexConverter.DoRun;
var
  ErrorMsg: String;
begin

  // quick check parameters
  ErrorMsg := CheckOptions('hfmxeck:', 'help file min max checksum key:');

  if ErrorMsg <> '' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h', 'help') then
  begin
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('m', 'min') then
      MinAddress := StrToInt(getOptionValue('m', 'min'))
  else begin
    Writeln('Use the ''m'' option to specify the minimum address to include in the output; example -m 0xAF00');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('x', 'max') then
     MaxAddress := StrToInt(getOptionValue('x', 'max'))
  else begin
    Writeln('Use the ''x'' option to specify the minimum address to include in the output; example -x 0x00FFAAAA');
    WriteHelp;
    Terminate;
    Exit;
  end;


  if HasOption('e', 'page') then
    PageSizeInAddresses := StrToInt(getOptionValue('e', 'erasepage'))  * 2 // This is in Instructions and we need Addresses (2 addresses per instruction)
  else begin
     Writeln('Use the ''e'' option to the number of INSTRUCTIONS in an erase page; example -e 1024');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('c', 'checksum') then
    CheckSumPageAddress := StrToInt(getOptionValue('c', 'checksum'))
  else begin
    Writeln('Use the ''c'' option to position the checksum in a page of memory; example -c 0x055000');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('k', 'key') then
    UniqueID := getOptionValue('k', 'key')
  else begin
    Writeln('Use the ''k'' option to define a unique string that is used to ensure the file is correct for the application; example -k MyUniqueKeyString');
    WriteHelp;
    Terminate;
    Exit;
  end;

  if HasOption('f', 'file') then
  begin

    writeln('file: ' + getOptionValue('f', 'file'));

    FileName := getOptionValue('f', 'file');
    if (FileExists(FileName)) then
    begin
      ProcessFile;
      ParseTo_dsHexFile;
      Generate_dsHexFile;
    end else
      WriteHelp;
      Terminate;
      Exit;
  end;

  // stop program loop
  Terminate;
end;

procedure TdsHexConverter.ProcessFile;

const
  OFFSET_COLON = 1;
  OFFSET_DATACOUNT = 2;
  OFFSET_ADDRESS = 4;
  OFFSET_RECORDTYPE = 8;
  OFFSET_DATA = 10;

  RECORDTYPE_DATA = 0;
  RECORDTYPE_EOF = 1;
  RECORDTYPE_EXTENDED_SEGMENT_ADDRESS = 2;
  RECORDTYPE_START_SEGMENT_ADDRESS = 3;
  RECORDTYPE_EXTENDED_LINEAR_ADDRESS = 4;
  RECORDTYPE_START_LINEAR_ADDRESS = 5;

var
  i, j: Integer;
  ByteCount: Integer;
  RecordType: Byte;
  AddressLo: DWord;
  AddressHi: DWord;
  StartingAddress: DWord;
  FixedInstruction: DWord;
  Temp1, Temp2: Word;
  TempStr: String;
  Instructions: array of DWord;
  InstructionCount: Integer;
  TempOffset: Integer;
  TotalInstructions: DWord;
begin

  Instructions := nil;
  SetLength(Instructions, 4);
  TotalInstructions := 0;
  AddressLo := 0;
  AddressHi := 0;
  StartingAddress := 0;
  InstructionList.Clear;

  IncomingHexFile.LoadFromFile(FileName);

  for i := 0 to IncomingHexFile.Count - 1 do
  begin
     TempStr := IncomingHexFile[i];
     ByteCount := StrToInt('$' + IncomingHexFile[i][OFFSET_DATACOUNT] + IncomingHexFile[i][OFFSET_DATACOUNT+1]);
     RecordType := StrToInt('$' + IncomingHexFile[i][OFFSET_RECORDTYPE]+ IncomingHexFile[i][OFFSET_RECORDTYPE+1]);
     TempStr := '$' + IncomingHexFile[i][OFFSET_ADDRESS] + IncomingHexFile[i][OFFSET_ADDRESS+1] + IncomingHexFile[i][OFFSET_ADDRESS+2] + IncomingHexFile[i][OFFSET_ADDRESS+3];
     AddressLo := StrToInt(TempStr);
     InstructionCount := ByteCount div 4;

     TotalInstructions := TotalInstructions + InstructionCount;

     StartingAddress := ((AddressHi shl 16) or AddressLo) div 2;

     for j := 0 to InstructionCount - 1 do
     begin
       TempOffset := j * 8;
       TempStr := '$' + IncomingHexFile[i][OFFSET_DATA+0+TempOffset] + IncomingHexFile[i][OFFSET_DATA+1+TempOffset] +
                        IncomingHexFile[i][OFFSET_DATA+2+TempOffset] + IncomingHexFile[i][OFFSET_DATA+3+TempOffset] +
                        IncomingHexFile[i][OFFSET_DATA+4+TempOffset] + IncomingHexFile[i][OFFSET_DATA+5+TempOffset] +
                        IncomingHexFile[i][OFFSET_DATA+6+TempOffset] + IncomingHexFile[i][OFFSET_DATA+7+TempOffset];
       Instructions[j] := StrToInt(TempStr);

       Temp1 := Hi(Instructions[j]);
       Temp2 := Lo(Instructions[j]);

       Temp1 := (Temp1 shr 8) or (Temp1 shl 8);
       Temp2 := (Temp2 shr 8) or (Temp2 shl 8);
       FixedInstruction := (Temp2 shl 16) or Temp1;
       InstructionList.Add( TInstruction.Create(StartingAddress + (j * 2), FixedInstruction));

     end;

     case RecordType of

      RECORDTYPE_DATA: begin
        end;
      RECORDTYPE_EOF: begin
        end;
      RECORDTYPE_EXTENDED_SEGMENT_ADDRESS: begin
          Exit;
        end;
      RECORDTYPE_START_SEGMENT_ADDRESS: begin
         Exit;
        end;
      RECORDTYPE_EXTENDED_LINEAR_ADDRESS: begin
         TempStr := IncomingHexFile[i];
         AddressHi := StrToInt('$' + IncomingHexFile[i][OFFSET_DATA] + IncomingHexFile[i][OFFSET_DATA+1] + IncomingHexFile[i][OFFSET_DATA+2] + IncomingHexFile[i][OFFSET_DATA+3]);
        end;
      RECORDTYPE_START_LINEAR_ADDRESS: begin
         Exit;
        end;
      else
        Exit;
     end;

   end;

end;

procedure TdsHexConverter.ParseTo_dsHexFile;

var
  i, j: Integer;

  InstructionPage, InstructionPageTemp: TInstructionPage;
  Instruction, InstructionTemp: TInstruction;
  MinValidAddressRange, MaxValidAddressRange, ErasePageSizeInAddresses, ErasePageSizesInInstructions, ErasePageCount, ErasePage, StartErasePageInAddresses, InstructionOffset: Integer;
  CheckSum: DWord;

begin

  InstructionErasePageList.Clear;

  MinValidAddressRange := MinAddress;
  MaxValidAddressRange := MaxAddress;

  ErasePageSizesInInstructions := PageSizeInAddresses div 2;
  ErasePageSizeInAddresses := PageSizeInAddresses;
  ErasePageCount := (MaxValidAddressRange - MinValidAddressRange) div ErasePageSizeInAddresses;
  StartErasePageInAddresses := MinValidAddressRange div ErasePageSizeInAddresses;

  if (MaxValidAddressRange - MinValidAddressRange) mod ErasePageSizeInAddresses <> 0 then
  begin
    WriteLn('Mininum and Maximum Addresses must be divisible by the PageSize.');
    Exit;
  end;

  // Create Pages and Instruction object for the entire possible space
  for i := 0 to ErasePageCount - 1 do
  begin
     InstructionPage := TInstructionPage.Create;
     InstructionPage.Address := MinValidAddressRange + (i * ErasePageSizeInAddresses);
     InstructionErasePageList.Add(InstructionPage);
     for j := 0 to (ErasePageSizesInInstructions) - 1 do
     begin
       Instruction := TInstruction.Create(InstructionPage.Address + (j * 2), $00FFFFFF);
       InstructionPage.Instructions.Add(Instruction);
     end;
  end;

  // Now copy the Instruction image to the correct Instruction Page
  for i := 0 to InstructionList.Count - 1 do
  begin

    Instruction := InstructionList[i] as TInstruction;

    ErasePage := Instruction.Address div ErasePageSizeInAddresses;
    if (ErasePage >= StartErasePageInAddresses) and (ErasePage < InstructionErasePageList.Count) then
    begin
      InstructionPage := InstructionErasePageList[ErasePage - StartErasePageInAddresses] as TInstructionPage;

      InstructionOffset := (Instruction.Address mod ErasePageSizeInAddresses) div 2;
      InstructionTemp := InstructionPage.Instructions[InstructionOffset] as TInstruction;

      if InstructionTemp.Address = Instruction.Address then
      begin
         InstructionTemp.Instruction := Instruction.Instruction;
         InstructionPage.ValidInstructions := InstructionPage.ValidInstructions + 1;
      end else begin
         Writeln('Error: Address Mismatch in Parse2HexFile');
         Exit;
      end;
    end;
  end;

  // Strip out anything that has no valid instructions
  for i := InstructionErasePageList.Count - 1 downto 0 do
  begin
     InstructionPage := InstructionErasePageList[i] as TInstructionPage;
     if InstructionPage.ValidInstructions = 0 then
       InstructionErasePageList.Delete(i);
  end;

  if InstructionErasePageList.Count = 0 then
    Exit;

  // Checksum
  CheckSum := 0;
  for i := 0 to InstructionErasePageList.Count - 1 do
  begin
     InstructionPage := InstructionErasePageList[i] as TInstructionPage;
     for j := 0 to InstructionPage.Instructions.Count - 1 do
     begin
        Instruction := InstructionPage.Instructions[j] as TInstruction;
        CheckSum := CheckSum + ((Instruction.Instruction shr 16) and $0FF) + ((Instruction.Instruction shr 8) and $0FF) + ((Instruction.Instruction shr 0) and $0FF);
     end;
  end;


  // Add a last page where the addresses and checksum will be written
  InstructionPage := TInstructionPage.Create;
  InstructionPage.Address := CheckSumPageAddress;

  // Start Address
  InstructionPageTemp := InstructionErasePageList[0] as TInstructionPage;
  Instruction := TInstruction.Create(InstructionPage.Address, InstructionPageTemp.Address);
  InstructionPage.Instructions.Add(Instruction);

  InstructionPageTemp := (InstructionErasePageList[InstructionErasePageList.Count-1] as TInstructionPage);
  InstructionTemp := (InstructionPageTemp.Instructions[InstructionPageTemp.Instructions.Count - 1] as TInstruction);
  Instruction := TInstruction.Create(InstructionPage.Address + 2, InstructionTemp.Address);
  InstructionPage.Instructions.Add(Instruction);


  Instruction := TInstruction.Create(InstructionPage.Address + 4, CheckSum AND $00FFFFFF);   // 24 bits
  InstructionPage.Instructions.Add(Instruction);

  Instruction := TInstruction.Create(InstructionPage.Address + 4, $00FFFFFF);   // make it an even number of instructions
  InstructionPage.Instructions.Add(Instruction);

  InstructionErasePageList.Add(InstructionPage);

end;

procedure TdsHexConverter.Generate_dsHexFile;
var
  Block: String;
  i, j: Integer;
  InstructionPage: TInstructionPage;
  Instruction: TInstruction;
begin

  // Add the first line which is an identifiction line
  // Format:
  // :{GUID Length: DWORD}{GUID String: 64 max}{Check Version: 1}{Version:

  Outgoing_dsHexFile.Clear;

  Block := ';' + IntToHex(Length(UniqueID), 8) + UniqueID;
  Outgoing_dsHexFile.Add(Block);

  for i := 0 to InstructionErasePageList.Count - 1 do
  begin
    InstructionPage := (InstructionErasePageList[i] as TInstructionPage);

    Block := ':' + IntToHex(InstructionPage.Address, 8);

    Block := Block + IntToHex(InstructionPage.Instructions.Count, 8);
    for j := 0 to InstructionPage.Instructions.Count - 1 do
    begin
      Instruction := (InstructionPage.Instructions[j] as TInstruction);
      Block := Block + IntToHex( Instruction.Instruction, 8);
    end;

    Outgoing_dsHexFile.Add(Block);

  end;

  // Add EOF flag
  Outgoing_dsHexFile.Add(':FFFFFFFF');

  Block := FileName + '.dshex';

  if FileExists(Block) then
    DeleteFile(Block);

  Outgoing_dsHexFile.SaveToFile(Block);

end;


destructor TdsHexConverter.Destroy;
begin
  inherited Destroy;
  IncomingHexFile.Free;
  InstructionList.Free;
  InstructionErasePageList.Free;
  Outgoing_dsHexFile.Free;
end;

constructor TdsHexConverter.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
  FIncomingHexFile := TStringList.Create;
  FInstructionList := TObjectList.Create(True);
  FInstructionErasePageList := TObjectList.Create(True);
  FOutgoing_dsHexFile := TStringList.Create;
end;


procedure TdsHexConverter.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ', ExeName, ' -h');
end;

var
  Application: TdsHexConverter;
begin
  Application:=TdsHexConverter.Create(nil);
  Application.Title:='Hex to dsHex Converter';
  Application.Run;
  Application.Free;
end.

