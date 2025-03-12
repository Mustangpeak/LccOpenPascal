unit unit_main;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ComCtrls,
  ExtCtrls, contnrs, LazFileUtils, LCLIntf;

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

  end;

  { TForm_hex2dshex }

  TForm_hex2dshex = class(TForm)
    Button_generate_dshex: TButton;
    Button_analyze: TButton;
    ButtonOpen: TButton;
    Button_generate_nodeid_dshex: TButton;
    CheckBox_new_version_only: TCheckBox;
    Edit_checksum: TEdit;
    Edit_checksum_page: TEdit;
    Edit_guid: TEdit;
    Edit_min_version: TEdit;
    Edit_nodeid: TEdit;
    Edit_min_address: TEdit;
    Edit_max_address: TEdit;
    Edit_erase_page_size: TEdit;
    Edit_nodeid_erasepage: TEdit;
    Label1: TLabel;
    Label10: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label_instruction_count: TLabel;
    Memo_hex: TMemo;
    OpenDialog: TOpenDialog;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    TreeView_addresses: TTreeView;
    TreeView_addresses_dshex: TTreeView;
    procedure Button_analyzeClick(Sender: TObject);
    procedure ButtonOpenClick(Sender: TObject);
    procedure Button_generate_dshexClick(Sender: TObject);
    procedure Button_generate_nodeid_dshexClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure Label6Click(Sender: TObject);
  private
    FFileName: string;
    FInstructionList: TObjectList;
    FInstructionErasePageList: TObjectList;
    FIncomingHexFile: TStringList;
    FOutgoing_dsHexFile: TStringList;

  protected

    procedure ProcessFile;
    procedure Generate_dsHexFile(Is_NodeID: Boolean);
    procedure LoadTreeview_with_dsHexFile;
    procedure Parse2dsHexFile;

    procedure ParseNodeId_dsHexFile;


  public

    // The string list that contains the original HEX file
    property IncomingHexFile: TStringList read FIncomingHexFile write FIncomingHexFile;

    // holds the instruction list that was pulled from the HEX file
    property InstructionList: TObjectList read FInstructionList write FInstructionList;

    // holds ErasePage blocks that contain the instructions (and addresses) for that particular block.  What the dsHEX is generated from
    property InstructionErasePageList: TObjectList read FInstructionErasePageList write FInstructionErasePageList;

    // The string list that contains the image of the text file that is the dsHEX file
    property Outgoing_dsHexFile: TStringList read FOutgoing_dsHexFile write FOutgoing_dsHexFile;

    // Incoming HEX filename
    property FileName: string read FFileName write FFileName;

  end;

var
  Form_hex2dshex: TForm_hex2dshex;

implementation

{$R *.lfm}

{ TInstruction }

constructor TInstruction.Create(AnAddress: DWord; AnInstruction: DWord);
begin
  FAddress := AnAddress;
  FInstruction := AnInstruction;
end;

{ TInstructionPage }

constructor TInstructionPage.Create;
begin
  FInstructions := TObjectList.Create(True);
end;

{ TForm_hex2dshex }

procedure TForm_hex2dshex.ButtonOpenClick(Sender: TObject);
begin
  if OpenDialog.Execute then
  begin
    IncomingHexFile.LoadFromFile(OpenDialog.FileName);
    Memo_hex.Lines.Text := IncomingHexFile.Text;
    FileName := OpenDialog.FileName;
    ProcessFile;
    Button_generate_dshex.Enabled := True;
    Button_generate_nodeid_dshex.Enabled := True;
    Edit_checksum.Text := '';
  end;
end;

procedure TForm_hex2dshex.Button_generate_dshexClick(Sender: TObject);
begin
  Parse2dsHexFile;
  Generate_dsHexFile(False);
  LoadTreeview_with_dsHexFile;
end;

procedure TForm_hex2dshex.Button_generate_nodeid_dshexClick(Sender: TObject);
begin
  ParseNodeId_dsHexFile;
  Generate_dsHexFile(True);
  LoadTreeview_with_dsHexFile;
end;

procedure TForm_hex2dshex.Button_analyzeClick(Sender: TObject);
var
  i: Integer;
  NextAddress: DWord;
begin

  NextAddress := 0;

  for i := 0 to InstructionList.Count - 1 do
  begin
    if (InstructionList[i] as TInstruction).Address <> NextAddress then
    begin

      Showmessage('Address Skip from: 0x' + IntToHex(NextAddress, 6) + ' to 0x' + IntToHex((InstructionList[i] as TInstruction).Address, 6));

    end;

    NextAddress := (InstructionList[i] as TInstruction).Address + 2;

  end;

end;

procedure TForm_hex2dshex.FormCreate(Sender: TObject);
begin
  FIncomingHexFile := TStringList.Create;
  FInstructionList := TObjectList.Create(True);
  FInstructionErasePageList := TObjectList.Create(True);
  FOutgoing_dsHexFile := TStringList.Create;
end;

procedure TForm_hex2dshex.Label6Click(Sender: TObject);
begin

end;

procedure TForm_hex2dshex.ProcessFile;

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
  TreeNode: TTreeNode;
  TotalInstructions: DWord;
begin
  TreeView_addresses.BeginUpdate;
  try
    TreeView_addresses.Items.Clear;
    Instructions := nil;
    SetLength(Instructions, 4);
    TotalInstructions := 0;
    AddressLo := 0;
    AddressHi := 0;
    StartingAddress := 0;
    InstructionList.Clear;

    for i := 0 to IncomingHexFile.Count - 1 do
    begin
       TempStr := IncomingHexFile[i];
       ByteCount := StrToInt('$' + IncomingHexFile[i][OFFSET_DATACOUNT] + IncomingHexFile[i][OFFSET_DATACOUNT+1]);
       RecordType := StrToInt('$' + IncomingHexFile[i][OFFSET_RECORDTYPE]+ IncomingHexFile[i][OFFSET_RECORDTYPE+1]);
       TempStr := '$' + IncomingHexFile[i][OFFSET_ADDRESS] + IncomingHexFile[i][OFFSET_ADDRESS+1] + IncomingHexFile[i][OFFSET_ADDRESS+2] + IncomingHexFile[i][OFFSET_ADDRESS+3];
       AddressLo := StrToInt(TempStr);
       InstructionCount := ByteCount div 4;


       if RecordType =  RECORDTYPE_DATA then
       begin
         TotalInstructions := TotalInstructions + InstructionCount;

         StartingAddress := ((AddressHi shl 16) or AddressLo) div 2;

         if ByteCount mod 4 <> 0 then
           ShowMessage('An odd count instruction was found at address: 0x' + IntToHex(StartingAddress));


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

       end;
       case RecordType of

        RECORDTYPE_DATA: begin

            TreeNode := TreeView_addresses.Items.Add(nil, '0x' + IntToHex( StartingAddress, 8));

            // Just for Reference
            TreeView_addresses.Items.AddChild(TreeNode, IncomingHexFile[i]);

            for j := 0 to InstructionCount - 1 do
               TreeView_addresses.Items.AddChild(TreeNode, '0x' + IntToHex( Instructions[j], 8));

          end;
        RECORDTYPE_EOF: begin

           Label_instruction_count.Caption := IntToStr(TotalInstructions) + '  (' + IntToStr(TotalInstructions * 4) + ' bytes)';

          end;
        RECORDTYPE_EXTENDED_SEGMENT_ADDRESS: begin
            ShowMessage('UnSupported Record Type Error');
            Exit;
          end;
        RECORDTYPE_START_SEGMENT_ADDRESS: begin
           ShowMessage('UnSupported Record Type Error');
           Exit;
          end;
        RECORDTYPE_EXTENDED_LINEAR_ADDRESS: begin
           TempStr := IncomingHexFile[i];
           AddressHi := StrToInt('$' + IncomingHexFile[i][OFFSET_DATA] + IncomingHexFile[i][OFFSET_DATA+1] + IncomingHexFile[i][OFFSET_DATA+2] + IncomingHexFile[i][OFFSET_DATA+3]);

          end;
        RECORDTYPE_START_LINEAR_ADDRESS: begin
           ShowMessage('UnSupported Record Type Error');
           Exit;
          end;

        else
          ShowMessage('Unknown Record Type Error');
          Exit;
       end;

     end;

  finally
    TreeView_addresses.EndUpdate;
  end;
end;

procedure TForm_hex2dshex.Generate_dsHexFile(Is_NodeID: Boolean);
var
  Block: String;
  i, j: Integer;
  InstructionPage: TInstructionPage;
  Instruction: TInstruction;
begin

    //  For test, easier to see the output in the file
  //Outgoing_dsHexFile.Clear;
  //for i := 0 to InstructionErasePageList.Count - 1 do
  //begin
  //  InstructionPage := (InstructionErasePageList[i] as TInstructionPage);
  //
  //  Outgoing_dsHexFile.Add( ':' + IntToHex(InstructionPage.Address, 8));
  //
  //  Outgoing_dsHexFile.Add( IntToHex(InstructionPage.Instructions.Count, 8));
  //
  //  for j := 0 to InstructionPage.Instructions.Count - 1 do
  //  begin
  //    Instruction := (InstructionPage.Instructions[j] as TInstruction);
  //    Outgoing_dsHexFile.Add(  IntToHex( Instruction.Instruction, 8));
  //  end;
  //end;



  Outgoing_dsHexFile.Clear;

  // Add the first line which is an identifiction line
  // Format:
  // :{GUID Length: DWORD}{GUID String: 64 max}{Check Version: 1}{Version:
  Block := ';' + IntToHex(Length(Edit_guid.Text), 8) + Edit_guid.Text;
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

  if Is_NodeID then
    Block := ExtractFileNameWithoutExt( FileName) + '.nodeid.dshex'
  else begin

    // Add EOF flag
    Outgoing_dsHexFile.Add(':FFFFFFFF');
    Block := ExtractFileNameWithoutExt( FileName) + '.dshex';
  end;


  if FileExists(Block) then
    DeleteFile(Block);

  Outgoing_dsHexFile.SaveToFile(Block);

  OpenDocument(Block);


end;

procedure TForm_hex2dshex.LoadTreeview_with_dsHexFile;
var
  i, j: Integer;
  InstructionPage: TInstructionPage;
  Instruction: TInstruction;
  TreeNode: TTreeNode;
begin

  TreeView_addresses_dshex.BeginUpdate;
  try
    TreeView_addresses_dshex.Items.Clear;

    for i := 0 to InstructionErasePageList.Count - 1 do
    begin
      InstructionPage := (InstructionErasePageList[i] as TInstructionPage);

        TreeNode := TreeView_addresses_dshex.Items.Add(nil, '0x' + IntToHex(InstructionPage.Address, 6) + '; Instruction Count: ' + IntToStr(InstructionPage.Instructions.Count));
        for j := 0 to InstructionPage.Instructions.Count - 1 do
        begin
          Instruction := (InstructionPage.Instructions[j] as TInstruction);
          TreeView_addresses_dshex.Items.AddChild(TreeNode, 'Address: 0x' +  IntToHex(Instruction.Address, 6) + ' - 0x' + IntToHex(Instruction.Instruction, 6));
        end;

    end;

  finally
    TreeView_addresses_dshex.EndUpdate;
  end;

end;

procedure TForm_hex2dshex.Parse2dsHexFile;
var
  i, j: Integer;

  InstructionPage, InstructionPageTemp: TInstructionPage;
  Instruction, InstructionTemp: TInstruction;
  MinValidAddressRange, MaxValidAddressRange, ErasePageSizeInAddresses, ErasePageSizesInInstructions, ErasePageCount, ErasePage, StartErasePageInAddresses, InstructionOffset: Integer;
  CheckSum: DWord;

begin

  InstructionErasePageList.Clear;

  MinValidAddressRange := StrToInt(Edit_min_address.Text);
  MaxValidAddressRange := StrToInt(Edit_max_address.Text);
  ErasePageSizesInInstructions := StrToInt(Edit_erase_page_size.Text);
  ErasePageSizeInAddresses := ErasePageSizesInInstructions * 2;
  ErasePageCount := (MaxValidAddressRange - MinValidAddressRange) div ErasePageSizeInAddresses;
  StartErasePageInAddresses := MinValidAddressRange div ErasePageSizeInAddresses;

  if (MaxValidAddressRange - MinValidAddressRange) mod ErasePageSizeInAddresses <> 0 then
  begin
    ShowMessage('The (maxAddress - minAddress) divided by the number of Addresses Per Page is not an integer value');
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
      end else
         ShowMessage('Error: Address Mismatch in Parse2HexFile');
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

  Edit_checksum.Text := '0x' + IntToHex(CheckSum, 8);

  // Add a last page where the addresses and checksum will be written
  InstructionPage := TInstructionPage.Create;
  InstructionPage.Address := StrToInt(Edit_checksum_page.Text);

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

procedure TForm_hex2dshex.ParseNodeId_dsHexFile;
var
  InstructionErasePage: TInstructionPage;
  NodeID: UInt64;

begin
  InstructionErasePageList.Clear;

  InstructionErasePage := TInstructionPage.Create;
  InstructionErasePage.Address := StrToInt(Edit_nodeid_erasepage.Text);

  NodeID := StrToUInt64(Edit_nodeid.Text);

  InstructionErasePage.Instructions.Add(TInstruction.Create(InstructionErasePage.Address, (NodeID >> 24) AND $00FFFFFF));
  InstructionErasePage.Instructions.Add(TInstruction.Create(InstructionErasePage.Address + 2, NodeID AND $00FFFFFF));
  InstructionErasePage.ValidInstructions := 2;

  InstructionErasePageList.Add(InstructionErasePage);
end;

end.

