unit lcc_protocol_utilities;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  {$IFDEF LCC_FPC}
    contnrs,
  {$ELSE}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  lcc_defines,
  lcc_utilities,
  lcc_node_messages;

const
  MAX_SNIP_MANUFACTURER_STRINGS = 4;
  TSnipManufacturerStrings: array[0..MAX_SNIP_MANUFACTURER_STRINGS-1] of String = ('', '', '', '');

type

  { TNodeProtocolBase }

  TNodeProtocolBase = class(TObject)
  private
    FWorkerMessage: TLccMessage;
  protected
    FValid: Boolean;
    procedure SetValid(AValue: Boolean); virtual;  // Just so it can be overridden for special behavior
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    procedure InitializeOutMessageNodeIDs(InMessage, OutMessage: TLccMessage);
  public
    property Valid: Boolean read FValid write SetValid;

    constructor Create; virtual;
    destructor Destroy; override;

    function DatagramReadRequest(LccMessage: TLccMessage; OutMessage: TLccMessage; AStream: TStream; {AutoGrow: Boolean;} MemOffset: Int64 = 0): Word; virtual;
    function DatagramWriteRequest(LccMessage: TLccMessage; AStream: TStream; AutoGrow: Boolean; MemOffset: Int64 = 0): Word; virtual;

    procedure CopyTo(ANodeProtocol: TNodeProtocolBase); virtual;
  end;

  { TProtocolSupportedProtocols }

  TProtocolSupportedProtocols = class(TNodeProtocolBase)
  private
    FAbbreviatedConfigurationDefinitionInfo: Boolean;
    FConfigurationDefinitionInfo: Boolean;
    FDatagram: Boolean;
    FDisplay: Boolean;
    FEventExchange: Boolean;
    FSimpleNode: Boolean;
    FTractionFunctionDefinitionInfo: Boolean;
    FIdentification: Boolean;
    FMemConfig: Boolean;
    FRemoteButton: Boolean;
    FReservation: Boolean;
    FSimpleNodeInfo: Boolean;
    FStream: Boolean;
    FTeach_Learn: Boolean;
    FTractionControl: Boolean;
    FTractionSimpleTrainNodeInfo: Boolean;
    FTractionFunctionConfiguration: Boolean;
    FFirmwareUpgradeActive: Boolean;
    FFirmwareUpgrade: Boolean;

  public
    Flags: TLccSupportedProtocolArray;

    procedure DecodeFlags;
    function EncodeFlags: TLccSupportedProtocolArray;

    property Datagram: Boolean read FDatagram write FDatagram;
    property Stream: Boolean read FStream write FStream;
    property MemConfig: Boolean read FMemConfig write FMemConfig;
    property Reservation: Boolean read FReservation write FReservation;
    property EventExchange: Boolean read FEventExchange write FEventExchange;
    property Identification: Boolean read FIdentification write FIdentification;
    property Teach_Learn: Boolean read FTeach_Learn write FTeach_Learn;
    property RemoteButton: Boolean read FRemoteButton write FRemoteButton;
    property AbbreviatedConfigurationDefinitionInfo: Boolean read FAbbreviatedConfigurationDefinitionInfo write FAbbreviatedConfigurationDefinitionInfo;
    property Display: Boolean read FDisplay write FDisplay;
    property SimpleNodeInfo: Boolean read FSimpleNodeInfo write FSimpleNodeInfo;
    property ConfigurationDefinitionInfo: Boolean read FConfigurationDefinitionInfo write FConfigurationDefinitionInfo;
    property TractionControl: Boolean read FTractionControl write FTractionControl;
    property TractionSimpleTrainNodeInfo: Boolean read FTractionSimpleTrainNodeInfo write FTractionSimpleTrainNodeInfo;
    property TractionFunctionDefinitionInfo: Boolean read FTractionFunctionDefinitionInfo write FTractionFunctionDefinitionInfo;
    property TractionFunctionConfiguration: Boolean read FTractionFunctionConfiguration write FTractionFunctionConfiguration;
    property FirmwareUpgrade: Boolean read FFirmwareUpgrade write FFirmwareUpgrade;
    property FirmwareUpgradeActive: Boolean read FFirmwareUpgradeActive write FFirmwareUpgradeActive;
    property SimpleNode: Boolean read FSimpleNode write FSimpleNode;

    procedure LoadFromLccMessage(SourceLccMessage: TLccMessage);
    function SupportedToStr: String;

    procedure CopyTo(ANodeProtocol: TNodeProtocolBase); override;
  end;


    { TLccEvent }

  TLccEvent = class(TObject)
  private
    FID: TEventID;
    FIDStr: string;
    FState: TEventState;
    procedure SetID(AValue: TEventID);
    procedure SetIDStr(AValue: string);
  public
    property ID: TEventID read FID write SetID;
    property IDStr: string read FIDStr write SetIDStr;
    property State: TEventState read FState write FState;
  end;

  { TLccEventAutoGenerate }

  TLccEventAutoGenerate = class(TObject)
  private
    FCount: Integer;
    FDefaultState: TEventState;
    FStartIndex: Integer;
    procedure SetCount(AValue: Integer);
    procedure SetDefaultState(AValue: TEventState);
    procedure SetStartIndex(AValue: Integer);
  public
    property Count: Integer read FCount write SetCount;
    property DefaultState: TEventState read FDefaultState write SetDefaultState;
    property StartIndex: Integer read FStartIndex write SetStartIndex;
  end;

  { TProtocolEvents }

  TProtocolEvents = class(TNodeProtocolBase)
  private
    FAutoGenerate: TLccEventAutoGenerate;
    FEventList: TObjectList;
    function GetCount: Integer;
    function GetEvent(Index: Integer): TLccEvent;
    function GetEventIDAsStr(Index: Integer): String;
  protected
    property EventList: TObjectList read FEventList write FEventList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Add(Event: TEventID; State: TEventState);
    procedure Clear;
    function Supports(Event: TEventID): TLccEvent;

    property AutoGenerate: TLccEventAutoGenerate read FAutoGenerate write FAutoGenerate;
    property Count: Integer read GetCount;
    property Event[Index: Integer]: TLccEvent read GetEvent; default;
    property EventIDAsStr[Index: Integer]: String read GetEventIDAsStr;
  end;

  { TConfigMemAddressSpaceInfoObject }

  TConfigMemAddressSpaceInfoObject = class
  private
    FHighAddress: DWord;
    FIsReadOnly: Boolean;
    FImpliedZeroLowAddress: Boolean;
    FLowAddress: DWord;
    FIsPresent: Boolean;
    FAddressSpace: Byte;
  public
    property AddressSpace: Byte read FAddressSpace;
    property IsPresent: Boolean read FIsPresent;
    property IsReadOnly: Boolean read FIsReadOnly;
    property ImpliedZeroLowAddress: Boolean read FImpliedZeroLowAddress;
    property LowAddress: DWord read FLowAddress;
    property HighAddress: DWord read FHighAddress;

    procedure CopyTo(AnInfoObject: TConfigMemAddressSpaceInfoObject);
    function Clone: TConfigMemAddressSpaceInfoObject;
  end;

  { TProtocolMemoryInfo }

  TProtocolMemoryInfo = class(TNodeProtocolBase)
  private
    FList: TList;
    function GetAddressSpace(Index: Integer): TConfigMemAddressSpaceInfoObject;
    function GetCount: Integer;
  protected
     property List: TList read FList write FList;

  public
    property AddressSpace[Index: Integer]: TConfigMemAddressSpaceInfoObject read GetAddressSpace; default;
    property Count: Integer read GetCount;

    constructor Create; override;
    destructor Destroy; override;
    procedure Add(_Space: Byte; _IsPresent, _IsReadOnly, _ImpliedZeroLowAddress: Boolean; _LowAddress, _HighAddress: DWord);
    procedure Clear;
    function FindByAddressSpace(Space: Byte): TConfigMemAddressSpaceInfoObject;
    procedure LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);

    procedure CopyTo(ANodeProtocol: TNodeProtocolBase); override;
  end;

  { TProtocolMemoryOptions }

  TProtocolMemoryOptions = class(TNodeProtocolBase)
  private
    FHighSpace: Byte;
    FLowSpace: Byte;
    FSupportACDIMfgRead: Boolean;
    FSupportACDIUserRead: Boolean;
    FSupportACDIUserWrite: Boolean;
    FUnAlignedReads: Boolean;
    FUnAlignedWrites: Boolean;
    FWriteArbitraryBytes: Boolean;
    FWriteLenFourBytes: Boolean;
    FWriteLenOneByte: Boolean;
    FWriteLenSixyFourBytes: Boolean;
    FWriteLenTwoBytes: Boolean;
    FWriteStream: Boolean;
    FWriteUnderMask: Boolean;
  public
    property WriteUnderMask: Boolean read FWriteUnderMask write FWriteUnderMask;
    property UnAlignedReads: Boolean read FUnAlignedReads write FUnAlignedReads;
    property UnAlignedWrites: Boolean read FUnAlignedWrites write FUnAlignedWrites;
    property SupportACDIMfgRead: Boolean read FSupportACDIMfgRead write FSupportACDIMfgRead;
    property SupportACDIUserRead: Boolean read FSupportACDIUserRead write FSupportACDIUserRead;
    property SupportACDIUserWrite: Boolean read FSupportACDIUserWrite write FSupportACDIUserWrite;
    property WriteLenOneByte: Boolean read FWriteLenOneByte write FWriteLenOneByte;
    property WriteLenTwoBytes: Boolean read FWriteLenTwoBytes write FWriteLenTwoBytes;
    property WriteLenFourBytes: Boolean read FWriteLenFourBytes write FWriteLenFourBytes;
    property WriteLenSixyFourBytes: Boolean read FWriteLenSixyFourBytes write FWriteLenSixyFourBytes;
    property WriteArbitraryBytes: Boolean read FWriteArbitraryBytes write FWriteArbitraryBytes;
    property WriteStream: Boolean read FWriteStream write FWriteStream;
    property HighSpace: Byte read FHighSpace write FHighSpace;
    property LowSpace: Byte read FLowSpace write FLowSpace;

    procedure LoadReply(LccMessage, OutMessage: TLccMessage);

    procedure CopyTo(ANodeProtocol: TNodeProtocolBase); override;
  end;

  { TProtocolSimpleNodeInfo }

  TProtocolSimpleNodeInfo = class(TNodeProtocolBase)
  private
    FHardwareVersion: string;
    FManufacturer: string;
    FModel: string;
    FSoftwareVersion: string;
    FUserDescription: string;
    FUserName: string;
    FUserVersion: Byte;
    FVersion: Byte;
  public
    property Version: Byte read FVersion write FVersion;
    property Manufacturer: string read FManufacturer write FManufacturer;
    property Model: string read FModel write FModel;
    property HardwareVersion: string read FHardwareVersion write FHardwareVersion;
    property SoftwareVersion: string read FSoftwareVersion write FSoftwareVersion;
    property UserVersion: Byte read FUserVersion write FUserVersion;
    property UserName: string read FUserName write FUserName;
    property UserDescription: string read FUserDescription write FUserDescription;
    property Valid: Boolean read FValid write FValid;

    function PackedFormat(StreamManufacturerInfo, StreamConfiguration: TStream): TLccDynamicByteArray;
    procedure LoadFromLccMessage(SourceLccMessage: TLccMessage);

    procedure CopyTo(ANodeProtocol: TNodeProtocolBase); override;
  end;


  { TProtocolMemoryAccess }

  TProtocolMemoryAccess = class(TNodeProtocolBase)
  public
  end;


implementation

{ TConfigMemAddressSpaceInfoObject }

procedure TConfigMemAddressSpaceInfoObject.CopyTo(
  AnInfoObject: TConfigMemAddressSpaceInfoObject);
begin
  AnInfoObject.FHighAddress := HighAddress;
  AnInfoObject.FIsReadOnly := IsReadOnly;
  AnInfoObject.FImpliedZeroLowAddress := ImpliedZeroLowAddress;
  AnInfoObject.FLowAddress := LowAddress;
  AnInfoObject.FIsPresent := IsPresent;
  AnInfoObject.FAddressSpace := AddressSpace;
end;

function TConfigMemAddressSpaceInfoObject.Clone: TConfigMemAddressSpaceInfoObject;
begin
  Result := TConfigMemAddressSpaceInfoObject.Create;
  CopyTo(Result);
end;

{ TNodeProtocolBase }

procedure TNodeProtocolBase.SetValid(AValue: Boolean);
begin
  if FValid=AValue then Exit;
  FValid:=AValue;
end;

procedure TNodeProtocolBase.InitializeOutMessageNodeIDs(InMessage, OutMessage: TLccMessage);
begin
   // Setup the OutMessage node info
  OutMessage.ZeroFields;
  OutMessage.SourceID := InMessage.DestID;
  OutMessage.DestID := InMessage.SourceID;
  OutMessage.SourceAlias := InMessage.DestAlias;
  OutMessage.DestAlias := InMessage.SourceAlias;
  OutMessage.MTI := MTI_DATAGRAM;
end;

function TNodeProtocolBase.DatagramWriteRequest(LccMessage: TLccMessage; AStream: TStream; AutoGrow: Boolean; MemOffset: Int64 = 0): Word;
var
 i: Integer;
 FirstDataByte, MemspaceDataSentCount : Integer;
 BytesToWrite, AddressStart: DWord;
 OldStreamSize: Int64;
 B: Byte;
begin
  // Assumption is this is a datagram message

  Result := S_OK;

  // First see if the memory space to work on is in byte 6 or part of the first byte
  // to determine where the first byte of real data is
  if LccMessage.DataArrayIndexer[1] and $03 = 0 then
    FirstDataByte := 7
  else
    FirstDataByte := 6;

  BytesToWrite := LccMessage.DataCount - FirstDataByte;
  AddressStart := LccMessage.ExtractDataBytesAsInt(2, 5);

  AddressStart := AddressStart + MemOffset;

  if BytesToWrite > 64 then
    Result := ERROR_CODE_PERMANENT_INVALID_ARGUMENTS
  else
  if BytesToWrite = 0 then
    Result := ERROR_CODE_PERMANENT_INVALID_ARGUMENTS
  else begin

    if AutoGrow then
    begin
      if AStream.Size < (AddressStart + BytesToWrite) then         // Grow the Address space
      begin
        OldStreamSize := AStream.Size;
        AStream.Size := AddressStart + BytesToWrite;
        AStream.Position := OldStreamSize;
        while AStream.Position < AStream.Size do
        begin
          B := 0;
          AStream.Write(B, SizeOf(B));
        end;

      end;
    end;

    if AddressStart >= AStream.Size then
      Result := ERROR_CODE_PERMANENT_INVALID_ARGUMENTS
    else begin

      MemspaceDataSentCount := LccMessage.DataCount - FirstDataByte;

      AStream.Position := AddressStart;
      for i := 0 to MemspaceDataSentCount - 1 do
      begin
        B := LccMessage.DataArray[i + FirstDataByte];
        AStream.Write(B, SizeOf(B));
      end;
    end
  end;

  // We don't need to send a WriteReply for this.. the Datagram OK is all that is needed...
  // Unless it will take a while then we can send a special Datagram OK and then send the WriteReply later.
end;

procedure TNodeProtocolBase.CopyTo(ANodeProtocol: TNodeProtocolBase);
begin
  ANodeProtocol.Valid := Valid;
end;

constructor TNodeProtocolBase.Create;
begin
  inherited Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TNodeProtocolBase.Destroy;
begin
  FWorkerMessage.Free;
  inherited Destroy;
end;

function TNodeProtocolBase.DatagramReadRequest(LccMessage: TLccMessage; OutMessage: TLccMessage; AStream: TStream; {AutoGrow: Boolean;} MemOffset: Int64 = 0): Word;
//
// Assumes the Source and Destination have already been set up
//
var
  FirstDataByte, BytesToRead: Integer;
  AddressStart: DWord;
  B: Byte;
begin
  // Assumption is this is a datagram message

  Result := S_OK;

  InitializeOutMessageNodeIDs(LccMessage, OutMessage);

  // Is the addressStart space in the header or is it the first byte in the data that
  // we need to skip over?
  if LccMessage.DataArray[1] and $03 = 0 then
    FirstDataByte := 7  // Skip over the addressStart space byte in the data
  else
    FirstDataByte := 6; // The addressStart space is encoded in the header so use all the data bytes
  BytesToRead := LccMessage.DataArray[FirstDataByte];                // number of bytes to read
  OutMessage.DataArrayIndexer[0] := LccMessage.DataArray[0];         // Just copy the original message
  OutMessage.DataArrayIndexer[1] := LccMessage.DataArray[1] or $10;  // except set the reply flag
  OutMessage.DataArrayIndexer[2] := LccMessage.DataArray[2];         // Copy the addressStart
  OutMessage.DataArrayIndexer[3] := LccMessage.DataArray[3];
  OutMessage.DataArrayIndexer[4] := LccMessage.DataArray[4];
  OutMessage.DataArrayIndexer[5] := LccMessage.DataArray[5];
  if FirstDataByte = 7 then
    OutMessage.DataArrayIndexer[6] := LccMessage.DataArray[6];

  AddressStart := LccMessage.ExtractDataBytesAsInt(2, 5);     // Pull out the AddressStart
  AddressStart := AddressStart + MemOffset;

  if BytesToRead > 64 then
  begin
    Result := ERROR_CODE_PERMANENT_INVALID_ARGUMENTS;
    OutMessage.DataArrayIndexer[1] := LccMessage.DataArray[1] or $08;  // Set a Failure Status
    OutMessage.InsertWordAsDataBytes(Result, FirstDataByte);           // Errorcode
    OutMessage.DataCount := 2;
  end else
  if BytesToRead = 0 then
  begin
    Result := ERROR_CODE_PERMANENT_INVALID_ARGUMENTS;
    OutMessage.DataArrayIndexer[1] := LccMessage.DataArray[1] or $08;  // Set a Failure Status
    OutMessage.InsertWordAsDataBytes(Result, FirstDataByte);           // Errorcode
    OutMessage.DataCount := 2;
  end else
  begin

  {  if AutoGrow then
    begin
      if AStream.Size < Int64( AddressStart) + Int64( BytesToRead) then         // Grow the Address space
      begin
        OldStreamSize := AStream.Size;
        AStream.Size := Int64( AddressStart) + Int64( BytesToRead);
        AStream.Position := OldStreamSize;
        while AStream.Position < AStream.Size do
        begin
          B := 0;
          AStream.Write(B, SizeOf(B));
        end;
      end;
    end; }

    if AddressStart >= AStream.Size then
    begin
      Result := ERROR_CODE_PERMANENT_INVALID_ARGUMENTS;
      OutMessage.DataArrayIndexer[1] := LccMessage.DataArray[1] or $08;  // Set a Failure Status
      OutMessage.InsertWordAsDataBytes(Result, FirstDataByte);         // Errorcode
      OutMessage.DataCount := 2;
    end else
    begin

      OutMessage.DataCount := 0;
      AStream.Position := AddressStart;
      while (AStream.Position < AStream.Size) and (OutMessage.DataCount < BytesToRead) do
      begin
        B := 0;
        AStream.Read(B, SizeOf(B));
        OutMessage.DataArrayIndexer[FirstDataByte + OutMessage.DataCount] := B;
        OutMessage.DataCount := OutMessage.DataCount + 1
      end;
      OutMessage.DataCount := OutMessage.DataCount + FirstDataByte; // Include the header info
    end;
  end;
end;

{ TProtocolSupportedProtocols }

procedure TProtocolSupportedProtocols.DecodeFlags;
begin
  if Length(Flags) > 0 then
  begin
    FSimpleNode := Flags[5] and PIP_SIMPLENODE <> 0;
    FDatagram := Flags[5] and PIP_DATAGRAM <> 0;
    FStream := Flags[5] and PIP_STREAM <> 0;
    FMemConfig := Flags[5] and PIP_MEMORY_CONFIG <> 0;
    FReservation := Flags[5] and PIP_RESERVATION <> 0;
    FEventExchange := Flags[5] and PIP_EVENT_EXCHANGE <> 0;
    FIdentification := Flags[5] and PIP_IDENTIFCIATION <> 0;
    FTeach_Learn := Flags[5] and PIP_TEACH_LEARN <> 0;

    FRemoteButton := Flags[4] and PIP_REMOTE_BUTTON <> 0;
    FAbbreviatedConfigurationDefinitionInfo := Flags[4] and PIP_ABBREVIATED_CDI <> 0;
    FDisplay := Flags[4] and PIP_DISPLAY <> 0;
    FSimpleNodeInfo := Flags[4] and PIP_SIMPLE_NODE_INFO <> 0;
    FConfigurationDefinitionInfo := Flags[4] and PIP_CDI <> 0;
    FTractionControl := Flags[4] and PIP_TRACTION <> 0;
    FTractionFunctionDefinitionInfo := Flags[4] and PIP_FDI <> 0;
    // Dcc_Command_Station Flags[4]

    FTractionSimpleTrainNodeInfo := Flags[3] and PIP_SIMPLE_TRAIN_NODE_INFO <> 0;
    FTractionFunctionConfiguration := Flags[3] and PIP_FUNCTION_CONFIGURATION <> 0;
    FFirmwareUpgrade := Flags[3] and PIP_FIRMWARE_UPGRADE <> 0;
    FFirmwareUpgradeActive := Flags[3] and PIP_FIRMWARE_UPGRADE_ACTIVE <> 0;

    Valid := True;
  end;
end;

function TProtocolSupportedProtocols.EncodeFlags: TLccSupportedProtocolArray;
var
  i: Integer;
begin
  for i := 0 to MAX_SUPPORTEDPROTOCOL_LEN - 1 do
    Result[i] := 0;

  if SimpleNode then Result[5] := Result[5] or PIP_SIMPLENODE;
  if Datagram then Result[5] := Result[5] or PIP_DATAGRAM;
  if Stream then Result[5] := Result[5] or PIP_STREAM;
  if MemConfig then Result[5] := Result[5] or PIP_MEMORY_CONFIG;
  if Reservation then Result[5] := Result[5] or PIP_RESERVATION;
  if EventExchange then Result[5] := Result[5] or PIP_EVENT_EXCHANGE;
  if Identification then Result[5] := Result[5] or PIP_IDENTIFCIATION;
  if Teach_Learn then Result[5] := Result[5] or PIP_TEACH_LEARN;

  if RemoteButton then Result[4] := Result[4] or PIP_REMOTE_BUTTON;
  if AbbreviatedConfigurationDefinitionInfo then Result[4] := Result[4] or PIP_ABBREVIATED_CDI;
  if Display then Result[4] := Result[4] or PIP_DISPLAY;
  if SimpleNodeInfo then Result[4] := Result[4] or PIP_SIMPLE_NODE_INFO;
  if ConfigurationDefinitionInfo then Result[4] := Result[4] or PIP_CDI;
  if TractionControl then Result[4] := Result[4] or PIP_TRACTION;
  if TractionFunctionDefinitionInfo then Result[4] := Result[4] or PIP_FDI;
// if DccCommand Station then Result[4] := .... depreciated

  if TractionSimpleTrainNodeInfo then Result[3] := Result[3] or PIP_SIMPLE_TRAIN_NODE_INFO;
  if TractionFunctionConfiguration then Result[3] := Result[3] or PIP_FUNCTION_CONFIGURATION;
  if FirmwareUpgrade then Result[3] := Result[3] or PIP_FIRMWARE_UPGRADE;
  if FirmwareUpgradeActive then Result[3] := Result[3] or PIP_FIRMWARE_UPGRADE_ACTIVE


end;

procedure TProtocolSupportedProtocols.LoadFromLccMessage(SourceLccMessage: TLccMessage);
begin
  Flags[0] := SourceLccMessage.DataArrayIndexer[5];
  Flags[1] := SourceLccMessage.DataArrayIndexer[4];
  Flags[2] := SourceLccMessage.DataArrayIndexer[3];
  Flags[3] := SourceLccMessage.DataArrayIndexer[2];
  Flags[4] := SourceLccMessage.DataArrayIndexer[1];
  Flags[5] := SourceLccMessage.DataArrayIndexer[0];

  DecodeFlags;
end;

function TProtocolSupportedProtocols.SupportedToStr: String;
begin
  Result := '';
  if Datagram then Result := Result + 'Datagrams Supported; ';
  if Stream then Result := Result + 'Stream Supported; ';
  if MemConfig then Result := Result + 'Memory Configuration Supported; ';
  if Reservation then Result := Result + 'Reservation Supported; ';
  if EventExchange then Result := Result + 'Events Supported; ';
  if Identification then Result := Result + 'Identification Supported; ';
  if Teach_Learn then Result := Result + 'Teach-Learn Supported; ';
  if RemoteButton then Result := Result + 'Remote Button Supported; ';
  if AbbreviatedConfigurationDefinitionInfo then Result := Result + 'ACDI Supported; ';
  if Display then Result := Result + 'Display Supported; ';
  if SimpleNodeInfo then Result := Result + 'SNIP Supported; ';
  if ConfigurationDefinitionInfo then Result := Result + 'CDI Supported; ';
  if TractionControl then Result := Result + 'Traction Control Supported; ';
  if TractionSimpleTrainNodeInfo then Result := Result + 'Train SNIP Supported; ';
  if TractionFunctionDefinitionInfo then Result := Result + 'FDI Supported; ';
  if TractionFunctionConfiguration then Result := Result + 'Function Config Mem Supported; ';
  if FirmwareUpgrade then Result := Result + 'Firmware Upgrade Supported; ';
  if FirmwareUpgradeActive then Result := Result + 'Firmware Upgrade Active Supported; ';
  if SimpleNode then Result := Result + 'Simple Node Supported; ';
end;

procedure TProtocolSupportedProtocols.CopyTo(ANodeProtocol: TNodeProtocolBase);
var
  Local: TProtocolSupportedProtocols;
begin
  inherited CopyTo(ANodeProtocol);

  Local := ANodeProtocol as TProtocolSupportedProtocols;

  Local.FAbbreviatedConfigurationDefinitionInfo := AbbreviatedConfigurationDefinitionInfo;
  Local.FConfigurationDefinitionInfo :=  ConfigurationDefinitionInfo;
  Local.FDatagram :=  Datagram;
  Local.FDisplay :=  Display;
  Local.FEventExchange :=  EventExchange;
  Local.FSimpleNode :=  SimpleNode;
  Local.FTractionFunctionDefinitionInfo :=  TractionFunctionDefinitionInfo;
  Local.FIdentification :=  Identification;
  Local.FMemConfig :=  MemConfig;
  Local.FRemoteButton :=  RemoteButton;
  Local.FReservation :=  Reservation;
  Local.FSimpleNodeInfo :=  SimpleNodeInfo;
  Local.FStream :=  Stream;
  Local.FTeach_Learn :=  Teach_Learn;
  Local.FTractionControl :=  TractionControl;
  Local.FTractionSimpleTrainNodeInfo :=  TractionSimpleTrainNodeInfo;
  Local.FTractionFunctionConfiguration := TractionFunctionConfiguration;
  Local.FFirmwareUpgradeActive := FirmwareUpgradeActive;
  Local.FFirmwareUpgrade :=  FirmwareUpgrade;
end;

{ TLccEvent }

procedure TLccEvent.SetID(AValue: TEventID);
begin
  FID[0] := AValue[0];
  FID[1] := AValue[1];
  FID[2] := AValue[2];
  FID[3] := AValue[3];
  FID[4] := AValue[4];
  FID[5] := AValue[5];
  FID[6] := AValue[6];
  FID[7] := AValue[7];
  FIDStr := EventIDToString(FID, False);
end;

procedure TLccEvent.SetIDStr(AValue: string);
begin
  if FIDStr = AValue then Exit;
  FIDStr := AValue;
  FID := StrToEventID(AValue)
end;

{ TLccEventAutoGenerate }

procedure TLccEventAutoGenerate.SetCount(AValue: Integer);
begin
  if FCount = AValue then Exit;
  FCount := AValue;
end;

procedure TLccEventAutoGenerate.SetDefaultState(AValue: TEventState);
begin
  if FDefaultState = AValue then Exit;
  FDefaultState := AValue;
end;

procedure TLccEventAutoGenerate.SetStartIndex(AValue: Integer);
begin
  if FStartIndex = AValue then Exit;
  FStartIndex := AValue;
end;

{ TProtocolEvents }

constructor TProtocolEvents.Create;
begin
  inherited Create;
  FEventList := TObjectList.Create;
  FAutoGenerate := TLccEventAutoGenerate.Create;
  EventList.OwnsObjects := False;
end;

destructor TProtocolEvents.Destroy;
begin
  Clear;
  FEventList.Free;
  FAutoGenerate.Free;
  inherited Destroy;
end;

function TProtocolEvents.GetEvent(Index: Integer): TLccEvent;
begin
  Result := TLccEvent( EventList[Index])
end;

function TProtocolEvents.GetEventIDAsStr(Index: Integer): String;
begin
  Result := EventIDToString(Event[Index].ID, True);
end;

function TProtocolEvents.GetCount: Integer;
begin
  Result := EventList.Count;
end;

procedure TProtocolEvents.Add(Event: TEventID; State: TEventState);
var
  LccEvent: TLccEvent;
begin
  LccEvent := Supports(Event);
  if Assigned(LccEvent) then
    LccEvent.State := State
  else begin
    LccEvent := TLccEvent.Create;
    LccEvent.ID := Event;
    LccEvent.State := State;
    EventList.Add(LccEvent);
  end;
end;

procedure TProtocolEvents.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to EventList.Count - 1 do
      TObject( EventList[i]).Free;
  finally
    EventList.Clear
  end;
end;

function TProtocolEvents.Supports(Event: TEventID): TLccEvent;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  while not Assigned(Result) and (i < EventList.Count) do
  begin
    if EqualEventID(Event, TLccEvent( EventList[i]).ID) then
      Result := TLccEvent( EventList[i]);
    Inc(i);
  end;
end;

{ TProtocolMemoryInfo }

procedure TProtocolMemoryInfo.Add(_Space: Byte; _IsPresent, _IsReadOnly, _ImpliedZeroLowAddress: Boolean; _LowAddress, _HighAddress: DWord);
var
 Info: TConfigMemAddressSpaceInfoObject;
begin
 Info := TConfigMemAddressSpaceInfoObject.Create;
 Info.FAddressSpace := _Space;
 Info.FIsPresent := _IsPresent;
 Info.FIsReadOnly := _IsReadOnly;
 Info.FImpliedZeroLowAddress := _ImpliedZeroLowAddress;
 Info.FLowAddress := _LowAddress;
 Info.FHighAddress := _HighAddress;
 List.Add(Info);
end;

procedure TProtocolMemoryInfo.Clear;
var
 i: Integer;
begin
 try
   for i := 0 to List.Count - 1 do
     TObject(List[i]).Free;
 finally
   List.Clear
 end;
end;

constructor TProtocolMemoryInfo.Create;
begin
  inherited Create;
  List := TList.Create;
end;

destructor TProtocolMemoryInfo.Destroy;
begin
 Clear;
 FList.Free;
 inherited;
end;

function TProtocolMemoryInfo.FindByAddressSpace(Space: Byte): TConfigMemAddressSpaceInfoObject;
var
 i: Integer;
begin
 i := 0;
 Result := nil;
 while (i < Count) and not Assigned(Result) do
 begin
   if AddressSpace[i].AddressSpace = Space then
     Result := AddressSpace[i];
   Inc(i);
 end;
end;

function TProtocolMemoryInfo.GetAddressSpace(Index: Integer): TConfigMemAddressSpaceInfoObject;
begin
 Result := TConfigMemAddressSpaceInfoObject( List[Index])
end;

function TProtocolMemoryInfo.GetCount: Integer;
begin
 Result := List.Count
end;

procedure TProtocolMemoryInfo.LoadReply(LccMessage: TLccMessage; OutMessage: TLccMessage);
var
 Info: TConfigMemAddressSpaceInfoObject;
begin
  // Decode the LccMessage

 InitializeOutMessageNodeIDs(LccMessage, OutMessage);

 Info := FindByAddressSpace( LccMessage.DataArrayIndexer[2]);
 OutMessage.DataArrayIndexer[0] := $20;
 if Assigned(Info) then
 begin
   if Info.IsPresent then
     OutMessage.DataArrayIndexer[1] := MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY
   else
     OutMessage.DataArrayIndexer[1] := MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY;
   OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];
   OutMessage.DataArrayIndexer[3] := _Highest(Info.FHighAddress);
   OutMessage.DataArrayIndexer[4] := _Higher(Info.FHighAddress);
   OutMessage.DataArrayIndexer[5] := _Hi(Info.FHighAddress);
   OutMessage.DataArrayIndexer[6] := _Lo(Info.FHighAddress);
   OutMessage.DataArrayIndexer[7] := 0;
   if Info.IsReadOnly then
     OutMessage.DataArrayIndexer[7] := OutMessage.DataArrayIndexer[7] or $01;
   OutMessage.DataCount := 8;
   if not Info.ImpliedZeroLowAddress then
   begin
     OutMessage.DataArrayIndexer[8] := _Highest(Info.FLowAddress);
     OutMessage.DataArrayIndexer[9] := _Higher(Info.FLowAddress);
     OutMessage.DataArrayIndexer[10] := _Hi(Info.FLowAddress);
     OutMessage.DataArrayIndexer[11] := _Lo(Info.FLowAddress);
     OutMessage.DataCount := 12;
   end;
 end else
 begin
   OutMessage.DataArrayIndexer[1] := MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY;
   OutMessage.DataArrayIndexer[2] := LccMessage.DataArrayIndexer[2];
   OutMessage.DataArrayIndexer[3] := 0;
   OutMessage.DataArrayIndexer[4] := 0;
   OutMessage.DataArrayIndexer[5] := 0;
   OutMessage.DataArrayIndexer[6] := 0;
   OutMessage.DataArrayIndexer[7] := $01;
   OutMessage.DataCount := 8;
 end;
end;

procedure TProtocolMemoryInfo.CopyTo(ANodeProtocol: TNodeProtocolBase);
var
  i: Integer;
begin
  inherited CopyTo(ANodeProtocol);

  for i := 0 to List.Count do
    (ANodeProtocol as TProtocolMemoryInfo).List.Add( TConfigMemAddressSpaceInfoObject( List[i]).Clone);
end;

{ TProtocolMemoryOptions }

procedure TProtocolMemoryOptions.LoadReply(LccMessage, OutMessage: TLccMessage);
var
  OpsMask: Word;
begin

  InitializeOutMessageNodeIDs(LccMessage, OutMessage);

  LccMessage.DataArrayIndexer[0] := $20;
  LccMessage.DataArrayIndexer[1] := MCP_OP_GET_CONFIG_OPTIONS_REPLY;
  LccMessage.DataArrayIndexer[5] := FHighSpace;
  LccMessage.DataArrayIndexer[6] := FLowSpace;
  LccMessage.DataArrayIndexer[4] := 0;
  if WriteLenOneByte then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_ONE_BYTE;
  if WriteLenTwoBytes then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_TWO_BYTE;
  if WriteLenFourBytes then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_FOUR_BYTE;
  if WriteLenSixyFourBytes then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_64_BYTE;
  if WriteArbitraryBytes then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_ARBITRARY_BYTE;
  if WriteStream then
    LccMessage.DataArrayIndexer[4] := LccMessage.DataArrayIndexer[4] or MCWL_STREAM_WRITE_SUPPORTED;
  OpsMask := 0;
  if WriteUnderMask then
    OpsMask := OpsMask or MCO_WRITE_UNDER_MASK;
  if UnAlignedReads then
    OpsMask := OpsMask or MCO_UNALIGNED_READS;
  if UnAlignedWrites then
    OpsMask := OpsMask or MCO_UNALIGNED_WRITES;
  if SupportACDIMfgRead then
    OpsMask := OpsMask or MCO_ACDI_MFG_READS;
  if SupportACDIUserRead then
    OpsMask := OpsMask or MCO_ACDI_USER_READS;
  if SupportACDIUserWrite then
    OpsMask := OpsMask or MCO_ACDI_USER_WRITES;
  LccMessage.DataArrayIndexer[2] := _Hi(OpsMask);
  LccMessage.DataArrayIndexer[3] := _Lo(OpsMask);
  LccMessage.DataCount := 7;
end;

procedure TProtocolMemoryOptions.CopyTo(ANodeProtocol: TNodeProtocolBase);
var
  Local: TProtocolMemoryOptions;
begin
  inherited CopyTo(ANodeProtocol);

  Local := ANodeProtocol as TProtocolMemoryOptions;

  Local.FHighSpace := FHighSpace;
  Local.FLowSpace :=  FLowSpace;
  Local.FSupportACDIMfgRead :=  FSupportACDIMfgRead;
  Local.FSupportACDIUserRead :=  FSupportACDIUserRead;
  Local.FSupportACDIUserWrite :=  FSupportACDIUserWrite;
  Local.FUnAlignedReads :=  FUnAlignedReads;
  Local.FUnAlignedWrites :=  FUnAlignedWrites;
  Local.FWriteArbitraryBytes :=  FWriteArbitraryBytes;
  Local.FWriteLenFourBytes :=  FWriteLenFourBytes;
  Local.FWriteLenOneByte:=  FWriteLenOneByte;
  Local.FWriteLenSixyFourBytes :=  FWriteLenSixyFourBytes;
  Local.FWriteLenTwoBytes :=  FWriteLenTwoBytes;
  Local.FWriteStream :=  FWriteStream;
  Local.FWriteUnderMask :=  FWriteUnderMask;

end;

{ TProtocolSimpleNodeInfo }

function TProtocolSimpleNodeInfo.PackedFormat(StreamManufacturerInfo, StreamConfiguration: TStream): TLccDynamicByteArray;
var
  Len, i: Integer;
  AByte: Byte;
begin

  Result := nil;

  Len := 1; // Version number
  StreamManufacturerInfo.Position := ADDRESS_MFG_NAME;
  while StreamReadByte(StreamManufacturerInfo) <> Ord(#0) do
   Inc(Len);
  StreamManufacturerInfo.Position := ADDRESS_MODEL_NAME;
  while StreamReadByte(StreamManufacturerInfo) <> Ord(#0) do
   Inc(Len);
  StreamManufacturerInfo.Position := ADDRESS_HARDWARE_VERSION;
  while StreamReadByte(StreamManufacturerInfo) <> Ord(#0) do
   Inc(Len);
  StreamManufacturerInfo.Position := ADDRESS_SOFTWARE_VERSION;
  while StreamReadByte(StreamManufacturerInfo) <> Ord(#0) do
   Inc(Len);
  Inc(Len); // Version Number
  StreamConfiguration.Position := ADDRESS_USER_NAME;
  while StreamReadByte(StreamConfiguration) <> Ord(#0) do
   Inc(Len);
  StreamConfiguration.Position := ADDRESS_USER_DESCRIPTION;
  while StreamReadByte(StreamConfiguration) <> Ord(#0) do
   Inc(Len);
  Inc(Len, 6);  // six NULLs for 6 strings

  SetLength(Result, Len);

  i := 0;
  Result[i] := 1; // Version number
  Inc(i);

  StreamManufacturerInfo.Position := ADDRESS_MFG_NAME;
  AByte := StreamReadByte(StreamManufacturerInfo);
  while AByte <> Ord(#0) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamManufacturerInfo);
    Inc(i);
  end;
  Result[i] := Ord(#0);  // null terminate string
  Inc(i);

  StreamManufacturerInfo.Position := ADDRESS_MODEL_NAME;
  AByte := StreamReadByte(StreamManufacturerInfo);
  while AByte <> Ord(#0) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamManufacturerInfo);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
  Inc(i);

  StreamManufacturerInfo.Position := ADDRESS_HARDWARE_VERSION;
  AByte := StreamReadByte(StreamManufacturerInfo);
  while AByte <> Ord( Ord(#0)) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamManufacturerInfo);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
  Inc(i);

  StreamManufacturerInfo.Position := ADDRESS_SOFTWARE_VERSION;
  AByte := StreamReadByte(StreamManufacturerInfo);
  while AByte <> Ord( Ord(#0)) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamManufacturerInfo);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
  Inc(i);

  Result[i] := 1; // Version number
  Inc(i);

  StreamConfiguration.Position := ADDRESS_USER_NAME;
  AByte := StreamReadByte(StreamConfiguration);
  while AByte <> Ord( Ord(#0)) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamConfiguration);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
  Inc(i);

  StreamConfiguration.Position := ADDRESS_USER_DESCRIPTION;
  AByte := StreamReadByte(StreamConfiguration);
  while AByte <> Ord( Ord(#0)) do
  begin
    Result[i] := AByte;
    AByte := StreamReadByte(StreamConfiguration);
    Inc(i);
  end;
  Result[i] :=  Ord(#0);  // null terminate string
end;

procedure TProtocolSimpleNodeInfo.LoadFromLccMessage(SourceLccMessage: TLccMessage);
var
  VersionStringCount: Byte;
  iArray, i: Integer;
  Str: String;
  StrLen: Integer;
begin
  iArray := 0;
  VersionStringCount := SourceLccMessage.DataArray[iArray];
  if VersionStringCount = 1 then
    VersionStringCount := 4;
  FVersion := VersionStringCount;
  Inc(iArray);

  for i := 0  to VersionStringCount - 1 do
  begin
    StrLen := 0;  // Read a null terminated string and return the StrLen
    Str := SourceLccMessage.ExtractDataBytesAsString(iArray, StrLen);
    case i of
      0 : FManufacturer := Str;
      1 : FModel := Str;
      2 : FHardwareVersion := Str;
      3 : FSoftwareVersion := Str;
    end;
    iArray := iArray + StrLen + 1 // Skip over the null
  end;

  VersionStringCount := SourceLccMessage.DataArray[iArray];
  if VersionStringCount = 1 then
    VersionStringCount := 2;
  FUserVersion := VersionStringCount;
  Inc(iArray);

  for i := 0 to VersionStringCount - 1 do
  begin
    StrLen := 0;  // Read a null terminated string and return the StrLen
    Str := SourceLccMessage.ExtractDataBytesAsString(iArray, StrLen);
    case i of
      0 : FUserName := Str;
      1 : FUserDescription := Str;
    end;
    iArray := iArray + StrLen + 1 // Skip over the null
  end;

  FValid := True;
end;

procedure TProtocolSimpleNodeInfo.CopyTo(ANodeProtocol: TNodeProtocolBase);
var
  Local: TProtocolSimpleNodeInfo;
begin
  inherited CopyTo(ANodeProtocol);
  Local := ANodeProtocol as TProtocolSimpleNodeInfo;
  Local.FHardwareVersion := HardwareVersion;
  Local.FManufacturer := Manufacturer;
  Local.FModel := Model;
  Local.FSoftwareVersion := SoftwareVersion;
  Local.FUserDescription := UserDescription;
  Local.FUserName := UserName;
  Local.FUserVersion := UserVersion;
  Local.FVersion := Version;
end;



end.

