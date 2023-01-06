unit lcc_node;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

{.$DEFINE LOG_MAPPING}

interface

{$I ..\lcc_compilers.inc}

uses
  Classes,
  SysUtils,
  Math,
  {$IFDEF FPC}
    contnrs,
    LazLogger,
    {$IFNDEF FPC_CONSOLE_APP}
      ExtCtrls,
    {$ENDIF}
  {$ELSE}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  lcc_protocol_utilities,
  lcc_defines,
  lcc_base_classes,
  lcc_node_messages,
  lcc_utilities,
  lcc_alias_server,
  lcc_train_server;

const
  ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH = $0001;

  TIMEOUT_TIME = 100; // milli seconds
  TIMEOUT_CONTROLLER_NOTIFY_WAIT = 5000;  // 5 seconds
  TIMEOUT_CONTROLLER_RESERVE_WAIT = 5000;
  TIMEOUT_NODE_VERIFIED_WAIT = 800;       // 800ms
  TIMEOUT_NODE_ALIAS_MAPPING_WAIT = 1000;       // 800ms
  TIMEOUT_CREATE_TRAIN_WAIT = 1000;       // 1000ms
  TIMEOUT_SNIP_REPONSE_WAIT = 500;
  TIMEOUT_LISTENER_ATTACH_TRAIN_WAIT = 5000;       // per listener, will have to map CAN Alias if on CAN so may take a bit...

const

 CDI_XML: string = (
'<?xml version="1.0" encoding="utf-8"?>'+
'<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
'<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
	'<identification>'+
		'<manufacturer>Mustangpeak</manufacturer>'+
		'<model>TC1000</model>'+
		'<hardwareVersion>1.0.0.0</hardwareVersion>'+
		'<softwareVersion>1.0.0.0</softwareVersion>'+
	'</identification>'+
	'<segment origin="1" space="253">'+
		'<name>User</name>'+
		'<description>User defined information</description>'+
		'<group>'+
			'<name>User Data</name>'+
			'<description>Add your own unique node info here</description>'+
			'<string size="63">'+
				'<name>User Name</name>'+
			'</string>'+
			'<string size="64">'+
				'<name>User Description</name>'+
			'</string>'+
		'</group>'+
	'</segment>'+
'</cdi>');


type
  TLccNode = class;
  TLccEngineMemorySpaceAccess = class;

    { TDatagramQueue }

  TDatagramQueue = class
  private
    {$IFDEF DELPHI}
    FQueue: TObjectList<TLccMessage>;
    {$ELSE}
    FQueue: TObjectList;
    {$ENDIF}
    FSendMessageFunc: TOnMessageEvent;
  protected
    {$IFDEF DELPHI}
    property Queue: TObjectList<TLccMessage> read FQueue write FQueue;
    {$ELSE}
    property Queue: TObjectList read FQueue write FQueue;
    {$ENDIF}

    function FindBySourceNode(LccMessage: TLccMessage): Integer;
  public
    property SendMessageFunc: TOnMessageEvent read FSendMessageFunc write FSendMessageFunc;

    constructor Create;
    destructor Destroy; override;
    function Add(LccMessage: TLccMessage): Boolean;
    procedure Clear;
    procedure Resend(LccMessage: TLccMessage);
    procedure Remove(LccMessage: TLccMessage);
    procedure TickTimeout;
  end;


  TOnEngineMemorySpaceAccessCallback = procedure(EngineMemorySpaceAccess: TLccEngineMemorySpaceAccess) of object;

  TLccEngineState = (lesIdle, lesRunning, lesStopped, lesComplete, lesError);

  { TLccEngineBase }

  TLccEngineBase = class
  private
    FEngineState: TLccEngineState;
    FErrorCode: Word;
    FOwnerNode: TLccNode;
    FTag: Integer;
    FTagObject: TObject;
    FWorkerMessage: TLccMessage;
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    procedure EngineStateComplete;
    procedure EngineStateError;
  public
    property OwnerNode: TLccNode read FOwnerNode write FOwnerNode;
    property Tag: Integer read FTag write FTag;
    property TagObject: TObject read FTagObject write FTagObject;
    property EngineState: TLccEngineState read FEngineState;
    property ErrorCode: Word read FErrorCode write FErrorCode;

    constructor Create(AnOwner: TLccNode); virtual;
    destructor Destroy; override;
    procedure Start; virtual;
    procedure Stop; virtual;
    procedure Reset; virtual;
    procedure Process(SourceMessage: TLccMessage); virtual; abstract;
  end;

  TLccEngineMemorySpaceReadWrite = (lems_Read, lems_Write);

  { TLccEngineMemorySpaceObject }

  TLccEngineMemorySpaceObject = class
  private
    FAddressHi: DWord;
    FAddressLo: DWord;
    FCallback: TOnEngineMemorySpaceAccessCallback;
    FCurrentAddress: DWord;
    FDataType: TLccConfigDataType;
    FIsString: Boolean;
    FMemorySpace: Byte;
    FReadWrite: TLccEngineMemorySpaceReadWrite;
    FTargetAlias: Word;
    FTargetNodeID: TNodeID;
    FUseAddresses: Boolean;
    FWriteEventID: TEventID;
    FWriteInteger: Integer;
    FWriteString: String;
    procedure SetWriteEventID(AValue: TEventID);
    procedure SetWriteInteger(AValue: Integer);
    procedure SetWriteString(AValue: String);
  public
    property AddressLo: DWord read FAddressLo write FAddressLo;
    property AddressHi: DWord read FAddressHi write FAddressHi;
    property IsString: Boolean read FIsString write FIsString;
    property TargetNodeID: TNodeID read FTargetNodeID write FTargetNodeID;
    property TargetAlias: Word read FTargetAlias write FTargetAlias;
    property CallBack: TOnEngineMemorySpaceAccessCallback read FCallback write FCallback;
    property CurrentAddress: DWord read FCurrentAddress write FCurrentAddress;
    property UseAddresses: Boolean read FUseAddresses write FUseAddresses;
    property MemorySpace: Byte read FMemorySpace write FMemorySpace;
    property ReadWrite: TLccEngineMemorySpaceReadWrite read FReadWrite write FReadWrite;

    property WriteString: String read FWriteString write SetWriteString;
    property WriteInteger: Integer read FWriteInteger write SetWriteInteger;
    property WriteEventID: TEventID read FWriteEventID write SetWriteEventID;
    property DataType: TLccConfigDataType read FDataType write FDataType;

    constructor Create(AReadWrite: TLccEngineMemorySpaceReadWrite; AMemorySpace: Byte; AnIsString: Boolean; AnAddressLo, AnAddressHi: DWORD; AnUseAddresses: Boolean; ATargetNodeID: TNodeID; ATargetAliasID: Word; ACallback: TOnEngineMemorySpaceAccessCallback);
  end;

  { TLccEngineMemorySpaceAccess }

  TLccEngineMemorySpaceAccess = class(TLccEngineBase)
  private
    FAddressHi: DWord;
    FAddressLo: DWord;
    FAddressSpaceValid: Boolean;
    FCallback: TOnEngineMemorySpaceAccessCallback;
    FCurrentAddress: DWord;
    FIsString: Boolean;
    FMemorySpace: Byte;
    FMemorySpaceQueue: TList;
    FMemoryStream: TMemoryStream;
    FPIPHelper: TProtocolSupportedProtocols;
    FReadWrite: TLccEngineMemorySpaceReadWrite;
    FTargetAlias: Word;
    FTargetNodeID: TNodeID;
    FUseAddresses: Boolean;
    FWritingChunk: Boolean;
    function GetStreamAsString: AnsiString;
    function GetStreamAsInt: Integer;
    function GetStreamAsEventID: TEventID;
    procedure SetStreamAsEventID(AValue: TEventID);
    procedure SetStreamAsInt(Size: Integer; AValue: Integer);
    procedure SetStreamAsString(AValue: AnsiString);
    function GetQueuedRequests: Integer;
  protected
    property CurrentAddress: DWord read FCurrentAddress write FCurrentAddress;
    property MemorySpaceQueue: TList read FMemorySpaceQueue write FMemorySpaceQueue;
    property WritingChunk: Boolean read FWritingChunk write FWritingChunk;
    property AddressSpaceValid: Boolean read FAddressSpaceValid write FAddressSpaceValid;

    procedure InternalStart;
    procedure HandleReadReply(SourceMessage: TLccMessage; DataStartIndex: Integer);
    procedure HandleWriteReply(SourceMessage: TLccMessage);
    procedure ReadNextChunk;
    procedure WriteNextChunk;
    procedure ValidatePIPAndProceed;
    procedure ValidateAddressSpaceAndProceed(SourceMessage: TLccMessage);
    procedure FlushMemorySpaceQueue;
    function NextMemorySpaceObjectFromQueue: Boolean;
    procedure CallbackAndNextMemorySpace;
    procedure StartOperation;
  public
    property AddressLo: DWord read FAddressLo write FAddressLo;
    property AddressHi: DWord read FAddressHi write FAddressHi;
    property CallBack: TOnEngineMemorySpaceAccessCallback read FCallback write FCallback;
    property IsString: Boolean read FIsString write FIsString;
    property MemoryStream: TMemoryStream read FMemoryStream;
    property MemorySpace: Byte read FMemorySpace write FMemorySpace;                               // MSI_xxxx constants
    property TargetNodeID: TNodeID read FTargetNodeID write FTargetNodeID;
    property TargetAlias: Word read FTargetAlias write FTargetAlias;
    property UseAddresses: Boolean read FUseAddresses write FUseAddresses;
    property ReadWrite: TLccEngineMemorySpaceReadWrite read FReadWrite write FReadWrite;

    property PIPHelper: TProtocolSupportedProtocols read FPIPHelper;
    property StreamAsString: AnsiString read GetStreamAsString;
    property StreamAsInt: Integer read GetStreamAsInt;
    property StreamAsEventID: TEventID read GetStreamAsEventID;
    property StringToStream: AnsiString write SetStreamAsString;
    property IntToStream[Size: Integer]: Integer write SetStreamAsInt;
    property EventIDToStream: TEventID write SetStreamAsEventID;
    property QueuedRequests: Integer read GetQueuedRequests;


    constructor Create(AnOwner: TLccNode); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Process(SourceMessage: TLccMessage); override;
    function Assign(AReadWrite: TLccEngineMemorySpaceReadWrite; AMemorySpace: Byte; AnIsString: Boolean; AnAddressLo, AnAddressHi: DWORD; AnUseAddresses: Boolean; ATargetNodeID: TNodeID; ATargetAliasID: Word; ACallback: TOnEngineMemorySpaceAccessCallback): TLccEngineMemorySpaceObject; // AMemorySpace = MSI_xxxx constants
  end;

  { TLccNode }

  TLccNode = class(TInterfacedObject)
  private
    FAliasID: Word;
    FDatagramResendQueue: TDatagramQueue;
    FDuplicateAliasDetected: Boolean;
    FEnabled: Boolean;
    FEnginesRegisteredList: TList;
    FGridConnect: Boolean;
    FLoginTimoutCounter: Integer;
    FPermitted: Boolean;
    FProtocolACDIMfg: TProtocolACDIMfg;
    FProtocolACDIUser: TProtocolACDIUser;
    FProtocolEventConsumed: TProtocolEvents;
    FProtocolEventsProduced: TProtocolEvents;
    FProtocolMemoryConfiguration: TProtocolMemoryConfiguration;
    FProtocolMemoryInfo: TProtocolMemoryInfo;
    FProtocolMemoryOptions: TProtocolMemoryOptions;
    FProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo;
    FProtocolSupportedProtocols: TProtocolSupportedProtocols;
    FProtocolMemoryConfigurationDefinitionInfo: TProtocolMemoryConfigurationDefinitionInfo;
    FSeedNodeID: TNodeID;
    FWorkerMessageDatagram: TLccMessage;
    FInitialized: Boolean;
    FNodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF};
    FSendMessageFunc: TOnMessageEvent;
    FStreamManufacturerData: TMemoryStream;        // Stream containing the Manufacturer Data stored like the User data with Fixed Offsets for read only data
                                                   // SNIP uses this structure to create a packed version of this information (null separated strings) +
                                                   // the user name and user description which it pulls out of the Configuration Stream
                                                   // Address 0 = Version
                                                   // Address 1 = Manufacturer
                                                   // Address 42 = Model
                                                   // Address 83 = Hardware Version
                                                   // Address 104 = Software Version
    FStreamCdi: TMemoryStream;                     // Stream containing the XML string for the CDI (Configuration Definition Info)
    FStreamConfig: TMemoryStream;                  // Stream containing the writable configuration memory where the Address = Offset in the stream
                                                   // and the following MUST be true
                                                   // Address 0 = User info Version number
                                                   // Address 1 = User Defined name (ACDI/SNIP)
                                                   // Address 64 = User defined description  (ACDI/SNIP)
                                                   // Address 128 = Node specific persistent data
    FStreamTractionConfig: TMemoryStream;          // Stream containing the writable configuration memory for a Traction node where the Address = Offset in the stream
    FStreamTractionFdi: TMemoryStream;             // Stream containing the XML string for the FDI (Function Definition Info)
    FWorkerMessage: TLccMessage;
    FEngineMemorySpaceAccess: TLccEngineMemorySpaceAccess;

    function GetAliasIDStr: String;
    procedure SetSendMessageFunc(AValue: TOnMessageEvent);
    function GetNodeIDStr(WithDots: Boolean): String;
  protected
    FNodeID: TNodeID;

    procedure DoMemorySpaceReadEngineDone(MemoryReadEngine: TLccEngineMemorySpaceAccess); virtual;

    // Datagram Message Handlers
    //**************************************************************************
    procedure HandleDatagramOkReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleDatagramRejectedReply(var SourceMessage: TLccMessage); virtual;

    // Datagram LogRequest Type Handlers
    procedure HandleProtocolLogRequest(var SourceMessage: TLccMessage); virtual;

    // Datagram Configuration Type Handlers
        // Datagram Configuration Type Handlers - Memory Space Write Handlers
    procedure HandleCDI_MemorySpaceWrite(var SourceMessage: TLccMessage); virtual;
    procedure HandleAll_MemorySpaceWrite(var SourceMessage: TLccMessage);virtual;
    procedure HandleConfiguration_MemorySpaceWrite(var SourceMessage: TLccMessage);virtual;
    procedure HandleACDI_Manufacturer_MemorySpaceWrite(var SourceMessage: TLccMessage); virtual;
    procedure HandleACDI_UserMemorySpaceWrite(var SourceMessage: TLccMessage);virtual;
    procedure HandleStreamRead;
    procedure HandleStreamWrite;
    procedure HandleTractionFDI_MemorySpaceWrite(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionFDI_ConfigurationMemorySpaceWrite(var SourceMessage: TLccMessage); virtual;
       // Datagram Configuration Type Handlers - Memory Space Read Handlers
    procedure HandleCDI_MemorySpaceRead(var SourceMessage: TLccMessage);virtual;
    procedure HandleAll_MemorySpaceRead(var SourceMessage: TLccMessage);virtual;
    procedure HandleConfiguration_MemorySpaceRead(var SourceMessage: TLccMessage); virtual;
    procedure HandleACDI_Manufacturer_MemorySpaceRead(var SourceMessage: TLccMessage);virtual;
    procedure HandleACDI_User_MemorySpaceRead(var SourceMessage: TLccMessage);virtual;
    procedure HandleTractionFDI_MemorySpaceRead(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionFDI_ConfigurationMemorySpaceRead(var SourceMessage: TLccMessage); virtual;
      // Datagram Configuration Options Handlers
    procedure HandleOperationGetAddressSpaceInfo(var SourceMessage: TLccMessage);
    procedure HandleOperationGetConfiguration(var SourceMessage: TLccMessage);
    procedure HandleOperationLock(var SourceMessage: TLccMessage);
    procedure HandleOperationGetUniqueID(var SourceMessage: TLccMessage);
    procedure HandleOperationFreeze(var SourceMessage: TLccMessage);
    procedure HandleOperationIndicate(var SourceMessage: TLccMessage);
    procedure HandleOperationReset(var SourceMessage: TLccMessage);
    //**************************************************************************


    // Event Message Handlers
    //**************************************************************************
    procedure HandleEventsIdentify; virtual;
    procedure HandleEventsIdentifyDest; virtual;
    procedure HandleConsumerIdentifiedClear(var SourceMessage: TLccMessage); virtual;
    procedure HandleConsumerIdentifiedSet(var SourceMessage: TLccMessage); virtual;
    procedure HandleConsumerIdentifiedUnknown(var SourceMessage: TLccMessage); virtual;
    procedure HandleConsumerIdentify(var SourceMessage: TLccMessage); virtual;
    procedure HandleProducerIdentifiedClear(var SourceMessage: TLccMessage); virtual;
    procedure HandleProducerIdentifiedSet(var SourceMessage: TLccMessage); virtual;
    procedure HandleProducerIdentifiedUnknown(var SourceMessage: TLccMessage); virtual;
    procedure HandleProducerIdentify(var SourceMessage: TLccMessage); virtual;
    //**************************************************************************

    // Protocol Information Protocol Handlers
    //**************************************************************************
    procedure HandleProtocolSupportInquiry(var SourceMessage: TLccMessage); virtual;
    procedure HandleProtocolSupportReply(var SourceMessage: TLccMessage); virtual;
    //**************************************************************************

    // Undknown MTI Handlers
    //**************************************************************************
    procedure HandleOptionalInteractionRejected(var SourceMessage: TLccMessage); virtual;
    //**************************************************************************

    // SNIP Handlers
    //**************************************************************************
    procedure HandleSimpleNodeInfoReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleSimpleNodeInfoRequest(var SourceMessage: TLccMessage); virtual;
    //**************************************************************************

    // Node Management Handlers
    //**************************************************************************
    procedure HandleVerifiedNodeIDNumber(var SourceMessage: TLccMessage); virtual;
    procedure HandleVerifyNodeIDNumber(var SourceMessage: TLccMessage); virtual;
    procedure HandleVerifyNodeIDNumberDest; virtual;
    //**************************************************************************

    // Traction Handlers - Does nothing here.  Traction Object Nodes decendants override these
    //**************************************************************************
    procedure HandleTractionControllerAssign(var SourceMessage: TLccMessage);  virtual;
    procedure HandleTractionControllerRelease(var SourceMessage: TLccMessage);  virtual;
    procedure HandleTractionControllerQuery(var SourceMessage: TLccMessage);  virtual;
    procedure HandleTractionControllerChangedNotify(var SourceMessage: TLccMessage);  virtual;  // Sent from Train To currently Assigned Controller if it has one
    procedure HandleTractionEStop(var SourceMessage: TLccMessage);  virtual;
    procedure HandleTractionListenerAttach(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionListenerDetach(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionListenerQuery(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionManageReserve(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionManageRelease(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionSetSpeed(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionSetFunction(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionSimpleTrainInfoReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionQuerySpeed(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionQueryFunction(var SourceMessage: TLccMessage); virtual;

    procedure HandleTractionControllerAssignReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionControllerQueryReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionListenerAttachReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionListenerDetachReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionListenerQueryReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionManageReserveReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionQuerySpeedReply(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionQueryFunctionReply(var SourceMessage: TLccMessage); virtual;
    //**************************************************************************


    property Enabled: Boolean read FEnabled write FEnabled;  // Internally used.. Set true by "LogIn" and false in "ReleaseAlias" so a new LogIn must be called
    property NodeManager:{$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF} read FNodeManager write FNodeManager;
    property StreamCdi: TMemoryStream read FStreamCdi write FStreamCdi;
    property StreamConfig: TMemoryStream read FStreamConfig write FStreamConfig;
    property StreamManufacturerData: TMemoryStream read FStreamManufacturerData write FStreamManufacturerData;
    property StreamTractionFdi: TMemoryStream read FStreamTractionFdi write FStreamTractionFdi;
    property StreamTractionConfig: TMemoryStream read FStreamTractionConfig write FStreamTractionConfig;
    property EnginesRegisteredList: TList read FEnginesRegisteredList write FEnginesRegisteredList;

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property WorkerMessageDatagram: TLccMessage read FWorkerMessageDatagram write FWorkerMessageDatagram;

    // GridConnect Helpers
    property DuplicateAliasDetected: Boolean read FDuplicateAliasDetected write FDuplicateAliasDetected;
    property SeedNodeID: TNodeID read FSeedNodeID write FSeedNodeID;
    property LoginTimoutCounter: Integer read FLoginTimoutCounter write FLoginTimoutCounter;

    procedure AfterLogin; virtual;
    procedure CreateNodeID(var Seed: TNodeID);
    function FindCdiElement(TestXML, Element: string; var Offset: Integer; var ALength: Integer): Boolean;
    function LoadManufacturerDataStream(ACdi: string): Boolean;
    procedure AutoGenerateEvents;
    procedure SendDatagramAckReply(SourceMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
    procedure SendDatagramRejectedReply(SourceMessage: TLccMessage; Reason: Word);
    procedure SendDatagramRequiredReply(SourceMessage, ReplyLccMessage: TLccMessage);
    function GetCdiFile: string; virtual;
    procedure BeforeLogin; virtual;
    procedure LccLogIn(ANodeID: TNodeID); virtual;

    // GridConnect Helpers
    function GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
    procedure GenerateNewSeed(var Seed: TNodeID);
    procedure Relogin;
    procedure NotifyAndUpdateMappingChanges;

  public
    property DatagramResendQueue: TDatagramQueue read FDatagramResendQueue;
    property GridConnect: Boolean read FGridConnect;
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr[WithDots: Boolean]: String read GetNodeIDStr;
    property Initialized: Boolean read FInitialized;
    property SendMessageFunc: TOnMessageEvent read FSendMessageFunc write SetSendMessageFunc;

    property EngineMemorySpaceAccess: TLccEngineMemorySpaceAccess read FEngineMemorySpaceAccess write FEngineMemorySpaceAccess;
    property ProtocolSupportedProtocols: TProtocolSupportedProtocols read FProtocolSupportedProtocols write FProtocolSupportedProtocols;
    property ProtocolEventConsumed: TProtocolEvents read FProtocolEventConsumed write FProtocolEventConsumed;
    property ProtocolEventsProduced: TProtocolEvents read FProtocolEventsProduced write FProtocolEventsProduced;
    property ProtocolMemoryInfo: TProtocolMemoryInfo read FProtocolMemoryInfo write FProtocolMemoryInfo;
    property ProtocolMemoryOptions: TProtocolMemoryOptions read FProtocolMemoryOptions write FProtocolMemoryOptions;
    property ProtocolMemoryConfigurationDefinitionInfo: TProtocolMemoryConfigurationDefinitionInfo read FProtocolMemoryConfigurationDefinitionInfo write FProtocolMemoryConfigurationDefinitionInfo;
    property ProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo read FProtocolSimpleNodeInfo write FProtocolSimpleNodeInfo;
    property ProtocolMemoryConfiguration: TProtocolMemoryConfiguration read FProtocolMemoryConfiguration write FProtocolMemoryConfiguration;
    property ProtocolACDIMfg: TProtocolACDIMfg read FProtocolACDIMfg write FProtocolACDIMfg;
    property ProtocolACDIUser: TProtocolACDIUser read FProtocolACDIUser write FProtocolACDIUser;

    // GridConnect Helpers
    property AliasID: Word read FAliasID;
    property AliasIDStr: String read GetAliasIDStr;
    property Permitted: Boolean read FPermitted;

    constructor Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); virtual;
    destructor Destroy; override;

    procedure Login(ANodeID: TNodeID); virtual;
    procedure On_100msTimer(Sender: TObject);  virtual;
    procedure ReleaseAlias(DelayTime_ms: Word); virtual;
    function ProcessMessage(SourceMessage: TLccMessage): Boolean; // Do not override this override the next 2
    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; virtual;
    function ProcessMessageGridConnect(SourceMessage: TLccMessage): Boolean; virtual;

    // Task Engine Helpers
    procedure RegisterEngine(AnEngine: TLccEngineBase);
    procedure UnRegisterEngine(AnEngine: TLccEngineBase);

    procedure SendEvents;
    procedure SendConsumedEvents;
    procedure SendConsumerIdentify(Event: TEventID);
    procedure SendProducedEvents;
    procedure SendProducerIdentify(Event: TEventID);
    procedure SendSNIPRequest(TargetNodeID: TNodeID; TargetAlias: Word);
    procedure SendTrainSNIPRequest(TargetNodeID: TNodeID; TargetAlias: Word);
  end;

  TLccNodeClass = class of TLccNode;


var
  InprocessMessageAllocated: Integer = 0;

implementation

uses
  lcc_node_manager;

{ TLccEngineMemorySpaceObject }

procedure TLccEngineMemorySpaceObject.SetWriteEventID(AValue: TEventID);
begin
  FWriteEventID := AValue;
  DataType := cdt_EventID;
end;

procedure TLccEngineMemorySpaceObject.SetWriteInteger(AValue: Integer);
begin
  FWriteInteger := AValue;
  DataType := cdt_Int;
end;

procedure TLccEngineMemorySpaceObject.SetWriteString(AValue: String);
begin
  FWriteString := AValue;
  DataType := cdt_String;
end;

constructor TLccEngineMemorySpaceObject.Create(
  AReadWrite: TLccEngineMemorySpaceReadWrite; AMemorySpace: Byte;
  AnIsString: Boolean; AnAddressLo, AnAddressHi: DWORD;
  AnUseAddresses: Boolean; ATargetNodeID: TNodeID; ATargetAliasID: Word;
  ACallback: TOnEngineMemorySpaceAccessCallback);
begin
  ReadWrite := AReadWrite;
  MemorySpace := AMemorySpace;
  IsString := AnIsString;
  AddressLo := AnAddressLo;
  AddressHi := AnAddressHi;
  UseAddresses := AnUseAddresses;
  TargetNodeID := ATargetNodeID;
  TargetAlias := ATargetAliasID;
  CallBack := ACallback;
end;

{ TLccEngineBase }

constructor TLccEngineBase.Create(AnOwner: TLccNode);
begin
  FOwnerNode := AnOwner;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TLccEngineBase.Destroy;
begin
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

procedure TLccEngineBase.EngineStateComplete;
begin
  FEngineState := lesComplete;
end;

procedure TLccEngineBase.EngineStateError;
begin
  FEngineState := lesError;
end;

procedure TLccEngineBase.Start;
begin
  FEngineState := lesRunning;
end;

procedure TLccEngineBase.Stop;
begin
  FEngineState := lesStopped;
end;

procedure TLccEngineBase.Reset;
begin
  FEngineState := lesIdle;
end;

{ TLccEngineMemorySpaceAccess }

procedure TLccEngineMemorySpaceAccess.HandleReadReply(SourceMessage: TLccMessage; DataStartIndex: Integer);
var
  i: Integer;
  NullFound: Boolean;
begin
  if EngineState = lesRunning then
  begin
    NullFound := False;
    for i := DataStartIndex to SourceMessage.DataCount - 1 do
    begin
      MemoryStream.Write(SourceMessage.DataArray[i], 1);
      if IsString then
      begin
        if Char( SourceMessage.DataArray[i]) = #0 then
          NullFound := True;
      end;
    end;

    if not NullFound and (CurrentAddress < (AddressHi-AddressLo)) then
      ReadNextChunk
    else begin
      EngineStateComplete;
      // So this allows a hook in the Node to track reads of memoryspaces to cache them like in the TractionNode and TractionServer
      OwnerNode.DoMemorySpaceReadEngineDone(Self);
      CallbackAndNextMemorySpace;
    end;
  end
end;

procedure TLccEngineMemorySpaceAccess.HandleWriteReply(SourceMessage: TLccMessage);
begin
  if MemoryStream.Position = MemoryStream.Size then
  begin
    EngineStateComplete;
    CallbackAndNextMemorySpace
  end else
    WriteNextChunk;
end;

procedure TLccEngineMemorySpaceAccess.ReadNextChunk;
var
  NextCount: Integer;
begin
  if EngineState = lesRunning then
  begin
    // Calculate the number of bytes to read
    if CurrentAddress + 64 > AddressHi then
      NextCount := AddressHi - CurrentAddress
    else
      NextCount := 64;

    // Request the read
    WorkerMessage.LoadConfigMemRead(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias, MemorySpace, CurrentAddress, NextCount);
    OwnerNode.SendMessageFunc(OwnerNode, WorkerMessage);

    // Update the current address pointer
    Inc(FCurrentAddress, NextCount)
  end;
end;

procedure TLccEngineMemorySpaceAccess.WriteNextChunk;
var
  NextCount: Integer;
  ByteArray: array of Byte;
  i: Integer;
  {$IFDEF DELPHI}
  B: Byte;
  {$ENDIF}
begin
  if EngineState = lesRunning then
  begin
    // Calculate how many more bytes based
    if MemoryStream.Size - MemoryStream.Position > 64 then
      NextCount := 64
    else
      NextCount := MemoryStream.Size - MemoryStream.Position;


    if NextCount > 0 then
    begin
      // Set the flag
      WritingChunk := True;

      // Copy the MemoryStream to the local array
      SetLength(ByteArray, NextCount);
      for i := 0 to NextCount - 1 do
      begin
        {$IFDEF DELPHI}
          MemoryStream.Read(B, 1);
          ByteArray[i] := B;
        {$ELSE}
          ByteArray[i] := MemoryStream.ReadByte;
        {$ENDIF}
      end;

      // Write the space
      WorkerMessage.LoadConfigMemWriteArray(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias, MemorySpace, CurrentAddress, ByteArray);
      OwnerNode.SendMessageFunc(OwnerNode, WorkerMessage);

      // Update the address pointer
      Inc(FCurrentAddress, NextCount)
    end;
  end;
end;

procedure TLccEngineMemorySpaceAccess.ValidatePIPAndProceed;
var
  ProtocolsSupported: Boolean;
begin
  if not AddressSpaceValid then
  begin
    ProtocolsSupported := False;

    PIPHelper.DecodeFlags;
    if PIPHelper.Datagram and PIPHelper.MemConfig then
    begin
      case MemorySpace of
        MSI_CDI                      : ProtocolsSupported := PIPHelper.ConfigurationDefinitionInfo;
        MSI_ALL                      : begin end; // We don't support this
        MSI_CONFIG                   : ProtocolsSupported := PIPHelper.MemConfig;
        MSI_ACDI_MFG                 : ProtocolsSupported := PIPHelper.AbbreviatedConfigurationDefinitionInfo;
        MSI_ACDI_USER                : ProtocolsSupported := PIPHelper.AbbreviatedConfigurationDefinitionInfo;
        MSI_TRACTION_FDI             : ProtocolsSupported := PIPHelper.TractionFunctionDefinitionInfo;
        MSI_TRACTION_FUNCTION_CONFIG : ProtocolsSupported := PIPHelper.TractionFunctionConfiguration;
      end;
    end;
    if ProtocolsSupported then
    begin
      // Now see if the Memory Space supports what we want to do with it
      WorkerMessage.LoadConfigMemAddressSpaceInfo(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias, MemorySpace);
      OwnerNode.SendMessageFunc(OwnerNode, WorkerMessage);
    end else
    begin
      ErrorCode := ENGINE_ERROR_MEMORY_SPACE_UNSUPPORTED_PROTOCOL;
      EngineStateError;
      CallbackAndNextMemorySpace;
    end;
  end;
end;

procedure TLccEngineMemorySpaceAccess.ValidateAddressSpaceAndProceed(SourceMessage: TLccMessage);
begin
  // We may have already validated this address space so we can save some time
  if not AddressSpaceValid then
  begin
    if SourceMessage.DataArray[2] = MemorySpace then
    begin
      // if this is a write to a read only memory space it is an error
      if not ((ReadWrite = lems_write) and (SourceMessage.DataArray[7] and $01 = 1)) then
      begin
        // Address space can do what we are asking
        AddressSpaceValid := True;

        // User did not pass addresses so use the limits returned by this memory space
        if not UseAddresses then
        begin
          AddressHi := SourceMessage.ExtractDataBytesAsInt(3, 6);
          if SourceMessage.DataArray[7] and $02 <> 0 then
            AddressLo := SourceMessage.ExtractDataBytesAsInt(8, 11)
          else
            AddressLo := 0;
        end;

        // Kick it off
        StartOperation;

      end else
      begin
        ErrorCode := ENGINE_ERROR_MEMORY_SPACE_WRITE_TO_READONLY_SPACE;
        EngineStateError;
        CallbackAndNextMemorySpace;
      end;
    end else
    begin
      ErrorCode := ENGINE_ERROR_MEMORY_SPACE_UNSUPPORTED_MEMORYSPACE;
      EngineStateError;
      CallbackAndNextMemorySpace;
    end;
  end
end;

function TLccEngineMemorySpaceAccess.Assign(
  AReadWrite: TLccEngineMemorySpaceReadWrite; AMemorySpace: Byte;
  AnIsString: Boolean; AnAddressLo, AnAddressHi: DWORD;
  AnUseAddresses: Boolean; ATargetNodeID: TNodeID; ATargetAliasID: Word;
  ACallback: TOnEngineMemorySpaceAccessCallback): TLccEngineMemorySpaceObject;
begin
  // Need to Assign everthing first.. maybe you can add in while its running... but it was not envisioned to do so
  Assert(EngineState = lesIdle, 'TMemorySpaceReadEngine.Assign is running');

  Result := TLccEngineMemorySpaceObject.Create(AReadWrite, AMemorySpace, AnIsString, AnAddressLo, AnAddressHi, AnUseAddresses, ATargetNodeID, ATargetAliasID, ACallback);
  if Assigned(Result) then
    MemorySpaceQueue.Add(Result);
end;

procedure TLccEngineMemorySpaceAccess.FlushMemorySpaceQueue;
var
  i: Integer;
begin
  try
    for i := 0 to MemorySpaceQueue.Count - 1 do
      TObject(MemorySpaceQueue[i]).Free;
  finally
    MemorySpaceQueue.Clear;
  end;
end;

constructor TLccEngineMemorySpaceAccess.Create(AnOwner: TLccNode);
begin
  inherited Create(AnOwner);
  FPIPHelper := TProtocolSupportedProtocols.Create;
  FMemoryStream := TMemoryStream.Create;
  FMemorySpaceQueue := TList.Create;
end;

destructor TLccEngineMemorySpaceAccess.Destroy;
begin
  FlushMemorySpaceQueue;
  FreeAndNil(FPIPHelper);
  FreeAndNil(FMemoryStream);
  inherited Destroy;
end;

function TLccEngineMemorySpaceAccess.GetQueuedRequests: Integer;
begin
  Result := FMemorySpaceQueue.Count
end;

procedure TLccEngineMemorySpaceAccess.InternalStart;
begin
  inherited Start;

  if ReadWrite = lems_Read then
      MemoryStream.Clear;

  if not AddressSpaceValid then
  begin
    // Get what protocols are supported
    WorkerMessage.LoadProtocolIdentifyInquiry(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias);
    OwnerNode.SendMessageFunc(OwnerNode, WorkerMessage);
  end else
    StartOperation;
end;

function TLccEngineMemorySpaceAccess.GetStreamAsEventID: TEventID;
begin
  Result := NULL_EVENT_ID;
end;

procedure TLccEngineMemorySpaceAccess.SetStreamAsEventID(AValue: TEventID);
begin
  // Reset for use
  MemoryStream.Position := 0;
end;

procedure TLccEngineMemorySpaceAccess.SetStreamAsInt(Size: Integer; AValue: Integer);
begin
  // Reset for use
  MemoryStream.Position := 0;
end;


procedure TLccEngineMemorySpaceAccess.SetStreamAsString(AValue: AnsiString);
var
  i: Integer;
  {$IFDEF DELPHI}
  B: Byte;
  {$ENDIF}
begin
  MemoryStream.Clear;
  for i := 1 to Length(AValue) do
  begin
    {$IFDEF DELPHI}
    B := Ord( AValue[i]);
    MemoryStream.Write(B, 1);
    {$ELSE}
    MemoryStream.WriteByte(Ord( AValue[i]));
    {$ENDIF}
  end;
 {$IFDEF DELPHI}
  B := Ord( #0);
  MemoryStream.Write(B, 1);
  {$ELSE}
  MemoryStream.WriteByte(Ord( #0));
  {$ENDIF}

  // Reset for use
  MemoryStream.Position := 0;
end;

function TLccEngineMemorySpaceAccess.NextMemorySpaceObjectFromQueue: Boolean;
var
  EngineMemorySpaceObject: TLccEngineMemorySpaceObject;
begin
  Result := False;
  // Do we have anything to send?
  if MemorySpaceQueue.Count > 0 then
  begin
    // Pull it out of the Queue and delete it from the Queue
    EngineMemorySpaceObject := TLccEngineMemorySpaceObject( MemorySpaceQueue[0]);
    try
      MemorySpaceQueue.Delete(0);


      // See if we need to recheck if if the Memory space is valid, if not need to recheck if it is supported
      if ((MemorySpace <> EngineMemorySpaceObject.MemorySpace) or (ReadWrite <> EngineMemorySpaceObject.ReadWrite)) or not EngineMemorySpaceObject.UseAddresses then
        AddressSpaceValid := False;

      ReadWrite := EngineMemorySpaceObject.ReadWrite;
      MemorySpace := EngineMemorySpaceObject.MemorySpace;
      IsString := EngineMemorySpaceObject.IsString;
      AddressLo := EngineMemorySpaceObject.AddressLo;
      CurrentAddress := EngineMemorySpaceObject.AddressLo;
      AddressHi := EngineMemorySpaceObject.AddressHi;
      UseAddresses := EngineMemorySpaceObject.UseAddresses;
      TargetNodeID := EngineMemorySpaceObject.TargetNodeID;
      TargetAlias := EngineMemorySpaceObject.TargetAlias;
      CallBack := EngineMemorySpaceObject.CallBack;


      // If it is a write the data to write is stored with the MemorySpaceObject so pull it into the local stream
      // A read just uses the local stream and immediately call the callback
      if ReadWrite = lems_Write then
      begin
        case EngineMemorySpaceObject.DataType of
          cdt_String  : StringToStream := AnsiString( EngineMemorySpaceObject.WriteString);
          cdt_Int     : IntToStream[AddressHi - AddressLo] := EngineMemorySpaceObject.WriteInteger;
          cdt_EventID : EventIDToStream := EngineMemorySpaceObject.WriteEventID;
        end;
      end else
      if ReadWrite = lems_Read then
      begin
        MemoryStream.Clear;   // Waste of time?
      end;

    finally
      EngineMemorySpaceObject.Free;
    end;

    Assert(not NullNodeID(FTargetNodeID), 'TMemorySpaceReadEngine, NodeID not set');
    Result := True;
  end;
end;

procedure TLccEngineMemorySpaceAccess.CallbackAndNextMemorySpace;
begin
  Callback(Self);
  if NextMemorySpaceObjectFromQueue then
    InternalStart;
end;

procedure TLccEngineMemorySpaceAccess.StartOperation;
begin
  // Start off the current address
  CurrentAddress := AddressLo;

  // Start the process
  case ReadWrite of
    lems_write : WriteNextChunk;
    lems_read  : ReadNextChunk;
  end;
end;

function TLccEngineMemorySpaceAccess.GetStreamAsInt: Integer;
var
  Str: AnsiString;
begin
  Result := 0;
  Str := StreamAsString;
  TryStrToInt(string( Str), Result);
end;

function TLccEngineMemorySpaceAccess.GetStreamAsString: AnsiString;
var
  i: Integer;
  C: AnsiChar;
begin
  Result := '';
  C := #0;
  MemoryStream.Position := 0;
  for i := 0 to MemoryStream.Size - 1 do
  begin
    MemoryStream.Read(C, 1);
    if C <> #0 then
      Result := Result + C
  end;
end;

procedure TLccEngineMemorySpaceAccess.Start;
begin
  if NextMemorySpaceObjectFromQueue then
    InternalStart;
end;

procedure TLccEngineMemorySpaceAccess.Process(SourceMessage: TLccMessage);


  function DecodeMemoryAddress(ASourceMessage: TLccMessage): Byte;
  begin
    // Figure out where the Memory space to work on is located, encoded in the header or in the first databyte slot.
    case ASourceMessage.DataArray[1] and $03 of
      MCP_NONE          : Result := SourceMessage.DataArray[6];
      MCP_CDI           : Result := MSI_CDI;
      MCP_ALL           : Result := MSI_ALL;
      MCP_CONFIGURATION : Result := MSI_CONFIG
    else
      Result := 0;
    end;
  end;

var
  ReplyPending: Boolean;
  ReplyEstimatedTime: extended;    // In Seconds

begin
  if EngineState = lesRunning then
  begin
    if EqualNodeID(TargetNodeID, SourceMessage.SourceID, False) then
    begin
      case SourceMessage.MTI of
        MTI_DATAGRAM_OK_REPLY :
          begin
            if WritingChunk then
            begin
               // Clear flag
              WritingChunk := False;

              // Writes typically only acknowledged with OK or Rejected on the Ack
              if SourceMessage.DataArray[0] <> 0 then
              begin
                // We have a Reply Pending Bit set.. the write may take a while and when done will send a Write Reply
                // if not then there is no Write Reply and this is the only response to success
                ReplyPending := SourceMessage.DataArray[0] and DATAGRAM_OK_ACK_REPLY_PENDING = DATAGRAM_OK_ACK_REPLY_PENDING;
                ReplyEstimatedTime := Power(2, SourceMessage.DataArray[0] and $0F);
              end else
              begin
                EngineStateComplete;
                ErrorCode := S_OK;
                HandleWriteReply(SourceMessage);
              end;
            end;
          end;
        MTI_DATAGRAM_REJECTED_REPLY :
          begin
            if WritingChunk then
            begin
              // Clear Flag
              WritingChunk := False;

              EngineStateError;
              ErrorCode := ENGINE_ERROR_MEMORY_SPACE_WRITE_ERROR;
              CallbackAndNextMemorySpace;
            end;
          end;
        MTI_PROTOCOL_SUPPORT_REPLY :
          begin
            PIPHelper.LoadFromLccMessage(SourceMessage);
            ValidatePIPAndProceed;
          end;
        MTI_DATAGRAM :
          begin
            case SourceMessage.DataArray[0] of
              DATAGRAM_PROTOCOL_CONFIGURATION :
              begin
                case SourceMessage.DataArray[1] of
                 MCP_WRITE_REPLY :  // This only happens if the write can take a while (like a CV write).  In that case a special Datagram OK message is sent.. see MTI_DATAGRAM_OK_REPLY above
                   begin
                     case SourceMessage.DataArray[6] of
                       MSI_ACDI_USER,
                       MSI_TRACTION_FDI             : if DecodeMemoryAddress(SourceMessage) = MemorySpace then
                                                        HandleWriteReply(SourceMessage);
                       MSI_CONFIG,
                       MSI_TRACTION_FUNCTION_CONFIG :if DecodeMemoryAddress(SourceMessage) = MemorySpace then
                                                       HandleWriteReply(SourceMessage);
                       MSI_ALL,
                       MSI_CDI,
                       MSI_ACDI_MFG                 : begin end;  // Should never happen
                     end;
                   end;
                 MCP_WRITE_REPLY_FAILURE,
                 MCP_WRITE_REPLY_FAILURE_CONFIG,
                 MCP_WRITE_REPLY_FAILURE_ALL,
                 MCP_WRITE_REPLY_FAILURE_CDI        : // This only happens if the write can take a while (like a CV write).   In that case a special Datagram Rejected message is sent.. see MTI_DATAGRAM_OK_REPLY above
                     begin
                       ErrorCode := ENGINE_ERROR_MEMORY_SPACE_WRITE_ERROR;
                       EngineStateError;
                       CallbackAndNextMemorySpace;
                     end;

                 MCP_READ_REPLY :
                   begin
                     case SourceMessage.DataArray[6] of
                       MSI_ALL                      : begin end;
                       MSI_CDI,
                       MSI_ACDI_MFG,
                       MSI_ACDI_USER,
                       MSI_TRACTION_FDI             : if DecodeMemoryAddress(SourceMessage) = MemorySpace then
                                                        HandleReadReply(SourceMessage, 7);
                       MSI_CONFIG,
                       MSI_TRACTION_FUNCTION_CONFIG :if DecodeMemoryAddress(SourceMessage) = MemorySpace then
                                                       HandleReadReply(SourceMessage, 7);
                     end;
                   end;
                 MCP_READ_REPLY_CONFIGURATION       : if DecodeMemoryAddress(SourceMessage) = MemorySpace then
                                                        HandleReadReply(SourceMessage, 6);
                 MCP_READ_REPLY_ALL                 : begin end;
                 MCP_READ_REPLY_CDI                 : if DecodeMemoryAddress(SourceMessage) = MemorySpace then
                                                        HandleReadReply(SourceMessage, 6);
                 MCP_READ_REPLY_FAILURE,
                 MCP_READ_REPLY_FAILURE_CONFIG,
                 MCP_READ_REPLY_FAILURE_ALL,
                 MCP_READ_REPLY_FAILURE_CDI                  :
                     begin
                       ErrorCode := ENGINE_ERROR_MEMORY_SPACE_READ_ERROR;
                       EngineStateError;
                       CallbackAndNextMemorySpace;
                     end;
                 MCP_OP_GET_CONFIG_OPTIONS_REPLY             : begin end;
                 MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY     : ValidateAddressSpaceAndProceed(SourceMessage);
                 MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY : ValidateAddressSpaceAndProceed(nil);
                 MCP_OP_LOCK_REPLY                           : begin end;
                 MCP_OP_GET_UNIQUEID_REPLY                   : begin end;
               end;
             end;
          end;
        end;
      end;
    end;
  end;
end;


{ TDatagramQueue }

procedure TDatagramQueue.Remove(LccMessage: TLccMessage);
var
  iLocalMessage: Integer;
begin
  iLocalMessage := FindBySourceNode(LccMessage);
  if iLocalMessage > -1 then
    Queue. Delete(iLocalMessage);
end;

function TDatagramQueue.Add(LccMessage: TLccMessage): Boolean;
begin
  Result := True;
  Queue.Add(LccMessage);
  LccMessage.RetryAttemptsDatagram := 0;
  LccMessage.AbandonCount := 0;
end;

constructor TDatagramQueue.Create;
begin
  inherited Create;
  {$IFDEF DELPHI}
  Queue := TObjectList<TLccMessage>.Create;
  {$ELSE}
  Queue := TObjectList.Create;
  {$ENDIF}
  Queue.OwnsObjects := True;
end;

destructor TDatagramQueue.Destroy;
begin
  {$IFDEF FPC}
  FreeAndNil(FQueue);
  {$ELSE}
    Queue.DisposeOf;
  {$ENDIF}
  inherited Destroy;
end;

function TDatagramQueue.FindBySourceNode(LccMessage: TLccMessage): Integer;
var
  i: Integer;
  QueueAlias: Word;
  QueueNodeID: TNodeID;
begin
  Result := -1;
  i := 0;
  while i < Queue.Count do
  begin
    QueueAlias := (Queue[i] as TLccMessage).CAN.DestAlias;
    QueueNodeID := (Queue[i] as TLccMessage).DestID;
    if (QueueAlias <> 0) and (LccMessage.CAN.SourceAlias <> 0) then
    begin
      if QueueAlias = LccMessage.CAN.SourceAlias then
      begin
        Result := i;
        Break
      end;
    end else
    if not NullNodeID(QueueNodeID) and not NullNodeID(LccMessage.SourceID) then
    begin
      if EqualNodeID(QueueNodeID, LccMessage.SourceID, False) then
      begin
        Result := i;
        Break
      end;
    end;
    Inc(i)
  end;
end;

procedure TDatagramQueue.Clear;
begin
  Queue.Clear;
end;

procedure TDatagramQueue.Resend(LccMessage: TLccMessage);
var
  iLocalMessage: Integer;
  LocalMessage: TLccMessage;
begin
  iLocalMessage := FindBySourceNode(LccMessage);
  if iLocalMessage > -1 then
  begin
    LocalMessage := Queue[iLocalMessage] as TLccMessage;
    if LocalMessage.RetryAttemptsDatagram < 5 then
    begin
      LocalMessage := Queue[iLocalMessage] as TLccMessage;
      SendMessageFunc(Self, LocalMessage);
      LocalMessage.RetryAttemptsDatagram := LocalMessage.RetryAttemptsDatagram + 1;
    end else
      {$IFDEF DWSCRIPT}
      Queue.Remove(Queue.IndexOf(LocalMessage));
      {$ELSE}
      Queue.Delete(iLocalMessage);
      {$ENDIF}
  end;
end;

procedure TDatagramQueue.TickTimeout;
var
  LocalMessage: TLccMessage;
  i: Integer;
begin
  for i := Queue.Count - 1 downto 0 do
  begin
    LocalMessage := Queue[i] as TLccMessage;
    if LocalMessage.AbandonCount < 6 then   // 800ms * 6
      LocalMessage.AbandonCount := LocalMessage.AbandonCount + 1
    else
      Queue.Delete(i);
  end;
end;

{TLccNode }

procedure TLccNode.SetSendMessageFunc(AValue: TOnMessageEvent);
begin
  FSendMessageFunc := AValue;
  ProtocolSupportedProtocols.SendMessageFunc := AValue;
  FProtocolSimpleNodeInfo.SendMessageFunc := AValue;
  ProtocolMemoryConfigurationDefinitionInfo.SendMessageFunc := AValue;
  ProtocolMemoryOptions.SendMessageFunc := AValue;
  ProtocolMemoryInfo.SendMessageFunc := AValue;
  ProtocolEventConsumed.SendMessageFunc := AValue;
  ProtocolEventsProduced.SendMessageFunc := AValue;
  DatagramResendQueue.SendMessageFunc := AValue;
end;

procedure TLccNode.DoMemorySpaceReadEngineDone(MemoryReadEngine: TLccEngineMemorySpaceAccess);
begin

end;

procedure TLccNode.HandleTractionFDI_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);  // Can't write to this memory space
end;

procedure TLccNode.HandleOptionalInteractionRejected(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoOptionalInteractionRejected(Self,SourceMessage);
end;

procedure TLccNode.HandleProducerIdentifiedClear(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleProducerIdentifiedSet(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleProducerIdentifiedUnknown(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleProducerIdentify(var SourceMessage: TLccMessage);
begin
  SendProducerIdentify(SourceMessage.ExtractDataBytesAsEventID(0));
end;

procedure TLccNode.HandleEventsIdentify;
begin
  SendConsumedEvents;
  SendProducedEvents;
end;

procedure TLccNode.HandleConsumerIdentify(var SourceMessage: TLccMessage);
begin
  SendConsumerIdentify(SourceMessage.ExtractDataBytesAsEventID(0));
end;

procedure TLccNode.HandleDatagramOkReply(var SourceMessage: TLccMessage);
begin
  DatagramResendQueue.Remove(SourceMessage);
end;

procedure TLccNode.HandleProtocolLogRequest(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);
end;

procedure TLccNode.HandleDatagramRejectedReply(var SourceMessage: TLccMessage);
begin
  DatagramResendQueue.Resend(SourceMessage);
end;

procedure TLccNode.HandleOperationGetAddressSpaceInfo(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
  ProtocolMemoryInfo.LoadReply(SourceMessage, WorkerMessage);
  SendDatagramRequiredReply(SourceMessage, WorkerMessage);
end;

procedure TLccNode.HandleOperationGetConfiguration(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID,
  SourceMessage.CAN.SourceAlias);
  ProtocolMemoryOptions.LoadReply(WorkerMessage);
  SendDatagramRequiredReply(SourceMessage,WorkerMessage);
end;

procedure TLccNode.HandleOperationLock(var SourceMessage: TLccMessage);
begin

end;

procedure TLccNode.HandleOperationGetUniqueID(var SourceMessage: TLccMessage);
begin

end;

procedure TLccNode.HandleOperationFreeze(var SourceMessage: TLccMessage);
begin

end;

procedure TLccNode.HandleOperationIndicate(var SourceMessage: TLccMessage);
begin

end;

procedure TLccNode.HandleOperationReset(var SourceMessage: TLccMessage);
begin

end;

procedure TLccNode.HandleConsumerIdentifiedClear(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleConfiguration_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);     // We will be sending a Write Reply
  ProtocolMemoryConfiguration.DatagramWriteRequest(SourceMessage, StreamConfig);
end;

procedure TLccNode.HandleACDI_Manufacturer_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);  // Can't write to this memory space
end;

procedure TLccNode.HandleConfiguration_MemorySpaceRead(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID,
  SourceMessage.CAN.SourceAlias);
  ProtocolMemoryConfiguration.DatagramReadRequest(SourceMessage, WorkerMessage, StreamConfig);
  SendDatagramRequiredReply(SourceMessage,WorkerMessage);
end;

procedure TLccNode.HandleACDI_UserMemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);     // We will be sending a Write Reply
  ProtocolACDIUser.DatagramWriteRequest(SourceMessage, StreamConfig);
end;

procedure TLccNode.HandleStreamRead;
begin

end;

procedure TLccNode.HandleStreamWrite;
begin

end;

procedure TLccNode.HandleACDI_User_MemorySpaceRead(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID,
  SourceMessage.CAN.SourceAlias);
  ProtocolACDIUser.DatagramReadRequest(SourceMessage, WorkerMessage, StreamConfig);
  SendDatagramRequiredReply(SourceMessage,WorkerMessage);
end;

procedure TLccNode.HandleACDI_Manufacturer_MemorySpaceRead(
  var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID,
  SourceMessage.CAN.SourceAlias);
  ProtocolACDIMfg.DatagramReadRequest(SourceMessage, WorkerMessage, StreamManufacturerData);
  SendDatagramRequiredReply(SourceMessage, WorkerMessage);
end;

procedure TLccNode.HandleAll_MemorySpaceRead(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);
end;

procedure TLccNode.HandleCDI_MemorySpaceRead(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
  ProtocolMemoryConfigurationDefinitionInfo.DatagramReadRequest(SourceMessage, WorkerMessage, StreamCdi);
  SendDatagramRequiredReply(SourceMessage, WorkerMessage);
end;

procedure TLccNode.HandleCDI_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);  // Can't write to this memory space
end;

procedure TLccNode.HandleAll_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);  // Can't write to this memory space
end;

procedure TLccNode.HandleConsumerIdentifiedSet(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleConsumerIdentifiedUnknown(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleEventsIdentifyDest;
begin
  SendConsumedEvents;  // already known the destination is us
  SendProducedEvents;
end;

procedure TLccNode.HandleProtocolSupportInquiry(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadProtocolIdentifyReply(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ProtocolSupportedProtocols.EncodeFlags);
  SendMessageFunc(Self, WorkerMessage);
end;

procedure TLccNode.HandleProtocolSupportReply(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoProtocolIdentifyReply(Self, SourceMessage);
end;

procedure TLccNode.HandleSimpleNodeInfoReply(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoSimpleNodeIdentReply(Self, SourceMessage);
end;

procedure TLccNode.HandleSimpleNodeInfoRequest(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, FAliasID,
  SourceMessage.SourceID, SourceMessage.CAN.SourceAlias,
  ProtocolSimpleNodeInfo.PackedFormat(StreamManufacturerData, StreamConfig));
  SendMessageFunc(Self, WorkerMessage);
end;

procedure TLccNode.HandleTractionFDI_ConfigurationMemorySpaceRead(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID,
  SourceMessage.CAN.SourceAlias);
  ProtocolMemoryConfiguration.DatagramReadRequest(SourceMessage, WorkerMessage, StreamTractionConfig);
  SendDatagramRequiredReply(SourceMessage, WorkerMessage);
end;

procedure TLccNode.HandleTractionFDI_MemorySpaceRead(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadDatagram(NodeID, FAliasID, SourceMessage.SourceID,SourceMessage.CAN.SourceAlias);
  ProtocolMemoryConfigurationDefinitionInfo.DatagramReadRequest(SourceMessage, WorkerMessage, StreamTractionFdi);
  SendDatagramRequiredReply(SourceMessage, WorkerMessage);
end;

procedure TLccNode.HandleTractionFDI_ConfigurationMemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  SendDatagramAckReply(SourceMessage, False, 0);     // We will be sending a Write Reply
  ProtocolMemoryConfiguration.DatagramWriteRequest(SourceMessage, StreamTractionConfig);
end;

procedure TLccNode.HandleVerifiedNodeIDNumber(var SourceMessage: TLccMessage);
begin
  (NodeManager as INodeManagerCallbacks).DoVerifiedNodeID(Self, SourceMessage);
end;

procedure TLccNode.HandleVerifyNodeIDNumber(var SourceMessage: TLccMessage);
var
  TestNodeID: TNodeID;
begin
  if SourceMessage.DataCount = 6 then
  begin
    TestNodeID := NULL_NODE_ID;
    SourceMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
    if EqualNodeID(TestNodeID, NodeID, False) then
    begin
      WorkerMessage.LoadVerifiedNodeID(NodeID, FAliasID);
      SendMessageFunc(Self, WorkerMessage);
    end
  end else
  begin
    WorkerMessage.LoadVerifiedNodeID(NodeID, FAliasID);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccNode.HandleVerifyNodeIDNumberDest;
begin
  WorkerMessage.LoadVerifiedNodeID(NodeID, FAliasID);
  SendMessageFunc(Self, WorkerMessage);
end;

procedure TLccNode.HandleTractionControllerAssign(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionControllerRelease(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionControllerQuery(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionControllerChangedNotify(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionEStop(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionListenerAttach(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionListenerDetach(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionListenerQuery(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionManageReserve(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionManageRelease(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionSetSpeed(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionSetFunction(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionSimpleTrainInfoReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionQuerySpeed(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionQueryFunction(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionControllerAssignReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionControllerQueryReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionListenerAttachReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionListenerDetachReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionListenerQueryReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionManageReserveReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionQuerySpeedReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionQueryFunctionReply(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.AfterLogin;
begin
  // For overriding in decentants
end;

function TLccNode.GetAliasIDStr: String;
begin
  Result := NodeAliasToString(AliasID);
end;

function TLccNode.LoadManufacturerDataStream(ACdi: string): Boolean;
var
  AnOffset, ALength, i: Integer;
begin
  Result := False;

  StreamManufacturerData.Size := LEN_MANUFACTURER_INFO;
  for i := 0 to StreamManufacturerData.Size - 1 do
    StreamWriteByte(StreamManufacturerData, 0);

  StreamManufacturerData.Position := ADDRESS_VERSION;
  StreamWriteByte(StreamManufacturerData, 1);

  AnOffset := 0;
  ALength := 0;
  if FindCdiElement(ACdi, '<manufacturer>', AnOffset, ALength) then
  begin
    if ALength < LEN_MFG_NAME then
    begin
      StreamManufacturerData.Position := ADDRESS_MFG_NAME;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<model>', AnOffset, ALength) then
  begin
    if ALength < LEN_MODEL_NAME then
    begin
      StreamManufacturerData.Position := ADDRESS_MODEL_NAME;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<hardwareVersion>', AnOffset, ALength) then
  begin
    if ALength < LEN_HARDWARE_VERSION then
    begin
      StreamManufacturerData.Position := ADDRESS_HARDWARE_VERSION;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<softwareVersion>', AnOffset, ALength) then
  begin
    if ALength < LEN_SOFTWARE_VERSION then
    begin
      StreamManufacturerData.Position := ADDRESS_SOFTWARE_VERSION;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  Result := True;
end;

constructor TLccNode.Create(ANodeManager: {$IFDEF DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean);
var
  i, Counter: Integer;
begin
  inherited Create;
  FProtocolSupportedProtocols := TProtocolSupportedProtocols.Create;
  FProtocolSimpleNodeInfo := TProtocolSimpleNodeInfo.Create;
  FProtocolMemoryConfigurationDefinitionInfo := TProtocolMemoryConfigurationDefinitionInfo.Create;
  FProtocolMemoryOptions := TProtocolMemoryOptions.Create;
  FProtocolMemoryConfiguration := TProtocolMemoryConfiguration.Create;
  FProtocolMemoryInfo := TProtocolMemoryInfo.Create;
  FProtocolEventConsumed := TProtocolEvents.Create;
  FProtocolEventsProduced := TProtocolEvents.Create;

  FProtocolACDIMfg := TProtocolACDIMfg.Create;
  FProtocolACDIUser := TProtocolACDIUser.Create;
  FStreamCdi := TMemoryStream.Create;
  FStreamConfig := TMemoryStream.Create;
  FStreamManufacturerData := TMemoryStream.Create;
  FStreamTractionConfig := TMemoryStream.Create;
  FStreamTractionFdi := TMemoryStream.Create;

  FDatagramResendQueue := TDatagramQueue.Create;
  FWorkerMessageDatagram := TLccMessage.Create;
  FWorkerMessage := TLccMessage.Create;
  FNodeManager := ANodeManager;
  FGridConnect := GridConnectLink;
  FEngineMemorySpaceAccess := TLccEngineMemorySpaceAccess.Create(Self);
  FEnginesRegisteredList := TList.Create;
  RegisterEngine(EngineMemorySpaceAccess);

 // FMessageIdentificationList := TLccMessageWithNodeIdentificationList.Create;
 // FMessageDestinationsWaitingForReply := TLccNodeIdentificationObjectList.Create(False);

  if CdiXML = '' then
    CdiXML := GetCdiFile;

  // Setup the Cdi Stream
  StreamCdi.Size := Int64( Length(CdiXML)) + 1;   // Need the null
  i := Low(CdiXML);
  for Counter := 0 to Length(CdiXML) - 1 do       // ios/android compatible
  begin
    StreamWriteByte(StreamCdi, Ord(CdiXML[i]));
    Inc(i);
  end;
  StreamWriteByte(StreamCdi, 0);

  // Setup the Manufacturer Data Stream from the XML to allow access for ACDI and SNIP
  LoadManufacturerDataStream(CdiXML);

  // Setup the Configuration Memory Stream
  StreamConfig.Size := LEN_USER_MANUFACTURER_INFO;
  StreamConfig.Position := 0;
  StreamWriteByte(StreamConfig, USER_MFG_INFO_VERSION_ID);
  while StreamConfig.Position < StreamConfig.Size do
    StreamWriteByte(StreamConfig, 0);

  // Setup the Fdi Stream

  // Setup the Function Configuration Memory Stream
end;

procedure TLccNode.AutoGenerateEvents;
var
  i: Integer;
  TempEventID: TEventID;
begin
  TempEventID := NULL_EVENT_ID;
  if ProtocolEventConsumed.AutoGenerate.Count > 0 then
  begin
    for i := 0 to ProtocolEventConsumed.AutoGenerate.Count - 1 do
    begin
      NodeIDToEventID(NodeID, ProtocolEventConsumed.AutoGenerate.StartIndex + i, TempEventID);
      ProtocolEventConsumed.Add(TempEventID, ProtocolEventConsumed.AutoGenerate.DefaultState);
    end;
    ProtocolEventConsumed.Valid := True;
  end;

  if ProtocolEventsProduced.AutoGenerate.Count > 0 then
  begin
    for i := 0 to ProtocolEventsProduced.AutoGenerate.Count - 1 do
    begin
      NodeIDToEventID(NodeID, ProtocolEventsProduced.AutoGenerate.StartIndex + i, TempEventID);
      ProtocolEventsProduced.Add(TempEventID, ProtocolEventsProduced.AutoGenerate.DefaultState);
    end;
    ProtocolEventsProduced.Valid := True;
  end;
end;

procedure TLccNode.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.AbbreviatedConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;
  ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := True;
  ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := True;
  ProtocolSupportedProtocols.TractionFunctionConfiguration := True;

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_TRACTION_FDI, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_TRACTION_FUNCTION_CONFIG, True, False, True, 0, $FFFFFFFF);

  ProtocolMemoryOptions.WriteUnderMask := True;
  ProtocolMemoryOptions.UnAlignedReads := True;
  ProtocolMemoryOptions.UnAlignedWrites := True;
  ProtocolMemoryOptions.SupportACDIMfgRead := True;
  ProtocolMemoryOptions.SupportACDIUserRead := True;
  ProtocolMemoryOptions.SupportACDIUserWrite := True;
  ProtocolMemoryOptions.WriteLenOneByte := True;
  ProtocolMemoryOptions.WriteLenTwoBytes := True;
  ProtocolMemoryOptions.WriteLenFourBytes := True;
  ProtocolMemoryOptions.WriteLenSixyFourBytes := True;
  ProtocolMemoryOptions.WriteArbitraryBytes := True;
  ProtocolMemoryOptions.WriteStream := False;
  ProtocolMemoryOptions.HighSpace := MSI_CDI;
  ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;

  // Create a few events for fun
  ProtocolEventConsumed.AutoGenerate.Count := 5;
  ProtocolEventConsumed.AutoGenerate.StartIndex := 0;
  ProtocolEventsProduced.AutoGenerate.Count := 5;
  ProtocolEventsProduced.AutoGenerate.StartIndex := 0;
end;

procedure TLccNode.LccLogIn(ANodeID: TNodeID);
begin
  if Enabled then
  begin
    BeforeLogin;
    if NullNodeID(ANodeID) then
      CreateNodeID(ANodeID);  // This should only be true if not GridConnect and the NodeID was not set
    FNodeID := ANodeID;
    (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);
    FInitialized := True;

    // Send Initialization Complete
    WorkerMessage.LoadInitializationComplete(NodeID, FAliasID);
    SendMessageFunc(Self, WorkerMessage);
    (NodeManager as INodeManagerCallbacks).DoInitializationComplete(Self);


    AutoGenerateEvents;
    SendEvents;
    (NodeManager as INodeManagerCallbacks).DoLogInNode(Self);
    AfterLogin;
  end;
end;

function TLccNode.ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;

var
  AddressSpace: Byte;
  i: Integer;
begin

  // By the time a messages drops into this method it is a fully qualified OpenLCB
  // message.  Any CAN messages that are sent as multi frames have been combined
  // into a full OpenLCB message.
  // On GridConnect all messages have the source NodeID mapped and on messages destined
  // for this node (DestID/DestAlias) the full NodeID has been filled in.  There is an
  // attempt to fill in the DestID for messages that are not for this node but
  // it can't be guarenteed that field will be valid.  This should not be of concern
  // as we should not be doing anything with those messages anyway other than possibly snooping

  Result := False;

  if not Initialized then
  begin
    Result := True; // Handled
    Exit;
  end;


  // Guarenteed to have Mappings before I ever get here

  // First look for a duplicate NodeID
  if EqualNodeID(NodeID, SourceMessage.SourceID, False) then
  begin
    // Think I am suppose to send a duplicate Node ID PCER or something here....
    Result := True; // Handled
    FInitialized := False;
    Exit;
  end;

  // Next look to see if it is an addressed message and if not for use just exit

  if SourceMessage.HasDestination then
  begin
    if not EqualNode(NodeID,  AliasID, SourceMessage.DestID, SourceMessage.CAN.DestAlias, True) then
      Exit;
  end;

  for i := 0 to EnginesRegisteredList.Count - 1 do
    TLccEngineBase(EnginesRegisteredList[i]).Process(SourceMessage);

  case SourceMessage.MTI of
    MTI_OPTIONAL_INTERACTION_REJECTED : HandleOptionalInteractionRejected(SourceMessage);

    // *************************************************************************
    // *************************************************************************
    MTI_VERIFY_NODE_ID_NUMBER      : HandleVerifyNodeIDNumber(SourceMessage);
    MTI_VERIFY_NODE_ID_NUMBER_DEST : HandleVerifyNodeIDNumberDest;
    MTI_VERIFIED_NODE_ID_NUMBER    : HandleVerifiedNodeIDNumber(SourceMessage);

    // *************************************************************************
    // *************************************************************************
    MTI_SIMPLE_NODE_INFO_REQUEST : HandleSimpleNodeInfoRequest(SourceMessage);
    MTI_SIMPLE_NODE_INFO_REPLY   : HandleSimpleNodeInfoReply(SourceMessage);

    // *************************************************************************
    // *************************************************************************
    MTI_PROTOCOL_SUPPORT_INQUIRY : HandleProtocolSupportInquiry(SourceMessage);
    MTI_PROTOCOL_SUPPORT_REPLY   : HandleProtocolSupportReply(SourceMessage);

    // *************************************************************************
    // Producer/Consumer tell me what events do you care about (for routers, getting mass
    // results for the state of the layout
    // *************************************************************************
    MTI_EVENTS_IDENTIFY      : HandleEventsIdentify;
    MTI_EVENTS_IDENTIFY_DEST : HandleEventsIdentifyDest;

    // *************************************************************************
    // General Producer/Consumer Queries
    // *************************************************************************
    MTI_PRODUCER_IDENDIFY : HandleProducerIdentify(SourceMessage);
    MTI_CONSUMER_IDENTIFY : HandleConsumerIdentify(SourceMessage);

    // *************************************************************************
     // This block of messages is if we sent at "Producer" or "Consumer" Identify
     // and these are the results coming back... I am not sure what "Consumer" Identify
     // needs different states as the replying node is not in control of the state only
     // the "Producer" is in control
     // *************************************************************************
    MTI_CONSUMER_IDENTIFIED_CLEAR   : HandleConsumerIdentifiedClear(SourceMessage);
    MTI_CONSUMER_IDENTIFIED_SET     : HandleConsumerIdentifiedSet(SourceMessage);
    MTI_CONSUMER_IDENTIFIED_UNKNOWN : HandleConsumerIdentifiedUnknown(SourceMessage);
    MTI_PRODUCER_IDENTIFIED_CLEAR   : HandleProducerIdentifiedClear(SourceMessage);
    MTI_PRODUCER_IDENTIFIED_SET     : HandleProducerIdentifiedSet(SourceMessage);
    MTI_PRODUCER_IDENTIFIED_UNKNOWN : HandleProducerIdentifiedUnknown(SourceMessage);

    // *************************************************************************
    // Traction (just so we say we handle them somewere and don't send a Optional Interaction Rejected below
    // *************************************************************************
    MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY : HandleTractionSimpleTrainInfoReply(SourceMessage);

    MTI_TRACTION_REPLY :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_QUERY_SPEED         : HandleTractionQuerySpeedReply(SourceMessage);
          TRACTION_QUERY_FUNCTION      : HandleTractionQueryFunctionReply(SourceMessage);

          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN  : HandleTractionControllerAssignReply(SourceMessage);
                TRACTION_CONTROLLER_CONFIG_QUERY   : HandleTractionControllerQueryReply(SourceMessage);
              end;
            end;
          TRACTION_LISTENER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_LISTENER_CONFIG_ATTACH : HandleTractionListenerAttachReply(SourceMessage);
                TRACTION_LISTENER_CONFIG_DETACH : HandleTractionListenerDetachReply(SourceMessage);
                TRACTION_LISTENER_CONFIG_QUERY  : HandleTractionListenerQueryReply(SourceMessage);
              end;
            end;
          TRACTION_MANAGE :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE : HandleTractionManageReserveReply(SourceMessage);
                TRACTION_MANANGE_HEARTBEAT_REPLY : begin end;  // TODO
              end
            end;
        end
      end;
    MTI_TRACTION_REQUEST :
      begin
        case SourceMessage.DataArray[0] of
          TRACTION_SET_SPEED_DIR       : HandleTractionSetSpeed(SourceMessage);
          TRACTION_SET_FUNCTION        : HandleTractionSetFunction(SourceMessage);
          TRACTION_SET_E_STOP          : HandleTractionEStop(SourceMessage);
          TRACTION_QUERY_SPEED         : HandleTractionQuerySpeed(SourceMessage);
          TRACTION_QUERY_FUNCTION      : HandleTractionQueryFunction(SourceMessage);
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN  : HandleTractionControllerAssign(SourceMessage);
                TRACTION_CONTROLLER_CONFIG_RELEASE : HandleTractionControllerRelease(SourceMessage);
                TRACTION_CONTROLLER_CONFIG_QUERY   : HandleTractionControllerQuery(SourceMessage);
                TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY : HandleTractionControllerChangedNotify(SourceMessage);
              end
            end;
          TRACTION_LISTENER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_LISTENER_CONFIG_ATTACH : HandleTractionListenerAttach(SourceMessage);
                TRACTION_LISTENER_CONFIG_DETACH : HandleTractionListenerDetach(SourceMessage);
                TRACTION_LISTENER_CONFIG_QUERY  : HandleTractionListenerQuery(SourceMessage);
              end;
            end;
          TRACTION_MANAGE :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_MANAGE_RESERVE : HandleTractionManageReserve(SourceMessage);
                TRACTION_MANAGE_RELEASE : HandleTractionManageRelease(SourceMessage);
              end
            end;
        end;
      end;


    // *************************************************************************
    // Datagram Messages
    // *************************************************************************
     MTI_DATAGRAM_REJECTED_REPLY : HandleDatagramRejectedReply(SourceMessage);
     MTI_DATAGRAM_OK_REPLY       : HandleDatagramOkReply(SourceMessage);
     MTI_DATAGRAM :
       begin
         case SourceMessage.DataArray[0] of
           DATAGRAM_PROTOCOL_LOGREQUEST : HandleProtocolLogRequest(SourceMessage); {0x01}  // Makes the Python Script Happy
           DATAGRAM_PROTOCOL_CONFIGURATION :                                       {0x20}  // Configuration
             begin
               AddressSpace := 0;

               // Figure out where the Memory space to work on is located, encoded in the header or in the first databyte slot.
               case SourceMessage.DataArray[1] and $03 of
                 MCP_NONE          : AddressSpace := SourceMessage.DataArray[6];
                 MCP_CDI           : AddressSpace := MSI_CDI;
                 MCP_ALL           : AddressSpace := MSI_ALL;
                 MCP_CONFIGURATION : AddressSpace := MSI_CONFIG;
               end;

               case SourceMessage.DataArray[1] and $F0 of
                 MCP_WRITE :
                   begin
                     case AddressSpace of
                       MSI_CDI                      : HandleCDI_MemorySpaceWrite(SourceMessage);
                       MSI_ALL                      : HandleAll_MemorySpaceWrite(SourceMessage);
                       MSI_CONFIG                   : HandleConfiguration_MemorySpaceWrite(SourceMessage); // Configuration Memory through the CDI protocol
                       MSI_ACDI_MFG                 : HandleACDI_Manufacturer_MemorySpaceWrite(SourceMessage);
                       MSI_ACDI_USER                : HandleACDI_UserMemorySpaceWrite(SourceMessage);            // Configuration Memory through the Abbreviated CDI protocol
                       MSI_TRACTION_FDI             : HandleTractionFDI_MemorySpaceWrite(SourceMessage);
                       MSI_TRACTION_FUNCTION_CONFIG : HandleTractionFDI_ConfigurationMemorySpaceWrite(SourceMessage); // Traction Function Configuration Memory
                     end;
                   end;
                 MCP_READ :
                   begin
                     case AddressSpace of
                       MSI_CDI                      : HandleCDI_MemorySpaceRead(SourceMessage);
                       MSI_ALL                      : HandleAll_MemorySpaceRead(SourceMessage);
                       MSI_CONFIG                   : HandleConfiguration_MemorySpaceRead(SourceMessage);
                       MSI_ACDI_MFG                 : HandleACDI_Manufacturer_MemorySpaceRead(SourceMessage);
                       MSI_ACDI_USER                : HandleACDI_User_MemorySpaceRead(SourceMessage);
                       MSI_TRACTION_FDI             : HandleTractionFDI_MemorySpaceRead(SourceMessage);
                       MSI_TRACTION_FUNCTION_CONFIG : HandleTractionFDI_ConfigurationMemorySpaceRead(SourceMessage);
                     end;
                   end;
                 MCP_WRITE_STREAM : HandleStreamWrite;
                 MCP_READ_STREAM  : HandleStreamRead;
                 MCP_OPERATION :
                   begin
                     case SourceMessage.DataArray[1] of
                       MCP_OP_GET_CONFIG_OPTIONS : HandleOperationGetConfiguration(SourceMessage);
                       MCP_OP_GET_ADD_SPACE_INFO : HandleOperationGetAddressSpaceInfo(SourceMessage);
                       MCP_OP_LOCK               : HandleOperationLock(SourceMessage);
                       MCP_OP_GET_UNIQUEID       : HandleOperationGetUniqueID(SourceMessage);
                       MCP_OP_FREEZE             : HandleOperationFreeze(SourceMessage);
                       MCP_OP_INDICATE           : HandleOperationIndicate(SourceMessage);
                       MCP_OP_RESETS             : HandleOperationReset(SourceMessage);
                     end // case
                   end;
               end
             end
         else begin {case else}
             // Unknown Datagram Type
             WorkerMessage.LoadDatagramRejected(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ERROR_PERMANENT_NOT_IMPLEMENTED or ERROR_TYPE);
             SendMessageFunc(Self, WorkerMessage);
           end;
         end;  // case
       end;
  else begin
      if SourceMessage.HasDestination then
      begin
        WorkerMessage.LoadOptionalInteractionRejected(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.CAN.SourceAlias, ERROR_PERMANENT_NOT_IMPLEMENTED or ERROR_MTI, SourceMessage.MTI);
        SendMessageFunc(Self, WorkerMessage)
      end;
    end;
  end; // case
end;

function TLccNode.ProcessMessageGridConnect(SourceMessage: TLccMessage): Boolean;

var
  TestNodeID: TNodeID;
begin
  Result := False;
  TestNodeID[0] := 0;
  TestNodeID[1] := 0;

  // Alias Allocation, duplicate checking after allocation**********************
  // Check for a message with the Alias equal to our own.
  if (AliasID <> 0) and (SourceMessage.CAN.SourceAlias = AliasID) then
  begin
    // Check if it is a Check ID message for a node trying to use our Alias and if so tell them no.
    if ((SourceMessage.CAN.MTI and $0F000000) >= MTI_CAN_CID6) and ((SourceMessage.CAN.MTI and $0F000000) <= MTI_CAN_CID0) then
    begin
      WorkerMessage.LoadRID(NodeID, AliasID);                   // sorry charlie this is mine
      SendMessageFunc(Self, WorkerMessage);
      Result := True;
    end else
    if Permitted then
    begin
      // Another node used our Alias, stop using this Alias, log out and allocate a new node and relog in
      ReleaseAlias(100);
      Relogin;
      Result := True;   // Logout covers any LccNode logoffs, so don't call ancester Process Message
    end
  end;
  // END: Alias Allocation, duplicate checking after allocation******************


  if not Permitted then
  begin
    // We are still trying to allocate a new Alias, someone else is using this alias
    if SourceMessage.CAN.SourceAlias = AliasID then
      DuplicateAliasDetected := True;
  end else
  begin  // Normal message loop once successfully allocating an Alias
    if SourceMessage.IsCAN then
    begin
      case SourceMessage.CAN.MTI of
        MTI_CAN_AME :          // Asking us for an Alias Map Enquiry
          begin
            if SourceMessage.DataCount = 6 then
            begin
              SourceMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
              if EqualNodeID(TestNodeID, NodeID, False) then
              begin
                WorkerMessage.LoadAMD(NodeID, AliasID);
                SendMessageFunc(Self, WorkerMessage);
              end
            end else
            begin
              WorkerMessage.LoadAMD(NodeID, AliasID);
              SendMessageFunc(Self, WorkerMessage);
            end;
          end;
      end;
      Result := True;
    end else
    begin
      if not Result then
        ProcessMessageLCC(SourceMessage);
    end;
  end;
end;

procedure TLccNode.RegisterEngine(AnEngine: TLccEngineBase);
begin
  EnginesRegisteredList.Add(AnEngine);
end;

procedure TLccNode.UnRegisterEngine(AnEngine: TLccEngineBase);
begin
  EnginesRegisteredList.Remove(AnEngine);
end;

function TLccNode.GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
begin
  Result := (Seed[0] xor Seed[1] xor (Seed[0] shr 12) xor (Seed[1] shr 12)) and $00000FFF;
end;

procedure TLccNode.GenerateNewSeed(var Seed: TNodeID);
var
  temp1,              // Upper 24 Bits of temp 48 bit number
  temp2: DWORD;       // Lower 24 Bits of temp 48 Bit number
begin
  temp1 := ((Seed[1] shl 9) or ((Seed[0] shr 15) and $000001FF)) and $00FFFFFF;   // x(i+1)(2^9 + 1)*x(i) + C  = 2^9 * x(i) + x(i) + C
  temp2 := (Seed[0] shl 9) and $00FFFFFF;                                                                  // Calculate 2^9 * x

  Seed[0] := Seed[0] + temp2 + $7A4BA9;   // Now y = 2^9 * x so all we have left is x(i+1) = y + x + c
  Seed[1] := Seed[1] + temp1 + $1B0CA3;

  Seed[1] := (Seed[1] and $00FFFFFF) or (Seed[0] and $FF000000) shr 24;   // Handle the carries of the lower 24 bits into the upper
  Seed[0] := Seed[0] and $00FFFFFF;
end;

procedure TLccNode.Relogin;
var
  Temp: TNodeID;
begin
  // Typically due to an alias conflict to create a new one
  Temp := FSeedNodeID;
  GenerateNewSeed(Temp);
  FSeedNodeID := Temp;
  FAliasID := GenerateID_Alias_From_Seed(Temp);
  WorkerMessage.LoadCID(NodeID, AliasID, 0);
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 1);
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 2);
  SendMessageFunc(Self, WorkerMessage);
  WorkerMessage.LoadCID(NodeID, AliasID, 3);
  SendMessageFunc(Self, WorkerMessage);

  LoginTimoutCounter := 0;
end;

procedure TLccNode.NotifyAndUpdateMappingChanges;
var
  LocalMapping: TLccAliasMapping;
  LocalNodeIdentificationObject: TLccNodeIdentificationObject;
  MappingList: TList;
  i: Integer;
begin
  MappingList := AliasServer.MappingList.LockList;
  try
    for i := MappingList.Count - 1 downto 0 do
    begin
      LocalMapping := TLccAliasMapping(MappingList[i]);
      if LocalMapping.MarkedForInsertion then
      begin
        (NodeManager as INodeManagerCallbacks).DoAliasMappingChange(Self, LocalMapping, True);
        LocalMapping.MarkedForInsertion := False;  // Handled
      end;
      if LocalMapping.MarkedForDeletion then
      begin
        (NodeManager as INodeManagerCallbacks).DoAliasMappingChange(Self, LocalMapping, False);
        {$IFDEF LOG_MAPPING}DebugLn('Mapping Deleted: 0x' + IntToHex(LocalMapping.NodeAlias, 4) + '; ' + NodeIDToString(LocalMapping.NodeID, True));{$ENDIF}
        LocalMapping.Free;
        MappingList.Delete(i);
      end;
    end;
  finally
    AliasServer.MappingList.UnlockList;
  end;

  MappingList := AliasServer.MappingRequestList.LockList;
  try
    for i := 0 to MappingList.Count - 1 do
    begin
      LocalNodeIdentificationObject := TLccNodeIdentificationObject( MappingList[i]);

      if LocalNodeIdentificationObject.AbandonCount = 0 then
        AliasServer.WorkerMappingRequestList.Add(LocalNodeIdentificationObject.Clone)
      else begin
        if LocalNodeIdentificationObject.AbandonCount > 20 then
        begin
          {$IFDEF LOG_MAPPING}DebugLn('Mapping Request Deleted: 0x' + IntToHex(LocalNodeIdentificationObject.Alias, 4) + '; ' + NodeIDToString(LocalNodeIdentificationObject.NodeID, True));{$ENDIF}
          FreeAndNil(LocalNodeIdentificationObject);
          MappingList[i] := nil;
        end;
      end;
      if Assigned(LocalNodeIdentificationObject) then
        LocalNodeIdentificationObject.AbandonCount := LocalNodeIdentificationObject.AbandonCount + 1;
    end;

    for i := MappingList.Count - 1 downto 0 do
      if MappingList[i] = nil then
        MappingList.Delete(i);
  finally
    AliasServer.MappingRequestList.UnlockList;
  end;


  // do this outside of the MappingRequestList lock
  try
    for i := 0 to AliasServer.WorkerMappingRequestList.Count - 1 do
    begin
      LocalNodeIdentificationObject := TLccNodeIdentificationObject( AliasServer.WorkerMappingRequestList[i]);

      if LocalNodeIdentificationObject.Alias <> 0 then
      begin
        // We have the Alias but not the NodeID so use addressed to that Alias (don't need the NodeID for CAN)
        WorkerMessage.LoadVerifyNodeIDAddressed(NodeID, AliasID, LocalNodeIdentificationObject.NodeID, LocalNodeIdentificationObject.Alias, NULL_NODE_ID);
        SendMessageFunc(Self, WorkerMessage);
        {$IFDEF LOG_MAPPING}DebugLn('Mapping Request Sent: 0x' + IntToHex(LocalNodeIdentificationObject.Alias, 4) + '; ' + NodeIDToString(LocalNodeIdentificationObject.NodeID, True));{$ENDIF}
      end else
      if not NullNodeID(LocalNodeIdentificationObject.NodeID) then
      begin
        WorkerMessage.LoadVerifyNodeID(NodeID, AliasID, LocalNodeIdentificationObject.NodeID);
        SendMessageFunc(Self, WorkerMessage);
        {$IFDEF LOG_MAPPING}DebugLn('Mapping Request Sent: 0x' + IntToHex(LocalNodeIdentificationObject.Alias, 4) + '; ' + NodeIDToString(LocalNodeIdentificationObject.NodeID, True));{$ENDIF}
      end;
    end;
  finally
    AliasServer.ClearWorkerMappingRequests;
  end;

end;

procedure TLccNode.CreateNodeID(var Seed: TNodeID);
begin
  Seed[1] := StrToInt('0x020112');
  Seed[0] := Random($FFFFFF);
end;

destructor TLccNode.Destroy;
begin
  NotifyAndUpdateMappingChanges; // fire any eventfor Mapping changes are are marked for deletion in the Logout method

  UnRegisterEngine(EngineMemorySpaceAccess);

  if GridConnect then
  begin
    if AliasID <> 0 then
      ReleaseAlias(100);
     (NodeManager as INodeManagerCallbacks).DoAliasIDChanged(Self);
  end;

  FNodeID[0] := 0;
  FNodeID[1] := 0;
  (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);

  (NodeManager as INodeManagerCallbacks).DoDestroyLccNode(Self);

  FreeAndNil(FProtocolSupportedProtocols);
  FreeAndNil(FProtocolSimpleNodeInfo);
  FreeAndNil(FProtocolEventConsumed);
  FreeAndNil(FProtocolEventsProduced);
  FreeAndNil(FProtocolMemoryOptions);
  FreeAndNil(FProtocolMemoryInfo);
  FreeAndNil(FProtocolACDIMfg);
  FreeAndNil(FProtocolACDIUser);
  FreeAndNil(FProtocolMemoryConfigurationDefinitionInfo);
  FreeAndNil(FDatagramResendQueue);
  FreeAndNil(FWorkerMessageDatagram);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FStreamCdi);
  FreeAndNil(FStreamConfig);
  FreeAndNil(FStreamManufacturerData);
  FreeAndNil(FStreamTractionConfig);
  FreeAndNil(FStreamTractionFdi);
  FreeAndNil(FEnginesRegisteredList);
  FreeAndNil(FEngineMemorySpaceAccess);
  FProtocolMemoryConfiguration.Free;
  inherited;
end;

function TLccNode.FindCdiElement(TestXML, Element: string; var Offset: Integer; var ALength: Integer): Boolean;
var
  OffsetEnd: Integer;
begin
  Result := False;
  TestXML := LowerCase(TestXML);
  Element := LowerCase(Element);
  Offset := Pos(Element, TestXML);
  if Offset > -1 then
  begin
    Inc(Offset, Length(Element));
    Element := StringReplace(Element, '<', '</', [rfReplaceAll]);
    OffsetEnd := Pos(Element, TestXML);
    if (OffsetEnd > -1) and (OffsetEnd > Offset) then
    begin
      ALength := OffsetEnd - Offset;
      Result := True;
      OffsetEnd := Low(TestXML);  // The "Low" would not work in the following if statement directly in Delphi
      if OffsetEnd = 0 then   // Mobile
        Dec(Offset, 1);
    end else
    Exit;
  end
end;

function TLccNode.GetCdiFile: string;
begin
  Result := CDI_XML;
end;

function TLccNode.GetNodeIDStr(WithDots: Boolean): String;
begin
  Result := NodeIDToString(NodeID, WithDots);
end;

procedure TLccNode.Login(ANodeID: TNodeID);
var
  Temp: TNodeID;
begin
  Enabled := True;
  if GridConnect then
  begin
    BeforeLogin;
    if NullNodeID(ANodeID) then
      CreateNodeID(ANodeID);
    SeedNodeID := ANodeID;
    Temp := FSeedNodeID;
    FAliasID := GenerateID_Alias_From_Seed(Temp);
    (NodeManager as INodeManagerCallbacks).DoNodeIDChanged(Self);
    FNodeID := ANodeID;

    WorkerMessage.LoadCID(NodeID, AliasID, 0);
    SendMessageFunc(Self, WorkerMessage);
    WorkerMessage.LoadCID(NodeID, AliasID, 1);
    SendMessageFunc(Self, WorkerMessage);
    WorkerMessage.LoadCID(NodeID, AliasID, 2);
    SendMessageFunc(Self, WorkerMessage);
    WorkerMessage.LoadCID(NodeID, AliasID, 3);
    SendMessageFunc(Self, WorkerMessage);

    LoginTimoutCounter := 0;
  end else
  begin
    BeforeLogIn;
    LccLogIn(ANodeID);
  end;
end;

procedure TLccNode.ReleaseAlias(DelayTime_ms: Word);
begin
  if AliasID <> 0 then
  begin
    if GridConnect then
    begin
      WorkerMessage.LoadAMR(NodeID, AliasID);
      SendMessageFunc(Self, WorkerMessage);
      // Wait for the message to get sent on the hardware layers.  Testing this happens is complicated
      // This assumes they are all running in separate thread and they keep running
      Sleep(DelayTime_ms);
      FPermitted := False;
      (NodeManager as INodeManagerCallbacks).DoAliasRelease(Self);
      AliasServer.MarkForRemovalByAlias(AliasID);
      FAliasID := 0;
      Enabled := False; // must call LogIn again to reenable
    end;
    DatagramResendQueue.Clear;
    FInitialized := False;
  end;
end;

procedure TLccNode.On_100msTimer(Sender: TObject);
var
  Temp: TNodeID;
begin
  if GridConnect then
  begin
    if Enabled then
    begin
    if not Permitted then
      begin
        Inc(FLoginTimoutCounter);
         // Did any node object to this Alias through ProcessMessage?
        if DuplicateAliasDetected then
        begin
          DuplicateAliasDetected := False;  // Reset
          Temp := FSeedNodeID;
          GenerateNewSeed(Temp);
          FSeedNodeID := Temp;
          FAliasID := GenerateID_Alias_From_Seed(Temp);
          WorkerMessage.LoadCID(NodeID, AliasID, 0);
          SendMessageFunc(Self, WorkerMessage);
          WorkerMessage.LoadCID(NodeID, AliasID, 1);
          SendMessageFunc(Self, WorkerMessage);
          WorkerMessage.LoadCID(NodeID, AliasID, 2);
          SendMessageFunc(Self, WorkerMessage);
          WorkerMessage.LoadCID(NodeID, AliasID, 3);
          SendMessageFunc(Self, WorkerMessage);
          LoginTimoutCounter := 0;
        end else
        begin
          if LoginTimoutCounter > 7 then
          begin
            FPermitted := True;
            WorkerMessage.LoadRID(NodeID, AliasID);
            SendMessageFunc(Self, WorkerMessage);
            WorkerMessage.LoadAMD(NodeID, AliasID);
            SendMessageFunc(Self, WorkerMessage);
            (NodeManager as INodeManagerCallbacks).DoAliasIDChanged(Self);
            LccLogIn(NodeID);
          end;
        end
      end else  // Is Permitted
      begin
        DatagramResendQueue.TickTimeout;
        NotifyAndUpdateMappingChanges;
      end;
    end
  end else  // Is not GridConnect
  begin
    DatagramResendQueue.TickTimeout;
  end;
end;

function TLccNode.ProcessMessage(SourceMessage: TLccMessage): Boolean;
begin
  if GridConnect then
    Result := ProcessMessageGridConnect(SourceMessage)   // When necessary ProcessMessageGridConnect drops the message into ProcessMessageLCC
  else
    Result := ProcessMessageLCC(SourceMessage);
end;

procedure TLccNode.SendDatagramAckReply(SourceMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  // Only Ack if we accept the datagram
  WorkerMessageDatagram.LoadDatagramAck(NodeID, FAliasID,
                                        SourceMessage.SourceID, SourceMessage.CAN.SourceAlias,
                                        True, ReplyPending, TimeOutValueN);
  SendMessageFunc(Self, WorkerMessageDatagram);
end;

procedure TLccNode.SendConsumedEvents;
var
  i: Integer;
  Temp: TEventID;
begin
  for i := 0 to ProtocolEventConsumed.Count - 1 do
  begin
    Temp := ProtocolEventConsumed.Event[i].ID;
    WorkerMessage.LoadConsumerIdentified(NodeID, FAliasID, Temp, ProtocolEventConsumed.Event[i].State);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccNode.SendConsumerIdentify(Event: TEventID);
var
  EventObj: TLccEvent;
  Temp: TEventID;
begin
  EventObj := ProtocolEventConsumed.Supports(Event);
  if Assigned(EventObj) then
  begin
    Temp := EventObj.ID;
    WorkerMessage.LoadConsumerIdentified(NodeID, FAliasID, Temp, EventObj.State);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccNode.SendDatagramRejectedReply(SourceMessage: TLccMessage; Reason: Word);
begin
  WorkerMessageDatagram.LoadDatagramRejected(NodeID, FAliasID,
                                             SourceMessage.SourceID, SourceMessage.CAN.SourceAlias,
                                             Reason);
  SendMessageFunc(Self, WorkerMessageDatagram);
end;

procedure TLccNode.SendDatagramRequiredReply(SourceMessage, ReplyLccMessage: TLccMessage);
begin
  if DatagramResendQueue.Add(ReplyLccMessage.Clone) then     // Waiting for an ACK
  begin
    SendDatagramAckReply(SourceMessage, False, 0);   // We will be sending a Read Reply
    SendMessageFunc(Self, ReplyLccMessage);
  end else
    SendDatagramRejectedReply(SourceMessage, ERROR_TEMPORARY_BUFFER_UNAVAILABLE)
end;

procedure TLccNode.SendEvents;
begin
  SendConsumedEvents;
  SendProducedEvents;
end;

procedure TLccNode.SendProducedEvents;
var
  i: Integer;
  Temp: TEventID;
begin
  for i := 0 to ProtocolEventsProduced.Count - 1 do
  begin
    Temp := ProtocolEventsProduced.Event[i].ID;
    WorkerMessage.LoadProducerIdentified(NodeID, FAliasID, Temp, ProtocolEventsProduced.Event[i].State);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccNode.SendProducerIdentify(Event: TEventID);
var
  EventObj: TLccEvent;
  Temp: TEventID;
begin
  EventObj := ProtocolEventsProduced.Supports(Event);
  if Assigned(EventObj) then
  begin
    Temp := EventObj.ID;
    WorkerMessage.LoadProducerIdentified(NodeID, FAliasID, Temp, EventObj.State);
    SendMessageFunc(Self, WorkerMessage);
  end;
end;

procedure TLccNode.SendSNIPRequest(TargetNodeID: TNodeID; TargetAlias: Word);
begin
  WorkerMessage.LoadSimpleNodeIdentInfoRequest(NodeID, AliasID, TargetNodeID, TargetAlias);
  SendMessageFunc(Self, WorkerMessage);
end;

procedure TLccNode.SendTrainSNIPRequest(TargetNodeID: TNodeID; TargetAlias: Word);
begin
  WorkerMessage.LoadSimpleTrainNodeIdentInfoRequest(NodeID, AliasID, TargetNodeID, TargetAlias);
  SendMessageFunc(Self, WorkerMessage);
end;

initialization
  Randomize;

finalization

end.


