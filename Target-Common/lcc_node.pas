unit lcc_node;


{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  Math,
  {$IFDEF LCC_FPC}
    contnrs,
  {$ELSE}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  lcc_protocol_utilities,
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
  lcc_alias_server;

const
  ERROR_CONFIGMEM_ADDRESS_SPACE_MISMATCH = $0001;

{  TIMEOUT_TIME = 100; // milli seconds
  TIMEOUT_CONTROLLER_NOTIFY_WAIT = 5000;  // 5 seconds
  TIMEOUT_CONTROLLER_RESERVE_WAIT = 5000;
  TIMEOUT_NODE_VERIFIED_WAIT = 800;       // 800ms
  TIMEOUT_NODE_ALIAS_MAPPING_WAIT = 1000;       // 800ms
  TIMEOUT_CREATE_TRAIN_WAIT = 1000;       // 1000ms
  TIMEOUT_SNIP_REPONSE_WAIT = 500;
  TIMEOUT_LISTENER_ATTACH_TRAIN_WAIT = 5000;       // per listener, will have to map CAN Alias if on CAN so may take a bit...
                                  }


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
  TLccTaskMemorySpaceAccess = class;
  TLccTaskBase = class;

    { TDatagramQueue }

  TDatagramQueue = class
  private
    FOwnerNode: TLccNode;
    {$IFDEF LCC_DELPHI}
    FQueue: TObjectList<TLccMessage>;
    {$ELSE}
    FQueue: TObjectList;
    {$ENDIF}
    function GetCount: Integer;
  protected
    {$IFDEF LCC_DELPHI}
    property Queue: TObjectList<TLccMessage> read FQueue write FQueue;
    {$ELSE}
    property Queue: TObjectList read FQueue write FQueue;
    {$ENDIF}

    function FindBySourceNode(LccMessage: TLccMessage): Integer;
  public
    property Count: Integer read GetCount;
    property OwnerNode: TLccNode read FOwnerNode write FOwnerNode;

    constructor Create;
    destructor Destroy; override;
    function Add(LccMessage: TLccMessage): Boolean;
    procedure Clear;
    procedure Resend(LccMessage: TLccMessage);
    procedure Remove(LccMessage: TLccMessage);
    procedure TickTimeout;
  end;



  TOnTaskCallback = procedure(ATask: TLccTaskBase) of object;

  TLccTaskState = (lesIdle, lesRunning, lesComplete, lesError, lesAbort, lesTimeout);

  { TLccTaskBase }

  TLccTaskBase = class
  private
    FCallback: TOnTaskCallback;
    FErrorMessage: String;
    FLiveTime: Int64;
    FTarget: TNodeID;
    FTaskState: TLccTaskState;
    FErrorCode: Word;
    FOwnerNode: TLccNode;
    FTag: Integer;
    FTagObject: TObject;
    FTimeoutLimit: Integer;
    FWorkerMessage: TLccMessage;
    FValid: Boolean;
    function GetIsAborted: Boolean;
    function GetIsComplete: Boolean;
    function GetIsError: Boolean;
    function GetIsIdle: Boolean;
    function GetIsRunning: Boolean;
    function GetIsTimedOut: Boolean;
  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property TimeoutLimit: Integer read FTimeoutLimit write FTimeoutLimit;

  public
    property OwnerNode: TLccNode read FOwnerNode write FOwnerNode;
    property Target: TNodeID read FTarget write FTarget;
    property Tag: Integer read FTag write FTag;
    property TagObject: TObject read FTagObject write FTagObject;
    property TaskState: TLccTaskState read FTaskState;
    property IsError: Boolean read GetIsError;
    property IsComplete: Boolean read GetIsComplete;
    property IsAborted: Boolean read GetIsAborted;
    property IsIdle: Boolean read GetIsIdle;
    property IsRunning: Boolean read GetIsRunning;
    property IsTimedOut: Boolean read GetIsTimedOut;
    property ErrorCode: Word read FErrorCode write FErrorCode;
    property LiveTime: Int64 read FLiveTime;
    property ErrorMessage: String read FErrorMessage write FErrorMessage;
    property Callback: TOnTaskCallback read FCallback write FCallback;
    property Valid: Boolean read FValid;

    constructor Create(AnOwner: TLccNode); virtual;
    destructor Destroy; override;
    procedure Start(ATimeout: Integer); virtual;
    procedure Error(AnErrorCode: Integer; AnErrorMessage: String); virtual;
    procedure Complete;
    procedure Abort;
    procedure Timeout;
    procedure Reset; virtual;
    procedure _100msTimeTick;
    procedure Process(SourceMessage: TLccMessage); virtual;
  end;

  TLccTaskMemorySpaceReadWrite = (lems_Read, lems_Write);


  // This class stores the information that is required to load into the TLccTaskMemorySpaceAccess
  // to execute on.  This helps with reading a bunch of different memory addresses (think CV's)
  // You can just load multiple memory space access tasks into the TLccTaskMemorySpaceAccess and
  // it will just loop through them all

  { TLccTaskMemorySpaceObject }

  TLccTaskMemorySpaceObject = class
  private
    FAddressHi: DWord;
    FAddressLo: DWord;
    FCallback: TOnTaskCallback;
    FCurrentAddress: DWord;
    FDataType: TLccConfigDataType;
    FIsString: Boolean;
    FMemorySpace: Byte;
    FReadWrite: TLccTaskMemorySpaceReadWrite;
    FTag: Integer;
    FTagObject: TObject;
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
    property Tag: Integer read FTag write FTag;
    property TagObject: TObject read FTagObject write FTagObject;
    property CallBack: TOnTaskCallback read FCallback write FCallback;
    property CurrentAddress: DWord read FCurrentAddress write FCurrentAddress;
    property UseAddresses: Boolean read FUseAddresses write FUseAddresses;
    property MemorySpace: Byte read FMemorySpace write FMemorySpace;
    property ReadWrite: TLccTaskMemorySpaceReadWrite read FReadWrite write FReadWrite;

    property WriteString: String read FWriteString write SetWriteString;
    property WriteInteger: Integer read FWriteInteger write SetWriteInteger;
    property WriteEventID: TEventID read FWriteEventID write SetWriteEventID;
    property DataType: TLccConfigDataType read FDataType write FDataType;

    constructor Create(AReadWrite: TLccTaskMemorySpaceReadWrite; AMemorySpace: Byte; AnIsString: Boolean; AnAddressLo, AnAddressHi: DWORD; AnUseAddresses: Boolean; ATargetNodeID: TNodeID; ATargetAliasID: Word; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject);
  end;


  { TLccTaskMemorySpaceAccess }

  TLccTaskMemorySpaceAccess = class(TLccTaskBase)
  private
    FAddressHi: DWord;
    FAddressLo: DWord;
    FAddressSpaceValid: Boolean;
    FAddressCurrent: DWord;
    FIsString: Boolean;
    FMemorySpace: Byte;
    FMemorySpaceQueue: TList;
    FMemoryStream: TMemoryStream;
    FPIPHelper: TProtocolSupportedProtocols;
    FReadWrite: TLccTaskMemorySpaceReadWrite;
    FTargetAlias: Word;
    FTargetNodeID: TNodeID;
    FUseAddresses: Boolean;
    FWritingChunk: Boolean;
    function GetStreamAsString: string;
    function GetStreamAsInt: Integer;
    function GetStreamAsEventID: TEventID;
    procedure SetStreamAsEventID(AValue: TEventID);
    procedure SetStreamAsInt(Size: Integer; AValue: Integer);
    procedure SetStreamAsString(AValue: string);
    function GetQueuedRequests: Integer;
  protected
    property MemorySpaceQueue: TList read FMemorySpaceQueue write FMemorySpaceQueue;
    property WritingChunk: Boolean read FWritingChunk write FWritingChunk;
    property AddressSpaceValid: Boolean read FAddressSpaceValid write FAddressSpaceValid;

    procedure InternalStart;
    procedure HandleReadReply(SourceMessage: TLccMessage);
    procedure HandleWriteReply(SourceMessage: TLccMessage);
    procedure ReadNextChunk;
    procedure WriteNextChunk;
    procedure ValidatePIPAndProceed;
    procedure ValidateAddressSpaceAndProceed(SourceMessage: TLccMessage);
    function NextMemorySpaceObjectFromQueue: Boolean;
    procedure StartOperation;


  public

    property AddressLo: DWord read FAddressLo write FAddressLo;
    property AddressHi: DWord read FAddressHi write FAddressHi;
    property AddressCurrent: DWord read FAddressCurrent;
    property IsString: Boolean read FIsString write FIsString;
    property MemoryStream: TMemoryStream read FMemoryStream;
    property MemorySpace: Byte read FMemorySpace write FMemorySpace;                               // MSI_xxxx constants
    property TargetNodeID: TNodeID read FTargetNodeID write FTargetNodeID;
    property TargetAlias: Word read FTargetAlias write FTargetAlias;
    property UseAddresses: Boolean read FUseAddresses write FUseAddresses;
    property ReadWrite: TLccTaskMemorySpaceReadWrite read FReadWrite write FReadWrite;

    property PIPHelper: TProtocolSupportedProtocols read FPIPHelper;
    property StreamAsString: string read GetStreamAsString;
    property StreamAsInt: Integer read GetStreamAsInt;
    property StreamAsEventID: TEventID read GetStreamAsEventID;
    property StringToStream: string write SetStreamAsString;
    property IntToStream[Size: Integer]: Integer write SetStreamAsInt;
    property EventIDToStream: TEventID write SetStreamAsEventID;
    property QueuedRequests: Integer read GetQueuedRequests;


    constructor Create(AnOwner: TLccNode); override;
    procedure CopyTo(ATaskMemorySpaceAccess: TLccTaskMemorySpaceAccess);
    destructor Destroy; override;
    procedure FlushMemorySpaceQueue;
    procedure Start(ATimeout: Integer); override;
    procedure Reset; override;
    procedure Process(SourceMessage: TLccMessage); override;
    function Assign(AReadWrite: TLccTaskMemorySpaceReadWrite; AMemorySpace: Byte; AnIsString: Boolean; AnAddressLo, AnAddressHi: DWORD; AnUseAddresses: Boolean; ATargetNodeID: TNodeID; ATargetAliasID: Word; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): TLccTaskMemorySpaceObject; // AMemorySpace = MSI_xxxx constants
  end;

  { TTaskReadPIP }

  TTaskReadPIP = class(TLccTaskBase)

  private
    FSupportedProtocols: TProtocolSupportedProtocols;
  public

    // Returns
    property SupportedProtocols: TProtocolSupportedProtocols read FSupportedProtocols write FSupportedProtocols;

    constructor Create(AnOwner: TLccNode); override;
    destructor Destroy; override;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TTaskReadSNIP }

  TTaskReadSNIP = class(TLccTaskBase)
  private
    FSimpleNodeInfo: TProtocolSimpleNodeInfo;
  public
    property SimpleNodeInfo: TProtocolSimpleNodeInfo read FSimpleNodeInfo write FSimpleNodeInfo;

    constructor Create(AnOwner: TLccNode); override;
    destructor Destroy; override;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccNode }

  TLccNode = class(TComponent)
  private
    FAliasID: Word;
    FDatagramResendQueue: TDatagramQueue;
    FDuplicateAliasDetected: Boolean;
    FLogInEngineEnabled: Boolean;
    FTaskReadPIP: TTaskReadPIP;
    FTaskReadSNIP: TTaskReadSNIP;
    FTasksList: TList;
    FLoginTimoutCounter: Integer;
    FPermitted: Boolean;
    FProtocolEventConsumed: TProtocolEvents;
    FProtocolEventsProduced: TProtocolEvents;
    FProtocolMemoryAccess: TProtocolMemoryAccess;
    FProtocolMemoryInfo: TProtocolMemoryInfo;
    FProtocolMemoryOptions: TProtocolMemoryOptions;
    FProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo;
    FProtocolSupportedProtocols: TProtocolSupportedProtocols;
    FSeedNodeID: TNodeID;
    FWorkerMessageForAckMessages: TLccMessage;
    FInitialized: Boolean;
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
    FTaskMemorySpaceAccess: TLccTaskMemorySpaceAccess;

    function GetAliasIDStr: String;
    function GetNodeIDStr(WithDots: Boolean): String;
  protected
    FNodeID: TNodeID;

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
    procedure HandleConfiguration_MemorySpaceWrite(var SourceMessage: TLccMessage; AutoGrowSpace: Boolean);virtual;
    procedure HandleACDI_Manufacturer_MemorySpaceWrite(var SourceMessage: TLccMessage); virtual;
    procedure HandleACDI_UserMemorySpaceWrite(var SourceMessage: TLccMessage);virtual;
    procedure HandleTractionFDI_MemorySpaceWrite(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionFDI_ConfigurationMemorySpaceWrite(var SourceMessage: TLccMessage); virtual;
    procedure SendMemoryWriteFailure(var MemoryWriteRequestMessage: TLccMessage; ErrorCode: Word; ErrorString: String); virtual;
    procedure HandleStreamWrite;
       // Datagram Configuration Type Handlers - Memory Space Read Handlers
    procedure HandleCDI_MemorySpaceRead(var SourceMessage: TLccMessage);virtual;
    procedure HandleAll_MemorySpaceRead(var SourceMessage: TLccMessage);virtual;
    procedure HandleConfiguration_MemorySpaceRead(var SourceMessage: TLccMessage; AutoGrowSpace: Boolean); virtual;
    procedure HandleACDI_Manufacturer_MemorySpaceRead(var SourceMessage: TLccMessage);virtual;
    procedure HandleACDI_User_MemorySpaceRead(var SourceMessage: TLccMessage);virtual;
    procedure HandleTractionFDI_MemorySpaceRead(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionFDI_ConfigurationMemorySpaceRead(var SourceMessage: TLccMessage); virtual;
    procedure SendMemoryReadFailure(var MemoryReadRequestMessage: TLccMessage; ErrorCode: Word; ErrorString: String); virtual;
    procedure HandleStreamRead;
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
  //  procedure HandleTractionControllerChangedNotify(var SourceMessage: TLccMessage);  virtual;  // Sent from Train To currently Assigned Controller if it has one
    procedure HandleTractionEStop(var SourceMessage: TLccMessage; ListenerForwarded: Boolean);  virtual;
    procedure HandleTractionListenerAttach(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionListenerDetach(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionListenerQuery(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionManageReserve(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionManageRelease(var SourceMessage: TLccMessage); virtual;
    procedure HandleTractionSetSpeed(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); virtual;
    procedure HandleTractionSetFunction(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); virtual;
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


    property LogInEngineEnabled: Boolean read FLogInEngineEnabled write FLogInEngineEnabled;  // Internally used.. Set true by "LogIn" and false in "ReleaseAlias" so a new LogIn must be called
    property StreamCdi: TMemoryStream read FStreamCdi write FStreamCdi;
    property StreamConfig: TMemoryStream read FStreamConfig write FStreamConfig;
    property StreamManufacturerData: TMemoryStream read FStreamManufacturerData write FStreamManufacturerData;
    property StreamTractionFdi: TMemoryStream read FStreamTractionFdi write FStreamTractionFdi;
    property StreamTractionConfig: TMemoryStream read FStreamTractionConfig write FStreamTractionConfig;

    property TaskReadPIP: TTaskReadPIP read FTaskReadPIP write FTaskReadPIP;
    property TaskReadSNIP: TTaskReadSNIP read FTaskReadSNIP write FTaskReadSNIP;
    property TaskMemorySpaceAccess: TLccTaskMemorySpaceAccess read FTaskMemorySpaceAccess write FTaskMemorySpaceAccess;

    property TasksList: TList read FTasksList write FTasksList;

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property WorkerMessageForAckMessages: TLccMessage read FWorkerMessageForAckMessages write FWorkerMessageForAckMessages;

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
    procedure QueueAndSendDatagramReplyToWaitForAck(DatagramRequest, DatagramReply: TLccMessage);
    function GetCdiFile: string; virtual;
    procedure BeforeLogin; virtual;
    procedure LccLogIn(ANodeID: TNodeID); virtual;
    function InternalRequestConfigMemRead(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; IsString: Boolean; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil; Queue: Boolean = False): Boolean;
    function InternalRequestConfigMemWriteString(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AString: String; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil; Queue: Boolean = False): Boolean;
    function InternalRequestConfigMemWriteInteger(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnInteger: Integer; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil; Queue: Boolean = False): Boolean;
    function InternalRequestConfigMemWriteEventID(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnEventID: TEventID; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil; Queue: Boolean = False): Boolean;

    // GridConnect Helpers
    function GenerateID_Alias_From_Seed(var Seed: TNodeID): Word;
    procedure GenerateNewSeed(var Seed: TNodeID);

  public
    property DatagramResendQueue: TDatagramQueue read FDatagramResendQueue;
    property NodeID: TNodeID read FNodeID;
    property NodeIDStr[WithDots: Boolean]: String read GetNodeIDStr;
    property Initialized: Boolean read FInitialized;

    property ProtocolSupportedProtocols: TProtocolSupportedProtocols read FProtocolSupportedProtocols write FProtocolSupportedProtocols;
    property ProtocolEventConsumed: TProtocolEvents read FProtocolEventConsumed write FProtocolEventConsumed;
    property ProtocolEventsProduced: TProtocolEvents read FProtocolEventsProduced write FProtocolEventsProduced;
    property ProtocolMemoryInfo: TProtocolMemoryInfo read FProtocolMemoryInfo write FProtocolMemoryInfo;
    property ProtocolMemoryOptions: TProtocolMemoryOptions read FProtocolMemoryOptions write FProtocolMemoryOptions;
    property ProtocolMemoryAccess: TProtocolMemoryAccess read FProtocolMemoryAccess write FProtocolMemoryAccess;
    property ProtocolSimpleNodeInfo: TProtocolSimpleNodeInfo read FProtocolSimpleNodeInfo write FProtocolSimpleNodeInfo;

    // GridConnect Helpers
    property AliasID: Word read FAliasID;
    property AliasIDStr: String read GetAliasIDStr;
    property Permitted: Boolean read FPermitted;

    constructor Create(AOwner: TComponent; CdiXML: string); reintroduce; virtual;
    destructor Destroy; override;

    procedure Login(ANodeID: TNodeID; InitialLogIn: Boolean = True); virtual;
    procedure On_100msTimer(Sender: TObject);  virtual;
    procedure ReleaseAlias(DelayTime_ms: Word); virtual;
    function ProcessMessage(SourceMessage: TLccMessage): Boolean; // Do not override this override the next 2
    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; virtual;
    function ProcessMessageGridConnect(SourceMessage: TLccMessage): Boolean; virtual;

    // Task Helpers
    procedure RegisterTask(ATask: TLccTaskBase);
    procedure UnRegisterTask(ATask: TLccTaskBase);

    procedure SendEvents;
    procedure SendConsumedEvents;
    procedure SendConsumerIdentify(Event: TEventID);
    procedure SendProducedEvents;
    procedure SendProducerIdentify(Event: TEventID);

    //  AnAssociatedNode is the node that matches the NodeID/AliasID so it is not sent back and generate a Duplicate Alias when dispatching back to other virtual nodes
    // This is tricky if a virtual node is created then its NodeID and Alias are used in response to a query and you forget to set the Associated node...
    // This forces you to think about it....
    // This can be nil if the message is not using a NodeID and Alias associated with an internal node
    procedure SendMessage(ALccMessage: TLccMessage; AnAssociatedNode: TLccNode);
    procedure RequestIdentifyEvents;
    procedure RequestIdentifyProducer(Event: TEventID);
    procedure RequestIdentifyConsumer(Event: TEventID);
    function RequestSNIP(ATarget: TNodeID; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;   // Callback: TTaskReadSNIP
    function RequestPIP(ATarget: TNodeID; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;    // Callback: TTaskReadPIP
    function RequestCDI(ATarget: TNodeID; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    function RequestConfigMemRead(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; IsString: Boolean; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    function RequestQueueConfigMemRead(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; IsString: Boolean; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    function RequestConfigMemWriteString(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AString: String; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    function RequestConfigMemWriteInteger(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnInteger: Integer; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    function RequestConfigMemWriteEventID(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnEventID: TEventID; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    function RequestQueueConfigMemWriteString(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AString: String; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    function RequestQueueConfigMemWriteInteger(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnInteger: Integer; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    function RequestQueueConfigMemWriteEventID(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnEventID: TEventID; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
    procedure RequestMemoryAccessQueueStart;
    procedure RequestMemoryAccessAbort;
  end;

  TLccNodeClass = class of TLccNode;

implementation

uses
  lcc_node_manager;

{ TTaskReadPIP }

constructor TTaskReadPIP.Create(AnOwner: TLccNode);
begin
  inherited Create(AnOwner);
  FSupportedProtocols := TProtocolSupportedProtocols.Create;
end;

destructor TTaskReadPIP.Destroy;
begin
  FreeAndNil(FSupportedProtocols);
  inherited Destroy;
end;

procedure TTaskReadPIP.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadProtocolIdentifyInquiry(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target));
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TTaskReadPIP.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_PROTOCOL_SUPPORT_REPLY then
  begin
    SupportedProtocols.LoadFromLccMessage(SourceMessage);
    Complete;
    Reset;
  end;
end;

{ TTaskReadSNIP }

constructor TTaskReadSNIP.Create(AnOwner: TLccNode);
begin
  inherited Create(AnOwner);
  SimpleNodeInfo := TProtocolSimpleNodeInfo.Create;
end;

destructor TTaskReadSNIP.Destroy;
begin
  FreeAndNil(FSimpleNodeInfo);
  inherited Destroy;
end;

procedure TTaskReadSNIP.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadSimpleNodeIdentInfoRequest(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target));
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TTaskReadSNIP.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_SIMPLE_NODE_INFO_REPLY then
  begin
    SimpleNodeInfo.LoadFromLccMessage(SourceMessage);
    Complete;
    Reset;
  end;
end;

{ TLccTaskMemorySpaceObject }

procedure TLccTaskMemorySpaceObject.SetWriteEventID(AValue: TEventID);
begin
  FWriteEventID := AValue;
  DataType := cdt_EventID;
end;

procedure TLccTaskMemorySpaceObject.SetWriteInteger(AValue: Integer);
begin
  FWriteInteger := AValue;
  DataType := cdt_Int;
end;

procedure TLccTaskMemorySpaceObject.SetWriteString(AValue: String);
begin
  FWriteString := AValue;
  DataType := cdt_String;
end;

constructor TLccTaskMemorySpaceObject.Create(
  AReadWrite: TLccTaskMemorySpaceReadWrite; AMemorySpace: Byte;
  AnIsString: Boolean; AnAddressLo, AnAddressHi: DWORD;
  AnUseAddresses: Boolean; ATargetNodeID: TNodeID; ATargetAliasID: Word;
  ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject);
begin
  ReadWrite := AReadWrite;
  MemorySpace := AMemorySpace;
  IsString := AnIsString;
  AddressLo := AnAddressLo;
  AddressHi := AnAddressHi;
  UseAddresses := AnUseAddresses;
  TargetNodeID := ATargetNodeID;
  TargetAlias := ATargetAliasID;
  Tag := ATag;
  TagObject := ATagObject;
  CallBack := ACallback;
end;

{ TLccTaskBase }

constructor TLccTaskBase.Create(AnOwner: TLccNode);
begin
  FOwnerNode := AnOwner;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TLccTaskBase.Destroy;
begin
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

function TLccTaskBase.GetIsAborted: Boolean;
begin
  Result := TaskState = lesAbort
end;

function TLccTaskBase.GetIsComplete: Boolean;
begin
  Result := TaskState = lesComplete
end;

function TLccTaskBase.GetIsError: Boolean;
begin
  Result := TaskState = lesError
end;

function TLccTaskBase.GetIsIdle: Boolean;
begin
  Result := TaskState = lesIdle
end;

function TLccTaskBase.GetIsRunning: Boolean;
begin
  Result := TaskState = lesRunning
end;

function TLccTaskBase.GetIsTimedOut: Boolean;
begin
  Result := FTaskState = lesTimeOut;
end;

procedure TLccTaskBase.Abort;
begin
  FValid := False;
  FTaskState := lesAbort;
  if Assigned(Callback) then
    Callback(Self);
end;

procedure TLccTaskBase.Timeout;
begin
  FTaskState := lesTimeout;
  FValid := False;
  if Assigned(Callback) then
    Callback(Self);
end;

procedure TLccTaskBase.Start(ATimeout: Integer);
begin
  FTaskState := lesRunning;
  FLiveTime := 0;

  // 0 means forever
  if ATimeout = 0 then
    FTimeoutLimit := 0
  else
  if ATimeout > 0 then
    FTimeoutLimit := ATimeout div 100;         // 100ms intervals
end;

procedure TLccTaskBase.Error(AnErrorCode: Integer; AnErrorMessage: String);
begin
  FValid := False;
  ErrorCode := AnErrorCode;
  ErrorMessage := AnErrorMessage;
  FTaskState := lesError;
  if Assigned(Callback) then
    Callback(Self);
end;

procedure TLccTaskBase.Complete;
begin
  FValid := True;
  FErrorCode := S_OK;
  FErrorMessage := '';
  FTaskState := lesComplete;
  if Assigned(Callback) then
    Callback(Self);
end;

procedure TLccTaskBase.Reset;
begin
  FValid := False;
  FTaskState := lesIdle;
end;

procedure TLccTaskBase._100msTimeTick;
begin
  if IsRunning then
  begin
    if (LiveTime > TimeoutLimit) and (TimeoutLimit > 0) then
    begin
      Timeout;
      Reset;
    end
    else
      Inc(FLiveTime)
  end;
end;

procedure TLccTaskBase.Process(SourceMessage: TLccMessage);
begin
  // override to do something useful
end;

{ TLccTaskMemorySpaceAccess }

procedure TLccTaskMemorySpaceAccess.HandleReadReply(SourceMessage: TLccMessage);
var
  i, DataStartIndex, MemspaceDataSentCount: Integer;
  NullFound: Boolean;
begin
  if TaskState = lesRunning then
  begin
    NullFound := False;

    if SourceMessage.DataArray[1] and $0F = 0 then     // $50 means Address space in Byte 6
      DataStartIndex := 7                    // $51...$53 for all others  (not true in decoding Reply Fail)
    else
      DataStartIndex := 6;

    MemspaceDataSentCount := SourceMessage.DataCount - DataStartIndex;

    for i := 0 to MemspaceDataSentCount - 1 do
    begin
      MemoryStream.Write(SourceMessage.DataArray[i + DataStartIndex], 1);
      Inc(FAddressCurrent);  // Update the current address pointer

      if IsString then
      begin
        if Char( SourceMessage.DataArray[i + DataStartIndex]) = #0 then
          NullFound := True;
      end;
    end;

    if not NullFound and (AddressCurrent < (AddressHi-AddressLo)) then
      ReadNextChunk
    else begin
      Complete;     // this one is done
      // Move to the next one, if available
      if NextMemorySpaceObjectFromQueue then
        InternalStart
      else
        Reset
    end;
  end
end;

procedure TLccTaskMemorySpaceAccess.HandleWriteReply(SourceMessage: TLccMessage);
begin
  if MemoryStream.Position = MemoryStream.Size then
  begin
    Complete;      // this one is done
    // Move to the next one, if available
    if NextMemorySpaceObjectFromQueue then
      InternalStart
    else
      Reset
  end else
    WriteNextChunk;
end;

procedure TLccTaskMemorySpaceAccess.ReadNextChunk;
var
  NextCount: Integer;
begin
  if TaskState = lesRunning then
  begin
    // Calculate the number of bytes to read
    if AddressCurrent + 64 > AddressHi then
      NextCount := AddressHi - AddressCurrent
    else
      NextCount := 64;

    // Request the read
    WorkerMessage.LoadConfigMemRead(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias, MemorySpace, AddressCurrent, NextCount);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);

    // Inprogress update
    if Assigned(CallBack) then
      CallBack(Self);
  end;
end;

procedure TLccTaskMemorySpaceAccess.WriteNextChunk;
var
  NextCount: Integer;
  ByteArray: array of Byte;
  i: Integer;
  {$IFDEF LCC_DELPHI}
  B: Byte;
  {$ENDIF}
begin
  ByteArray := nil;

  if TaskState = lesRunning then
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
        {$IFDEF LCC_DELPHI}
          MemoryStream.Read(B, 1);
          ByteArray[i] := B;
        {$ELSE}
          ByteArray[i] := MemoryStream.ReadByte;
        {$ENDIF}
      end;

      // Write the space
      WorkerMessage.LoadConfigMemWrite(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias, MemorySpace, AddressCurrent, ByteArray);
      OwnerNode.SendMessage(WorkerMessage, OwnerNode);

      // Inprocess Update
      if Assigned(Callback) then
        Callback(Self);

      // Update the address pointer
      Inc(FAddressCurrent, NextCount)
    end;
  end;
end;

procedure TLccTaskMemorySpaceAccess.ValidatePIPAndProceed;
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
      OwnerNode.SendMessage(WorkerMessage, OwnerNode);
    end else
    begin
      Error(TASK_ERROR_MEMORY_SPACE_UNSUPPORTED_PROTOCOL, 'Unsupported Protocol');
      // Move to the next one, if available
      if NextMemorySpaceObjectFromQueue then
        InternalStart
      else
        Reset
    end;
  end;
end;

procedure TLccTaskMemorySpaceAccess.ValidateAddressSpaceAndProceed(SourceMessage: TLccMessage);
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
        Error(TASK_ERROR_MEMORY_SPACE_WRITE_TO_READONLY_SPACE, 'Writing to a Read Only Memory Space');
        // Move to the next one, if available
        if NextMemorySpaceObjectFromQueue then
          InternalStart
        else
          Reset
      end;
    end else
    begin
      Error(TASK_ERROR_MEMORY_SPACE_UNSUPPORTED_MEMORYSPACE, 'Accessing and unsupported Memory Space');
      // Move to the next one, if available
      if NextMemorySpaceObjectFromQueue then
        InternalStart
      else
        Reset
    end;
  end
end;

function TLccTaskMemorySpaceAccess.Assign(
  AReadWrite: TLccTaskMemorySpaceReadWrite; AMemorySpace: Byte;
  AnIsString: Boolean; AnAddressLo, AnAddressHi: DWORD;
  AnUseAddresses: Boolean; ATargetNodeID: TNodeID; ATargetAliasID: Word;
  ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject
  ): TLccTaskMemorySpaceObject;
begin
  Result := nil;
  // Need to Assign everthing first.. maybe you can add in while its running... but it was not envisioned to do so
  Assert(TaskState = lesIdle, 'TMemorySpaceReadEngine.Assign is running');

  if Assigned(OwnerNode) and ((ATargetAliasID <> 0) or not NullNodeID(ATargetNodeID)) then
  begin
    Result := TLccTaskMemorySpaceObject.Create(AReadWrite, AMemorySpace, AnIsString, AnAddressLo, AnAddressHi, AnUseAddresses, ATargetNodeID, ATargetAliasID, ACallback, Tag, TagObject);
    if Assigned(Result) then
      MemorySpaceQueue.Add(Result);
  end;
end;

procedure TLccTaskMemorySpaceAccess.FlushMemorySpaceQueue;
var
  i: Integer;
  MemorySpaceObject: TLccTaskMemorySpaceObject;
begin
  try
    for i := 0 to MemorySpaceQueue.Count - 1 do
    begin
      MemorySpaceObject := TLccTaskMemorySpaceObject( MemorySpaceQueue[i]);
      MemorySpaceObject.Free;
    end;
  finally
    MemorySpaceQueue.Clear;
  end;
end;

constructor TLccTaskMemorySpaceAccess.Create(AnOwner: TLccNode);
begin
  inherited Create(AnOwner);
  FPIPHelper := TProtocolSupportedProtocols.Create;
  FMemoryStream := TMemoryStream.Create;
  FMemorySpaceQueue := TList.Create;
end;

procedure TLccTaskMemorySpaceAccess.CopyTo(ATaskMemorySpaceAccess: TLccTaskMemorySpaceAccess);
begin

  ATaskMemorySpaceAccess.FAddressHi := AddressHi;
  ATaskMemorySpaceAccess.FAddressLo := AddressLo;
  ATaskMemorySpaceAccess.FAddressSpaceValid := AddressSpaceValid;
  ATaskMemorySpaceAccess.FAddressCurrent := AddressCurrent;
  ATaskMemorySpaceAccess.FIsString := IsString;
  ATaskMemorySpaceAccess.FMemorySpace := MemorySpace;
  ATaskMemorySpaceAccess.FTargetAlias := TargetAlias;
  ATaskMemorySpaceAccess.FTargetNodeID := TargetNodeID;
  ATaskMemorySpaceAccess.FUseAddresses := UseAddresses;
  ATaskMemorySpaceAccess.FWritingChunk := WritingChunk;
  ATaskMemorySpaceAccess.FReadWrite := ReadWrite;
  ATaskMemorySpaceAccess.Tag := Tag;
  ATaskMemorySpaceAccess.TagObject := TagObject;
  ATaskMemorySpaceAccess.FValid := Valid;

  MemoryStream.Position := 0;
  ATaskMemorySpaceAccess.MemoryStream.CopyFrom(MemoryStream, MemoryStream.Size);
  PIPHelper.CopyTo(ATaskMemorySpaceAccess.PIPHelper);
end;

destructor TLccTaskMemorySpaceAccess.Destroy;
begin
  FlushMemorySpaceQueue;
  FreeAndNil(FPIPHelper);
  FreeAndNil(FMemoryStream);
  inherited Destroy;
end;

function TLccTaskMemorySpaceAccess.GetQueuedRequests: Integer;
begin
  Result := FMemorySpaceQueue.Count
end;

procedure TLccTaskMemorySpaceAccess.InternalStart;
begin
  inherited Start(TimeoutLimit);

  if ReadWrite = lems_Read then
  begin
    MemoryStream.Position := 0;
    MemoryStream.Size := 0;
  end;

  if not AddressSpaceValid then
  begin
    // Get what protocols are supported
    WorkerMessage.LoadProtocolIdentifyInquiry(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end else
    StartOperation;
end;

function TLccTaskMemorySpaceAccess.GetStreamAsEventID: TEventID;
var
  Hex, Temp: string;
  i: Integer;
  B: Byte;
begin
  MemoryStream.Position := 0;

  Temp := '';
  for i := 0 to MemoryStream.Size - 1 do
  begin
    B := 0;
    MemoryStream.Read(B, SizeOf(B));
    Hex := IntToHex(B, 2);
    Temp := Temp + Hex;
  end;

  Result := StrToEventID( string( Temp));

  // Reset for use
  MemoryStream.Position := 0;
end;

procedure TLccTaskMemorySpaceAccess.SetStreamAsEventID(AValue: TEventID);
var
  EventIDStr, Temp: string;
  StartIndex, i: Integer;
  CharPtr: PChar;
  B: Byte;
begin
  MemoryStream.Position := 0;

  EventIDStr := EventIDToString(AValue, False);

  {$IFDEF LCC_MOBILE}
  StartIndex := 0;
  {$ELSE}
  StartIndex := 1;
  {$ENDIF}
  CharPtr := @EventIDStr[StartIndex];

  // Implicit 8 Bytes
  for i := 0 to 7 do
  begin
    Temp := '';
    Temp := CharPtr^;
    Inc(CharPtr);
    Temp := Temp + CharPtr^;
    Inc(CharPtr);
    B := StrToInt('$' + String( Temp));
    MemoryStream.Write(B, SizeOf(B));
  end;

  // Reset for use
  MemoryStream.Position := 0;
end;

procedure TLccTaskMemorySpaceAccess.SetStreamAsInt(Size: Integer; AValue: Integer);
var
  Hex, Temp: string;
  CharPtr: PChar;
  B: Byte;
  StartIndex, i: Integer;
begin
  MemoryStream.Clear;

  Hex := IntToHex(AValue, Size * 2);

  {$IFDEF LCC_MOBILE}
  StartIndex := 0;
  {$ELSE}
  StartIndex := 1;
  {$ENDIF}
  CharPtr := @Hex[StartIndex];

  for i := 0 to Size - 1 do
  begin
    Temp := '';
    Temp := CharPtr^;
    Inc(CharPtr);
    Temp := Temp + CharPtr^;
    Inc(CharPtr);
    B := StrToInt('$' + String( Temp));
    MemoryStream.Write(B, SizeOf(B));
  end;

  // Reset for use
  MemoryStream.Position := 0;
end;


procedure TLccTaskMemorySpaceAccess.SetStreamAsString(AValue: string);
var
  i: Integer;
  B: Byte;
begin
  MemoryStream.Clear;
  {$IFDEF LCC_MOBILE}
  for i := 0 to Length(AValue) - 1 do
  {$ELSE}
  for i := 1 to Length(AValue) do
  {$ENDIF}
  begin
    B := Ord( AValue[i]);
    MemoryStream.Write(B, SizeOf(B));
  end;

  B := Ord( #0);
  MemoryStream.Write(B, SizeOf(B));

  // Reset for use
  MemoryStream.Position := 0;
end;

function TLccTaskMemorySpaceAccess.NextMemorySpaceObjectFromQueue: Boolean;
var
  EngineMemorySpaceObject: TLccTaskMemorySpaceObject;
begin
  Result := False;
  // Do we have anything to send?
  if MemorySpaceQueue.Count > 0 then
  begin
    // Pull it out of the Queue and delete it from the Queue
    EngineMemorySpaceObject := TLccTaskMemorySpaceObject( MemorySpaceQueue[0]);
    try
      MemorySpaceQueue.Delete(0);


      // See if we need to recheck if if the Memory space is valid, if not need to recheck if it is supported
      if ((MemorySpace <> EngineMemorySpaceObject.MemorySpace) or (ReadWrite <> EngineMemorySpaceObject.ReadWrite)) or not EngineMemorySpaceObject.UseAddresses then
        AddressSpaceValid := False;

      ReadWrite := EngineMemorySpaceObject.ReadWrite;
      MemorySpace := EngineMemorySpaceObject.MemorySpace;
      IsString := EngineMemorySpaceObject.IsString;
      AddressLo := EngineMemorySpaceObject.AddressLo;
      FAddressCurrent := EngineMemorySpaceObject.AddressLo;
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
          cdt_String  : StringToStream := EngineMemorySpaceObject.WriteString;
          cdt_Int     : IntToStream[AddressHi - AddressLo] := EngineMemorySpaceObject.WriteInteger;
          cdt_EventID : EventIDToStream := EngineMemorySpaceObject.WriteEventID;
        end;
      end else
      if ReadWrite = lems_Read then
      begin
        MemoryStream.Position := 0;
        MemoryStream.Size := 0;
      end;

    finally
      EngineMemorySpaceObject.Free;
    end;

    Assert(not NullNodeID(FTargetNodeID), 'TMemorySpaceReadEngine, NodeID not set');
    Result := True;
  end;
end;

procedure TLccTaskMemorySpaceAccess.StartOperation;
begin
  // Start off the current address
  FAddressCurrent := AddressLo;

  // Start the process
  case ReadWrite of
    lems_write : WriteNextChunk;
    lems_read  : ReadNextChunk;
  end;
end;

function TLccTaskMemorySpaceAccess.GetStreamAsInt: Integer;
var
  Hex, Temp: String;
  B: Byte;
  i: Integer;
begin

  MemoryStream.Position := 0;

  Temp := '';
  for i := 0 to MemoryStream.Size - 1 do
  begin
    B := 0;
    MemoryStream.Read(B, SizeOf(B));
    Hex := IntToHex(B, 2);
    Temp := Temp + Hex;
  end;

  if not TryStrToInt('$' + String( Temp), Result) then
    Result := 0;

  // Reset for use
  MemoryStream.Position := 0;
end;

function TLccTaskMemorySpaceAccess.GetStreamAsString: string;
var
  i: Integer;
  C: UTF8Char;
begin
  Result := '';
  C := #0;
  MemoryStream.Position := 0;
  for i := 0 to MemoryStream.Size - 1 do
  begin
    MemoryStream.Read(C, SizeOf(C));
    if C <> #0 then
      Result := Result + string( C)
  end;
end;

procedure TLccTaskMemorySpaceAccess.Start(ATimeout: Integer);
begin
  FValid := False;
  if NextMemorySpaceObjectFromQueue then
    InternalStart
  else begin
    Error(TASK_ERROR_MEMORY_SPACE_NO_JOB_ASSIGNED, 'No job was assigined, used the Assign() function to define a memory space to read/write');
    Reset;
  end;
end;

procedure TLccTaskMemorySpaceAccess.Reset;
begin
  FValid := False;
  AddressSpaceValid := False;
  FlushMemorySpaceQueue;
  MemoryStream.Clear;
  PIPHelper.Valid := False;
  inherited Reset;
end;

procedure TLccTaskMemorySpaceAccess.Process(SourceMessage: TLccMessage);
var
  ReplyPending: Boolean;
  ReplyEstimatedTime: extended;    // In Seconds
  DecodedMemorySpace: Byte;
  MessageAddress: DWord;
begin
  if TaskState = lesRunning then
  begin
    if EqualNodeID(TargetNodeID, SourceMessage.SourceID, False) then
    begin
      case SourceMessage.MTI of
        MTI_DATAGRAM_OK_REPLY :
          begin


            // So the spec says a read/write failure will send OK with the pending flag set then a failure code will be sent,
            // this is important because a Write typically only sends an Datagram OK message...

    //        DOES THIS WORK NOW??????

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
                HandleWriteReply(SourceMessage);
            end;
          end;
        MTI_DATAGRAM_REJECTED_REPLY :
          begin

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
                 MCP_WRITE_REPLY,  // This only happens if the write can take a while (like a CV write).  In that case a special Datagram OK message is sent.. see MTI_DATAGRAM_OK_REPLY above
                 MCP_WRITE_REPLY_CONFIG,
                 MCP_WRITE_REPLY_ALL,
                 MCP_WRITE_REPLY_CDI :
                   begin
                     DecodedMemorySpace := SourceMessage.DecodeMemorySpace;
                     case DecodedMemorySpace of
                       MSI_ACDI_USER,
                       MSI_CONFIG,
                       MSI_TRACTION_FUNCTION_CONFIG :
                         begin
                           MessageAddress := SourceMessage.ExtractDataBytesAsDWord(2);
                            if (DecodedMemorySpace = MemorySpace) and (AddressCurrent = MessageAddress) then
                              HandleWriteReply(SourceMessage);
                         end;
                       MSI_TRACTION_FDI,
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
                       Error(TASK_ERROR_MEMORY_SPACE_WRITE_ERROR, 'Memory Space Write Error');
                       // Move to the next one, if available
                       if NextMemorySpaceObjectFromQueue then
                         InternalStart
                       else
                         Reset
                     end;
                 MCP_READ_REPLY,
                 MCP_READ_REPLY_CONFIG,
                 MCP_READ_REPLY_ALL,
                 MCP_READ_REPLY_CDI :
                   begin
                     DecodedMemorySpace := SourceMessage.DecodeMemorySpace;
                     case DecodedMemorySpace of
                       MSI_ALL                      : begin end;
                       MSI_CDI,
                       MSI_ACDI_MFG,
                       MSI_ACDI_USER,
                       MSI_TRACTION_FDI,
                       MSI_CONFIG,
                       MSI_TRACTION_FUNCTION_CONFIG :
                         begin
                           MessageAddress := SourceMessage.ExtractDataBytesAsDWord(2);
                            if (DecodedMemorySpace = MemorySpace) and (AddressCurrent = MessageAddress) then
                              HandleReadReply(SourceMessage);
                         end;
                     end;
                   end;
                 MCP_READ_REPLY_FAILURE,
                 MCP_READ_REPLY_FAILURE_CONFIG,
                 MCP_READ_REPLY_FAILURE_ALL,
                 MCP_READ_REPLY_FAILURE_CDI                  :
                     begin
                       Error(TASK_ERROR_MEMORY_SPACE_READ_ERROR, 'Memory Space Read Error');
                       // Move to the next one, if available
                       if NextMemorySpaceObjectFromQueue then
                         InternalStart
                       else
                         Reset
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
  LccMessage.RetryAttempts := 0;
  LccMessage.AbandonTickCount := 0;
end;

constructor TDatagramQueue.Create;
begin
  inherited Create;
  {$IFDEF LCC_DELPHI}
  Queue := TObjectList<TLccMessage>.Create;
  {$ELSE}
  Queue := TObjectList.Create;
  {$ENDIF}
  Queue.OwnsObjects := True;
end;

destructor TDatagramQueue.Destroy;
begin
  {$IFDEF LCC_FPC}
  FreeAndNil(FQueue);
  {$ELSE}
    Queue.Free;
    Queue := nil;
  {$ENDIF}
  inherited Destroy;
end;

function TDatagramQueue.GetCount: Integer;
begin
  Result :=  FQueue.Count;
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
    QueueAlias := (Queue[i] as TLccMessage).DestAlias;
    QueueNodeID := (Queue[i] as TLccMessage).DestID;
    if (QueueAlias <> 0) and (LccMessage.SourceAlias <> 0) then
    begin
      if QueueAlias = LccMessage.SourceAlias then
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
    if LocalMessage.RetryAttempts < 5 then
    begin
      LocalMessage := Queue[iLocalMessage] as TLccMessage;
      OwnerNode.SendMessage(LocalMessage, OwnerNode);
      LocalMessage.RetryAttempts := LocalMessage.RetryAttempts + 1;
    end else
      Queue.Delete(iLocalMessage);
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
    if LocalMessage.AbandonTickCount < 6 then   // 800ms * 6
      LocalMessage.AbandonTickCount := LocalMessage.AbandonTickCount + 1
    else
      Queue.Delete(i);
  end;
end;

{TLccNode }

procedure TLccNode.SendMessage(ALccMessage: TLccMessage; AnAssociatedNode: TLccNode);
begin
  ALccMessage.AssociatedNode := AnAssociatedNode;
  (Owner as TLccNodeManager).SendLccMessageNodeManager(ALccMessage);
end;

procedure TLccNode.RequestIdentifyEvents;
begin
  WorkerMessage.LoadIdentifyEvents(NodeID, AliasID);
  SendMessage(WorkerMessage, Self);
end;

procedure TLccNode.RequestIdentifyProducer(Event: TEventID);
begin
  WorkerMessage.LoadProducerIdentify(NodeID, AliasID, Event);
  SendMessage(WorkerMessage, Self);
end;

procedure TLccNode.RequestIdentifyConsumer(Event: TEventID);
begin
  WorkerMessage.LoadConsumerIdentify(NodeID, AliasID, Event);
  SendMessage(WorkerMessage, Self);
end;

function TLccNode.RequestSNIP(ATarget: TNodeID; ACallback: TOnTaskCallback ; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
begin
  Result := False;
  if TaskReadSNIP.IsIdle then
  begin
    TaskReadSNIP.Target := ATarget;
    TaskReadSNIP.Callback := ACallback;
    TaskReadSNIP.Tag := ATag;
    TaskReadSNIP.TagObject := ATagObject;
    TaskReadSNIP.Target := ATarget;
    TaskReadSNIP.Start(TIMEOUT_TASK_MESSAGES);
  end;
end;

function TLccNode.RequestPIP(ATarget: TNodeID; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
begin
  Result := False;
  if TaskReadPIP.IsIdle then
  begin
    TaskReadPIP.Target := ATarget;
    TaskReadPIP.Callback := ACallback;
    TaskReadPIP.Tag := ATag;
    TaskReadPIP.TagObject := ATagObject;
    TaskReadPIP.Target := ATarget;
    TaskReadPIP.Start(TIMEOUT_TASK_MESSAGES);
  end;
end;

function TLccNode.RequestCDI(ATarget: TNodeID; ACallback: TOnTaskCallback; ATag: Integer = 0; ATagObject: TObject = nil): Boolean;
begin
  Result := False;
  if TaskMemorySpaceAccess.TaskState = lesIdle then
  begin
    TaskMemorySpaceAccess.Callback := ACallback;
    TaskMemorySpaceAccess.Tag := ATag;
    TaskMemorySpaceAccess.TagObject := ATagObject;
    TaskMemorySpaceAccess.Target := ATarget;
    TaskMemorySpaceAccess.Assign(lems_Read, MSI_CDI, True, 0, 0, False, ATarget, AliasServer.FindAlias(ATarget), ACallback, ATag, ATagObject);
    TaskMemorySpaceAccess.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccNode.RequestConfigMemRead(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; IsString: Boolean; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): Boolean;
begin
  Result := InternalRequestConfigMemRead(ATarget, MemAddressStart, MemAddressEnd, IsString, ACallback, ATag, ATagObject, False);
end;

function TLccNode.RequestQueueConfigMemRead(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; IsString: Boolean; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): Boolean;
begin
  Result := InternalRequestConfigMemRead(ATarget, MemAddressStart, MemAddressEnd, IsString, ACallback, ATag, ATagObject, True);
end;

function TLccNode.RequestConfigMemWriteString(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AString: String; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): Boolean;
begin
  Result := InternalRequestConfigMemWriteString(ATarget, MemAddressStart, MemAddressEnd, AString, ACallback, ATag, ATagObject, False);
end;

function TLccNode.RequestConfigMemWriteInteger(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnInteger: Integer; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): Boolean;
begin
  Result := InternalRequestConfigMemWriteInteger(ATarget, MemAddressStart, MemAddressEnd, AnInteger, ACallback, ATag, ATagObject, False);
end;

function TLccNode.RequestConfigMemWriteEventID(ATarget: TNodeID;
  MemAddressStart, MemAddressEnd: DWord; AnEventID: TEventID;
  ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): Boolean;
begin
  Result := InternalRequestConfigMemWriteEventID(ATarget, MemAddressStart, MemAddressEnd, AnEventID, ACallback, ATag, ATagObject, False);
end;

function TLccNode.RequestQueueConfigMemWriteString(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AString: String; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): Boolean;
begin
  Result := InternalRequestConfigMemWriteString(ATarget, MemAddressStart, MemAddressEnd, AString, ACallback, ATag, ATagObject, True);
end;

function TLccNode.RequestQueueConfigMemWriteInteger(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnInteger: Integer; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): Boolean;
begin
  Result := InternalRequestConfigMemWriteInteger(ATarget, MemAddressStart, MemAddressEnd, AnInteger, ACallback, ATag, ATagObject, True);
end;

function TLccNode.RequestQueueConfigMemWriteEventID(ATarget: TNodeID; MemAddressStart, MemAddressEnd: DWord; AnEventID: TEventID; ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject): Boolean;
begin
  Result := InternalRequestConfigMemWriteEventID(ATarget, MemAddressStart, MemAddressEnd, AnEventID, ACallback, ATag, ATagObject, True);
end;

procedure TLccNode.RequestMemoryAccessQueueStart;
begin
  if TaskMemorySpaceAccess.QueuedRequests > 0 then
    TaskMemorySpaceAccess.Start(TIMEOUT_TASK_MESSAGES);
end;

procedure TLccNode.RequestMemoryAccessAbort;
begin
  TaskMemorySpaceAccess.Abort;
  TaskMemorySpaceAccess.Reset;
end;

procedure TLccNode.HandleTractionFDI_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  // Can't write to this memory space
  SendDatagramAckReply(SourceMessage, False, 0);
end;

procedure TLccNode.HandleOptionalInteractionRejected(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoOptionalInteractionRejected(Self,SourceMessage);
end;

procedure TLccNode.HandleProducerIdentifiedClear(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleProducerIdentifiedSet(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleProducerIdentifiedUnknown(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoProducerIdentified(Self, SourceMessage);
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
  ProtocolMemoryInfo.LoadReply(SourceMessage, WorkerMessage);
  QueueAndSendDatagramReplyToWaitForAck(SourceMessage, WorkerMessage); // Source may not have memory to take the data, set it up to resend if needed
end;

procedure TLccNode.HandleOperationGetConfiguration(var SourceMessage: TLccMessage);
begin
  ProtocolMemoryOptions.LoadReply(WorkerMessage, WorkerMessage);
  QueueAndSendDatagramReplyToWaitForAck(SourceMessage, WorkerMessage); // Source may not have memory to take the data, set it up to resend if needed
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
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleConfiguration_MemorySpaceWrite(var SourceMessage: TLccMessage; AutoGrowSpace: Boolean);
var
  Code: Word;
begin
  // Note here that for ACDI overlays the ConfigMemory so the ConfigMemory Address
  // must be offset by 1 as the ACDI has byte 0 = the ACDI version number

  Code := ProtocolMemoryAccess.DatagramWriteRequest(SourceMessage, StreamConfig, AutoGrowSpace, 1);
  case Code of
     S_OK : SendDatagramAckReply(SourceMessage, False, 0);      // All Ok just an Ack
  else
    SendMemoryWriteFailure(SourceMessage, Code, '');
  end;
end;

procedure TLccNode.HandleACDI_Manufacturer_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  // Can't write to this memory space
  SendMemoryWriteFailure(SourceMessage, ERROR_CODE_PERMANENT_NOT_IMPLEMENTED, '');
end;

procedure TLccNode.HandleConfiguration_MemorySpaceRead(var SourceMessage: TLccMessage; AutoGrowSpace: Boolean);
var
  Code: Word;
begin
  // Note here that for ACDI overlays the ConfigMemory so the ConfigMemory Address
  // must be offset by 1 as the ACDI has byte 0 = the ACDI version number
  Code := ProtocolMemoryAccess.DatagramReadRequest(SourceMessage, WorkerMessage, StreamConfig, AutoGrowSpace, 1);
  case Code of
     S_OK : QueueAndSendDatagramReplyToWaitForAck(SourceMessage, WorkerMessage);   // Source may not have memory to take the data, set it up to resend if needed
  else
    SendMemoryReadFailure(SourceMessage, Code, '');
  end;

end;

procedure TLccNode.HandleACDI_UserMemorySpaceWrite(var SourceMessage: TLccMessage);
var
  Code: Word;
begin
  // Note here that for ACDI overlays the ConfigMemory so the ConfigMemory Address
  // must be offset by 1 as the ACDI has byte 0 = the ACDI version number

  Code := ProtocolMemoryAccess.DatagramWriteRequest(SourceMessage, StreamConfig, True);
  case Code of
     S_OK : SendDatagramAckReply(SourceMessage, False, 0);     // All OK send an Ack
  else
    SendMemoryWriteFailure(SourceMessage, Code, '');
  end;
end;

procedure TLccNode.HandleStreamRead;
begin

end;

procedure TLccNode.HandleStreamWrite;
begin

end;

procedure TLccNode.HandleACDI_User_MemorySpaceRead(var SourceMessage: TLccMessage);
var
  Code: Word;
begin
  // Note here that for ACDI overlays the ConfigMemory so the ConfigMemory Address
  // must be offset by 1 as the ACDI has byte 0 = the ACDI version number
  Code := ProtocolMemoryAccess.DatagramReadRequest(SourceMessage, WorkerMessage, StreamConfig, False);
  case Code of
     S_OK : QueueAndSendDatagramReplyToWaitForAck(SourceMessage, WorkerMessage);   // Source may not have memory to take the data, set it up to resend if needed
  else
    SendMemoryReadFailure(SourceMessage, Code, '');
  end;
end;

procedure TLccNode.HandleACDI_Manufacturer_MemorySpaceRead(var SourceMessage: TLccMessage);
var
  Code: Word;
begin
  Code := ProtocolMemoryAccess.DatagramReadRequest(SourceMessage, WorkerMessage, StreamManufacturerData, False);
  case Code of
     S_OK : QueueAndSendDatagramReplyToWaitForAck(SourceMessage, WorkerMessage);   // Source may not have memory to take the data, set it up to resend if needed
  else
    SendMemoryReadFailure(SourceMessage, Code, '');
  end;
end;

procedure TLccNode.HandleAll_MemorySpaceRead(var SourceMessage: TLccMessage);
begin
  SendMemoryReadFailure(SourceMessage, ERROR_CODE_PERMANENT_NOT_IMPLEMENTED, '');
end;

procedure TLccNode.HandleCDI_MemorySpaceRead(var SourceMessage: TLccMessage);
var
  Code: Word;
begin
  Code := ProtocolMemoryAccess.DatagramReadRequest(SourceMessage, WorkerMessage, StreamCdi, False);
  case Code of
     S_OK : QueueAndSendDatagramReplyToWaitForAck(SourceMessage, WorkerMessage);   // Source may not have memory to take the data, set it up to resend if needed
  else
    SendMemoryReadFailure(SourceMessage, Code, '');
  end;
end;

procedure TLccNode.HandleCDI_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  // Can't write to this memory space
  SendMemoryWriteFailure(SourceMessage, ERROR_CODE_PERMANENT_NOT_IMPLEMENTED, '');
end;

procedure TLccNode.HandleAll_MemorySpaceWrite(var SourceMessage: TLccMessage);
begin
  // Can't write to this memory space
  SendMemoryWriteFailure(SourceMessage, ERROR_CODE_PERMANENT_NOT_IMPLEMENTED, '');
end;

procedure TLccNode.HandleConsumerIdentifiedSet(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleConsumerIdentifiedUnknown(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoConsumerIdentified(Self, SourceMessage);
end;

procedure TLccNode.HandleEventsIdentifyDest;
begin
  SendConsumedEvents;  // already known the destination is us
  SendProducedEvents;
end;

procedure TLccNode.HandleProtocolSupportInquiry(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadProtocolIdentifyReply(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ProtocolSupportedProtocols.EncodeFlags);
  SendMessage(WorkerMessage, Self);
end;

procedure TLccNode.HandleProtocolSupportReply(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoProtocolIdentifyReply(Self, SourceMessage);
end;

procedure TLccNode.HandleSimpleNodeInfoReply(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoSimpleNodeIdentReply(Self, SourceMessage);
end;

procedure TLccNode.HandleSimpleNodeInfoRequest(var SourceMessage: TLccMessage);
begin
  WorkerMessage.LoadSimpleNodeIdentInfoReply(NodeID, FAliasID,
  SourceMessage.SourceID, SourceMessage.SourceAlias,
  ProtocolSimpleNodeInfo.PackedFormat(StreamManufacturerData, StreamConfig));
  SendMessage(WorkerMessage, Self);
end;

procedure TLccNode.HandleTractionFDI_ConfigurationMemorySpaceRead(var SourceMessage: TLccMessage);
var
  Code: Word;
begin
  Code := ProtocolMemoryAccess.DatagramReadRequest(SourceMessage, WorkerMessage, StreamTractionConfig, False);
  case Code of
     S_OK : QueueAndSendDatagramReplyToWaitForAck(SourceMessage, WorkerMessage);   // Source may not have memory to take the data, set it up to resend if needed
  else
    SendMemoryReadFailure(SourceMessage, Code, '');
  end;
end;

procedure TLccNode.SendMemoryReadFailure(var MemoryReadRequestMessage: TLccMessage; ErrorCode: Word; ErrorString: String);
begin
  // No reason to Queue this to wait for the reply Ack as the receiving node has not reason to reject it
  SendDatagramAckReply(MemoryReadRequestMessage, True, 0);   // Per the Spec set pending true as we are sending the Failure Reply
  WorkerMessage.LoadConfigMemReadReplyError(NodeID, AliasID, MemoryReadRequestMessage.SourceID, MemoryReadRequestMessage.SourceAlias, MemoryReadRequestMessage.DecodeMemorySpace, MemoryReadRequestMessage.ExtractDataBytesAsInt(2, 5), ErrorCode, ErrorString);
  SendMessage(WorkerMessage, Self);
end;

procedure TLccNode.HandleTractionFDI_MemorySpaceRead(var SourceMessage: TLccMessage);
var
  Code: Word;
begin
  Code := ProtocolMemoryAccess.DatagramReadRequest(SourceMessage, WorkerMessage, StreamTractionFdi, False);
  case Code of
     S_OK : QueueAndSendDatagramReplyToWaitForAck(SourceMessage, WorkerMessage);   // Source may not have memory to take the data, set it up to resend if needed
  else
    SendMemoryReadFailure(SourceMessage, Code, '');
  end;
end;

procedure TLccNode.HandleTractionFDI_ConfigurationMemorySpaceWrite(var SourceMessage: TLccMessage);
var
  Code: Word;
begin
  Code := ProtocolMemoryAccess.DatagramWriteRequest(SourceMessage, StreamTractionConfig, True);
  case Code of
     S_OK : SendDatagramAckReply(SourceMessage, False, 0);     // All OK send an Ack
  else
    SendMemoryWriteFailure(SourceMessage, Code, '');
  end;
end;

procedure TLccNode.SendMemoryWriteFailure(var MemoryWriteRequestMessage: TLccMessage; ErrorCode: Word; ErrorString: String);
begin
  // No reason to Queue this to wait for the reply Ack as the receiving node has not reason to reject it
  SendDatagramAckReply(MemoryWriteRequestMessage, True, 0);   // Per the Spec set pending true as we are sending the Failure Reply
  WorkerMessage.LoadConfigMemWriteReplyError(NodeID, AliasID, MemoryWriteRequestMessage.SourceID, MemoryWriteRequestMessage.SourceAlias, MemoryWriteRequestMessage.DecodeMemorySpace, MemoryWriteRequestMessage.ExtractDataBytesAsInt(2, 5), ErrorCode, ErrorString);
  SendMessage(WorkerMessage, Self);
end;

procedure TLccNode.HandleVerifiedNodeIDNumber(var SourceMessage: TLccMessage);
begin
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoVerifiedNodeID(Self, SourceMessage);
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
      SendMessage(WorkerMessage, Self);
    end
  end else
  begin
    WorkerMessage.LoadVerifiedNodeID(NodeID, FAliasID);
    SendMessage(WorkerMessage, Self);
  end;
end;

procedure TLccNode.HandleVerifyNodeIDNumberDest;
begin
  // Must always respond (if we get through the destintaion Alias check) per the TN
  WorkerMessage.LoadVerifiedNodeID(NodeID, FAliasID);
  SendMessage(WorkerMessage, Self);
end;

procedure TLccNode.HandleTractionControllerAssign(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionControllerRelease(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionControllerQuery(var SourceMessage: TLccMessage); begin end;

//procedure TLccNode.HandleTractionControllerChangedNotify(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionEStop(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); begin end;

procedure TLccNode.HandleTractionListenerAttach(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionListenerDetach(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionListenerQuery(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionManageReserve(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionManageRelease(var SourceMessage: TLccMessage); begin end;

procedure TLccNode.HandleTractionSetSpeed(var SourceMessage: TLccMessage;
  ListenerForwarded: Boolean); begin end;

procedure TLccNode.HandleTractionSetFunction(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); begin end;

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
    if ALength < LEN_SNIP_MFG_NAME then
    begin
      StreamManufacturerData.Position := ADDRESS_MFG_NAME;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<model>', AnOffset, ALength) then
  begin
    if ALength < LEN_SNIP_MODEL then
    begin
      StreamManufacturerData.Position := ADDRESS_MODEL_NAME;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<hardwareVersion>', AnOffset, ALength) then
  begin
    if ALength < LEN_SNIP_HARDWARE_VERSION then
    begin
      StreamManufacturerData.Position := ADDRESS_HARDWARE_VERSION;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  if FindCdiElement(ACdi, '<softwareVersion>', AnOffset, ALength) then
  begin
    if ALength < LEN_SNIP_SOFTWARE_VERSION then
    begin
      StreamManufacturerData.Position := ADDRESS_SOFTWARE_VERSION;
      for i := AnOffset to AnOffset + ALength - 1 do
        StreamWriteByte(StreamManufacturerData, Ord(ACdi[i]));
    end else Exit;
  end else Exit;
  Result := True;
end;

constructor TLccNode.Create(AOwner: TComponent; CdiXML: string);
var
  i, Counter: Integer;
begin
  inherited Create(AOwner);
  FProtocolSupportedProtocols := TProtocolSupportedProtocols.Create;
  FProtocolSimpleNodeInfo := TProtocolSimpleNodeInfo.Create;
  FProtocolMemoryOptions := TProtocolMemoryOptions.Create;
  FProtocolMemoryAccess := TProtocolMemoryAccess.Create;
  FProtocolMemoryInfo := TProtocolMemoryInfo.Create;
  FProtocolEventConsumed := TProtocolEvents.Create;
  FProtocolEventsProduced := TProtocolEvents.Create;

  FStreamCdi := TMemoryStream.Create;
  FStreamConfig := TMemoryStream.Create;
  FStreamManufacturerData := TMemoryStream.Create;
  FStreamTractionConfig := TMemoryStream.Create;
  FStreamTractionFdi := TMemoryStream.Create;

  FDatagramResendQueue := TDatagramQueue.Create;
  DatagramResendQueue.OwnerNode := Self;
  FWorkerMessageForAckMessages := TLccMessage.Create;
  FWorkerMessage := TLccMessage.Create;

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

  // Setup the Configuration Memory Stream this is good for a single node...
  // Note here that for ACDI overlays the ConfigMemory so the ConfigMemory Address
  // must be offset by 1 as the ACDI has byte 0 = the ACDI version number
  StreamConfig.Size := LEN_USER_MANUFACTURER_INFO;
  StreamConfig.Position := 0;
  StreamWriteByte(StreamConfig, USER_MFG_INFO_VERSION_ID);
  while StreamConfig.Position < StreamConfig.Size do
    StreamWriteByte(StreamConfig, 0);

  // Setup the Fdi Stream

  // Setup the Function Configuration Memory Stream

   FTasksList := TList.Create;

   FTaskMemorySpaceAccess := TLccTaskMemorySpaceAccess.Create(Self);
   FTaskReadPIP := TTaskReadPIP.Create(Self);
   FTaskReadSNIP := TTaskReadSNIP.Create(Self);

   RegisterTask(TaskReadPIP);
   RegisterTask(TaskReadSNIP);
   RegisterTask(TaskMemorySpaceAccess);

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

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, StreamCdi.Size);
  ProtocolMemoryInfo.Add(MSI_ALL, False, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);

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
  ProtocolMemoryOptions.LowSpace := MSI_ACDI_USER;

  // Create a few events for fun
  ProtocolEventConsumed.AutoGenerate.Count := 5;
  ProtocolEventConsumed.AutoGenerate.StartIndex := 0;
  ProtocolEventsProduced.AutoGenerate.Count := 5;
  ProtocolEventsProduced.AutoGenerate.StartIndex := 0;
end;

procedure TLccNode.LccLogIn(ANodeID: TNodeID);
begin
  if LogInEngineEnabled then
  begin
    BeforeLogin;
    if NullNodeID(ANodeID) then
      CreateNodeID(ANodeID);  // This should only be true if not GridConnect and the NodeID was not set
    FNodeID := ANodeID;
    ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoNodeIDChanged(Self);
    FInitialized := True;

    // Send Initialization Complete
    WorkerMessage.LoadInitializationComplete(NodeID, FAliasID);
    SendMessage(WorkerMessage, Self);
    ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoInitializationComplete(Self);


    AutoGenerateEvents;
    SendEvents;
    ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoLogInNode(Self);
    AfterLogin;
  end;
end;

function TLccNode.InternalRequestConfigMemRead(ATarget: TNodeID;
  MemAddressStart, MemAddressEnd: DWord; IsString: Boolean;
  ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject; Queue: Boolean
  ): Boolean;
begin
  Result := False;
  if TaskMemorySpaceAccess.TaskState = lesIdle then
  begin
    TaskMemorySpaceAccess.Callback := ACallback;
    TaskMemorySpaceAccess.Tag := ATag;
    TaskMemorySpaceAccess.TagObject := ATagObject;
    TaskMemorySpaceAccess.Target := ATarget;
    TaskMemorySpaceAccess.Assign(lems_Read, MSI_CONFIG, IsString, MemAddressStart, MemAddressEnd, True, ATarget, AliasServer.FindAlias(ATarget), ACallback, ATag, ATagObject);
    if not Queue then
      TaskMemorySpaceAccess.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccNode.InternalRequestConfigMemWriteString(ATarget: TNodeID;
  MemAddressStart, MemAddressEnd: DWord; AString: String;
  ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject; Queue: Boolean
  ): Boolean;
var
  MemSpaceObj: TLccTaskMemorySpaceObject;
begin
  Result := False;
  if TaskMemorySpaceAccess.TaskState = lesIdle then
  begin
    TaskMemorySpaceAccess.Callback := ACallback;
    TaskMemorySpaceAccess.Tag := ATag;
    TaskMemorySpaceAccess.TagObject := ATagObject;
    TaskMemorySpaceAccess.Target := ATarget;
    MemSpaceObj := TaskMemorySpaceAccess.Assign(lems_Write, MSI_CONFIG, True, MemAddressStart, MemAddressEnd, True, ATarget, AliasServer.FindAlias(ATarget), ACallback, ATag, ATagObject);
    MemSpaceObj.WriteString := AString;
    if not Queue then
      TaskMemorySpaceAccess.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccNode.InternalRequestConfigMemWriteInteger(ATarget: TNodeID;
  MemAddressStart, MemAddressEnd: DWord; AnInteger: Integer;
  ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject; Queue: Boolean
  ): Boolean;
var
  MemSpaceObj: TLccTaskMemorySpaceObject;
begin
  Result := False;
  if TaskMemorySpaceAccess.TaskState = lesIdle then
  begin
    TaskMemorySpaceAccess.Callback := ACallback;
    TaskMemorySpaceAccess.Tag := ATag;
    TaskMemorySpaceAccess.TagObject := ATagObject;
    TaskMemorySpaceAccess.Target := ATarget;
    MemSpaceObj := TaskMemorySpaceAccess.Assign(lems_Write, MSI_CONFIG, False, MemAddressStart, MemAddressEnd, True, ATarget, AliasServer.FindAlias(ATarget), ACallback, ATag, ATagObject);
    MemSpaceObj.WriteInteger := AnInteger;
    if not Queue then
      TaskMemorySpaceAccess.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccNode.InternalRequestConfigMemWriteEventID(ATarget: TNodeID;
  MemAddressStart, MemAddressEnd: DWord; AnEventID: TEventID;
  ACallback: TOnTaskCallback; ATag: Integer; ATagObject: TObject; Queue: Boolean
  ): Boolean;
var
  MemSpaceObj: TLccTaskMemorySpaceObject;
begin
  Result := False;
  if TaskMemorySpaceAccess.TaskState = lesIdle then
  begin
    TaskMemorySpaceAccess.Callback := ACallback;
    TaskMemorySpaceAccess.Tag := ATag;
    TaskMemorySpaceAccess.TagObject := ATagObject;
    TaskMemorySpaceAccess.Target := ATarget;
    MemSpaceObj := TaskMemorySpaceAccess.Assign(lems_Write, MSI_CONFIG, False, MemAddressStart, MemAddressEnd, True, ATarget, AliasServer.FindAlias(ATarget), ACallback, ATag, ATagObject);
    MemSpaceObj.WriteEventID := AnEventID;
    if not Queue then
      TaskMemorySpaceAccess.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
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
    if not EqualNode(NodeID,  AliasID, SourceMessage.DestID, SourceMessage.DestAlias, True) then
      Exit;
  end;

  for i := 0 to TasksList.Count - 1 do
    TLccTaskBase(TasksList[i]).Process(SourceMessage);

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
          TRACTION_SET_SPEED_DIR       : HandleTractionSetSpeed(SourceMessage, False);
          TRACTION_SET_SPEED_DIR_LISTENER_FORWARDED : HandleTractionSetSpeed(SourceMessage, True);
          TRACTION_SET_FUNCTION        : HandleTractionSetFunction(SourceMessage, False);
          TRACTION_SET_FUNCTION_LISTENER_FORWARDED : HandleTractionSetFunction(SourceMessage, True);
          TRACTION_SET_E_STOP          : HandleTractionEStop(SourceMessage, False);
          TRACTION_SET_E_STOP_LISTENER_FORWARDED : HandleTractionEStop(SourceMessage, True);
          TRACTION_QUERY_SPEED         : HandleTractionQuerySpeed(SourceMessage);
          TRACTION_QUERY_FUNCTION      : HandleTractionQueryFunction(SourceMessage);
          TRACTION_CONTROLLER_CONFIG :
            begin
              case SourceMessage.DataArray[1] of
                TRACTION_CONTROLLER_CONFIG_ASSIGN  : HandleTractionControllerAssign(SourceMessage);
                TRACTION_CONTROLLER_CONFIG_RELEASE : HandleTractionControllerRelease(SourceMessage);
                TRACTION_CONTROLLER_CONFIG_QUERY   : HandleTractionControllerQuery(SourceMessage);
    //            TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY : HandleTractionControllerChangedNotify(SourceMessage);
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
                       MSI_CONFIG                   : HandleConfiguration_MemorySpaceWrite(SourceMessage, False); // Configuration Memory through the CDI protocol
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
                       MSI_CONFIG                   : HandleConfiguration_MemorySpaceRead(SourceMessage, False);
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
                  //     MCP_OP_INDICATE           : HandleOperationIndicate(SourceMessage);
                  //     MCP_OP_RESETS             : HandleOperationReset(SourceMessage);
                     end // case
                   end;
               end
             end
         else begin {case else}
             // Unknown Datagram Type
             WorkerMessage.LoadDatagramRejected(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ERROR_CODE_PERMANENT_TYPE_UNKNOWN);
             SendMessage(WorkerMessage, Self);
           end;
         end;  // case
       end;
  else begin
      if SourceMessage.HasDestination then
      begin
        WorkerMessage.LoadOptionalInteractionRejected(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ERROR_CODE_PERMANENT_MTI_TRANSPORT_UNKNOWN, SourceMessage.MTI);
        SendMessage(WorkerMessage, Self);
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
  if (AliasID <> 0) and (SourceMessage.SourceAlias = AliasID) then
  begin
    // Check if it is a Check ID message for a node trying to use our Alias and if so tell them no.
    if ((SourceMessage.CAN_MTI and $0F000000) >= MTI_CAN_CID6) and ((SourceMessage.CAN_MTI and $0F000000) <= MTI_CAN_CID0) then
    begin
      WorkerMessage.LoadRID(NodeID, AliasID);                   // sorry charlie this is mine
      SendMessage(WorkerMessage, Self);
      Result := True;
    end else
    if Permitted then
    begin
      // Another node used our Alias, stop using this Alias, log out and allocate a new node and relog in
      ReleaseAlias(100);
      Login(NULL_NODE_ID, False);
      Result := True;   // Logout covers any LccNode logoffs, so don't call ancester Process Message
    end
  end;
  // END: Alias Allocation, duplicate checking after allocation******************


  if not Permitted then
  begin
    // We are still trying to allocate a new Alias, someone else is using this alias
    if SourceMessage.SourceAlias = AliasID then
      DuplicateAliasDetected := True;
  end else
  begin  // Normal message loop once successfully allocating an Alias
    if SourceMessage.IsCAN then
    begin
      case SourceMessage.CAN_MTI of
        MTI_CAN_AME :          // Asking us for an Alias Map Enquiry
          begin
            if SourceMessage.DataCount = 6 then
            begin
              SourceMessage.ExtractDataBytesAsNodeID(0, TestNodeID);
              if EqualNodeID(TestNodeID, NodeID, False) then
              begin
                WorkerMessage.LoadAMD(NodeID, AliasID);
                SendMessage(WorkerMessage, Self);
              end
            end else
            begin

              WorkerMessage.LoadAMD(NodeID, AliasID);
              SendMessage(WorkerMessage, Self);
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

procedure TLccNode.RegisterTask(ATask: TLccTaskBase);
begin
  TasksList.Add(ATask);
end;

procedure TLccNode.UnRegisterTask(ATask: TLccTaskBase);
begin
  TasksList.Remove(ATask);
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

procedure TLccNode.CreateNodeID(var Seed: TNodeID);
begin
  Seed[1] := StrToInt('0x020112');
  Seed[0] := Random($FFFFFF);
end;

destructor TLccNode.Destroy;
begin
//  NotifyAndUpdateMappingChanges; // fire any eventfor Mapping changes are are marked for deletion in the Logout method

  UnRegisterTask(TaskReadPIP);
  UnRegisterTask(TaskReadSNIP);
  UnRegisterTask(TaskMemorySpaceAccess);



  if AliasID <> 0 then
    ReleaseAlias(100);
   ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoAliasIDChanged(Self);

  FNodeID[0] := 0;
  FNodeID[1] := 0;
  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoNodeIDChanged(Self);

  ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoDestroyLccNode(Self);

  FreeAndNil(FProtocolSupportedProtocols);
  FreeAndNil(FProtocolSimpleNodeInfo);
  FreeAndNil(FProtocolEventConsumed);
  FreeAndNil(FProtocolEventsProduced);
  FreeAndNil(FProtocolMemoryOptions);
  FreeAndNil(FProtocolMemoryInfo);
  FreeAndNil(FProtocolMemoryAccess);
  FreeAndNil(FDatagramResendQueue);
  FreeAndNil(FWorkerMessageForAckMessages);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FStreamCdi);
  FreeAndNil(FStreamConfig);
  FreeAndNil(FStreamManufacturerData);
  FreeAndNil(FStreamTractionConfig);
  FreeAndNil(FStreamTractionFdi);
  FreeAndNil(FTasksList);
  FreeAndNil(FTaskMemorySpaceAccess);
  FreeAndNil(FTaskReadPIP);
  FreeAndNil(FTaskReadSNIP);
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

procedure TLccNode.Login(ANodeID: TNodeID; InitialLogIn: Boolean);
var
  Temp: TNodeID;
begin
  LogInEngineEnabled := True;

  if (Owner as TLccNodeManager).EmulateCanNetworkLogin then
  begin
    BeforeLogin;

    if InitialLogIn then
    begin
      if NullNodeID(ANodeID) then
        CreateNodeID(ANodeID);
      SeedNodeID := ANodeID;
      Temp := FSeedNodeID;
      FNodeID := ANodeID;
      ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoNodeIDChanged(Self);
    end else
    begin
      // Typically due to an alias conflict to create a new one
      AliasServer.RemoveMapping(AliasID, True);
      Temp := FSeedNodeID;
      GenerateNewSeed(Temp);
      FSeedNodeID := Temp;
    end;
    FAliasID := GenerateID_Alias_From_Seed(Temp);

    WorkerMessage.LoadCID(NodeID, AliasID, 0);
    SendMessage(WorkerMessage, Self);
    WorkerMessage.LoadCID(NodeID, AliasID, 1);
    SendMessage(WorkerMessage, Self);
    WorkerMessage.LoadCID(NodeID, AliasID, 2);
    SendMessage(WorkerMessage, Self);
    WorkerMessage.LoadCID(NodeID, AliasID, 3);
    SendMessage(WorkerMessage, Self);

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
    if (Owner as TLccNodeManager).EmulateCanNetworkLogin then
    begin
      WorkerMessage.LoadAMR(NodeID, AliasID);
      SendMessage(WorkerMessage, Self);
      // Wait for the message to get sent on the hardware layers.  Testing this happens is complicated
      // This assumes they are all running in separate thread and they keep running
      Sleep(DelayTime_ms);
      FPermitted := False;
      AliasServer.RemoveMapping(AliasID, True);
      ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoAliasRelease(Self);
      FAliasID := 0;
      LogInEngineEnabled := False; // must call LogIn again to reenable
    end;
    DatagramResendQueue.Clear;
    FInitialized := False;
  end;
end;

procedure TLccNode.On_100msTimer(Sender: TObject);
var
  Temp: TNodeID;
  i: Integer;
begin
  if (Owner as TLccNodeManager).EmulateCanNetworkLogin then
  begin
    if LogInEngineEnabled then
    begin
    if not Permitted then
      begin
        Inc(FLoginTimoutCounter);
         // Did any node object to this Alias registration attempt?
        if DuplicateAliasDetected then
        begin
          DuplicateAliasDetected := False;  // Reset
          Temp := FSeedNodeID;
          GenerateNewSeed(Temp);
          FSeedNodeID := Temp;
          FAliasID := GenerateID_Alias_From_Seed(Temp);
          WorkerMessage.LoadCID(NodeID, AliasID, 0);
          SendMessage(WorkerMessage, Self);
          WorkerMessage.LoadCID(NodeID, AliasID, 1);
          SendMessage(WorkerMessage, Self);
          WorkerMessage.LoadCID(NodeID, AliasID, 2);
          SendMessage(WorkerMessage, Self);
          WorkerMessage.LoadCID(NodeID, AliasID, 3);
          SendMessage(WorkerMessage, Self);
          LoginTimoutCounter := 0;
        end else
        begin
          if LoginTimoutCounter > 7 then
          begin
            FPermitted := True;
            WorkerMessage.LoadRID(NodeID, AliasID);
            SendMessage(WorkerMessage, Self);
            WorkerMessage.LoadAMD(NodeID, AliasID);
            SendMessage(WorkerMessage, Self);
            ((Owner as TLccNodeManager) as INodeManagerCallbacks).DoAliasIDChanged(Self);
            LccLogIn(NodeID);
          end;
        end
      end else  // Is Permitted
      begin
        DatagramResendQueue.TickTimeout;
        for i := 0 to TasksList.Count - 1 do TLccTaskBase(TasksList[i])._100msTimeTick;
      end;
    end
  end else  // Is not GridConnect
  begin
    DatagramResendQueue.TickTimeout;
    for i := 0 to TasksList.Count - 1 do TLccTaskBase(TasksList[i])._100msTimeTick;
  end;
end;

function TLccNode.ProcessMessage(SourceMessage: TLccMessage): Boolean;
begin
  if (Owner as TLccNodeManager).EmulateCanNetworkLogin then
    Result := ProcessMessageGridConnect(SourceMessage)   // When necessary ProcessMessageGridConnect drops the message into ProcessMessageLCC
  else
    Result := ProcessMessageLCC(SourceMessage);
end;

procedure TLccNode.SendDatagramAckReply(SourceMessage: TLccMessage; ReplyPending: Boolean; TimeOutValueN: Byte);
begin
  // Only Ack if we accept the datagram
  WorkerMessageForAckMessages.LoadDatagramAck(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, True, ReplyPending, TimeOutValueN);
  SendMessage(FWorkerMessageForAckMessages, Self);
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
    SendMessage(WorkerMessage, Self);
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
    SendMessage(WorkerMessage, Self);
  end;
end;

procedure TLccNode.SendDatagramRejectedReply(SourceMessage: TLccMessage; Reason: Word);
begin
  WorkerMessageForAckMessages.LoadDatagramRejected(NodeID, FAliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, Reason);
  SendMessage(WorkerMessageForAckMessages, Self);
end;

procedure TLccNode.QueueAndSendDatagramReplyToWaitForAck(DatagramRequest, DatagramReply: TLccMessage);
begin
  if DatagramResendQueue.Add(DatagramReply.Clone) then     // Message that is waiting for an ACK for possible resend
  begin
    SendDatagramAckReply(DatagramRequest, False, 0);       // Ack the Request
    SendMessage(DatagramReply, Self);                  // Now send our reply
  end else
    SendDatagramRejectedReply(DatagramRequest, ERROR_CODE_TEMPORARY_BUFFER_UNAVAILABLE)   // We have problems so ask the node to resend if it wants
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
    SendMessage(WorkerMessage, Self);
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
    SendMessage(WorkerMessage, Self);
  end;
end;

initialization
  {$IFNDEF PYTHON_SCRIPT_COMPATIBLE}
  Randomize;
  {$ENDIF}

finalization

end.


