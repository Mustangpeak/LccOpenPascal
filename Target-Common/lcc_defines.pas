unit lcc_defines;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  {$IFDEF LCC_FPC}
    {$IFNDEF WEB_APP}
     {$IFDEF FPC_CONSOLE_APP}
       fptimer,
     {$ELSE}
       ExtCtrls,
     {$ENDIF}
    {$ELSE}
      timer,
    {$ENDIF}
  {$ELSE}
    FMX.Types,
  {$ENDIF}
  Classes,
  SysUtils;

const
  TASK_ERROR_MEMORY_SPACE_UNSUPPORTED_PROTOCOL      = $0001;
  TASK_ERROR_MEMORY_SPACE_UNSUPPORTED_MEMORYSPACE   = $0002;
  TASK_ERROR_MEMORY_SPACE_READ_ERROR                = $0003;
  TASK_ERROR_MEMORY_SPACE_WRITE_ERROR               = $0004;
  TASK_ERROR_MEMORY_SPACE_WRITE_TO_READONLY_SPACE   = $0005;
  TASK_ERROR_MEMORY_SPACE_NO_JOB_ASSIGNED           = $0006;

  ENGINE_ERROR_TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN = $0001;

const
  TIMEOUT_RECIEVE_THREAD = 20;             // 20ms sleep time
  TIMEOUT_CONTROLLER_NOTIFY_WAIT = 2000;   // milliseconds   How long we wait for the Notified message to come from the currently assigned Controller
  TIMEOUT_TASK_MESSAGES = 5000;
  TIMEOUT_TASK_MESSSAGE_INFINITY = 0;

const
  PATH_OSX_RESOURCES = 'Contents/Resources/';
  PATH_OSX_EXECUTABLE = 'Contents/MacOS/';
  PATH_UNIX_APPLICATION = '/usr/share/';    // Typical place to store the application foldler
  PATH_UNIX_SETTINGS = '/home/{user}/.config/{executable_name}';  // GetAppConfigDir  does this for us but this is what it returns
  PATH_LINUX_DEV = '/dev/';
  PATH_OSX_DEV = 'dev/';

const
  LCC_BYTE_COUNT            = 1024;       // This is longest data structure defined in Lcc
  LEN_DATAGRAM_MAX       = 72;
  LEN_EVENT_MAX             = 8;
  LEN_NODEID_MAX            = 6;
  LEN_MULTIFRAME_MAX        = 12;
  LEN_SUPPORTEDPROTOCOL_MAX = 6;

  {$IFDEF LCC_DELPHI}    // Must be Delphi
  type
    DWord = Cardinal;
    QWord = UInt64;
    SizeUInt = NativeUInt;
  const
    DefaultStackSize = 4*102 *1024;
    {$IFDEF LCC_MOBILE}
 //     AnsiChar = Char;
 //     AnsiString = string;
//      PAnsiString = ^string;
 //     PAnsiChar = ^Char;
    {$ENDIF}
  {$ENDIF}

type

  {$IFDEF FPC_CONSOLE_APP}
    TLccTimer = TFPTimer;
  {$ELSE}
    TLccTimer = TTimer;
  {$ENDIF}


type
  TLccByteArray = array[0..LCC_BYTE_COUNT-1] of Byte;
  TLccSupportedProtocolArray = array[0..LEN_SUPPORTEDPROTOCOL_MAX] of Byte;

  TNodeID = array[0..1] of DWord;
  {$IFNDEF WEB_APP}
  PNodeID = ^TNodeID;
  {$ENDIF}

  { TNodeIDObj }

  TNodeIDObj = class
  private
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
  end;

  TFunctionStatesArray = array[0..28] of Word;
  TLccDynamicByteArray = array of Byte;
  {$IFNDEF WEB_APP}
  PLccDynamicByteArray = ^TLccDynamicByteArray;
  {$ENDIF}

type
  TDatagramArray = array[0..LEN_DATAGRAM_MAX-1] of Byte;

  TEventID = array[0..LEN_EVENT_MAX-1] of Byte;
  {$IFNDEF WEB_APP}
  PEventID = ^TEventID;
  {$ENDIF}

  THexArray = TEventID;

  TMultiFrameArray = array[0..LEN_MULTIFRAME_MAX-1] of Byte;

  TLccAliasMappingRec = record
    NodeID: TNodeID;
    Alias: Word;
  end;

  TNodeIDArray = array of TNodeID;

const
  MAX_FUNCTIONS = 29;

  MAX_MESSAGE_RETRY_ATTEMPTS = 5;

  MAX_DCC_ADDRESS = 10239;

type
  TEventState = (evs_Unknown, evs_Valid, evs_InValid);
  TLccConfigDataType = (cdt_String, cdt_Int, cdt_EventID, cdt_Bit);

  TLccConnectionState = (lcsDisconnected, lcsConnecting, lcsConnected, lcsDisconnecting, lcsClientConnected, lcsClientDisconnected);    // make disconnected the default value

  TLccTrainDirection = (tdForward, tdReverse);
  TLccFunctions = array[0..MAX_FUNCTIONS - 1] of Word;
  TMessageComPort = procedure(Sender: TObject; var GridConnectStyleMessage: string) of object;

// Solves circular reference as the parser need to know about lcc_nodemanager and vice versa
type
  TLccCdiParserBase = class(TComponent)
  public
    procedure DoConfigMemReadReply(ANode: TObject); virtual; abstract;
    procedure DoConfigMemWriteReply(ANode: TObject); virtual; abstract;
    procedure NotifyLccNodeDestroy(LccNode: TObject); virtual; abstract;
  end;

const
  MFG_INFO_VERSION_ID      = 1;
  USER_MFG_INFO_VERSION_ID = 1;

  LEN_SNIP_MFG_VERSION      = 1;
  LEN_SNIP_MFG_NAME         = 41;
  LEN_SNIP_MODEL       = 41;
  LEN_SNIP_HARDWARE_VERSION = 21;
  LEN_SNIP_SOFTWARE_VERSION = 21;

  LEN_SNIP_USER_VERSION     = 1;
  LEN_SNIP_USER_NAME        = 63;
  LEN_SNIP_USER_DESCRIPTION = 64;


  LEN_MANUFACTURER_INFO = LEN_SNIP_MFG_VERSION + LEN_SNIP_MFG_NAME + LEN_SNIP_MODEL + LEN_SNIP_HARDWARE_VERSION + LEN_SNIP_SOFTWARE_VERSION;
  LEN_USER_MANUFACTURER_INFO = LEN_SNIP_USER_VERSION + LEN_SNIP_USER_NAME + LEN_SNIP_USER_DESCRIPTION;

  ADDRESS_VERSION           = 0;     // Same for both fixed and user

  ADDRESS_MFG_NAME          = 1;
  ADDRESS_MODEL_NAME        = 42;
  ADDRESS_HARDWARE_VERSION  = 83;
  ADDRESS_SOFTWARE_VERSION  = 104;

  ADDRESS_USER_NAME        = 1;
  ADDRESS_USER_DESCRIPTION  = 64;

const
  // Full CAN MTI
  MTI_CAN_ADDRESS_PRESENT                = $00008000;                                // Address in the CAN Data present if set

  MTI_CAN_CAN                            = $00000000;                                // Frame Type CAN Control Message
  MTI_CAN_CID0                           = $07000000;                                // First 12 Bits of 48 bit Node ID
  MTI_CAN_CID1                           = $06000000;                                // 2rd 12 Bits of 48 bit Node ID
  MTI_CAN_CID2                           = $05000000;                                // 3nd 12 Bits of 48 bit Node ID
  MTI_CAN_CID3                           = $04000000;                                // Last 12 Bits of 48 bit Node ID
  MTI_CAN_CID4                           = $03000000;                                // non-OpenLCB Protocol
  MTI_CAN_CID5                           = $02000000;                                // non-OpenLCB Protocol
  MTI_CAN_CID6                           = $01000000;                                // non-OpenLCB Protocol

  MTI_CAN_RID                            = $00700000;                                // Reserve ID
  MTI_CAN_AMD                            = $00701000;                                // Alias Map Definition
  MTI_CAN_AME                            = $00702000;                                // Alias Mapping Enquiry
  MTI_CAN_AMR                            = $00703000;                                // Alias Map Reset Frame

  MTI_CAN_MASK                              = $0FFFF000;
  MTI_CAN_FRAME_TYPE_MASK                   = $0F000000;
  MTI_CAN_FRAME_TYPE_GENERAL                = $09000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_ONLY    = $0A000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_START   = $0B000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME         = $0C000000;
  MTI_CAN_FRAME_TYPE_DATAGRAM_FRAME_END     = $0D000000;

  MTI_CAN_ADDRESSED_MASK                 = $00008000;
  MTI_CAN_SIMPLE_PROTOCOL_MASK           = $00010000;
  MTI_CAN_EVENT_PRESENT_MASK             = $00002000;

  MTI_CAN_INITIALIZATION_COMPLETE        = $09100000;                                // Databytes = Full Node ID
  MTI_CAN_VERIFY_NODE_ID_NUMBER_DEST     = $09488000;                                // Databytes = Destination Alias
  MTI_CAN_VERIFY_NODE_ID_NUMBER          = $09490000;                                //
  MTI_CAN_VERIFIED_NODE_ID_NUMBER        = $09170000;                                // {Optional Full Node ID}
  MTI_CAN_OPTIONAL_INTERACTION_REJECTED  = $09068000;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_CAN_TERMINATE_DUE_TO_ERROR         = $090A8000;                                // Databytes = Destination Alias, Error, {Optional Info}

  MTI_CAN_PROTOCOL_SUPPORT_INQUIRY       = $09828000;                                // Databytes = Destination Alias
  MTI_CAN_PROTOCOL_SUPPORT_REPLY         = $09668000;                                // Databytes = Destination Alias, Protocol Flags

  MTI_CAN_CONSUMER_IDENTIFY              = $098F4000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFY_RANGE        = $094A4000;                                // Databytes = EventID with Mask
  MTI_CAN_CONSUMER_IDENTIFIED_UNKNOWN    = $094C7000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_SET        = $094C4000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_CLEAR      = $094C5000;                                // Databytes = EventID
  MTI_CAN_CONSUMER_IDENTIFIED_RESERVED   = $094C6000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENDIFY              = $09914000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFY_RANGE        = $09524000;                                // Databytes = EventID with Mask
  MTI_CAN_PRODUCER_IDENTIFIED_UNKNOWN    = $09547000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_SET        = $09544000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_CLEAR      = $09545000;                                // Databytes = EventID
  MTI_CAN_PRODUCER_IDENTIFIED_RESERVED   = $09546000;                                // Databytes = EventID
  MTI_CAN_EVENTS_IDENTIFY_DEST           = $09968000;                                // Databytes = Destination Alias
  MTI_CAN_EVENTS_IDENTIFY                = $09970000;                                //
  MTI_CAN_EVENT_LEARN                    = $09594000;                                // Databytes = EventID
  MTI_CAN_PC_EVENT_REPORT                = $095B4000;                                // Databytes = EventID  (Infamouse PCER)

  MTI_CAN_SIMPLE_NODE_INFO_REQUEST       = $09DE8000;                                // Databytes = Destination Alias
  MTI_CAN_SIMPLE_NODE_INFO_REPLY         = $09A08000;                                // Databytes = Destination Alias, ACDI Data

  MTI_CAN_SIMPLE_TRAIN_INFO_REQUEST      = $09DA8000;                                // Databytes = Destination Alias
  MTI_CAN_SIMPLE_TRAIN_INFO_REPLY        = $099C8000;                                // Databytes = Destination Alias, ACDI Data

  MTI_CAN_TRACTION_PROTOCOL              = $095EB000;                                // Databyte = depends;
  MTI_CAN_TRACTION_REPLY                 = $091E9000;                                // Databyte = depends

  MTI_CAN_STREAM_INIT_REQUEST            = $09CC8000;
  MTI_CAN_STREAM_INIT_REPLY              = $09868000;
  MTI_CAN_FRAME_TYPE_CAN_STREAM_SEND     = $0F000000;
  MTI_CAN_STREAM_PROCEED                 = $09888000;
  MTI_CAN_STREAM_COMPLETE                = $098A8000;

  MTI_CAN_DATAGRAM_OK_REPLY              = $09A28000;                                // Databytes = Destination Alias
  MTI_CAN_DATAGRAM_REJECTED_REPLY        = $09A48000;                                // Databytes = Destination Alias, Error Code

  // Raw MTI
  MTI_ADDRESSED_MASK                 = $0008;
  MTI_SIMPLE_PROTOCOL_MASK           = $0010;
  MTI_EVENT_PRESENT_MASK             = $0002;

  MTI_INITIALIZATION_COMPLETE        = $0100;                                // Databytes = Full Node ID
  MTI_VERIFY_NODE_ID_NUMBER_DEST     = $0488;                                // Databytes = Destination Alias
  MTI_VERIFY_NODE_ID_NUMBER          = $0490;                                //
  MTI_VERIFIED_NODE_ID_NUMBER        = $0170;                                // {Optional Full Node ID}
  MTI_OPTIONAL_INTERACTION_REJECTED  = $0068;                                // Databytes = Destination Alias, Error, {Optional Info}
  MTI_TERMINATE_DUE_TO_ERROR         = $00A8;                                // Databytes = Destination Alias, Error, {Optional Info}

  MTI_PROTOCOL_SUPPORT_INQUIRY       = $0828;                                // Databytes = Destination Alias
  MTI_PROTOCOL_SUPPORT_REPLY         = $0668;                                // Databytes = Destination Alias, Protocol Flags

  MTI_CONSUMER_IDENTIFY              = $08F4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFY_RANGE        = $04A4;                                // Databytes = EventID with Mask
  MTI_CONSUMER_IDENTIFIED_UNKNOWN    = $04C7;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_SET        = $04C4;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_CLEAR      = $04C5;                                // Databytes = EventID
  MTI_CONSUMER_IDENTIFIED_RESERVED   = $04C6;                                // Databytes = EventID
  MTI_PRODUCER_IDENDIFY              = $0914;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFY_RANGE        = $0524;                                // Databytes = EventID with Mask
  MTI_PRODUCER_IDENTIFIED_UNKNOWN    = $0547;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_SET        = $0544;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_CLEAR      = $0545;                                // Databytes = EventID
  MTI_PRODUCER_IDENTIFIED_RESERVED   = $0546;                                // Databytes = EventID
  MTI_EVENTS_IDENTIFY_DEST           = $0968;                                // Databytes = Destination Alias
  MTI_EVENTS_IDENTIFY                = $0970;                                //
  MTI_EVENT_LEARN                    = $0594;                                // Databytes = EventID
  MTI_PC_EVENT_REPORT                = $05B4;                                // Databytes = EventID  (Infamouse PCER)

  MTI_SIMPLE_NODE_INFO_REQUEST       = $0DE8;                                // Databytes = Destination Alias
  MTI_SIMPLE_NODE_INFO_REPLY         = $0A08;                                // Databytes = Destination Alias, ACDI Data

  MTI_TRACTION_SIMPLE_TRAIN_INFO_REQUEST = $0DA8;                                // Databytes = Destination Alias
  MTI_TRACTION_SIMPLE_TRAIN_INFO_REPLY   = $09C8;                                // Databytes = Destination Alias, ACDI Data

  MTI_TRACTION_REQUEST               = $05EB;                                // Databyte = depends
  MTI_TRACTION_REPLY                 = $01E9;                                // Databyte = depends

  MTI_REMOTE_BUTTON_REQUEST          = $0948;
  MTI_REMOTE_BUTTON_REPLY            = $0949;

  MTI_STREAM_INIT_REQUEST            = $0CC8;
  MTI_STREAM_INIT_REPLY              = $0868;
  MTI_STREAM_SEND                    = $1F88;
  MTI_STREAM_PROCEED                 = $0888;
  MTI_STREAM_COMPLETE                = $08A8;

  MTI_DATAGRAM                       = $1C48;
  MTI_DATAGRAM_OK_REPLY              = $0A28;                                // Databytes = Destination Alias
  MTI_DATAGRAM_REJECTED_REPLY        = $0A48;                                // Databytes = Destination Alias, Error Code

  DATAGRAM_OK_ACK_REPLY_PENDING      = $80;

  STREAM_REPLY_CONTENT_TYPE                               = $01;                // LSB = 1 = first 6 bytes in data are UID of data type in stream
  STREAM_REPLY_UNEXPECTED_ERROR                           = $02;                // Bit 2 = 1 the Stream was Rejected, Out of order, or other "should not happen" error
  STREAM_REPLY_PERMANENT_ERROR                            = $40;                // Bit 6 = 1 = if STREAM_REPLY_ACCEPT = 1 then this is the error type where 1 = permanent
  STREAM_REPLY_ACCEPT                                     = $80;                // MSB = 1 = Accept

  STREAM_REPLY_ERROR_LOGGED                               = $01;                // Error was logged
  STREAM_REPLY_INVALID_REQUEST                            = $20;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_SOURCE_NOT_PERMITTED                       = $40;                // if Error is permanent then these are the possible reasons
  STREAM_REPLY_STREAM_NOT_ACCEPTED                        = $80;                // if Error is permanent then these are the possible reasons

  MASK_SOURCE_ALIAS                  = $00000FFF;

 PSI_SIMPLE                        = $800000;
 PSI_DATAGRAM                      = $400000;
 PSI_STREAM                        = $200000;
 PSI_MEMORY_CONFIGURATION          = $100000;

 PSI_RESERVATION                   = $080000;
 PSI_EVENT_EXCHANGE                = $040000;
 PSI_IDENTIFICATION                = $020000;
 PSI_TEACHING_LEARNING             = $010000;

 PSI_REMOTE_BUTTON                 = $008000;
 PSI_ABBREVIATED_DEFAULT_CDI       = $004000;
 PSI_DISPLAY                       = $002000;
 PSI_SIMPLE_NODE_INFORMATION       = $001000;

 PSI_CONFIGURATION_DESCRIPTION_INFO = $000800;
 PSI_TRAIN_CONTROL                 = $000400;
 PSI_FUNCTION_DESCRIPTION          = $000200;
 PSI_RESERVED_0                    = $000100;

 PSI_RESERVED_1                    = $000080;
 PSI_FUNCTION_CONFIGURATION        = $000040;
 PSI_FIRMWARE_UPGRADE              = $000020;
 PSI_FIRMWARE_UPGRADE_ACTIVE       = $000010;// Masks out just the Source Alias Address

  // Byte 3
  PIP_SIMPLENODE                     = $80;
  PIP_DATAGRAM                       = $40;
  PIP_STREAM                         = $20;
  PIP_MEMORY_CONFIG                  = $10;
  PIP_RESERVATION                    = $08;
  PIP_EVENT_EXCHANGE                 = $04;
  PIP_IDENTIFCIATION                 = $02;
  PIP_TEACH_LEARN                    = $01;

  // Byte 2
  PIP_REMOTE_BUTTON                  = $80;
  PIP_ABBREVIATED_CDI                = $40;
  PIP_DISPLAY                        = $20;
  PIP_SIMPLE_NODE_INFO               = $10;
  PIP_CDI                            = $08;
  PIP_TRACTION                       = $04;
  PIP_FDI                            = $02;
// PIP_DCC_COMMAND_STATION            = $01; depreciated

  // Byte 1
  PIP_SIMPLE_TRAIN_NODE_INFO         = $80;
  PIP_FUNCTION_CONFIGURATION         = $40;
  PIP_FIRMWARE_UPGRADE               = $20;
  PIP_FIRMWARE_UPGRADE_ACTIVE        = $10;


  STR_PIP_PIP                        = 'Protocol Identification Protocol';
  STR_PIP_DATAGRAM                   = 'Datagram Protocol';
  STR_PIP_STREAM                     = 'Stream Protocol';
  STR_PIP_MEMORY_CONFIG              = 'Memory Configuration Protocol';
  STR_PIP_RESERVATION                = 'Reservation Protocol';
  STR_PIP_EVENT_EXCHANGE             = 'Event Exchange Protocol';
  STR_PIP_IDENTIFCIATION             = 'Identification Protocol';
  STR_PIP_TEACH_LEARN                = 'Teach/Learn Protocol';
  STR_PIP_REMOTE_BUTTON              = 'Remote Button Protocol';
  STR_PIP_ABBREVIATED_CDI            = 'Abbreviated CDI Protocol';
  STR_PIP_DISPLAY                    = 'Display Protocol';
  STR_PIP_SIMPLE_NODE_ID             = 'Simple Node ID (SNII/SNIP) Protocol';
  STR_PIP_CDI                        = 'Configuration Description Information (CDI) Protocol';
  STR_PIP_TRACTION                   = 'Traction Protocol';
  STR_PIP_FDI                        = 'Function Description Information (FDI) Protocol';
  STR_PIP_FIRMWARE_UPGRADE           = 'Firmware Upgrade Protocol';
  STR_PIP_FIRMWARE_UPGRADE_ACTIVE    = 'Firmware Upgrade Active Protocol';

const
  ADDRESS_SPACE_CONFIG_DEFINITION_INFO   = $FF;
  ADDRESS_SPACE_ALL                      = $FE;
  ADDRESS_SPACE_CONFIG_MEMORY            = $FD;
  ADDRESS_SPACE_ACDI_MFG                 = $FC;
  ADDRESS_SPACE_ACDI_USER                = $FB;
  ADDRESS_SPACE_FUNCTION_DEFINITION_INFO = $FA;
  ADDRESS_SPACE_FUNCTION_MEMORY          = $F9;

  MCP_OPERATION                       = $80;                                    // MemoryConfigurationProtocol - Operation Mask
  MCP_OPERATION_REPLY                 = $80;


  MCP_READ                            = $40;                                    // MemoryConfigurationProtocol - Read Memory Mask
  MCP_READ_CONFIGURATION              = $41;
  MCP_READ_ALL                        = $42;
  MCP_READ_CDI                        = $43;

  MCP_READ_STREAM                     = $60;
  MCP_READ_REPLY                      = $50;                                    // MemoryConfigurationProtocol - Read Reply Mask [Does not include the Address Space Mask "or" it with the the Address space masks below]
  MCP_READ_REPLY_CONFIG               = $51;
  MCP_READ_REPLY_ALL                  = $52;
  MCP_READ_REPLY_CDI                  = $53;
  MCP_READ_REPLY_FAILURE              = $58;
  MCP_READ_REPLY_FAILURE_CONFIG       = $59;
  MCP_READ_REPLY_FAILURE_ALL          = $5A;
  MCP_READ_REPLY_FAILURE_CDI          = $5B;

  MCP_WRITE                           = $00;                                    // MemoryConfigurationProtocol - Write Memory Mask
  MCP_WRITE_CONFIGURATION             = $01;
  MCP_WRITE_ALL                       = $02;
  MCP_WRITE_CDI                       = $03;

  MCP_WRITE_UNDER_MASK                = $08;                                    // MemoryConfigurationProtocol - Write Memory Mask
  MCP_WRITE_CONFIGURATION_UNDER_MASK  = $09;
  MCP_WRITE_ALL_UNDER_MASK            = $0A;
  MCP_WRITE_CDI_UNDER_MASK            = $0B;

  MCP_WRITE_REPLY                     = $10;                                    // MemoryConfigurationProtocol - Read Reply Mask [Does not include the Address Space Mask "or" it with the the Address space masks below]
  MCP_WRITE_REPLY_CONFIG              = $11;
  MCP_WRITE_REPLY_ALL                 = $12;
  MCP_WRITE_REPLY_CDI                 = $13;
  MCP_WRITE_REPLY_FAILURE             = $18;
  MCP_WRITE_REPLY_FAILURE_CONFIG      = $19;
  MCP_WRITE_REPLY_FAILURE_ALL         = $1A;
  MCP_WRITE_REPLY_FAILURE_CDI         = $1B;


  MCP_WRITE_STREAM                    = $20;

  MCP_READ_STREAM_REPLY               = $70;

  MCP_CDI                             = $03;                                    // Address space = CDI ($FF) access Mask
  MCP_ALL                             = $02;                                    // Address space = All ($FE) access Mask
  MCP_CONFIGURATION                   = $01;                                    // Address space = Basic Configuration ($FD) access Mask
  MCP_NONE                            = $00;                                    // Use the optional {Space} byte in the datagram to defin the address space

  MCP_OP_GET_CONFIG_OPTIONS          = $80;                                     // MemoryConfigurationProtocol Operation - Get Configuration Options
  MCP_OP_GET_CONFIG_OPTIONS_REPLY    = $82;                                     // MemoryConfigurationProtocol Operation - Get Configuration Reply
  MCP_OP_GET_ADD_SPACE_INFO          = $84;                                     // MemoryConfigurationProtocol Operation - Get Add Space Info
  MCP_OP_GET_ADD_SPACE_INFO_PRESENT_REPLY     = $87;
  MCP_OP_GET_ADD_SPACE_INFO_NOT_PRESENT_REPLY = $86;                            // MemoryConfigurationProtocol Operation - Get Add Space Info Reply
  MCP_OP_LOCK                        = $88;                                     // MemoryConfigurationProtocol Operation - Lock Node
  MCP_OP_LOCK_REPLY                  = $8A;                                     // MemoryConfigurationProtocol Operation - Lock Node Reply
  MCP_OP_GET_UNIQUEID                = $8C;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key
  MCP_OP_GET_UNIQUEID_REPLY          = $8D;                                     // MemoryConfigurationProtocol Operation - Get Unique ID Key Reply

  MCP_OP_FREEZE                      = $A1;                                     // MemoryConfigurationProtocol Operation - Freeze Node
  MC_OP_UNFREEZE                     = $A0;
  MCP_OP_UPDATE_COMPLETE             = $A8;                                     // MemoryConfigurationProtocol Operation -
  MPC_OP_RESET_REBOOT                = $A9;
  MP_OP_REINITIALIZE_FACTORY_RESET   = $AA;


  MSI_CDI                            = $FF;                                     // MemorySpaceIdentifier - Access the Configuration Definition Infomation (CDI)
  MSI_ALL                            = $FE;                                     // MemorySpaceIdentifier - Access All memory (define all in the application)
  MSI_CONFIG                         = $FD;                                     // MemorySpaceIdentifier - Access basic configuration memory that feeds into the CDI
  MSI_ACDI_MFG                       = $FC;                                     // MemorySpaceIdentifier - Access the ACDI Manfacturers Info
  MSI_ACDI_USER                      = $FB;                                     // MemorySpaceIdentifier - Access the ACDI User definable Info
  MSI_TRACTION_FDI                   = $FA;                                     // MemorySpaceIdentifier - Access the Traction Functions definable Info
  MSI_TRACTION_FUNCTION_CONFIG       = $F9;                                     // MemorySpaceIdentifier = Access the Traction Function State Information

  MCO_WRITE_UNDER_MASK               = $8000;                                   // MemoryConfigurationOptions - Write under mask supported
  MCO_UNALIGNED_READS                = $4000;                                   // MemoryConfigurationOptions - Unaligned memory Reads supported
  MCO_UNALIGNED_WRITES               = $2000;                                   // MemoryConfigurationOptions - Unaligned memory Writes supported
  MCO_ACDI_MFG_READS                 = $0800;                                   // MemoryConfigurationOptions - Address Space 0xFC supported (ACDI Manufacturer Area) for reads
  MCO_ACDI_USER_READS                = $0400;                                   // MemoryConfigurationOptions - Address Space 0xFB supported (ACDI User Defined Area) for reads
  MCO_ACDI_USER_WRITES               = $0200;                                   // MemoryConfigurationOptions - Address Space 0xFB supported (ACDI User Defined Area) for writes
  MCO_RESERVED                       = $1FFF;

  MCWL_ONE_BYTE                      = $80;                                     // MemoryConfigurationWriteLength - 1 Byte Write Supported
  MCWL_TWO_BYTE                      = $40;                                     // MemoryConfigurationWriteLength - 2 Byte Write Supported
  MCWL_FOUR_BYTE                     = $20;                                     // MemoryConfigurationWriteLength - 4 Byte Write Supported
  MCWL_64_BYTE                       = $10;                                     // MemoryConfigurationWriteLength - 64 Byte (exactly) Write Supported
  MCWL_ARBITRARY_BYTE                = $02;                                     // MemoryConfigurationWriteLength - Any Number of Byte Write Supported
  MCWL_STREAM_WRITE_SUPPORTED        = $01;                                     // MemoryConfigurationWriteLength - Stream Write Supported
  MCWL_RESERVED                      = $0C;

  TRACTION_FORWARED_LISTENER_FLAG     = $80;

  TRACTION_SET_SPEED_DIR              = $00;
  TRACTION_SET_SPEED_DIR_LISTENER_FORWARDED = TRACTION_SET_SPEED_DIR or TRACTION_FORWARED_LISTENER_FLAG;
  TRACTION_SET_FUNCTION               = $01;
  TRACTION_SET_FUNCTION_LISTENER_FORWARDED = TRACTION_SET_FUNCTION or TRACTION_FORWARED_LISTENER_FLAG;
  TRACTION_SET_E_STOP                 = $02;
  TRACTION_SET_E_STOP_LISTENER_FORWARDED = TRACTION_SET_E_STOP or TRACTION_FORWARED_LISTENER_FLAG;
  TRACTION_QUERY_SPEED                = $10;
  TRACTION_QUERY_FUNCTION             = $11;

  TRACTION_SPEED_STATUS_E_STOP        = $01;


  TRACTION_CONTROLLER_CONFIG                 = $20;
  TRACTION_CONTROLLER_CONFIG_REPLY           = $20;
  TRACTION_CONTROLLER_CONFIG_ASSIGN          = $01;
  TRACTION_CONTROLLER_CONFIG_RELEASE         = $02;
  TRACTION_CONTROLLER_CONFIG_QUERY           = $03;
  TRACTION_CONTROLLER_CONFIG_CHANGED_NOTIFY  = $04;

  TRACTION_CONTROLLER_CONFIG_REPLY_OK = $00;
//  TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_ASSIGNED_CONTROLLER = $01;  // Bit 0     Depreciated... not a good idea
  TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN = $02; // Bit 1                    Depreciated... not a good idea

  TRACTION_LISTENER_CONFIG            = $30;
  TRACTION_LISTENER_CONFIG_ATTACH     = $01;
  TRACTION_LISTENER_CONFIG_DETACH     = $02;
  TRACTION_LISTENER_CONFIG_QUERY      = $03;

  TRACTION_LISTENER_FLAG_ALIAS_VALID  = $0001;   // Depreciated, Aliases are not portable across ethernet bridges.  Alias parameter no longer used
  TRACTION_LISTENER_FLAG_REVERSE_DIR  = $0002;   // Reverse Direction
  TRACTION_LISTENER_FLAG_LINK_F0      = $0004;   // Pass forward the F0 function
  TRACTION_LISTENER_FLAG_LINK_FN      = $0008;   // Pass forward all functions
  TRACTION_LISTENER_FLAG_HIDDEN       = $0080;   // Listener is hidden (not shown in UI).  I think is should be defined as "non-Train" Listener

  TRACTION_LISTENER_ATTACH_REPLY_OK               = $0000;
  TRACTION_LISTENER_ATTACH_REPLY_NOTFOUND         = $1030;
  TRACTION_LISTENER_ATTACH_REPLY_PERMANTENT_ERROR = $1032;

  TRACTION_MANAGE                     = $40;
  TRACTION_MANAGE_RESERVE             = $01;
  TRACTION_MANAGE_RELEASE             = $02;
  TRACTION_MANAGE_HEARTBEAT           = $03;
  TRACTION_MANAGE_NO_OP                = $03;

  TRACTION_MANAGE_RESERVE_REPLY_OK   = $00;    // Failed is not 0
  TRACTION_MANAGE_RESERVE_REPLY_FAIL = $01;
  TRACTION_MANANGE_HEARTBEAT_REPLY   = $03;

type
  TLccDccSpeedStep = (ldssDefault, ldss14, ldss28, ldss128);
  TLccMarklinProtocolVersion = (lmpvDefault, lmvpVer1, lmvpVer2, lmvpVer2ExtFunction);

const


  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_MASK            = $18; // 0001 1000

  // Bit 3-4 options
  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY             = $00; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY        = $08; // b0000 1000
  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_RESERVED_1      = $10; // b0001 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_RESERVED_2      = $18; // b0001 1000

  // Bit 2 options
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT    = $00; // b0000 0000 valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY only
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG       = $04; // b0000 0100 valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY only
  TRACTION_SEARCH_TRACK_PROTOCOL_NON_MARKLIN            = $00; // b0000 0000 valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY only
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN                = $04; // b0000 0100 valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY only

  TRACTION_SEARCH_PROTCOL_DETAILS_MASK                  = $03; // b0000 0011 bottom 2 bits

  // Bit 0-1
  // valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY and TRACTION_SEARCH_TRACK_PROTOCOL_NON_MARKLIN only
  TRACTION_SEARCH_TRACK_PROTOCOL_ALL                    = $00; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_NATIVE_OPENLCB         = $01; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_MFX_M4                 = $02; // b0000 0010
  TRACTION_SEARCH_TRACK_PROTOCOL_RESERVED_1             = $03; // b0000 0011

  // valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_ANY and TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN only
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_ANY            = $00; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_1      = $01; // b0000 0001
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2      = $02; // b0000 0010
  TRACTION_SEARCH_TRACK_PROTOCOL_MARKLIN_VERSION_2_F8   = $03; // b0000 0011

  // valid with TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP     = $00; // b0000 0000
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP      = $01; // b0000 0001
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP      = $02; // b0000 0010
  TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP     = $03; // b0000 0011

  // Bit 7
  TRACTION_SEARCH_ALLOCATE_FORCE                        = $80; // b1000 0000
  TRACTION_SEARCH_ALLOCATE_EXISTING_ONLY                = $00; // b0000 0000

  // Bit 6
  TRACTION_SEARCH_TYPE_EXACT_MATCH                      = $40; // b0100 0000
  TRACTION_SEARCH_TYPE_ALL_MATCH                        = $00; // b0000 0000

  // Bit 5
  TRACTION_SEARCH_TARGET_ADDRESS_MATCH                  = $20; // b0010 0000
  TRACTION_SEARCH_TARGET_ANY_MATCH                      = $00; // b0000 0000

  MAX_CONFIG_MEM_READWRITE_SIZE = 64;

  // Permanent error modifier
  ERROR_CODE_PERMANENT                       = $1000; // major code: Permanent error.
  ERROR_CODE_PERMANENT_SOURCE_NOT_PERMITED   = $1020; // major code: Source not permitted.
  ERROR_CODE_PERMANENT_NOT_FOUND             = $1030;
  ERROR_CODE_PERMANENT_NOT_IMPLEMENTED       = $1040; // major code: Not implemented.
  ERROR_CODE_PERMANENT_INVALID_ARGUMENTS     = $1080; // major code: Invalid arguments. Some of the values sent in the message fall outside
                                                      //             of the expected range, or do not match the expectations of the receiving node.
  ERROR_CODE_PERMANENT_SUBCOMMAND_UNKNOWN    = $1041; // complete code: Not implemented, subcommand is unknown.
  ERROR_CODE_PERMANENT_TYPE_UNKNOWN          = $1042; // complete code: Not implemented, Datagram-type, Stream-type, or command is unknown.
  ERROR_CODE_PERMANENT_MTI_TRANSPORT_UNKNOWN = $1043; // complete code: Not implemented, unknown MTI, or Transport protocol (datagrams/streams) is not supported.
  ERROR_CODE_PERMANENT_COUNT_OUT_OF_RANGE    = $1044;

  // Temporary error modifier
  ERROR_CODE_TEMPORARY                       = $2000; // major code: Temporary error, not further not specified
  ERROR_CODE_TEMPORARY_TIMEOUT               = $2010; // major code: Timeout, the expected message or message-part did not arrive in time.
  ERROR_CODE_TEMPORARY_BUFFER_UNAVAILABLE    = $2020; // major code: Buffer unavailable or destination node busy.
  ERROR_CODE_TEMPORARY_NOT_EXPECTED          = $2040; // major code: Not expected, Out of order. An inconsistency was found in the message or frame
                                                      //             sequence received, the arrived message is unexpected or does not match the state of the receiving node.
  ERROR_CODE_TEMPORARY_TRANSFER_ERROR        = $2080; // major code: Transfer error. The message or received message was ill-formed, failed checksum, or is
                                                      //             otherwise uninterpretable. On CAN, this is handled by the hardware

  ERROR_CODE_TEMPORARY_TIMEOUT_OF_END_FRAME = $2011;  // complete code: Time-out, waiting for End-frame.
  ERROR_CODE_OUT_OF_ORDER_NO_START_FRAME    = $2041;  // complete code: Out of Order, Middle- or End-frame without a Start-frame.
  ERROR_CODE_OUT_OF_ORDER_START_BEFORE_END  = $2042;  // complete code: Out of Order, Start-frame before finishing previous message.

  ERROR_CODE_ACCEPT                         = $8000;  // major code: Accept, no error. This value shall not be used in reject messages.


  DATAGRAM_PROTOCOL_LOGREQUEST             = $01;
  DATAGRAM_PROTOCOL_LOGREPLY               = $02;
  DATAGRAM_PROTOCOL_CONFIGURATION          = $20;
  DATAGRAM_PROTOCOL_REMOTEBUTTON           = $21;
  DATAGRAM_PROTOCOL_DISPLAY                = $28;
  DATAGRAM_PROTOCOL_TRAINCONTROL           = $30;
  DATAGRAM_PROTOCOL_TWOBYTE                = $E0;
  DATAGRAM_PROTOCOL_SIXBYTE                = $F0;

  ACK_REPLY_PENDING_MASK                   = $80;
  ACK_REPLY_TIMEOUT_MASK                   = $0F;
  ACK_REPLY_RESERVED_MASK                  = $70;

  ACDI_MFG_SIZE_VERSION                    = 1;
  ACDI_MFG_SIZE_MANUFACTURER               = 41;
  ACDI_MFG_SIZE_MODEL                      = 41;
  ACDI_MFG_SIZE_HARDWARE_VERSION           = 21;
  ACDI_MFG_SIZE_SOFTWARE_VERSION           = 21;
  ACDI_MFG_SIZE                            = ACDI_MFG_SIZE_VERSION + ACDI_MFG_SIZE_MANUFACTURER + ACDI_MFG_SIZE_MODEL + ACDI_MFG_SIZE_HARDWARE_VERSION + ACDI_MFG_SIZE_SOFTWARE_VERSION;

  ACDI_USER_SIZE_VERSION                   = 1;
  ACDI_USER_SIZE_NAME                      = 63;
  ACDI_USER_SIZE_DESCRIPTION               = 64;
  ACDI_USER_SIZE                           = ACDI_USER_SIZE_VERSION + ACDI_USER_SIZE_NAME + ACDI_USER_SIZE_DESCRIPTION;

  ACDI_MFG_OFFSET_VERSION                   = 0;
  ACDI_MFG_OFFSET_MANUFACTURER              = ACDI_MFG_SIZE_VERSION;
  ACDI_MFG_OFFSET_MODEL                     = ACDI_MFG_OFFSET_MANUFACTURER + ACDI_MFG_SIZE_MANUFACTURER;
  ACDI_MFG_OFFSET_HARDWARE_VERSION          = ACDI_MFG_OFFSET_MODEL +  ACDI_MFG_SIZE_MODEL;
  ACDI_MFG_OFFSET_SOFTWARE_VERSION          = ACDI_MFG_OFFSET_HARDWARE_VERSION + ACDI_MFG_SIZE_SOFTWARE_VERSION;

  ACDI_USER_OFFSET_VERSION                  = 0;
  ACDI_USER_OFFSET_NAME                     =  ACDI_USER_SIZE_VERSION;
  ACDI_USER_OFFSET_DESCRIPTION              =  ACDI_USER_OFFSET_NAME + ACDI_USER_SIZE_NAME;

var
  NULL_NODE_ID: TNodeID;

  NULL_EVENT_ID              : TEventID;
  EVENT_EMERGENCY_OFF        : TEventID;
  EVENT_EMERGENCY_OFF_CLEAR  : TEventID;
  EVENT_EMERGENCY_STOP       : TEventID;
  EVENT_EMERGENCY_STOP_CLEAR : TEventID;
  EVENT_NEW_LOG_ENTRY        : TEventID;
  EVENT_POWERSUPPLY_BROWNOUT_NODE     : TEventID;
  EVENT_POWERSUPPLY_BROWNOUT_STANDARD : TEventID;
  EVENT_IDENT_BUTTON_PRESSED : TEventID;
  EVENT_DUPLICATE_ID_DETECTED: TEventID;
  EVENT_IS_TRAIN             : TEventID;
 // EVENT_IS_PROXY             : TEventID;        depreciated
  EVENT_DELIVERS_CLOCK       : TEventID;

  // TCP Header:
  //  |   Flags 16 Bits   |       Size 24 Bits          |             Originating/Gateway Node 48 Bits              |             Message Capture Time 48 Bits                  |
  //  | Byte 00 | Byte 01 | Byte 02 | Byte 03 | Byte 04 | Byte 05 | Byte 06 | Byte 07 | Byte 08 | Byte 09 | Byte 10 | Byte 11 | Byte 12 | Byte 13 | Byte 14 | Byte 15 | Byte 16 |
  //         2 bytes      +          3 bytes            +                       6 bytes                             +                      6 bytes

  // TCP Message:
  //  |  MTI 16 bits      |               Source Node ID 48 bits                      |                Dest Node ID 48 bits {optional}            | Data Payload
  //  | Byte 17 | Byte 18 | Byte 19 | Byte 20 | Byte 21 | Byte 22 | Byte 23 | Byte 24 | Byte 25 | Byte 26 | Byte 27 | Byte 28 | Byte 29 | Byte 30 | Byte 31 | Byte 32 | Byte ... |
  //         2 bytes      +                 6 bytes                             +                      6 bytes

  // OR

  // TCP Message:
  //  |    MTI 16 bits    |               Source Node ID 48 bits                      |                        Data Payload
  //  | Byte 17 | Byte 18 | Byte 19 | Byte 20 | Byte 21 | Byte 22 | Byte 23 | Byte 24 | Byte 25 | Byte 26 | Byte ...|
  //         2 bytes      +                 6 bytes

  const
    OPSTACK_TCP_FLAG_LCC_MESSAGE = $8000;        //  Link Message = 0
    OPSTACK_TCP_FLAG_CHAINING     = $4000;
    // $2000, $1000 reserved
    OPSTACK_TCP_FLAG_MULTI_PART   = $0C00;        // $0000 = Single part, $0020 = First part, $0030 = center part, $0040 = last part
    // Rest are reserved

  const
    LEN_LCC_TCP_MESSAGE_DATA_MAX = 253;
    LEN_LCC_TCP_MESSAGE_PREAMBLE_MAX = 14;      // 2 - MTI, 6 - Source ID, 6 - Dest ID
    LEN_LCC_TCP_MESSAGE_PREAMBLE_MIN = 8;       // 2 - MTI, 6 - Source ID
    LEN_TCP_HEADER_MAX = 17;                    // Single Header... may contain nested headers  2 - Flags; 3 - Length of message; 6 - Source NodeID; 6 - Capture Time
    OFFSET_HEADER_TCP_FLAGS = 0;
    OFFSET_HEADER_TCP_MESSAGE_LENGTH = 2;
    OFFSET_HEADER_TCP_SOURCEID = 5;
    OFFSET_HEADER_TCP_CAPTURETIME = 11;
    LEN_HEADER_CONTRIBUTION_TO_SIZE_FIELD_MAX = 12;

    LEN_LCC_TCP_FRAME_MAX = LEN_LCC_TCP_MESSAGE_DATA_MAX + LEN_LCC_TCP_MESSAGE_PREAMBLE_MAX;    // Max frame sizse for a TCP message with Header and all included
    LEN_TCP_MESSAGE_ONLY_MAX = LEN_LCC_TCP_FRAME_MAX - LEN_TCP_HEADER_MAX;         // Max frame size for just the Lcc Message itself

var
  {$IFDEF PYTHON_SCRIPT_COMPATIBLE}
  MAX_ALLOWED_BUFFERS: Integer = 1;
  {$ELSE}
  MAX_ALLOWED_BUFFERS: Integer = 100;
  {$ENDIF}

implementation

initialization

  // Necessary because of bug in Smart Mobile Studio Compiler.... 12/18/2019
  NULL_NODE_ID[0]               := 0;
  NULL_NODE_ID[1]               := 0;

  NULL_EVENT_ID[0]               := 0;
  NULL_EVENT_ID[1]               := 0;
  NULL_EVENT_ID[2]               := 0;
  NULL_EVENT_ID[3]               := 0;
  NULL_EVENT_ID[4]               := 0;
  NULL_EVENT_ID[5]               := 0;
  NULL_EVENT_ID[6]               := 0;
  NULL_EVENT_ID[7]               := 0;


  EVENT_EMERGENCY_OFF[0]        := $01;
  EVENT_EMERGENCY_OFF[1]        := $00;
  EVENT_EMERGENCY_OFF[2]        := $00;
  EVENT_EMERGENCY_OFF[3]        := $00;
  EVENT_EMERGENCY_OFF[4]        := $00;
  EVENT_EMERGENCY_OFF[5]        := $00;
  EVENT_EMERGENCY_OFF[6]        := $FF;
  EVENT_EMERGENCY_OFF[7]        := $FF;

  EVENT_EMERGENCY_OFF_CLEAR[0]  := $01;
  EVENT_EMERGENCY_OFF_CLEAR[1]  := $00;
  EVENT_EMERGENCY_OFF_CLEAR[2]  := $00;
  EVENT_EMERGENCY_OFF_CLEAR[3]  := $00;
  EVENT_EMERGENCY_OFF_CLEAR[4]  := $00;
  EVENT_EMERGENCY_OFF_CLEAR[5]  := $00;
  EVENT_EMERGENCY_OFF_CLEAR[6]  := $FF;
  EVENT_EMERGENCY_OFF_CLEAR[7]  := $FE;

  EVENT_EMERGENCY_STOP[0]        := $01;
  EVENT_EMERGENCY_STOP[1]        := $00;
  EVENT_EMERGENCY_STOP[2]        := $00;
  EVENT_EMERGENCY_STOP[3]        := $00;
  EVENT_EMERGENCY_STOP[4]        := $00;
  EVENT_EMERGENCY_STOP[5]        := $00;
  EVENT_EMERGENCY_STOP[6]        := $FF;
  EVENT_EMERGENCY_STOP[7]        := $FD;

  EVENT_EMERGENCY_STOP_CLEAR[0]  := $01;
  EVENT_EMERGENCY_STOP_CLEAR[1]  := $00;
  EVENT_EMERGENCY_STOP_CLEAR[2]  := $00;
  EVENT_EMERGENCY_STOP_CLEAR[3]  := $00;
  EVENT_EMERGENCY_STOP_CLEAR[4]  := $00;
  EVENT_EMERGENCY_STOP_CLEAR[5]  := $00;
  EVENT_EMERGENCY_STOP_CLEAR[6]  := $FF;
  EVENT_EMERGENCY_STOP_CLEAR[7]  := $FC;

  EVENT_NEW_LOG_ENTRY[0]         := $01;
  EVENT_NEW_LOG_ENTRY[1]         := $00;
  EVENT_NEW_LOG_ENTRY[2]         := $00;
  EVENT_NEW_LOG_ENTRY[3]         := $00;
  EVENT_NEW_LOG_ENTRY[4]         := $00;
  EVENT_NEW_LOG_ENTRY[5]         := $00;
  EVENT_NEW_LOG_ENTRY[6]         := $FF;
  EVENT_NEW_LOG_ENTRY[7]         := $F8;

  EVENT_POWERSUPPLY_BROWNOUT_NODE[0]  := $01;
  EVENT_POWERSUPPLY_BROWNOUT_NODE[1]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_NODE[2]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_NODE[3]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_NODE[4]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_NODE[5]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_NODE[6]  := $FF;
  EVENT_POWERSUPPLY_BROWNOUT_NODE[7]  := $F1;

  EVENT_POWERSUPPLY_BROWNOUT_STANDARD[0]  := $01;
  EVENT_POWERSUPPLY_BROWNOUT_STANDARD[1]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_STANDARD[2]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_STANDARD[3]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_STANDARD[4]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_STANDARD[5]  := $00;
  EVENT_POWERSUPPLY_BROWNOUT_STANDARD[6]  := $FF;
  EVENT_POWERSUPPLY_BROWNOUT_STANDARD[7]  := $F0;

  EVENT_IDENT_BUTTON_PRESSED[0]  := $01;
  EVENT_IDENT_BUTTON_PRESSED[1]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[2]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[3]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[4]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[5]  := $00;
  EVENT_IDENT_BUTTON_PRESSED[6]  := $FE;
  EVENT_IDENT_BUTTON_PRESSED[7]  := $00;

  EVENT_DUPLICATE_ID_DETECTED[0] := $01;
  EVENT_DUPLICATE_ID_DETECTED[1] := $10;
  EVENT_DUPLICATE_ID_DETECTED[2] := $00;
  EVENT_DUPLICATE_ID_DETECTED[3] := $00;
  EVENT_DUPLICATE_ID_DETECTED[4] := $00;
  EVENT_DUPLICATE_ID_DETECTED[5] := $00;
  EVENT_DUPLICATE_ID_DETECTED[6] := $02;
  EVENT_DUPLICATE_ID_DETECTED[7] := $01;

  EVENT_IS_TRAIN[0]              := $01;
  EVENT_IS_TRAIN[1]              := $01;
  EVENT_IS_TRAIN[2]              := $00;
  EVENT_IS_TRAIN[3]              := $00;
  EVENT_IS_TRAIN[4]              := $00;
  EVENT_IS_TRAIN[5]              := $00;
  EVENT_IS_TRAIN[6]              := $03;
  EVENT_IS_TRAIN[7]              := $03;

  // EVENT_IS_PROXY Depreciated

  EVENT_DELIVERS_CLOCK[0]        := $01;
  EVENT_DELIVERS_CLOCK[1]        := $01;
  EVENT_DELIVERS_CLOCK[2]        := $00;
  EVENT_DELIVERS_CLOCK[3]        := $00;
  EVENT_DELIVERS_CLOCK[4]        := $00;
  EVENT_DELIVERS_CLOCK[5]        := $00;
  EVENT_DELIVERS_CLOCK[6]        := $05;
  EVENT_DELIVERS_CLOCK[7]        := $01;


end.

