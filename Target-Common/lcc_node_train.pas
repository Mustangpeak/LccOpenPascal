unit lcc_node_train;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface


{.$DEFINE DISABLE_STATE_TIMEOUTS_FOR_DEBUG}

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
  lcc_utilities,
  lcc_defines,
  lcc_node_messages,
  lcc_alias_server,
  lcc_math_float16,
  lcc_node;

const
  MAX_NMRA_DCC_DATA = 5;     // Number of Bytes in a valid NMRA DCC Message
  NMRA_LONGADDRESS_MASK_BYTE         = $C0;
  NMRA_LONGADDRESS_MASK_WORD         = $C000;
  MAX_DCC_FUNCTIONS                  = 29;

  // Mappings of the train speed step to the encoded Byte
  // 128 step is a direct mapping with the high bit the direction
  const
  _28_STEP_TABLE: array[0..28] of Byte = (
    {$IFNDEF LCC_FPC}
    $00,    // Stop
    $02,    // Step 1
    $12,    // Step 2
    $03,    // Step 3
    $13,    // Step 4
    $04,    // Step 5
    $14,    // Step 6
    $05,    // Step 7
    $15,    // Step 8
    $06,    // Step 9
    $16,    // Step 10
    $07,    // Step 11
    $17,    // Step 12
    $08,    // Step 13
    $18,    // Step 14
    $09,    // Step 15
    $19,    // Step 16
    $0A,    // Step 17
    $1A,    // Step 18
    $0B,    // Step 19
    $1B,    // Step 20
    $0C,    // Step 21
    $1C,    // Step 22
    $0D,    // Step 23
    $1D,    // Step 24
    $0E,    // Step 25
    $1E,    // Step 26
    $0F,    // Step 27
    $0F     // Step 28
    {$ELSE}
    %00000000,    // Stop
    %00000010,    // Step 1
    %00010010,    // Step 2
    %00000011,    // Step 3
    %00010011,    // Step 4
    %00000100,    // Step 5
    %00010100,    // Step 6
    %00000101,    // Step 7
    %00010101,    // Step 8
    %00000110,    // Step 9
    %00010110,    // Step 10
    %00000111,    // Step 11
    %00010111,    // Step 12
    %00001000,    // Step 13
    %00011000,    // Step 14
    %00001001,    // Step 15
    %00011001,    // Step 16
    %00001010,    // Step 17
    %00011010,    // Step 18
    %00001011,    // Step 19
    %00011011,    // Step 20
    %00001100,    // Step 21
    %00011100,    // Step 22
    %00001101,    // Step 23
    %00011101,    // Step 24
    %00001110,    // Step 25
    %00011110,    // Step 26
    %00001111,    // Step 27
    %00011111     // Step 28
    {$ENDIF}
  );

  _14_STEP_TABLE: array[0..14] of Byte = (
    {$IFNDEF LCC_FPC}
    $00,    // Stop
    $02,    // Step 1
    $03,    // Step 3
    $04,    // Step 5
    $05,    // Step 7
    $06,    // Step 9
    $07,    // Step 11
    $08,    // Step 13
    $09,    // Step 15
    $0A,    // Step 17
    $0B,    // Step 19
    $0C,    // Step 21
    $0D,    // Step 23
    $0E,    // Step 25
    $0F     // Step 27
    {$ELSE}
    %00000000,    // Stop
    %00000010,    // Step 1
    %00000011,    // Step 3
    %00000100,    // Step 5
    %00000101,    // Step 7
    %00000110,    // Step 9
    %00000111,    // Step 11
    %00001000,    // Step 13
    %00001001,    // Step 15
    %00001010,    // Step 17
    %00001011,    // Step 19
    %00001100,    // Step 21
    %00001101,    // Step 23
    %00001110,    // Step 25
    %00001111     // Step 27
    {$ENDIF}
  );

type
    // ***************************************************************************
  // Implements the raw byte array that hold the NMRA DCC Message bytes
  // ***************************************************************************
  TDCCPacketBytesArray = array[0..MAX_NMRA_DCC_DATA-1] of Byte;
 // PDCCPacketBytesArray = ^TDCCPacketBytesArray;


  TDCCPacket = record
    PacketBytes: TDCCPacketBytesArray;       // NMRA defines at most 5 data LoadTransmitter per message packet
    Flags: Byte;                               // See the QUEUE_MESSAGE_XXXXX Flags
                                               // Bit 0 1 2         = Number of Valid data bytes in the message.
                                               // Bit 3             = Address is a Multi-Function decoder with 7 bit address (short address)
                                               // Bit 4             = Address is a Basic Accessory Decoder with 9 bit address and Extended Accessory Decoder with 11 bit address
                                               // Bit 5             = Address is a Multi-Function decoder with a 14 bit address (extended address)
                                               // Bit 6             = Address is in the NMRA Reserved range
                                               // Bit 7             = Address is special ($00 = Reset; $FF = Idle; $FE = ??? but defined in S-9.2);
  end;


const

 CDI_XML_TRAIN_NODE: string = ('<?xml version="1.0"?><cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/schema/cdi/1/1/cdi.xsd"><identification><manufacturer>Train Control Systems (TCS)</manufacturer><model>Virtual train node</model><hardwareVersion>Rev A</hardwareVersion><softwareVersion>0.0.2:4dfa68b:-d-u</softwareVersion></identification><segment space=''253''><name>Train</name><description>Configures a single train</description><string size=''63''><name>Name</name><description>Name to display on the throttle for this train.</description></string><string size=''64''><name>User Description</name><description>You can write additional notes about this train here.</description></string><int size=''1''><name>Speed Step Mode</name><description>Configures the Track Protocol options, such as speed step mode or which version of the track protocol to use.</description><default>11</default><map><relation><property>0</property><value>Default</value></relation><relation><property>10</property><value>DCC 28-step</value></relation><relation><property>11</property><value>DCC 128-step</value></relation><relation><property>5</property><value>Marklin-Motorola I</value></relation><relation><property>6</property><value>Marklin-Motorola II</value></relation><relation><property>14</property><value>DCC 28-step (forced long address)</value></relation><relation><property>15</property><value>DCC 128-step (forced long address)</value></relation></map></int><group><name>F0</name><description>F0 is permanently assigned to Light.</description><int size=''1''><name>Consist Behavior</name><description>A ''Consist'' function will react to changes from every cab. A ''Current Cab Only'' function will only react if this Loco is the current cab on the throttle.</description><default>0</default><map><relation><property>0</property><value>Consist</value></relation><relation><property>1</property><value>Current Cab Only</value></relation></map></int><int size=''1''><name>Directional</name><description>When enabled, the F0 function needs to be separately turned on for forward and reverse direction.</description><default>0</default><map><relation><property>0</property><value>Disabled</value></relation><relation><property>1</property><value>Enabled</value></relation></map></int><int size=''2''><name>MU switch</name><description>Set this according to where this unit is in a consist or train. As the direction is changed, the headlight will automatically turn off instead of shining onto a coupled train or engine.</description><default>0</default><map><relation><property>0</property><value>Solo Unit</value></relation><relation><property>1</property><value>Coupled at Rear</value></relation><relation><property>256</property><value>Coupled at Front</value></relation><relation><property>257</property><value>Middle Unit</value></relation></map></int></group><group replication=''28''><name>Functions</name><description>Defines what each function button does.</description><repname>F</repname><int size=''1''><name>Display</name><description>Defines how throttles display this function.</description><default>0</default><map><relation><property>0</property><value>Unavailable</value></relation><relation><property>1</property><value>Light</value></relation><relation><property>2</property><value>Beamer</value></relation><relation><property>3</property><value>Bell</value></relation><relation><property>4</property><value>Horn</value></relation><relation><property>5</property><value>Shunting mode</value></relation><relation><property>6</property><value>Pantograph</value></relation><relation><property>7</property><value>Smoke</value></relation><relation><property>8</property><value>Momentum off</value></relation><relation><property>9</property><value>Whistle</value></relation><relation><property>10</property><value>Sound</value></relation><relation><property>11</property><value>F</value></relation><relation><property>12</property><value>Announce</value></relation><relation><property>13</property><value>Engine</value></relation><relation><property>14</property><value>Light1</value></relation><relation><property>15</property><value>Light2</value></relation><relation><property>17</property><value>Uncouple</value></relation><relation><property>255</property><value>Unavailable_</value></relation></map></int><int size=''1''><name>Momentary</name><description>Momentary functions are automatically turned off when you release the respective button on the throttles.</description><default>0</default><map><relation><property>0</property><value>Latching</value></relation><relation><property>1</property><value>Momentary</value></relation></map></int><int size=''1''><name>Consist Behavior</name><description>A ''Consist'' function will react to changes from every cab. A ''Current Cab Only'' function will only react if this Loco is the current cab on the throttle.</description><default>0</default><map><relation><property>0</property><value>Consist</value></relation><relation><property>1</property><value>Current Cab Only</value></relation></map></int><string size=''32''><name>Description</name><description>Specifies a custom name of this function. To change back to the default function name, set this field to empty.</description></string></group><int size=''1''><name>Delete From Roster?</name><description>Selecting ''Delete'' will remove all settings of this train (like name and function mapping) from the roster. WARNING: this operation cannot be undone.</description><min>0</min><max>1</max><default>0</default><map><relation><property>0</property><value>Keep</value></relation><relation><property>1</property><value>Delete All Settings</value></relation></map></int></segment><segment space=''248'' origin=''2131755008''><name>Programming Track Operation</name><description>Use this component to read and write CVs on the programming track of the command station.</description><int size=''4''><name>Operating Mode</name><map><relation><property>0</property><value>Disabled</value></relation><relation><property>1</property><value>Direct mode</value></relation><relation><property>2</property><value>POM mode</value></relation><relation><property>3</property><value>Paged mode</value></relation><relation><property>10</property><value>Advanced mode</value></relation></map></int><int size=''4''><name>CV Number</name><description>Number of CV to read or write (1..1024).</description><min>0</min><max>1024</max><default>0</default></int><int size=''4''><name>CV Value</name><description>Set ''Operating Mode'' and ''CV Number'' first, then: hit ''Refresh'' to read the entire CV, or enter a value and hit ''Write'' to set the CV.</description><min>0</min><max>255</max><default>0</default></int><int size=''4''><name>Bit Change</name><description>Set ''Operating Mode'' and ''CV Number'' first, then: write 1064 to set the single bit whose value is 64, or 2064 to clear that bit. Write 100 to 107 to set bit index 0 to 7, or 200 to 207 to clear bit 0 to 7. Values outside of these two ranges do nothing.</description><min>100</min><max>2128</max><default>1000</default></int><string size=''24''><name>Read Bits Decomposition</name><description>Hit Refresh on this line after reading a CV value to see which bits are set.</description></string><group><name>Advanced Settings</name><int size=''4''><name>Repeat count for verify packets</name><description>How many times a direct mode bit verify packet needs to be repeated for an acknowledgement to be generated.</description><min>0</min><max>255</max><default>3</default></int><int size=''4''><name>Repeat count for reset packets after verify</name><description>How many reset packets to send after a verify.</description><min>0</min><max>255</max><default>6</default></int></group></segment></cdi>');


//  CDI_XML_TRAIN_NODE: string = ('<?xml version=''1.0'' encoding=''UTF-8''?><?xml-stylesheet type=''text/xsl'' href=''xslt/cdi.xsl''?><cdi xmlns:xsi=''http://www.w3.org/2001/XMLSchema-instance'' xsi:noNamespaceSchemaLocation=''http://openlcb.org/trunk/prototypes/xml/schema/cdi.xsd''><identification>  <manufacturer>Railstars Limited</manufacturer>  <model>Io</model>  <hardwareVersion>1.0</hardwareVersion>  <softwareVersion>1.5</softwareVersion></identification><acdi /><segment origin=''1028'' space=''253''>  <group offset=''0''>    <name>User Identification</name>    <description>Add your own descriptive name and description for this node.</description>    <string size=''32''>      <name>Node Name</name>    </string>    <string size=''64''>      <name>Node Description</name>    </string>  </group></segment><segment origin=''4'' space=''253''>  <group replication=''8''>    <name>Inputs</name>    <repname>Input</repname>    <string size=''16'' offset=''260''>      <name>Description</name>    </string>    <eventid offset=''-276'' >      <name>Activation Event</name>    </eventid>    <eventid>      <name>Inactivation Event</name>    </eventid>  </group>    <group replication=''8''>    <name>Outputs</name>    <repname>Output</repname>    <string size=''16'' offset=''260''>      <name>Description</name>    </string>    <eventid offset=''-276'' >      <name>Activation Event</name>    </eventid>    <eventid>      <name>Inactivation Event</name>    </eventid>  </group></segment><segment origin=''0'' space=''1''>  <int size=''2'' offset=''4086''>    <name>Mode Selection</name>    <description>Instruct board to reboot into normal operating or firmware upload mode.</description>    <map>      <relation><property>65535</property><value>Normal Operating Mode</value></relation>      <relation><property>0</property><value>Firmware Upload Mode</value></relation>    </map>  </int></segment></cdi>');

 { CDI_XML_TRAIN_NODE: string = (
  '<?xml version="1.0" encoding="utf-8"?>'+
  '<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
  '<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
         '<identification>'+
       	  '<manufacturer>Mustangpeak</manufacturer>'+
       	  '<model>VTN1000</model>'+
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

     }
type

  { TListenerNode }

  TListenerNode = class
  private
    FAliasID: Word;
    FHidden: Boolean;
    FLinkF0: Boolean;
    FLinkFn: Boolean;
    FNodeID: TNodeID;
    FReverseDirection: Boolean;
  public
    property AliasID: Word read FAliasID write FAliasID;
    property NodeID: TNodeID read FNodeID write FNodeID;
    property ReverseDirection: Boolean read FReverseDirection write FReverseDirection;
    property LinkF0: Boolean read FLinkF0 write FLinkF0;
    property LinkFn: Boolean read FLinkFn write FLinkFn;
    property Hidden: Boolean read FHidden write FHidden;

    procedure DecodeFlags(Flags: Byte);
    function EncodeFlags: Byte;
  end;

  { TListenerList }

  TListenerList = class(TList)
  private
    function GetListener(Index: Integer): TListenerNode;
  protected

    function FindListenerIndex(NodeID: TNodeID): Integer;
  public
    property Listener[Index: Integer]: TListenerNode read GetListener; default;

    destructor Destroy; override;

    procedure ClearListeners;
    function DeleteListener(NodeID: TNodeID): Boolean;
    function FindListener(NodeID: TNodeID): TListenerNode;
  end;

  TAttachedController = record
    NodeID: TNodeID;
    AliasID: Word;
    AttatchNotifyNodeID: TNodeID;
    AttachNotifyAliasID: Word;
  end;

  { TControllerState }

  TControllerState = class
  private
    FAttachedController: TLccAliasMappingRec;
    FRequestingController: TLccAliasMappingRec;
    FWaitingForNotifyReply: Boolean;
    FWaitingForNotifyReplyTimer: Integer;
  public
    property AttachedController: TLccAliasMappingRec read FAttachedController write FAttachedController;
    property RequestingController: TLccAliasMappingRec read FRequestingController write FRequestingController;
    property WaitingForNotifyReply: Boolean read FWaitingForNotifyReply;
    property WaitingForNotifyReplyTimer: Integer read FWaitingForNotifyReplyTimer write FWaitingForNotifyReplyTimer;

    procedure AssignController(ANodeID: TNodeId; AnAlias: Word);
    procedure AssignRequestingController(ANodeID: TNodeId; AnAlias: Word);
    procedure AcceptRequestingController;
    procedure Clear;
    procedure ClearAttachedController;
    procedure ClearRequestingController;
    function IsControllerAssigned: Boolean;
    function IsControllerEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
    procedure On_100msTimer;
    function WaitingForNotifyReplyTimerExpired: Boolean;

  end;

  { TReservationState }

  TReservationState = class
  private
    FReservedNode: TLccAliasMappingRec;
  public
    property ReservedNode: TLccAliasMappingRec read FReservedNode write FReservedNode;

    procedure AssignReservedNode(ANodeID: TNodeId; AnAlias: Word);
    procedure ClearReservedNode;
    function IsAssigned: Boolean;
    function IsEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
  end;

type
  { TLccTrainDccNode }

  TLccTrainDccNode = class(TLccNode)
  private
    FControllerState: TControllerState;
    // DCC Only fields, should make a common class and derive a DCC specific one from it
    FDccAddress: Word;
    FDccLongAddress: Boolean;
    FDccSpeedStep: TLccDccSpeedStep;
    FOnSendMessageComPort: TMessageComPort;

    // Lcc Traction fields
    FFunctions: TLccFunctions;
    FReservedNodeState: TReservationState;
    FSpeed: THalfFloat;

    FSearchEvent: TEventID;  // When creating a train node this is the temporary storage for the train node as the node is being created and allocating is NodeID/AliasID
    FListeners: TListenerList;

    function GetFunctions(Index: Integer; AMessage: TLccMessage): Word;
    function GetSpeed(AMessage: TLccMessage): THalfFloat;

    procedure SetDccAddress(AValue: Word);
    procedure SetDccLongAddress(AValue: Boolean);
    procedure SetDccSpeedStep(AValue: TLccDccSpeedStep);

    procedure SetFunctions(Index: Integer; AMessage: TLccMessage; AValue: Word);
    procedure SetSpeed(AMessage: TLccMessage; AValue: THalfFloat);
  protected
    property ControllerState: TControllerState read FControllerState write FControllerState;
    property ReservedNodeState: TReservationState read FReservedNodeState write FReservedNodeState;

    function GetCdiFile: string; override;
    procedure BeforeLogin; override;
    function EncodeFunctionValuesDccStyle: DWORD;
    function EncodeToDccGridConnect(DccPacket: TDCCPacket): String;
    procedure DoSendMessageComPort(GridConnectString: string);

    function DccFunctionHandler(DccAddress: Word; LongAddress: Boolean; FunctionAddress: DWORD; AllDccFunctionBitsEncoded: DWORD): TDCCPacket;
    function DccSpeedDirHandler(DccAddress: Word; LongAddress: Boolean; SpeedDir: THalfFloat; DccSpeedStep: TLccDccSpeedStep): TDCCPacket;
    procedure DccLoadPacket(var NewMessage: TDCCPacket; Data1, Data2, Data3, Data4, Data5, ValidDataByes: Byte);

    procedure HandleTractionControllerAssign(var SourceMessage: TLccMessage); override;
    procedure HandleTractionControllerRelease(var SourceMessage: TLccMessage); override;
    procedure HandleTractionControllerQuery(var SourceMessage: TLccMessage); override;
 //   procedure HandleTractionControllerChangedNotify(var SourceMessage: TLccMessage); override;
    procedure HandleTractionEStop(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); override;
    procedure HandleTractionListenerAttach(var SourceMessage: TLccMessage); override;
    procedure HandleTractionListenerDetach(var SourceMessage: TLccMessage);override;
    procedure HandleTractionListenerQuery(var SourceMessage: TLccMessage); override;
    procedure HandleTractionManageReserve(var SourceMessage: TLccMessage); override;
    procedure HandleTractionManageRelease(var SourceMessage: TLccMessage); override;
    procedure HandleTractionSetSpeed(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); override;
    procedure HandleTractionSetFunction(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); override;
    procedure HandleTractionQuerySpeed(var SourceMessage: TLccMessage); override;
    procedure HandleTractionQueryFunction(var SourceMessage: TLccMessage); override;
    procedure HandleTractionSimpleTrainInfoReply(var SourceMessage: TLccMessage); override;

    procedure HandleConfiguration_MemorySpaceRead(var SourceMessage: TLccMessage; AutoGrowSpace: Boolean);override;
    procedure HandleConfiguration_MemorySpaceWrite(var SourceMessage: TLccMessage; AutoGrowSpace: Boolean);override;

    procedure LccLogIn(ANodeID: TNodeID); override;

  public
    property DccAddress: Word read FDccAddress write SetDccAddress;
    property DccLongAddress: Boolean read FDccLongAddress write SetDccLongAddress;
    property DccSpeedStep: TLccDccSpeedStep read FDccSpeedStep write SetDccSpeedStep;

    property Listeners: TListenerList read FListeners write FListeners;
    property Speed[AMessage: TLccMessage]: THalfFloat read GetSpeed write SetSpeed;
    property Functions[Index: Integer; AMessage: TLccMessage]: Word read GetFunctions write SetFunctions;
    property OnSendMessageComPort: TMessageComPort read FOnSendMessageComPort write FOnSendMessageComPort;
    property SearchEvent: TEventID read FSearchEvent write FSearchevent;  // The TractionSearch Event associated with this train

    constructor Create(AOwner: TComponent; CdiXML: String); override;
    destructor Destroy; override;

    procedure On_100msTimer(Sender: TObject); override;
  end;

  TLccTrainCanNodeClass = class of TLccTrainDccNode;

  function SpeedStepToString(SpeedStep: TLccDccSpeedStep; Verbose: Boolean): string;
  function SpeedStepToIndex(SpeedStep: TLccDccSpeedStep): Integer;
  function IndexToSpeedStep(Index: Integer): TLccDccSpeedStep;
  function AddressBooleanToText(IsLong: Boolean; Verbose: Boolean): string;

implementation

uses
  lcc_node_manager;

function SpeedStepToString(SpeedStep: TLccDccSpeedStep; Verbose: Boolean): string;
begin
  if Verbose then
  begin
    case SpeedStep of
      ldssDefault : Result := 'Default';
      ldss14      : Result := '14 Step';
      ldss28      : Result := '28 Step';
      ldss128     : Result := '128 Step';
     else
       Result := 'Unknown'
     end;
  end else
  begin
    case SpeedStep of
      ldssDefault : Result := 'S=Def';
      ldss14      : Result := 'S=14';
      ldss28      : Result := 'S=28';
      ldss128     : Result := 'S=128';
     else
       Result := 'Unknown'
     end;
  end;
end;

function SpeedStepToIndex(SpeedStep: TLccDccSpeedStep): Integer;
begin
  case SpeedStep of
    ldssDefault : Result := 0;
    ldss14      : Result := 1;
    ldss28      : Result := 2;
    ldss128     : Result := 3;
   else
     Result := -1;
   end;
end;

function IndexToSpeedStep(Index: Integer): TLccDccSpeedStep;
begin
  case Index of
     0 : Result := ldssDefault;
     1 : Result := ldss14;
     2 : Result := ldss28;
     3 : Result := ldss128;
   else
     Result := ldssDefault;
   end;
end;

function AddressBooleanToText(IsLong: Boolean; Verbose: Boolean): string;
begin
  if Verbose then
  begin
    if IsLong then
      Result := 'Long Address'
    else
      Result := 'Short Address'
  end else
  begin
     if IsLong then
      Result := 'L'
    else
      Result := 'S'
  end
end;

{ TReservationState }

procedure TReservationState.AssignReservedNode(ANodeID: TNodeId; AnAlias: Word);
begin
  FReservedNode.NodeID := ANodeId;
  FReservedNode.Alias := AnAlias;
end;

procedure TReservationState.ClearReservedNode;
begin
  FReservedNode.NodeID := NULL_NODE_ID;
  FReservedNode.Alias := 0;
end;

function TReservationState.IsAssigned: Boolean;
begin
  Result := not NullNodeID(ReservedNode.NodeID) or (ReservedNode.Alias <> 0)
end;

function TReservationState.IsEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
begin
  Result := EqualNodeID(ATestNodeID, ReservedNode.NodeID, False) or (ATestNodeAlias = ReservedNode.Alias);
end;

{ TControllerState }

procedure TControllerState.AssignController(ANodeID: TNodeId; AnAlias: Word);
begin
  FAttachedController.NodeID := ANodeId;
  FAttachedController.Alias := AnAlias;
  FWaitingForNotifyReply := False;
end;

procedure TControllerState.AssignRequestingController(ANodeID: TNodeId; AnAlias: Word);
begin
  FRequestingController.NodeID := ANodeId;
  FRequestingController.Alias := AnAlias;
  FWaitingForNotifyReply := True;
  FWaitingForNotifyReplyTimer := 0;
end;

procedure TControllerState.AcceptRequestingController;
begin
  AssignController(RequestingController.NodeId, RequestingController.Alias);
  ClearRequestingController;
end;

procedure TControllerState.Clear;
begin
  ClearAttachedController;
  ClearRequestingController;
  FWaitingForNotifyReply := False;
  FWaitingForNotifyReplyTimer := 0;
end;

procedure TControllerState.ClearAttachedController;
begin
  FAttachedController.NodeID := NULL_NODE_ID;
  FAttachedController.Alias := 0;
end;

procedure TControllerState.ClearRequestingController;
begin
  FRequestingController.NodeID := NULL_NODE_ID;
  FRequestingController.Alias := 0;
  FWaitingForNotifyReply := False;
  FWaitingForNotifyReplyTimer := 0;
end;

function TControllerState.IsControllerAssigned: Boolean;
begin
  Result := not NullNodeID(AttachedController.NodeID) or (AttachedController.Alias <> 0)
end;

function TControllerState.IsControllerEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
begin
  Result := EqualNodeID(ATestNodeID, AttachedController.NodeID, False) or (ATestNodeAlias = AttachedController.Alias);
end;

procedure TControllerState.On_100msTimer;
begin
  if WaitingForNotifyReply then
    WaitingForNotifyReplyTimer := WaitingForNotifyReplyTimer + 1;
end;

function TControllerState.WaitingForNotifyReplyTimerExpired: Boolean;
begin
  Result := WaitingForNotifyReplyTimer > TIMEOUT_CONTROLLER_NOTIFY_WAIT div 100; // 100ms timer
end;


{ TListenerNode }

procedure TListenerNode.DecodeFlags(Flags: Byte);
begin
  FReverseDirection := Flags and TRACTION_LISTENER_FLAG_REVERSE_DIR = TRACTION_LISTENER_FLAG_REVERSE_DIR;
  FLinkF0 := Flags and TRACTION_LISTENER_FLAG_LINK_F0 = TRACTION_LISTENER_FLAG_LINK_F0;
  FLinkFn := Flags and TRACTION_LISTENER_FLAG_LINK_FN = TRACTION_LISTENER_FLAG_LINK_FN;
  FHidden := Flags and TRACTION_LISTENER_FLAG_HIDDEN = TRACTION_LISTENER_FLAG_HIDDEN;
end;

function TListenerNode.EncodeFlags: Byte;
begin
  Result := 0;
  if ReverseDirection then Result := Result or TRACTION_LISTENER_FLAG_REVERSE_DIR;
  if FLinkF0 then Result := Result or TRACTION_LISTENER_FLAG_LINK_F0;
  if FLinkFn then Result := Result or TRACTION_LISTENER_FLAG_LINK_FN;
  if FHidden then Result := Result or TRACTION_LISTENER_FLAG_HIDDEN;
end;

{ TListenerList }

procedure TListenerList.ClearListeners;
var
  i: Integer;
begin
  try
    for i := 0 to Count - 1 do
      TObject( Items[i]).Free;
  finally
    Clear;
  end;
end;

function TListenerList.DeleteListener(NodeID: TNodeID): Boolean;
var
  Index: Integer;
  ListenerNode: TListenerNode;
begin
  Result := False;
  Index := FindListenerIndex(NodeID);
  if Index >= 0 then
  begin
    try
      ListenerNode := TListenerNode( Items[Index]);
      ListenerNode.Free;
    finally
      Delete(Index);
      Result := True;
    end;
  end;
end;

destructor TListenerList.Destroy;
begin
  ClearListeners;
  inherited Destroy;
end;

function TListenerList.FindListener(NodeID: TNodeID): TListenerNode;
var
  i: Integer;
  ListenerNode: TListenerNode;
begin
  Result := nil;
  i := 0;
  while (Result = nil) and (i < Count) do
  begin
    ListenerNode := TListenerNode( Items[i]);
    if (ListenerNode.NodeID[0] = NodeID[0]) and (ListenerNode.NodeID[1] = NodeID[1]) then
      Result := ListenerNode;
    Inc(i);
  end;
end;

function TListenerList.FindListenerIndex(NodeID: TNodeID): Integer;
var
  i: Integer;
  ListenerNode: TListenerNode;
begin
  Result := -1;
  i := 0;
  while (Result < 0) and (i < Count) do
  begin
    ListenerNode := TListenerNode(Items[i]);
    if (ListenerNode.NodeID[0] = NodeID[0]) and (ListenerNode.NodeID[1] = NodeID[1]) then
      Result := i;
    Inc(i);
  end;
end;

function TListenerList.GetListener(Index: Integer): TListenerNode;
begin
  Result := TListenerNode( Items[Index])
end;


{ TLccTrainDccNode }

constructor TLccTrainDccNode.Create(AOwner: TComponent; CdiXML: String);
begin
  inherited Create(AOwner, CdiXML);
  FListeners := TListenerList.Create;
  FControllerState := TControllerState.Create;
  FReservedNodeState := TReservationState.Create;
end;

procedure TLccTrainDccNode.BeforeLogin;
var
  DccAddressStr: string;
  i: Integer;
  {$IFDEF LCC_DELPHI}
  B: Byte;
  {$ENDIF}
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.MemConfig := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;
//  ProtocolSupportedProtocols.TractionSimpleTrainNodeInfo := True;
//  ProtocolSupportedProtocols.TractionFunctionDefinitionInfo := True;
//  ProtocolSupportedProtocols.TractionFunctionConfiguration := True;

  ProtocolEventsProduced.Add(EVENT_IS_TRAIN, evs_Valid);
  ProtocolEventConsumed.Add(EVENT_EMERGENCY_STOP, evs_InValid);

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, StreamCdi.Size);
  ProtocolMemoryInfo.Add(MSI_ALL, False, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_CONFIG, True, False, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_MFG, True, True, True, 0, $FFFFFFFF);
  ProtocolMemoryInfo.Add(MSI_ACDI_USER, True, False, True, 0, $FFFFFFFF);

  // Support these later
 // ProtocolMemoryInfo.Add(MSI_TRACTION_FDI, True, True, True, 0, $FFFFFFFF);
 // ProtocolMemoryInfo.Add(MSI_TRACTION_FUNCTION_CONFIG, True, False, True, 0, $FFFFFFFF);

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
 // ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;
  ProtocolMemoryOptions.LowSpace := MSI_ACDI_USER;

  // TODO... this should be a persistent Stream if the user changes the name in the Configuration Dialog
  //         save to a file with the DccAddress as the filename??????
  StreamConfig.Position := ADDRESS_USER_NAME;
  {$IFDEF LCC_DELPHI}
  StreamConfig.ReadData(B);
  if B = 0 then
  {$ELSE}
  if StreamConfig.ReadByte = 0 then
  {$ENDIF}
  begin
    StreamConfig.Position := ADDRESS_USER_NAME;

    DccAddressStr := IntToStr(DccAddress);
    if DccAddress < 128 then
    begin
      if DccLongAddress then
        DccAddressStr := DccAddressStr + 'L'
      else
        DccAddressStr := DccAddressStr + 'S';
    end;

    for i := 1 to Length(DccAddressStr) do
    {$IFDEF LCC_MOBILE}
      StreamWriteByte(StreamConfig, Ord(DccAddressStr[i-1]))
    {$ELSE}
      StreamWriteByte(StreamConfig, Ord(DccAddressStr[i]))
    {$ENDIF}
  end;
end;

function TLccTrainDccNode.EncodeFunctionValuesDccStyle: DWORD;
var
  i: Integer;
begin
  Result := 0;
  for i := MAX_DCC_FUNCTIONS - 1 downto 0 do
  begin
    if Functions[i, nil] > 0 then
      Result := Result or $00000001;
    if i > 0 then
      Result := Result shl 1
  end;
end;

function TLccTrainDccNode.EncodeToDccGridConnect(DccPacket: TDCCPacket): String;
var
  i: Integer;
begin
  Result := ':R' + IntToHex(DccPacket.Flags, 8) +  'N';   // make it the same size header as OpenLCB
  for i := 0 to DccPacket.Flags - 1 do
    Result := Result + IntToHex(DccPacket.PacketBytes[i], 2);
  Result := Result + ';';
end;

procedure TLccTrainDccNode.DoSendMessageComPort(GridConnectString: string);
begin
  if Assigned(OnSendMessageComPort) then
    OnSendMessageComPort(Self, GridConnectString);
end;

function TLccTrainDccNode.DccFunctionHandler(DccAddress: Word;
  LongAddress: Boolean; FunctionAddress: DWORD; AllDccFunctionBitsEncoded: DWORD
  ): TDCCPacket;
var
  FunctionMask, FunctionExtendedCode: Byte;
  AddressHi, AddressLo: Byte;
begin
  Result.Flags := 0;
  FunctionMask := 0;
  FunctionExtendedCode := 0;

  // Split the address to make clear when loading bytes
  AddressHi := (DccAddress shr 8) and $00FF;
  if LongAddress then
    AddressHi := AddressHi or NMRA_LONGADDRESS_MASK_BYTE;
  AddressLo := DccAddress and $00FF;

  if FunctionAddress < 29 then
  begin
    if FunctionAddress < 5 then
    begin
        // a bit weird that bit 0 is Function 1 and bit 4 could be Function 0 if CV29 is set right
        FunctionMask := (AllDccFunctionBitsEncoded and $0F) shr 1;
        if AllDccFunctionBitsEncoded and $00000001 = 0 then                      // Look at Function 0, it is contained in Bit 4
          FunctionMask := FunctionMask and not $10                                // Clear Bit 4
        else
          FunctionMask := FunctionMask or $10;                                    // Set Bit 4
      FunctionMask := FunctionMask or {$IFNDEF LCC_FPC}$80{$ELSE}%10000000{$ENDIF};                                // Opcode bits
    end else
    if FunctionAddress < 9 then
    begin
      FunctionMask := (AllDccFunctionBitsEncoded shr 5) and $0F;
      FunctionMask := FunctionMask or  {$IFNDEF LCC_FPC}$B0{$ELSE}%10110000{$ENDIF};                               // Opcode bits
    end else
    if FunctionAddress < 13 then
    begin
      FunctionMask := (AllDccFunctionBitsEncoded shr 9) and $0F;
      FunctionMask := FunctionMask or {$IFNDEF LCC_FPC}$A0{$ELSE}%10100000{$ENDIF};                               // Opcode bits
    end else
    if FunctionAddress < 21 then
    begin
      FunctionMask := AllDccFunctionBitsEncoded shr 13;
      FunctionExtendedCode := {$IFNDEF LCC_FPC}$DE{$ELSE}%11011110{$ENDIF};
    end else
    if FunctionAddress < 29 then
    begin
      FunctionMask := AllDccFunctionBitsEncoded shr 21;
      FunctionExtendedCode := {$IFNDEF LCC_FPC}$DF{$ELSE}%11011111{$ENDIF};
    end;

    // Now create the DCC Packet
    if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
    begin
      if FunctionAddress < 13 then
        DccLoadPacket(Result, AddressHi, AddressLo, FunctionMask, 0, 0, 3)
      else
        DccLoadPacket(Result, AddressHi, AddressLo, FunctionExtendedCode, FunctionMask, 0, 4)
    end else
    begin
      if FunctionAddress < 13 then
        DccLoadPacket(Result, AddressLo, FunctionMask, 0, 0, 0, 2)
      else
        DccLoadPacket(Result, AddressLo, FunctionExtendedCode, FunctionMask, 0, 0, 3)
    end;
  end
end;

function TLccTrainDccNode.DccSpeedDirHandler(DccAddress: Word;
  LongAddress: Boolean; SpeedDir: THalfFloat; DccSpeedStep: TLccDccSpeedStep
  ): TDCCPacket;
var
  IsForward: Boolean;
  AbsoluteSpeed: single;
  LocalSpeedStep: Word;
  AddressHi, AddressLo: Byte;
begin
  Result.Flags := 0;

  IsForward := SpeedDir and $8000 <> $8000;                                                      // Split the Speed and Direction
  AbsoluteSpeed := HalfToFloat( SpeedDir and not $8000);
  if LongAddress then
    DccAddress := DccAddress or NMRA_LONGADDRESS_MASK_WORD;

  // Split the address to make clear when loading bytes
  AddressHi := Hi(DccAddress);
  AddressLo := Lo(DccAddress);

  case DccSpeedStep of
    ldssDefault,
    ldss14 :
          begin
            AbsoluteSpeed := (14/100) * AbsoluteSpeed;
            LocalSpeedStep := Trunc(AbsoluteSpeed);

            LocalSpeedStep := _14_STEP_TABLE[LocalSpeedStep];
            if IsForward then
              LocalSpeedStep := LocalSpeedStep or $60
            else
              LocalSpeedStep := LocalSpeedStep or $40;
            if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
              DccLoadPacket(Result, AddressHi, AddressLo, LocalSpeedStep, 0, 0, 3)
            else
              DccLoadPacket(Result, AddressLo, LocalSpeedStep, 0, 0, 0, 2);
          end;
    ldss28  :
          begin
            AbsoluteSpeed := (28/100) * AbsoluteSpeed;
            LocalSpeedStep := Trunc(AbsoluteSpeed);
            LocalSpeedStep := _28_STEP_TABLE[LocalSpeedStep];
            if IsForward then
              LocalSpeedStep := LocalSpeedStep or $60
            else
              LocalSpeedStep := LocalSpeedStep or $40;

            if AddressHi and NMRA_LONGADDRESS_MASK_BYTE = NMRA_LONGADDRESS_MASK_BYTE then
              DccLoadPacket(Result, AddressHi, AddressLo, LocalSpeedStep, 0, 0, 3)
            else
              DccLoadPacket(Result, AddressLo, LocalSpeedStep, 0, 0, 0, 2);

          end;
    ldss128 :
          begin
             // Allow a mistaken short address to work here by adding the $C0  Per Tim
            AddressHi := AddressHi or NMRA_LONGADDRESS_MASK_BYTE;

            AbsoluteSpeed := (127/100) * AbsoluteSpeed;
            LocalSpeedStep := Trunc(AbsoluteSpeed);
            if LocalSpeedStep > 0 then
              Inc(LocalSpeedStep);   // 1 = EStop
            if IsForward then
              LocalSpeedStep := LocalSpeedStep or $80;
            DccLoadPacket(Result, AddressHi, AddressLo, {$IFNDEF LCC_FPC}$3F{$ELSE}%00111111{$ENDIF}, LocalSpeedStep, 0, 4);
          end;

  end;
end;

destructor TLccTrainDccNode.Destroy;
begin
  FreeAndNil(FListeners);
  FreeAndNil(FControllerState);
  FreeAndNil(FReservedNodeState);
  inherited Destroy;
end;

procedure TLccTrainDccNode.DccLoadPacket(var NewMessage: TDCCPacket; Data1, Data2, Data3, Data4, Data5, ValidDataByes: Byte);
begin
  NewMessage.PacketBytes[0] := Data1;
  NewMessage.PacketBytes[1] := Data2;
  NewMessage.PacketBytes[2] := Data3;
  NewMessage.PacketBytes[3] := Data4;
  NewMessage.PacketBytes[4] := Data5;
  NewMessage.Flags := ValidDataByes;
end;

procedure TLccTrainDccNode.HandleTractionControllerAssign(var SourceMessage: TLccMessage);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionControllerAssign(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    // First check to see if the train is assigned to a controller.  If it is then tell that controller it is being released
    if ControllerState.IsControllerAssigned and not ControllerState.IsControllerEqual(SourceMessage.SourceID, SourceMessage.SourceAlias) then
    begin
      WorkerMessage.LoadTractionControllerChangedNotify(NodeID, AliasID, ControllerState.AttachedController.NodeID, ControllerState.AttachedController.Alias, SourceMessage.SourceID );
      SendMessage(WorkerMessage, Self);
    end;

    // We just assign regardless of if another controller is
    // TODO:
    //   Should notify existing controller they are loosing control
    ControllerState.AssignController(SourceMessage.SourceID, SourceMessage.SourceAlias);
    WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
    SendMessage(WorkerMessage, Self);
  end;
end;

procedure TLccTrainDccNode.HandleTractionControllerRelease(var SourceMessage: TLccMessage);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionControllerRelease(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    if ControllerState.IsControllerAssigned and ControllerState.IsControllerEqual(SourceMessage.SourceID, SourceMessage.SourceAlias) then
      ControllerState.Clear
  end
end;

procedure TLccTrainDccNode.HandleTractionControllerQuery(var SourceMessage: TLccMessage);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionControllerQuery(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    // I could just return ControllerState.AttachedController.NodeID as if not assigned it will be NULL anyway
    if ControllerState.IsControllerAssigned then
    begin
      WorkerMessage.LoadTractionControllerQueryReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ControllerState.AttachedController.NodeID);
    end else
      WorkerMessage.LoadTractionControllerQueryReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, NULL_NODE_ID);
    SendMessage(WorkerMessage, Self);
  end;
end;

{
procedure TLccTrainDccNode.HandleTractionControllerChangedNotify(var SourceMessage: TLccMessage);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionControllerChangedNotify(Self, SourceMessage, DoDefault);
end;
}

procedure TLccTrainDccNode.HandleTractionManageReserve(var SourceMessage: TLccMessage);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionManageReserve(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    // If we are reserved and not reserved by the node calling then fail
    if ReservedNodeState.IsAssigned and not ReservedNodeState.IsEqual(SourceMessage.SourceID, SourceMessage.SourceAlias) then
      WorkerMessage.LoadTractionManageReply(SourceMessage.DestID, SourceMessage.DestAlias, SourceMessage.SourceID, SourceMessage.SourceAlias, False)
    else begin
      ReservedNodeState.AssignReservedNode(SourceMessage.DestID, SourceMessage.DestAlias);
      WorkerMessage.LoadTractionManageReply(SourceMessage.DestID, SourceMessage.DestAlias, SourceMessage.SourceID, SourceMessage.SourceAlias, True)
    end;
    SendMessage(WorkerMessage, Self);
  end;
end;

procedure TLccTrainDccNode.HandleTractionManageRelease(var SourceMessage: TLccMessage);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionManageRelease(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    // If we are reserved and reserved by the node then release it
    if ReservedNodeState.IsAssigned and ReservedNodeState.IsEqual(SourceMessage.SourceID, SourceMessage.SourceAlias) then
      ReservedNodeState.ClearReservedNode;
  end
end;

procedure TLccTrainDccNode.HandleTractionListenerAttach(var SourceMessage: TLccMessage);
var
  NewListenerNode: TListenerNode;
  ListenerAttachFlags: Byte;
  ListenerNodeID: TNodeID;
  DoDefault: Boolean;
begin
  // Things to concider:
  //   1) Can't add a Node as its own Listener
  //   2) Listener could already be in the list, don't add duplicates but may just want to update flags
  //   3) Only the connected Controller can Attach a Listener : Balazs does not agree
  //   4) The Train must be Reserved by the connected controller
  //   5) Not enough memory to allocate another Listener

  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionListenerAttach(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    ListenerNodeID := SourceMessage.TractionExtractListenerID;
    ListenerAttachFlags := SourceMessage.TractionExtractListenerFlags;

    if EqualNodeID(NodeID, ListenerNodeID, False) then  // Trying to create a Listener that is the nodes itself.... will cause infinte loops
      WorkerMessage.LoadTractionListenerAttachReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ListenerNodeID, ERROR_CODE_PERMANENT_INVALID_ARGUMENTS)
    else begin
      NewListenerNode := Listeners.FindListener(ListenerNodeID);
      if Assigned(NewListenerNode) then
      begin  // Simple update of flags
        NewListenerNode.DecodeFlags(ListenerAttachFlags);
        WorkerMessage.LoadTractionListenerAttachReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ListenerNodeID, S_OK)
      end else
      begin  // Add The listener to the list
        NewListenerNode := TListenerNode.Create;
        if Assigned(NewListenerNode) then
        begin
          NewListenerNode.NodeID := ListenerNodeID;
          NewListenerNode.AliasID := AliasServer.FindAlias(ListenerNodeID);
          NewListenerNode.DecodeFlags(ListenerAttachFlags);
          Listeners.Add(NewListenerNode);
          WorkerMessage.LoadTractionListenerAttachReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ListenerNodeID, S_OK)
        end else
          WorkerMessage.LoadTractionListenerAttachReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ListenerNodeID, ERROR_CODE_TEMPORARY_BUFFER_UNAVAILABLE)
      end;
    end;
    SendMessage(WorkerMessage, Self);

    ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionListenerAttached(Self, SourceMessage);
  end
end;

procedure TLccTrainDccNode.HandleTractionListenerDetach(var SourceMessage: TLccMessage);
var
  ListenerNodeID: TNodeID;
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionListenerDetach(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    ListenerNodeID := SourceMessage.TractionExtractListenerID;

    if Listeners.DeleteListener(ListenerNodeID) then
      WorkerMessage.LoadTractionListenerDetachReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ListenerNodeID, S_OK)
    else
      WorkerMessage.LoadTractionListenerDetachReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, ListenerNodeID, ERROR_CODE_PERMANENT_INVALID_ARGUMENTS);
    SendMessage(WorkerMessage, Self);

    ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionListenerDetached(Self, SourceMessage);
  end;
end;

procedure TLccTrainDccNode.HandleTractionListenerQuery(var SourceMessage: TLccMessage);
var
  Listener: TListenerNode;
  RequestedIndex: Integer;
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionListenerQuery(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    if (SourceMessage.DataCount = 2) then // no query data, just wants the total number of listeners retured
      WorkerMessage.LoadTractionListenerQueryReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, Listeners.Count, 0, NULL_NODE_ID, 0)
    else begin
      RequestedIndex := SourceMessage.TractionExtractListenerIndex;
      if RequestedIndex < Listeners.Count then
      begin
        Listener := Listeners[RequestedIndex];
        WorkerMessage.LoadTractionListenerQueryReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, Listeners.Count, RequestedIndex, Listener.NodeID, Listener.EncodeFlags);
      end else
        WorkerMessage.LoadTractionListenerQueryReply(NodeID, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, Listeners.Count, 0, NULL_NODE_ID, 0);   // Outside of range, bad index
    end;
    SendMessage(WorkerMessage, Self);
  end;
end;

procedure TLccTrainDccNode.HandleTractionSetSpeed(var SourceMessage: TLccMessage; ListenerForwarded: Boolean);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionSetSpeed(Self, SourceMessage, ListenerForwarded, DoDefault);

  if DoDefault then
  begin
  // Should be from assigned controller only but.....
    SetSpeed(SourceMessage, SourceMessage.TractionExtractSetSpeed);
  end
end;

procedure TLccTrainDccNode.HandleTractionSetFunction(var SourceMessage: TLccMessage; ListenerForwarded: Boolean);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionSetFunction(Self, SourceMessage, ListenerForwarded, DoDefault);

  if DoDefault then
  begin
    // Should be from assigned controller only but.....
    SetFunctions(SourceMessage.TractionExtractFunctionAddress, SourceMessage, SourceMessage.TractionExtractFunctionValue);
  end;
end;

procedure TLccTrainDccNode.HandleTractionEStop(var SourceMessage: TLccMessage; ListenerForwarded: Boolean);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionEmergencyStop(Self, SourceMessage, ListenerForwarded, DoDefault);

  if DoDefault then
  begin
    SetSpeed(SourceMessage, 0);
  end;
end;

procedure TLccTrainDccNode.HandleTractionQuerySpeed(var SourceMessage: TLccMessage);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionQuerySpeed(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    WorkerMessage.LoadTractionQuerySpeedReply(NodeId, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, GetSpeed(SourceMessage), 0, HalfNaN, HalfNaN);
    SendMessage(WorkerMessage, Self);
  end;
end;

procedure TLccTrainDccNode.HandleTractionQueryFunction(var SourceMessage: TLccMessage);
var
  FunctionAddress: DWORD;
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionQueryFunction(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin
    FunctionAddress := SourceMessage.TractionExtractFunctionAddress;
    WorkerMessage.LoadTractionQueryFunctionReply(NodeId, AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, FunctionAddress, Functions[FunctionAddress, nil]);
    SendMessage(WorkerMessage, Self);
  end;
end;

procedure TLccTrainDccNode.HandleTractionSimpleTrainInfoReply(var SourceMessage: TLccMessage);
var
  DoDefault: Boolean;
begin
  DoDefault := True;
  ((Owner as TLccNodeManager) as INodeManagerTractionCallbacks).DoTractionTrainSNIP(Self, SourceMessage, DoDefault);

  if DoDefault then
  begin

  end;
end;

procedure TLccTrainDccNode.HandleConfiguration_MemorySpaceRead(var SourceMessage: TLccMessage; AutoGrowSpace: Boolean);
begin
  inherited HandleConfiguration_MemorySpaceRead(SourceMessage, True);
end;

procedure TLccTrainDccNode.HandleConfiguration_MemorySpaceWrite(var SourceMessage: TLccMessage; AutoGrowSpace: Boolean);
begin
  inherited HandleConfiguration_MemorySpaceWrite(SourceMessage, True);
end;

procedure TLccTrainDccNode.LccLogIn(ANodeID: TNodeID);
begin
  // This is now a defined Event for this node.
  if not EqualEventID(SearchEvent, NULL_EVENT_ID) then
    ProtocolEventsProduced.Add(SearchEvent, evs_Valid);
  inherited LccLogIn(ANodeID);
end;

procedure TLccTrainDccNode.On_100msTimer(Sender: TObject);
begin
  inherited On_100msTimer(Sender);
  ControllerState.On_100msTimer;
  if ControllerState.WaitingForNotifyReply then
    if ControllerState.WaitingForNotifyReplyTimerExpired then
    begin
      ControllerState.AcceptRequestingController;
       WorkerMessage.LoadTractionControllerAssignReply(NodeID, AliasID, ControllerState.AttachedController.NodeID, ControllerState.AttachedController.Alias, TRACTION_CONTROLLER_CONFIG_REPLY_OK);
      SendMessage(WorkerMessage, Self);
    end;
end;

function TLccTrainDccNode.GetCdiFile: string;
begin
  Result := CDI_XML_TRAIN_NODE
end;

function TLccTrainDccNode.GetFunctions(Index: Integer; AMessage: TLccMessage): Word;
begin
  if (Index >= 0) and (Index < Length(FFunctions)) then
    Result := FFunctions[Index]
  else
    Result := $FFFF
end;

function TLccTrainDccNode.GetSpeed(AMessage: TLccMessage): THalfFloat;
begin
  Result := FSpeed;
end;

procedure TLccTrainDccNode.SetDccAddress(AValue: Word);
begin
  if FDccAddress = AValue then Exit;
  FDccAddress := AValue;
end;

procedure TLccTrainDccNode.SetDccLongAddress(AValue: Boolean);
begin
  if FDccLongAddress = AValue then Exit;
  FDccLongAddress := AValue;
end;

procedure TLccTrainDccNode.SetFunctions(Index: Integer; AMessage: TLccMessage; AValue: Word);
var
  DccPacket: TDCCPacket;
  DccGridConnect: string;
  i: Integer;
  ListenerNode: TListenerNode;
begin
  if (Index >= 0) and (Index < Length(FFunctions)) then
  begin
    if FFunctions[Index] = AValue then
      Exit;

    FFunctions[Index] := AValue;
    DccPacket := DccFunctionHandler(DccAddress, DccLongAddress, Index, EncodeFunctionValuesDccStyle);
    DccGridConnect := EncodeToDccGridConnect( DccPacket);
    DoSendMessageComPort(DccGridConnect);

    for i := 0 to Listeners.Count - 1 do
    begin
      ListenerNode := TListenerNode(Listeners[i]);
      // Don't sent back to the same place that sent the message
      if not EqualNode(ListenerNode.NodeID, ListenerNode.AliasID, AMessage.SourceID, AMessage.SourceAlias, True) then
      begin
        if ListenerNode.LinkF0 and (Index = 0 ) then
        begin
          WorkerMessage.LoadTractionSetFunction(NodeID, AliasID, ListenerNode.NodeID, ListenerNode.AliasID, Index, AValue, True);
          SendMessage(WorkerMessage, Self);
        end else
        if ListenerNode.LinkFn and (Index > 0) then
        begin
          WorkerMessage.LoadTractionSetFunction(NodeID, AliasID, ListenerNode.NodeID, ListenerNode.AliasID, Index, AValue, True);
          SendMessage(WorkerMessage, Self);
        end;
      end;
    end;
  end;
end;

procedure TLccTrainDccNode.SetSpeed(AMessage: TLccMessage; AValue: THalfFloat);
var
  DccPacket: TDCCPacket;
  DccGridConnect: string;
  i: Integer;
  ListenerNode: TListenerNode;
begin
  if AValue = FSpeed then Exit;

  FSpeed := AValue;
  DccPacket := DccSpeedDirHandler(DccAddress, DccLongAddress, FSpeed, DccSpeedStep);
  DccGridConnect := EncodeToDccGridConnect( DccPacket);
  DoSendMessageComPort(DccGridConnect);

  for i := 0 to Listeners.Count - 1 do
  begin
    ListenerNode := Listeners[i];

    if not EqualNode(ListenerNode.NodeID, ListenerNode.AliasID, AMessage.SourceID, AMessage.SourceAlias, True) then
    begin
      if ListenerNode.ReverseDirection then
        WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, ListenerNode.NodeID, ListenerNode.AliasID, FlipHalfFloatSign(FSpeed), True)
      else
        WorkerMessage.LoadTractionSetSpeed(NodeID, AliasID, ListenerNode.NodeID, ListenerNode.AliasID, FSpeed, True);
      SendMessage(WorkerMessage, Self);
    end;
  end;
end;

procedure TLccTrainDccNode.SetDccSpeedStep(AValue: TLccDccSpeedStep);
begin
  if FDccSpeedStep = AValue then Exit;
  FDccSpeedStep := AValue;
end;

end.


