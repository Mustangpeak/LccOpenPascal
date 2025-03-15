unit lcc_node_controller;

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
  lcc_node,
  lcc_math_float16,
  lcc_defines,
  lcc_alias_server,
  lcc_protocol_utilities,

  lcc_node_messages;

const
  CDI_XML_CONTROLLER: string = (
  '<?xml version="1.0" encoding="utf-8"?>'+
  '<?xml-stylesheet type="text/xsl" href="http://openlcb.org/trunk/prototypes/xml/xslt/cdi.xsl"?>'+
  '<cdi xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xsi:noNamespaceSchemaLocation="http://openlcb.org/trunk/specs/schema/cdi.xsd">'+
         '<identification>'+
       	  '<manufacturer>Mustangpeak</manufacturer>'+
       	  '<model>TCN1000</model>'+
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

  TLccTrainController = class;
  TLccTaskSearchTrain = class;
  TTrainInfo = class;

  TOnSetSpeedListener = procedure(Traininfo: TTrainInfo; SetSpeed: Single; Reverse: Boolean) of object;
  TOnSetFunctionListener = procedure(TrainInfo: TTrainInfo; FunctionAddress, FunctionValue: Word) of object;

  { TTrainInfo }

  TTrainInfo = class
  private
    FCDI: String;
    FDefaultName: String;
    FNodeID: TNodeID;
    FPIP: TProtocolSupportedProtocols;
    FSNIP: TProtocolSimpleNodeInfo;
    function GetUserName: String;
  public
    property DefaultName: String read FDefaultName write FDefaultName;
    property NodeID: TNodeID read FNodeID write FNodeID;
    property SNIP: TProtocolSimpleNodeInfo read FSNIP write FSNIP;
    property PIP: TProtocolSupportedProtocols read FPIP write FPIP;
    property CDI: String read FCDI write FCDI;
    property UserName: String read GetUserName;

    constructor Create(ADefaultName: String = ''); overload;
    constructor Create(ANodeID: TNodeID; ADefaultName: String = ''); overload;
    destructor Destroy; override;
    function Equal(TestNodeID: TNodeID): Boolean;
  end;

  TOnTLccTaskSearchTrainCallback = procedure(ATask: TLccTaskSearchTrain) of object;

  { TLccTaskSearchTrain }

  TLccTaskSearchTrain = class(TLccTaskBase)
  private
    FDccAddress: String;
    FIsLongAddress: Boolean;
    FSearchCriteria: DWord;
    FSpeedSteps: TLccDccSpeedStep;
    FTrainNodeIDReply: TNodeID;
  public
    property SearchCriteria: DWord read FSearchCriteria write FSearchCriteria;
    property DccAddress: String read FDccAddress write FDccAddress;
    property IsLongAddress: Boolean read FIsLongAddress write FIsLongAddress;
    property SpeedSteps: TLccDccSpeedStep read FSpeedSteps write FSpeedSteps;

    // Reply
    property TrainNodeIDReply: TNodeID read FTrainNodeIDReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskControllerAttach }

  TLccTaskControllerAttach = class(TLccTaskBase)
  public

    // Replies
    //   Just possible error code

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskControllerRelease }

  TLccTaskControllerRelease = class(TLccTaskBase)
  private
    FControllerNode: TNodeID;
  public
    property ControllerNode: TNodeID read FControllerNode write FControllerNode;

    // Replies
    //   None

    procedure Start(ATimeout: Integer); override;
  end;

  { TLccTaskControllerQuery }

  TLccTaskControllerQuery = class(TLccTaskBase)
  private
    FActiveControllerReply: TNodeID;
    FFlagsReply: Byte;
  public

    // Replies
    property FlagsReply: Byte read FFlagsReply;
    property ActiveControllerReply: TNodeID read FActiveControllerReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskListenerAttach }

  TLccTaskListenerAttach = class(TLccTaskBase)
  private
    FHidden: Boolean;
    FLinkFn: Boolean;
    FListener: TNodeID;
    FReplyCode: Byte;
    FListenerNodeIDReply: TNodeID;
    FReverseDir: Boolean;
  public
    property Listener: TNodeID read FListener write FListener;
    property ReverseDir: Boolean read FReverseDir write FReverseDir;
    property LinkF0: Boolean read FLinkFn write FLinkFn;
    property Hidden: Boolean read FHidden write FHidden;

    // Replies
    property ReplyCode: Byte read FReplyCode;
    property ListenerNodeIDReply: TNodeID read FListenerNodeIDReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskListenerDetach }

  TLccTaskListenerDetach = class(TLccTaskBase)
  private
    FListener: TNodeID;
    FReplyCode: Byte;
    FListenerNodeIDReply: TNodeID;
  public
    property Listener: TNodeID read FListener write FListener;

    // Replies
    property ReplyCode: Byte read FReplyCode;
    property ListenerNodeIDReply: TNodeID read FListenerNodeIDReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskListenerQueryAtIndex }

  TLccTaskListenerQueryAtIndex = class(TLccTaskBase)
  private
    FFlagsReply: Byte;
    FIndexReply: Byte;
    FQueryIndex: Byte;
    FReplyCode: Byte;
    FListenerNodeIDReply: TNodeID;
  public
    property QueryIndex: Byte read FQueryIndex write FQueryIndex;

    // Replies
    property ReplyCode: Byte read FReplyCode;
    property ListenerNodeIDReply: TNodeID read FListenerNodeIDReply;
    property FlagsReply: Byte read FFlagsReply;
    property IndexReply: Byte read FIndexReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskListenerQueryCount }

  TLccTaskListenerQueryCount = class(TLccTaskBase)
  private
    FNodeCountReply: Byte;
  public

    // Replies
    property NodeCountReply: Byte read FNodeCountReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

    { TTrainListener }

  TTrainListener = class
  private
    FFlags: Byte;
    FIndex: Byte;
    FNodeID: TNodeID;
    function GetF0Forward: Boolean;
    function GetFnForward: Boolean;
    function GetHidden: Boolean;
    function GetReverseDir: Boolean;
    procedure SetF0Forward(AValue: Boolean);
    procedure SetFnForward(AValue: Boolean);
    procedure SetHidden(AValue: Boolean);
    procedure SetReverseDir(AValue: Boolean);
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property Flags: Byte read FFlags write FFlags;
    property Index: Byte read FIndex write FIndex;

    property ReverseDir: Boolean read GetReverseDir write SetReverseDir;
    property F0Forward: Boolean read GetF0Forward write SetF0Forward;
    property FnForward: Boolean read GetFnForward write SetFnForward;
    property Hidden: Boolean read GetHidden write SetHidden;

    constructor Create; overload;
    constructor Create(ANode: TNodeID; AFlags: Byte; AnIndex: Byte); overload;

    procedure CopyTo( ATarget: TTrainListener);

  end;

  { TLccTaskListenersEnumerate }

  TLccTaskListenersEnumerate = class(TLccTaskBase)
  private
    FiListener: Byte;
    FListenerCount: Byte;
    FTrainListener: TTrainListener;

  protected


  public

    // Replies
    property TrainListener: TTrainListener read FTrainListener write FTrainListener;
    property iListener: Byte read FiListener;
    property ListenerCount: Byte read FListenerCount;

    constructor Create(AnOwnerNode: TLccNode); override;
    destructor Destroy; override;
    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskManagementReserveTrain }

  TLccTaskManagementReserveTrain = class(TLccTaskBase)

  public

    // Replies
    //   None

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskManagementReleaseTrain }

  TLccTaskManagementReleaseTrain = class(TLccTaskBase)
  public

    // Replies
    //   None

    procedure Start(ATimeout: Integer); override;
  end;

  { TLccTaskManagementNoOp }

  TLccTaskManagementNoOp = class(TLccTaskBase)
  private
    FTimeoutSecondsReply: Byte;
  public

    // Replies
    property TimeoutSecondsReply: Byte read FTimeoutSecondsReply write FTimeoutSecondsReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskSetSpeedDir }

  TLccTaskSetSpeedDir = class(TLccTaskBase)
  private
    FForwardedListenerMessage: Boolean;
    FSpeed: single;
  public
    property Speed: single read FSpeed write FSpeed;
    property ForwardedListenerMessage: Boolean read FForwardedListenerMessage write FForwardedListenerMessage;

    // Replies
    //   None

    procedure Start(ATimeout: Integer); override;
  end;

  { TLccTaskSetFunction }

  TLccTaskSetFunction = class(TLccTaskBase)
  private
    FAddress: DWord;
    FForwardedListenerMessage: Boolean;
    FValue: Word;
  public
    property Address: DWord read FAddress write FAddress;
    property Value: Word read FValue write FValue;
    property ForwardedListenerMessage: Boolean read FForwardedListenerMessage write FForwardedListenerMessage;

    // Replies
    //   None

    procedure Start(ATimeout: Integer); override;
  end;

  TLccTaskSetFunctionArray = array of TLccTaskSetFunction;

  { TLccTaskEmergencyStop }

  TLccTaskEmergencyStop = class(TLccTaskBase)
  public

    // Replies
      //   None

    procedure Start(ATimeout: Integer); override;
  end;

  { TLccTaskQuerySpeed }

  TLccTaskQuerySpeed = class(TLccTaskBase)
  private
    FActualSpeedReply: single;
    FActualSpeedReverseReply: Boolean;
    FCommandedSpeedReply: single;
    FCommandedSpeedReverseReply: Boolean;
    FSetSpeedReply: single;
    FSetSpeedReverseReply: Boolean;
  public

    // Replies
    property ActualSpeedReply: single read FActualSpeedReply;
    property ActualSpeedReverseReply: Boolean read FActualSpeedReverseReply;
    property SetSpeedReply: single read FSetSpeedReply;
    property SetSpeedReverseReply: Boolean read FSetSpeedReverseReply;
    property CommandedSpeedReply: single read FCommandedSpeedReply;
    property CommandedSpeedReverseReply: Boolean read FCommandedSpeedReverseReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  { TLccTaskQueryFunction }

  TLccTaskQueryFunction = class(TLccTaskBase)
  private
    FAddress: DWord;
    FValueReply: Word;
  public
    property Address: DWord read FAddress write FAddress;

    // Replies
    property ValueReply: Word read FValueReply write FValueReply;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;

  TLccTaskQueryFunctionArray = array of TLccTaskQueryFunction;

  { TLccTaskTrainRoster }

  TLccTaskTrainRoster = class(TLccTaskBase)
  private
    FOwnerNode: TLccNode;
    FRosterList: TList;
    FActiveTrain: TTrainInfo;
    function GetCount: Integer;
    function GetTrain(Index: Integer): TTrainInfo;

  protected
    property RosterList: TList read FRosterList write FRosterList;
    property OwnerNode: TLccNode read FOwnerNode write FOwnerNode;

  public
    property ActiveTrain: TTrainInfo read FActiveTrain;  // Use ActivateTrain method to set
    property Count: Integer read GetCount;
    property Train[Index: Integer]: TTrainInfo read GetTrain; default;

    constructor Create(AnOwner: TLccNode); override;
    destructor Destroy; override;

    function Add(ATrain: TTrainInfo): TTrainInfo;
    function IndexOf(ATrain: TTrainInfo): Integer;
    procedure Remove(ATrain: TTrainInfo; FreeTrain: Boolean);
    function Delete(Index: Integer; FreeTrain: Boolean): TTrainInfo;
    function FindByNodeID(ANodeID: TNodeID): TTrainInfo;
    function TrainActivate(ANodeID: TNodeID): TTrainInfo;
    procedure TrainDeActivate;

    procedure Start(ATimeout: Integer); override;
    procedure Process(SourceMessage: TLccMessage); override;
  end;



  // ******************************************************************************

  { TLccTrainController }

  TLccTrainController = class(TLccNode)
  private
    FOnSetFunctionListener: TOnSetFunctionListener;
    FOnSetSpeedListener: TOnSetSpeedListener;
    FTaskControllerAttach: TLccTaskControllerAttach;
    FTaskControllerQuery: TLccTaskControllerQuery;
    FTaskControllerRelease: TLccTaskControllerRelease;
    FTaskEmergencyStop: TLccTaskEmergencyStop;
    FTaskListenerAttach: TLccTaskListenerAttach;
    FTaskListenerDetach: TLccTaskListenerDetach;
    FTaskListenerQueryAtIndex: TLccTaskListenerQueryAtIndex;
    FTaskListenerQueryCount: TLccTaskListenerQueryCount;
    FTaskManagementNoOp: TLccTaskManagementNoOp;
    FTaskManagementReleaseTrain: TLccTaskManagementReleaseTrain;
    FTaskManagementReserveTrain: TLccTaskManagementReserveTrain;
    FTaskListenerEnumerate: TLccTaskListenersEnumerate;
    FTaskQueryFunctions: TLccTaskQueryFunctionArray;
    FTaskQuerySpeed: TLccTaskQuerySpeed;
    FTaskSearchTrain: TLccTaskSearchTrain;
    FTaskSetFunctions: TLccTaskSetFunctionArray;
    FTaskSetSpeedDir: TLccTaskSetSpeedDir;
    FTrainRoster: TLccTaskTrainRoster;

  protected
    property TaskSearchTrain: TLccTaskSearchTrain read FTaskSearchTrain write FTaskSearchTrain;
    property TaskControllerAttach: TLccTaskControllerAttach read FTaskControllerAttach write FTaskControllerAttach;
    property TaskControllerRelease: TLccTaskControllerRelease read FTaskControllerRelease write FTaskControllerRelease;
    property TaskControllerQuery: TLccTaskControllerQuery read FTaskControllerQuery write FTaskControllerQuery;
    property TaskListenerAttach: TLccTaskListenerAttach read FTaskListenerAttach write FTaskListenerAttach;
    property TaskListenerDetach: TLccTaskListenerDetach read FTaskListenerDetach write FTaskListenerDetach;
    property TaskListenerQueryAtIndex: TLccTaskListenerQueryAtIndex read FTaskListenerQueryAtIndex write FTaskListenerQueryAtIndex;
    property TaskListenerQueryCount: TLccTaskListenerQueryCount read FTaskListenerQueryCount write FTaskListenerQueryCount;
    property TaskListenerEnumerate: TLccTaskListenersEnumerate read FTaskListenerEnumerate;
    property TaskManagementReserveTrain: TLccTaskManagementReserveTrain read FTaskManagementReserveTrain write FTaskManagementReserveTrain;
    property TaskManagementReleaseTrain: TLccTaskManagementReleaseTrain read FTaskManagementReleaseTrain write FTaskManagementReleaseTrain;
    property TaskManagementNoOp: TLccTaskManagementNoOp read FTaskManagementNoOp write FTaskManagementNoOp;
    property TaskSetSpeedDir: TLccTaskSetSpeedDir read FTaskSetSpeedDir write FTaskSetSpeedDir;
    property TaskSetFunctions: TLccTaskSetFunctionArray read FTaskSetFunctions write FTaskSetFunctions;
    property TaskEmergencyStop: TLccTaskEmergencyStop read FTaskEmergencyStop write FTaskEmergencyStop;
    property TaskQuerySpeed:  TLccTaskQuerySpeed read FTaskQuerySpeed write FTaskQuerySpeed;
    property TaskQueryFunctions:  TLccTaskQueryFunctionArray read FTaskQueryFunctions write FTaskQueryFunctions;


    function GetCdiFile: string; override;
    procedure BeforeLogin; override;
    procedure HandleTractionSetSpeed(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); override;
    procedure HandleTractionSetFunction(var SourceMessage: TLccMessage; ListenerForwarded: Boolean); override;

  public
    property TrainRoster: TLccTaskTrainRoster read FTrainRoster;
    property OnSetSpeedListener: TOnSetSpeedListener read FOnSetSpeedListener write FOnSetSpeedListener;
    property OnSetFunctionListener: TOnSetFunctionListener read FOnSetFunctionListener write FOnSetFunctionListener;

    constructor Create(AOwner: TComponent; CdiXML: string); override;
    destructor Destroy; override;

    procedure AfterLogin; override;

    // Search Methods
    function SearchByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnTaskCallback): Boolean; overload;  // Callback -> TLccTaskSearchTrain
    function SearchByDccAddress(DccAddress: String; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnTaskCallback): Boolean; overload; // Callback -> TLccTaskSearchTrain

    // Controller Methods
    function ControllerAssign(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;                         // Callback -> TLccTaskControllerAttach
    function ControllerRelease(ATractionID, AControllerID: TNodeID): Boolean;                                   // Callback -> None
    function ControllerQueryActive(ACallback: TOnTaskCallback): Boolean;                                          // Callback -> TLccTaskControllerDetach

    // Listener Methods
    function ListenerAttach(ATractionID, AListenerToAttach: TNodeID; ReverseDir, LinkF0, LinkFn, Hide: Boolean; ACallback: TOnTaskCallback): Boolean; // Callback -> TLccTaskListenerAttach
    function ListenerDetach(ATractionID, AListenerToDetach: TNodeID; ACallback: TOnTaskCallback): Boolean;                                     // Callback -> TLccTaskListenerDetach
    function ListenerQueryAtIndex(ATractionID: TNodeID; ListenerIndex: Byte; ACallback: TOnTaskCallback): Boolean; // Callback -> TLccTaskListenerQueryAtIndex
    function ListenerQueryCount(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;                        // Callback -> TLccTaskListenerQueryCount
    function ListenerEnumerate(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;                         // Callback -> TLccTaskListenersEnumerate

    // Management Methods
    function ManagementReserveTrain(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;              // Callback -> TLccTaskManagementReserveTrain
    function ManagementReleaseTrain(ATractionID: TNodeID): Boolean;                                          // Callback -> None
    function ManagementNoOp(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;                      // Callback -> TLccTaskManagementNoOp

    // Control Methods
    function SetSpeedDir(ATractionID: TNodeID; Speed: single): Boolean; overload;                            // Callback -> None
    function SetSpeedDir(ATractionID: TNodeID; Speed: single; Forward: Boolean): Boolean; overload;         // Callback -> None
    function SetFunction(ATractionID: TNodeID; FunctionAddress: DWord; Value: Word): Boolean;                // Callback -> None
    function EmergencyStop(ATractionID: TNodeID): Boolean;                                                   // Callback -> None

    // Query Methods
    function QuerySpeedDir(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;                          // Callback -> TLccTaskQuerySpeed
    function QueryFunction(ATractionID: TNodeID; FunctionAddress: DWord; ACallback: TOnTaskCallback): Boolean;  // Callback -> TLccTaskQueryFunction
    function QueryAttachedListeners(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;                 // Callback -> TLccTaskListenersEnumerate

    procedure FindAllTrains;
    procedure ReleaseTrain;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TTrainListener }

function TTrainListener.GetF0Forward: Boolean;
begin
  Result := Flags and TRACTION_LISTENER_FLAG_LINK_F0 <> 0;
end;

function TTrainListener.GetFnForward: Boolean;
begin
  Result := Flags and TRACTION_LISTENER_FLAG_LINK_FN <> 0;
end;

function TTrainListener.GetHidden: Boolean;
begin
  Result := Flags and TRACTION_LISTENER_FLAG_HIDDEN <> 0;
end;

function TTrainListener.GetReverseDir: Boolean;
begin
  Result := Flags and TRACTION_LISTENER_FLAG_REVERSE_DIR <> 0;
end;

procedure TTrainListener.SetF0Forward(AValue: Boolean);
begin
  Flags := Flags or TRACTION_LISTENER_FLAG_LINK_F0;
end;

procedure TTrainListener.SetFnForward(AValue: Boolean);
begin
  Flags := Flags or TRACTION_LISTENER_FLAG_LINK_FN
end;

procedure TTrainListener.SetHidden(AValue: Boolean);
begin
  Flags := Flags or TRACTION_LISTENER_FLAG_HIDDEN;
end;

procedure TTrainListener.SetReverseDir(AValue: Boolean);
begin
  Flags := Flags or TRACTION_LISTENER_FLAG_REVERSE_DIR;
end;

constructor TTrainListener.Create;
begin
  inherited Create;
end;

constructor TTrainListener.Create(ANode: TNodeID; AFlags: Byte; AnIndex: Byte);
begin
  inherited Create;
  FNodeID := ANode;
  FFlags := AFlags;
  FIndex := AnIndex;
end;

procedure TTrainListener.CopyTo(ATarget: TTrainListener);
begin
  ATarget.NodeID := NodeID;
  ATarget.Flags := Flags;
  ATarget.Index := Index;
end;

{ TLccTaskListenersEnumerate }

constructor TLccTaskListenersEnumerate.Create(AnOwnerNode: TLccNode);
begin
  inherited Create(AnOwnerNode);
  TrainListener := TTrainListener.Create;
end;

destructor TLccTaskListenersEnumerate.Destroy;
begin
  FreeAndNil(FTrainListener);
  inherited Destroy;
end;

procedure TLccTaskListenersEnumerate.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  FiListener := 0;
  WorkerMessage.LoadTractionListenerQueryCount(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target));
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskListenersEnumerate.Process(SourceMessage: TLccMessage);
var
  i: Integer;
begin
  case SourceMessage.MTI of
     MTI_TRACTION_REPLY :
       begin
         case SourceMessage.DataArray[0] of
           TRACTION_LISTENER_CONFIG :
             begin
                case SourceMessage.DataArray[1] of
                   TRACTION_LISTENER_CONFIG_QUERY :
                     begin     // TODO Should RESERVE the node first
                       if SourceMessage.DataCount = 3 then
                       begin
                         FListenerCount := SourceMessage.TractionExtractListenerQueryNodeCountReply;
                         for i := 0 to ListenerCount - 1 do
                         begin
                           WorkerMessage.LoadTractionListenerQuery(OwnerNode.NodeID, OwnerNode.AliasID, SourceMessage.SourceID, SourceMessage.SourceAlias, i);
                           OwnerNode.SendMessage(WorkerMessage, OwnerNode);
                         end;
                       end else
                       begin
                          TrainListener.NodeID := SourceMessage.TractionExtractListenerQueryNodeIDReply;
                          TrainListener.Flags := SourceMessage.TractionExtractListenerQueryNodeFlagsReply;
                          TrainListener.Index := SourceMessage.TractionExtractListenerQueryNodeIndexReply;

                          if iListener < ListenerCount then
                          begin
                            if Assigned(Callback) then
                              Callback(Self);
                            Inc(FiListener);
                          end else
                          begin
                            Complete;
                            Reset;
                          end;
                       end;
                     end;
                end;
             end;
         end;
       end;
  end;

end;

{ TLccTaskTrainRoster }

{ TLccTaskTrainRoster }

function TLccTaskTrainRoster.GetCount: Integer;
begin
  Result := RosterList.Count
end;

function TLccTaskTrainRoster.GetTrain(Index: Integer): TTrainInfo;
begin
  Result := nil;
  if Index < Count then
    Result := TTrainInfo( RosterList[Index])
end;

constructor TLccTaskTrainRoster.Create(AnOwner: TLccNode);
begin
  inherited Create(AnOwner);
  FOwnerNode := AnOwner;
  RosterList := TList.Create;
end;

destructor TLccTaskTrainRoster.Destroy;
begin
  ListClearObjects(RosterList);
  inherited Destroy;
end;

function TLccTaskTrainRoster.Add(ATrain: TTrainInfo): TTrainInfo;
begin
  Result := ATrain;
  if IndexOf(ATrain) < 0 then
    RosterList.Add(ATrain);
end;

function TLccTaskTrainRoster.IndexOf(ATrain: TTrainInfo): Integer;
begin
  Result := RosterList.IndexOf(ATrain);
end;

procedure TLccTaskTrainRoster.Remove(ATrain: TTrainInfo; FreeTrain: Boolean);
begin
  RosterList.Remove(ATrain);

  if ATrain = ActiveTrain then
    TrainDeActivate;

  if FreeTrain then
    ATrain.Free;
end;

function TLccTaskTrainRoster.Delete(Index: Integer; FreeTrain: Boolean): TTrainInfo;
begin
  Result := nil;
  if (Index < RosterList.Count) then
  begin
    Result := TTrainInfo( RosterList[Index]);
    if Result = ActiveTrain then
      TrainDeActivate;

    RosterList.Delete(Index);

    if FreeTrain then
    begin
      Result := nil;
      Result.Free;
    end;
  end;
end;

function TLccTaskTrainRoster.FindByNodeID(ANodeID: TNodeID): TTrainInfo;
var
  i: Integer;
  LocalTrainInfo: TTrainInfo;
begin
  Result := nil;
  if EqualNodeID(ANodeID, NULL_NODE_ID, True) then Exit;

  for i := 0 to RosterList.Count - 1 do
  begin
    LocalTrainInfo := TTrainInfo( RosterList[i]);
    if EqualNodeID(LocalTrainInfo.NodeID, ANodeID, False) then
    begin
      Result := LocalTrainInfo;
      Break;
    end;
  end;
end;

function TLccTaskTrainRoster.TrainActivate(ANodeID: TNodeID): TTrainInfo;
begin
  Result := FindByNodeID(ANodeID);
  FActiveTrain := Result;
end;

procedure TLccTaskTrainRoster.TrainDeActivate;
begin
  FActiveTrain := nil;
end;

procedure TLccTaskTrainRoster.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);
end;

procedure TLccTaskTrainRoster.Process(SourceMessage: TLccMessage);
var
  LocalTrain: TTrainInfo;
begin
  inherited Process(SourceMessage);


  case SourceMessage.MTI of
    MTI_PROTOCOL_SUPPORT_REPLY :  // PIP
      begin
        LocalTrain := FindByNodeID(SourceMessage.SourceID);
        if Assigned(LocalTrain) then
        begin
          LocalTrain.PIP.LoadFromLccMessage(SourceMessage);
          if LocalTrain.PIP.SimpleNodeInfo then
          begin
            WorkerMessage.LoadSimpleNodeIdentInfoRequest(OwnerNode.NodeID, OwnerNode.AliasID, LocalTrain.NodeID, AliasServer.FindAlias(LocalTrain.NodeID));
            OwnerNode.SendMessage(WorkerMessage, OwnerNode);
          end else
          if LocalTrain.PIP.AbbreviatedConfigurationDefinitionInfo then
          begin
            // TODO
            // Can find info drop it
            Remove(LocalTrain, True);
          end else
          if LocalTrain.PIP.Datagram then
          begin
            // TODO
            Remove(LocalTrain, True);
          end else
          begin
            Remove(LocalTrain, True);
          end
        end;
      end;
    MTI_SIMPLE_NODE_INFO_REPLY :  // SNIP
      begin
        LocalTrain := FindByNodeID(SourceMessage.SourceID);
        if Assigned(LocalTrain) then
        begin
          LocalTrain.SNIP.LoadFromLccMessage(SourceMessage);
          if Assigned(Callback) then
            Callback(Self)
        end
      end;
    MTI_PRODUCER_IDENTIFIED_UNKNOWN,
    MTI_PRODUCER_IDENTIFIED_SET,
    MTI_PRODUCER_IDENTIFIED_CLEAR :      // ISTRAIN Event
      begin
        if SourceMessage.IsEqualEventID(EVENT_IS_TRAIN) then
        begin
         LocalTrain := FindByNodeID(SourceMessage.SourceID);
         if not Assigned(LocalTrain) then
         begin
           LocalTrain := TTrainInfo.Create(SourceMessage.SourceID);
           Add(LocalTrain);
         end;
         WorkerMessage.LoadProtocolIdentifyInquiry(OwnerNode.NodeID, OwnerNode.AliasID, LocalTrain.NodeID, AliasServer.FindAlias(LocalTrain.NodeID));
         OwnerNode.SendMessage(WorkerMessage, OwnerNode);
        end
      end;
  end; // case
end;

{ TTrainInfo }

function TTrainInfo.GetUserName: String;
begin
  if not SNIP.Valid then
    Result := DefaultName
  else
    Result := SNIP.UserName;
end;

constructor TTrainInfo.Create(ADefaultName: String = '');
begin
  FDefaultName := ADefaultName;
  FSNIP := TProtocolSimpleNodeInfo.Create;
  FPIP := TProtocolSupportedProtocols.Create;
end;

constructor TTrainInfo.Create(ANodeID: TNodeID; ADefaultName: String = '');
begin
  inherited Create;
  FDefaultName := ADefaultName;
  FSNIP := TProtocolSimpleNodeInfo.Create;
  FPIP := TProtocolSupportedProtocols.Create;
  FNodeID := ANodeID;
end;

destructor TTrainInfo.Destroy;
begin
  FreeAndNil(FSNIP);
  FreeAndNil(FPIP);
  inherited Destroy;
end;

function TTrainInfo.Equal(TestNodeID: TNodeID): Boolean;
begin
  Result := EqualNodeID(NodeID, TestNodeID, False);
end;

{ TLccTaskManagementReleaseTrain }

procedure TLccTaskManagementReleaseTrain.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionManage(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), False);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  Complete;
  Reset;
end;

{ TLccTaskEmergencyStop }

procedure TLccTaskEmergencyStop.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionEStop(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target));
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  Complete;
  Reset;
end;

{ TLccTaskSetFunction }

procedure TLccTaskSetFunction.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionSetFunction(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), Address, Value, ForwardedListenerMessage);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  Complete;
  Reset;
end;

{ TLccTaskSetSpeedDir }

procedure TLccTaskSetSpeedDir.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionSetSpeed(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), Speed, ForwardedListenerMessage);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  Complete;
  Reset;
end;

{ TLccTaskListenerQueryCount }

procedure TLccTaskListenerQueryCount.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionListenerQueryCount(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target));
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskListenerQueryCount.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_LISTENER_CONFIG then
      if SourceMessage.DataArray[1] = TRACTION_LISTENER_CONFIG_QUERY then
      begin
        FNodeCountReply := SourceMessage.TractionExtractListenerQueryNodeCountReply;
        Complete;
        Reset;
      end;
end;

{ TLccTaskManagementNoOp }

procedure TLccTaskManagementNoOp.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionManage(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), False);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  Complete;
  Reset;
end;

procedure TLccTaskManagementNoOp.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_MANAGE then
      if SourceMessage.DataArray[1] = TRACTION_MANAGE_HEARTBEAT then
      begin
        FTimeoutSecondsReply := SourceMessage.TractionExtractHeartBeatTimeoutReply;
        Complete;
         Reset;
      end
end;

{ TLccTaskListenerDetach }

procedure TLccTaskListenerDetach.Start(ATimeout: Integer);
var
  Flags: Byte;
begin
  inherited Start(ATimeout);

  Flags := 0;
  WorkerMessage.LoadTractionListenerDetach(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), Flags, Listener);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskListenerDetach.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_LISTENER_CONFIG then
      if SourceMessage.DataArray[1] = TRACTION_LISTENER_CONFIG_DETACH then
      begin
        FReplyCode := SourceMessage.TractionExtractListenerCodeReply;
        SourceMessage.ExtractDataBytesAsNodeID(3, FListenerNodeIDReply);
        Complete;
        Reset;
      end;
end;

{ TLccTaskListenerQueryAtIndex }

procedure TLccTaskListenerQueryAtIndex.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionListenerQuery(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), QueryIndex);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskListenerQueryAtIndex.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_LISTENER_CONFIG then
      if SourceMessage.DataArray[1] = TRACTION_LISTENER_CONFIG_QUERY then
      begin
        FReplyCode := SourceMessage.TractionExtractListenerQueryNodeCountReply;
        if SourceMessage.DataCount = 0 then
        begin
          // Index out of bounds
          FListenerNodeIDReply := NULL_NODE_ID;
        end else
        begin
          FListenerNodeIDReply := SourceMessage.TractionExtractListenerQueryNodeIDReply;
          FFlagsReply := SourceMessage.TractionExtractListenerQueryNodeFlagsReply;
          FIndexReply := SourceMessage.TractionExtractListenerQueryNodeIndexReply;
        end;
        Complete;
        Reset;
      end;
end;

{ TLccTaskManagementReserveTrain }

procedure TLccTaskManagementReserveTrain.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionManage(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), True);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskManagementReserveTrain.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_MANAGE then
      if SourceMessage.DataArray[1] = TRACTION_MANAGE_RESERVE then
      begin
        case SourceMessage.DataArray[2] of
          S_OK : begin
                   Complete;
                   Reset;
                 end
          else begin
             Error(1, 'Train Refused to be Reserved');
             Reset;
          end;
      end
  end;
end;

{ TLccTaskQuerySpeed }

procedure TLccTaskQuerySpeed.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionQuerySpeed(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target));
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskQuerySpeed.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_QUERY_SPEED then
    begin
      FActualSpeedReply := HalfToFloat( SourceMessage.TractionExtractActualSpeed);
      FActualSpeedReverseReply := HalfIsNegative( SourceMessage.TractionExtractActualSpeed);
      FSetSpeedReply := HalfToFloat(SourceMessage.TractionExtractSetSpeed);
      FSetSpeedReverseReply := HalfIsNegative( SourceMessage.TractionExtractSetSpeed);
      FCommandedSpeedReply := HalfToFloat(SourceMessage.TractionExtractCommandedSpeed);
      FCommandedSpeedReverseReply := HalfIsNegative( SourceMessage.TractionExtractCommandedSpeed);
      Complete;
      Reset;
    end;
end;

{ TLccTaskListenerAttach }

procedure TLccTaskListenerAttach.Start(ATimeout: Integer);
var
  Flags: Byte;
begin
  inherited Start(ATimeout);

  Flags := WorkerMessage.TractionEncodeListenerFlags(ReverseDir, LinkF0, FLinkFn, Hidden);
  WorkerMessage.LoadTractionListenerAttach(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), Flags, Listener);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskListenerAttach.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_LISTENER_CONFIG then
      if SourceMessage.DataArray[1] = TRACTION_LISTENER_CONFIG_ATTACH then
      begin
        FReplyCode := SourceMessage.TractionExtractListenerCodeReply;
        SourceMessage.ExtractDataBytesAsNodeID(3, FListenerNodeIDReply);
        Complete;
        Reset;
      end;
end;

{ TLccTaskQueryFunction }

procedure TLccTaskQueryFunction.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionQueryFunction(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), Address);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskQueryFunction.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_QUERY_FUNCTION then
    begin
      if Address = SourceMessage.TractionExtractFunctionAddress then
      begin
        FValueReply := SourceMessage.TractionExtractFunctionValue;
        Complete;
        Reset;
      end;
    end;
end;

{ TLccTaskControllerQuery }

procedure TLccTaskControllerQuery.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionControllerQuery(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target));
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);  // There is no reply from this
  Complete;
  Reset;
end;

procedure TLccTaskControllerQuery.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_CONTROLLER_CONFIG_REPLY then
      if SourceMessage.DataArray[1] = TRACTION_CONTROLLER_CONFIG_QUERY then
      begin
        FFlagsReply := SourceMessage.DataArray[2];
        SourceMessage.ExtractDataBytesAsNodeID(3, FActiveControllerReply);
        Complete;
        Reset;
      end;
end;

{ TLccTaskControllerRelease }

procedure TLccTaskControllerRelease.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionControllerRelease(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), ControllerNode);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);  // There is no reply from this
  Complete;
  Reset;
end;

{ TLccTaskControllerAttach }

procedure TLccTaskControllerAttach.Start(ATimeout: Integer);
begin
  inherited Start(ATimeout);

  WorkerMessage.LoadTractionControllerAssign(OwnerNode.NodeID, OwnerNode.AliasID, Target, AliasServer.FindAlias(Target), OwnerNode.NodeID);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskControllerAttach.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_CONTROLLER_CONFIG_REPLY then
      if SourceMessage.DataArray[1] = TRACTION_CONTROLLER_CONFIG_ASSIGN then
      begin
        case SourceMessage.DataArray[2] of
          S_OK : begin
                   Complete;
                   Reset;
                 end;
         TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                begin
                   Error(TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN, 'Train Refused to connect to the throttle');
                   Reset;
                end;
      end
  end;
end;

{ TLccTaskSearchTrain }

procedure TLccTaskSearchTrain.Start(ATimeout: Integer);
var
  TrackProtocolFlags: Word;
begin

  inherited Start(ATimeout);

  FTrainNodeIDReply := NULL_NODE_ID;

  TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or TRACTION_SEARCH_ALLOCATE_FORCE or TRACTION_SEARCH_TYPE_EXACT_MATCH or
                        TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;

  if IsLongAddress then
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG
  else
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT;

  case SpeedSteps of
     ldssDefault : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
     ldss14      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
     ldss28      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
     ldss128     : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
  end;

  SearchCriteria := 0;
  WorkerMessage.TractionSearchEncodeSearchString(DccAddress, TrackProtocolFlags, FSearchCriteria);
  WorkerMessage.LoadTractionSearch(OwnerNode.NodeID, OwnerNode.AliasID, SearchCriteria);
  OwnerNode.SendMessage(WorkerMessage, OwnerNode);
end;

procedure TLccTaskSearchTrain.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.IsCAN then
    Exit;
  if IsRunning then
  begin
    // For now we just grab the first one... future state is multiple replys may come
    case SourceMessage.MTI of
       MTI_PRODUCER_IDENTIFIED_UNKNOWN,
       MTI_PRODUCER_IDENTIFIED_SET,
       MTI_PRODUCER_IDENTIFIED_CLEAR:
         begin
            if SourceMessage.TractionIsSearchEvent then
            begin
              if SourceMessage.TractionSearchExtractSearchData = SearchCriteria then
              begin
                FTrainNodeIDReply := SourceMessage.SourceID;
                Complete;
                Reset;
              end
            end;
         end;
    end
  end;
end;

{ TLccTrainController }

procedure TLccTrainController.BeforeLogin;
begin
  ProtocolSupportedProtocols.ConfigurationDefinitionInfo := True;
  ProtocolSupportedProtocols.MemConfig := True;
  ProtocolSupportedProtocols.Datagram := True;
  ProtocolSupportedProtocols.EventExchange := True;
  ProtocolSupportedProtocols.SimpleNodeInfo := True;
  ProtocolSupportedProtocols.TractionControl := True;

  ProtocolEventsProduced.Add(EVENT_EMERGENCY_STOP, evs_InValid);

  ProtocolMemoryInfo.Add(MSI_CDI, True, True, True, 0, StreamCdi.Size);
  ProtocolMemoryInfo.Add(MSI_ALL, True, True, True, 0, $FFFFFFFF);
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
  ProtocolMemoryOptions.LowSpace := MSI_TRACTION_FUNCTION_CONFIG;
end;

procedure TLccTrainController.HandleTractionSetSpeed(var SourceMessage: TLccMessage; ListenerForwarded: Boolean);
var
  TrainInfo: TTrainInfo;
begin
  // We can be a listener so we could get notified
  if Assigned(OnSetSpeedListener) and ListenerForwarded then
  begin
    TrainInfo := TrainRoster.FindByNodeID(SourceMessage.SourceID);
    if Assigned(TrainInfo) then
      OnSetSpeedListener(TrainInfo,  Abs( HalfToFloat( SourceMessage.TractionExtractSetSpeed)), HalfIsNegative(SourceMessage.TractionExtractSetSpeed));
  end;
end;

procedure TLccTrainController.HandleTractionSetFunction(var SourceMessage: TLccMessage; ListenerForwarded: Boolean);
var
  TrainInfo: TTrainInfo;
begin
  // We can be a listener so we could get notified
  if Assigned(OnSetFunctionListener) and ListenerForwarded then
  begin
    TrainInfo := TrainRoster.FindByNodeID(SourceMessage.SourceID);
    if Assigned(TrainInfo) then
      OnSetFunctionListener(TrainInfo, SourceMessage.TractionExtractFunctionAddress, SourceMessage.TractionExtractFunctionValue);
  end;
end;


procedure TLccTrainController.FindAllTrains;
begin
  // Send global Producer Identify.  When done the TrainServer property will have the trains and the information
   WorkerMessage.LoadTractionIsTrainProducer(NodeID, AliasID, NULL_NODE_ID, 0);
   SendMessage(WorkerMessage, Self);
end;

procedure TLccTrainController.ReleaseTrain;
begin
//
end;

constructor TLccTrainController.Create(AOwner: TComponent; CdiXML: string);
var
  i: Integer;
begin
  inherited Create(AOwner, CdiXML);

  FTaskControllerAttach := TLccTaskControllerAttach.Create(Self);
  FTaskControllerQuery := TLccTaskControllerQuery.Create(Self);
  FTaskControllerRelease := TLccTaskControllerRelease.Create(Self);
  FTaskEmergencyStop := TLccTaskEmergencyStop.Create(Self);
  FTaskListenerAttach := TLccTaskListenerAttach.Create(Self);
  FTaskListenerDetach := TLccTaskListenerDetach.Create(Self);
  FTaskListenerQueryAtIndex := TLccTaskListenerQueryAtIndex.Create(Self);
  FTaskListenerQueryCount := TLccTaskListenerQueryCount.Create(Self);
  FTaskManagementNoOp := TLccTaskManagementNoOp.Create(Self);
  FTaskManagementReleaseTrain := TLccTaskManagementReleaseTrain.Create(Self);
  FTaskManagementReserveTrain := TLccTaskManagementReserveTrain.Create(Self);
  SetLength(FTaskQueryFunctions, MAX_FUNCTIONS);
  for i := 0 to MAX_FUNCTIONS - 1 do
    TaskQueryFunctions[i] := TLccTaskQueryFunction.Create(Self);
  FTaskQuerySpeed := TLccTaskQuerySpeed.Create(Self);
  FTaskSearchTrain := TLccTaskSearchTrain.Create(Self);
  SetLength(FTaskSetFunctions, MAX_FUNCTIONS);
  for i := 0 to MAX_FUNCTIONS - 1 do
    TaskSetFunctions[i] := TLccTaskSetFunction.Create(Self);
  FTaskSetSpeedDir := TLccTaskSetSpeedDir.Create(Self);
  FTrainRoster := TLccTaskTrainRoster.Create(Self);
  TrainRoster.Start(TIMEOUT_TASK_MESSSAGE_INFINITY);    // Alway run
  FTaskListenerEnumerate := TLccTaskListenersEnumerate.Create(Self);

  RegisterTask(TaskControllerAttach);
  RegisterTask(TaskControllerQuery);
  RegisterTask(TaskControllerRelease);
  RegisterTask(TaskListenerAttach);
  RegisterTask(TaskListenerDetach);
  RegisterTask(TaskListenerQueryAtIndex);
  RegisterTask(TaskListenerQueryCount);
  RegisterTask(TaskManagementNoOp);
  RegisterTask(TaskManagementReleaseTrain);
  RegisterTask(TaskManagementReserveTrain);
  for i := 0 to MAX_FUNCTIONS - 1 do
    RegisterTask(TaskQueryFunctions[i]);
  RegisterTask(TaskQuerySpeed);
  RegisterTask(TaskSearchTrain);
  for i := 0 to MAX_FUNCTIONS - 1 do
    RegisterTask(TaskSetFunctions[i]);
  RegisterTask(TaskSetSpeedDir);
  RegisterTask(TrainRoster);
  RegisterTask(TaskListenerEnumerate);
end;

destructor TLccTrainController.Destroy;
var
  i: Integer;
begin
  UnRegisterTask(TaskControllerAttach);
  UnRegisterTask(TaskControllerQuery);
  UnRegisterTask(TaskControllerRelease);
  UnRegisterTask(TaskListenerAttach);
  UnRegisterTask(TaskListenerDetach);
  UnRegisterTask(TaskListenerQueryAtIndex);
  UnRegisterTask(TaskListenerQueryCount);
  UnRegisterTask(TaskManagementNoOp);
  UnRegisterTask(TaskManagementReleaseTrain);
  UnRegisterTask(TaskManagementReserveTrain);
  for i := 0 to MAX_FUNCTIONS - 1 do
    UnRegisterTask(TaskQueryFunctions[i]);
  UnRegisterTask(TaskQuerySpeed);
  UnRegisterTask(TaskSearchTrain);
  for i := 0 to MAX_FUNCTIONS - 1 do
    UnRegisterTask(TaskSetFunctions[i]);
  UnRegisterTask(TaskSetSpeedDir);
  UnRegisterTask(TrainRoster);
  UnRegisterTask(TaskListenerEnumerate);

  FreeAndNil(FTaskControllerAttach);
  FreeAndNil(FTaskControllerQuery);
  FreeAndNil(FTaskControllerRelease);
  FreeAndNil(FTaskListenerAttach);
  FreeAndNil(FTaskListenerDetach);
  FreeAndNil(FTaskListenerQueryAtIndex);
  FreeAndNil(FTaskListenerQueryCount);
  FreeAndNil(FTaskManagementNoOp);
  FreeAndNil(FTaskManagementReleaseTrain);
  FreeAndNil(FTaskManagementReserveTrain);
  for i := 0 to MAX_FUNCTIONS - 1 do
    FreeAndNil(TaskQueryFunctions[i]);
  FreeAndNil(FTaskQuerySpeed);
  FreeAndNil(FTaskSearchTrain);
  for i := 0 to MAX_FUNCTIONS - 1 do
    FreeAndNil(FTaskSetFunctions[i]);
  FreeAndNil(FTaskSetSpeedDir);
  FreeAndNil(FTrainRoster);
  FreeAndNil(FTaskListenerEnumerate);

  inherited Destroy;
end;

procedure TLccTrainController.AfterLogin;
begin
  inherited AfterLogin;
 // FindAllTrains;
end;


function TLccTrainController.SearchByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnTaskCallback): Boolean;
begin
  Result := SearchByDccAddress(IntToStr( DccAddress), IsLongAddress, SpeedSteps, ACallback);
end;

function TLccTrainController.SearchByDccAddress(DccAddress: String; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskSearchTrain.IsIdle then
  begin
    TaskSearchTrain.DccAddress := DccAddress;
    TaskSearchTrain.IsLongAddress := IsLongAddress;
    TaskSearchTrain.SpeedSteps := SpeedSteps;
    TaskSearchTrain.Callback := ACallback;
    TaskSearchTrain.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ControllerAssign(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskControllerAttach.IsIdle then
  begin
    TaskControllerAttach.Callback := ACallback;
    TaskControllerAttach.Target := ATractionID;
    TaskControllerAttach.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ControllerRelease(ATractionID, AControllerID: TNodeID): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskControllerRelease.IsIdle then
  begin
    TaskControllerRelease.Target := ATractionID;
    TaskControllerRelease.ControllerNode := AControllerID;
    TaskControllerRelease.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ControllerQueryActive(ACallback: TOnTaskCallback): Boolean;
begin
   Result := False;
  if TaskControllerQuery.IsIdle then
  begin
    TaskControllerQuery.Callback := ACallback;
    TaskControllerQuery.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ListenerAttach(ATractionID, AListenerToAttach: TNodeID; ReverseDir, LinkF0, LinkFn, Hide: Boolean; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) or NullNodeID(AListenerToAttach) then
    Exit;

  if TaskListenerAttach.IsIdle then
  begin
    TaskListenerAttach.ReverseDir := ReverseDir;
    TaskListenerAttach.LinkF0 := LinkF0;
    TaskListenerAttach.FLinkFn := LinkFn;
    TaskListenerAttach.Hidden := Hide;
    TaskListenerAttach.Listener := AListenerToAttach;
    TaskListenerAttach.Target := ATractionID;
    TaskListenerAttach.Callback := ACallback;
    TaskListenerAttach.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ListenerDetach(ATractionID, AListenerToDetach: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) or NullNodeID(AListenerToDetach) then
    Exit;

  if TaskListenerDetach.IsIdle then
  begin
    TaskListenerDetach.Listener := AListenerToDetach;
    TaskListenerDetach.Target := ATractionID;
    TaskListenerDetach.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ListenerQueryAtIndex(ATractionID: TNodeID; ListenerIndex: Byte; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskListenerQueryAtIndex.IsIdle then
  begin
    TaskListenerQueryAtIndex.Target := ATractionID;
    TaskListenerQueryAtIndex.Callback := ACallback;
    TaskListenerQueryAtIndex.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ListenerQueryCount(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskListenerQueryCount.IsIdle then
  begin
    TaskListenerQueryCount.Target := ATractionID;
    TaskListenerQueryCount.Callback := ACallback;
    TaskListenerQueryCount.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ListenerEnumerate(ATractionID: TNodeID;ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskListenerEnumerate.IsIdle then
  begin
    TaskListenerEnumerate.Target := ATractionID;
    TaskListenerEnumerate.Callback := ACallback;
    TaskListenerEnumerate.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ManagementReserveTrain(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskManagementReserveTrain.IsIdle then
  begin
    TaskManagementReserveTrain.Target := ATractionID;
    TaskManagementReserveTrain.Callback := ACallback;
    TaskManagementReserveTrain.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ManagementReleaseTrain(ATractionID: TNodeID): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskManagementReleaseTrain.IsIdle then
  begin
    TaskManagementReleaseTrain.Target := ATractionID;
    TaskManagementReleaseTrain.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ManagementNoOp(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskManagementNoOp.IsIdle then
  begin
    TaskManagementNoOp.Target := ATractionID;
    TaskManagementNoOp.Callback := ACallback;
    TaskManagementNoOp.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.SetSpeedDir(ATractionID: TNodeID; Speed: single): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskSetSpeedDir.IsIdle then
  begin
    TaskSetSpeedDir.Target := ATractionID;
    TaskSetSpeedDir.Speed := Speed;
    TaskSetSpeedDir.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.SetSpeedDir(ATractionID: TNodeID; Speed: single; Forward: Boolean): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if not Forward then Speed := -Speed;
  Result := SetSpeedDir(ATractionID, Speed);
end;

function TLccTrainController.SetFunction(ATractionID: TNodeID; FunctionAddress: DWord; Value: Word): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskSetFunctions[FunctionAddress].IsIdle then
  begin
    TaskSetFunctions[FunctionAddress].Address := FunctionAddress;
    TaskSetFunctions[FunctionAddress].Value := Value;
    TaskSetFunctions[FunctionAddress].Target := ATractionID;
    TaskSetFunctions[FunctionAddress].Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.EmergencyStop(ATractionID: TNodeID): Boolean;
begin
  Result := False;

  if NullNodeID(ATractionID) then
    Exit;
  if TaskEmergencyStop.IsIdle then
  begin
    TaskEmergencyStop.Target := ATractionID;
    TaskEmergencyStop.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.QuerySpeedDir(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskQuerySpeed.IsIdle then
  begin
    TaskQuerySpeed.Target := ATractionID;
    TaskQuerySpeed.Callback := ACallback;
    TaskQuerySpeed.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.QueryFunction(ATractionID: TNodeID; FunctionAddress: DWord; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskQueryFunctions[FunctionAddress].IsIdle then
  begin
    TaskQueryFunctions[FunctionAddress].Address := FunctionAddress;
    TaskQueryFunctions[FunctionAddress].Target := ATractionID;
    TaskQueryFunctions[FunctionAddress].Callback := ACallback;
    TaskQueryFunctions[FunctionAddress].Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.QueryAttachedListeners(ATractionID: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if NullNodeID(ATractionID) then
    Exit;

  if TaskListenerEnumerate.IsIdle then
  begin
    TaskListenerEnumerate.Target := ATractionID;
    TaskListenerEnumerate.Callback := ACallback;
    TaskListenerEnumerate.Start(TIMEOUT_TASK_MESSAGES);
    Result := True
  end;
end;

function TLccTrainController.GetCdiFile: string;
begin
  Result := CDI_XML_CONTROLLER;
end;

end.

