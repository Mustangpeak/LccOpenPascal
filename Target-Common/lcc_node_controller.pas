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

  { TAssignedTrainState }

  TAssignedTrainState = class
  private
    FOwner: TLccTrainController;
    FSearchCriteriaPending: DWORD;
    FTrain: TLccAliasMappingRec;
    FSearchCriteria: DWORD;
    FWorkerMessage: TLccMessage;
  public
    property Owner: TLccTrainController read FOwner write FOwner;
    property Train: TLccAliasMappingRec read FTrain write FTrain;
    property SearchCriteriaPending: DWORD read FSearchCriteriaPending write FSearchCriteriaPending;
  //  property SearchCriteria: DWORD read FSearchCriteria write FSearchCriteria;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    constructor Create;
    destructor Destroy; override;

    procedure AcceptSearchCriteriaPending;
    procedure ClearTrain;
    function IsAssigned: Boolean;
    function IsEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
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
    FQueryIndex: Byte;
    FReplyCode: Byte;
    FListenerNodeIDReply: TNodeID;
  public
    property QueryIndex: Byte read FQueryIndex write FQueryIndex;

    // Replies
    property ReplyCode: Byte read FReplyCode;
    property ListenerNodeIDReply: TNodeID read FListenerNodeIDReply;

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
    FCommandedSpeedReply: single;
    FSetSpeedReply: single;
  public

    // Replies
    property ActualSpeedReply: single read FActualSpeedReply;
    property SetSpeedReply: single read FSetSpeedReply;
    property CommandedSpeedReply: single read FCommandedSpeedReply;

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

  // ******************************************************************************

  { TLccTrainController }

  TLccTrainController = class(TLccNode)
  private
    FAssignedTrain: TAssignedTrainState;
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
    FTaskQueryFunctions: TLccTaskQueryFunctionArray;
    FTaskQuerySpeed: TLccTaskQuerySpeed;
    FTaskSearchTrain: TLccTaskSearchTrain;
    FTaskSetFunctions: TLccTaskSetFunctionArray;
    FTaskSetSpeedDir: TLccTaskSetSpeedDir;

  protected
    property TaskSearchTrain: TLccTaskSearchTrain read FTaskSearchTrain write FTaskSearchTrain;
    property TaskControllerAttach: TLccTaskControllerAttach read FTaskControllerAttach write FTaskControllerAttach;
    property TaskControllerRelease: TLccTaskControllerRelease read FTaskControllerRelease write FTaskControllerRelease;
    property TaskControllerQuery: TLccTaskControllerQuery read FTaskControllerQuery write FTaskControllerQuery;
    property TaskListenerAttach: TLccTaskListenerAttach read FTaskListenerAttach write FTaskListenerAttach;
    property TaskListenerDetach: TLccTaskListenerDetach read FTaskListenerDetach write FTaskListenerDetach;
    property TaskListenerQueryAtIndex: TLccTaskListenerQueryAtIndex read FTaskListenerQueryAtIndex write FTaskListenerQueryAtIndex;
    property TaskListenerQueryCount: TLccTaskListenerQueryCount read FTaskListenerQueryCount write FTaskListenerQueryCount;
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

  public

    property AssignedTrain: TAssignedTrainState read FAssignedTrain write FAssignedTrain;



    constructor Create(AOwner: TComponent; CdiXML: string); override;
    destructor Destroy; override;

    procedure AfterLogin; override;

    // Search Methods
    function SearchByDccAddress(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnTaskCallback): Boolean;   // Callback -> TLccTaskSearchTrain
    function SearchByDccAddress(DccAddress: String; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnTaskCallback): Boolean; // Callback -> TLccTaskSearchTrain

    // Controller Methods
    function ControllerAssign(ATrainID: TNodeID; ACallback: TOnTaskCallback): Boolean;                            // Callback -> TLccTaskControllerAttach
    function ControllerRelease(ATrainID: TNodeID): Boolean;                                                       // Callback -> None
    function ControllerQueryActive(ACallback: TOnTaskCallback): Boolean;                                          // Callback -> TLccTaskControllerDetach

    // Listener Methods
    function ListenerAttach(Listener, AListenerToAttach: TNodeID; ReverseDir, LinkF0, LinkFn, Hide: Boolean; ACallback: TOnTaskCallback): Boolean; // Callback -> TLccTaskListenerAttach
    function ListenerDetach(Listener, AListenerToDetach: TNodeID): Boolean;                                     // Callback -> TLccTaskListenerDetach
    function ListenerQueryAtIndex(Listener: TNodeID; ListenerIndex: Byte; ACallback: TOnTaskCallback): Boolean; // Callback -> TLccTaskListenerQueryAtIndex
    function ListenerQueryCount(Listener: TNodeID; ACallback: TOnTaskCallback): Boolean;                        // Callback -> TLccTaskListenerQueryCount

    // Management Methods
    function ManagementReserveTrain(Listener: TNodeID; ACallback: TOnTaskCallback): Boolean;              // Callback -> TLccTaskManagementReserveTrain
    function ManagementReleaseTrain(Listener: TNodeID): Boolean;                                          // Callback -> None
    function ManagementNoOp(Listener: TNodeID; ACallback: TOnTaskCallback): Boolean;                      // Callback -> TLccTaskManagementNoOp

    // Control Methods
    function SetSpeedDir(Listener: TNodeID; Speed: single): Boolean; overload;                            // Callback -> None
    function SetSpeedDir(Listener: TNodeID; Speed: Integer; Forward: Boolean): Boolean; overload;         // Callback -> None
    function SetFunction(Listener: TNodeID; FunctionAddress: DWord; Value: Word): Boolean;                // Callback -> None
    function EmergencyStop(Listener: TNodeID): Boolean;                                                   // Callback -> None

    // Query Methods
    function QuerySpeedDir(Listener: TNodeID; ACallback: TOnTaskCallback): Boolean;                          // Callback -> TLccTaskQuerySpeed
    function QueryFunction(Listener: TNodeID; FunctionAddress: DWord; ACallback: TOnTaskCallback): Boolean;  // Callback -> TLccTaskQueryFunction

    procedure FindAllTrains;
    procedure ReleaseTrain;
    function IsTrainAssigned: Boolean;
  end;

  TLccTrainControllerClass = class of TLccTrainController;


implementation

{ TLccTaskManagementReleaseTrain }

procedure TLccTaskManagementReleaseTrain.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionManage(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, False);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end;
end;

{ TLccTaskEmergencyStop }

procedure TLccTaskEmergencyStop.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionEStop(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end;
end;

{ TLccTaskSetFunction }

procedure TLccTaskSetFunction.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionSetFunction(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, Address, Value, ForwardedListenerMessage);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end;
end;

{ TLccTaskSetSpeedDir }

procedure TLccTaskSetSpeedDir.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionSetSpeed(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, Speed, ForwardedListenerMessage);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end;
end;

{ TLccTaskListenerQueryCount }

procedure TLccTaskListenerQueryCount.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionListenerQueryCount(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
end;

procedure TLccTaskListenerQueryCount.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_LISTENER_CONFIG then
      if SourceMessage.DataArray[1] = TRACTION_LISTENER_CONFIG_QUERY then
      begin
        FNodeCountReply := SourceMessage.TractionExtractListenerQueryNodeCountReply;
        Complete;
        if Assigned(Callback) then
          Callback(Self);
        Reset;
      end;
end;

{ TLccTaskManagementNoOp }

procedure TLccTaskManagementNoOp.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionManage(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, False);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end;
end;

procedure TLccTaskManagementNoOp.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_MANAGE then
      if SourceMessage.DataArray[1] = TRACTION_MANAGE_HEARTBEAT then
      begin
        FTimeoutSecondsReply := SourceMessage.TractionExtractHeartBeatTimeoutReply;
        Complete;
         if Assigned(Callback) then
           Callback(Self);
         Reset;
      end
end;

{ TLccTaskListenerDetach }

procedure TLccTaskListenerDetach.Start(ATimeout: Integer);
var
  Alias: Word;
  Flags: Byte;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    Flags := 0;
    WorkerMessage.LoadTractionListenerDetach(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, Flags, Listener);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
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
        if Assigned(Callback) then
          Callback(Self);
        Reset;
      end;
end;

{ TLccTaskListenerQueryAtIndex }

procedure TLccTaskListenerQueryAtIndex.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionListenerQuery(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, QueryIndex);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
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
        end;
        Complete;
          if Assigned(Callback) then
            Callback(Self);
        Reset;
      end;
end;

{ TLccTaskManagementReserveTrain }

procedure TLccTaskManagementReserveTrain.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionManage(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, True);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
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
                   if Assigned(Callback) then
                     Callback(Self);
                   Reset;
                 end
          else begin
             Error(1, 'Train Refused to be Reserved');
             if Assigned(Callback) then
               Callback(Self);
             Reset;
          end;
      end
  end;
end;

{ TLccTaskQuerySpeed }

procedure TLccTaskQuerySpeed.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionQuerySpeed(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
end;

procedure TLccTaskQuerySpeed.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_QUERY_SPEED then
    begin
      FActualSpeedReply := HalfToFloat( SourceMessage.TractionExtractActualSpeed);
      FSetSpeedReply := HalfToFloat(SourceMessage.TractionExtractSetSpeed);
      FCommandedSpeedReply := HalfToFloat(SourceMessage.TractionExtractCommandedSpeed);
      Complete;
      if Assigned(Callback) then
        Callback(Self);
      Reset;
    end;
end;

{ TLccTaskListenerAttach }

procedure TLccTaskListenerAttach.Start(ATimeout: Integer);
var
  Alias: Word;
  Flags: Byte;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    Flags := WorkerMessage.TractionEncodeListenerFlags(ReverseDir, LinkF0, FLinkFn, Hidden);
    WorkerMessage.LoadTractionListenerAttach(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, Flags, Listener);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
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
        if Assigned(Callback) then
          Callback(Self);
        Reset;
      end;
end;

{ TLccTaskQueryFunction }

procedure TLccTaskQueryFunction.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionQueryFunction(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, Address);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
end;

procedure TLccTaskQueryFunction.Process(SourceMessage: TLccMessage);
begin
  if SourceMessage.MTI = MTI_TRACTION_REPLY then
    if SourceMessage.DataArray[0] = TRACTION_QUERY_FUNCTION then
    begin
      FValueReply := SourceMessage.TractionExtractFunctionValue;
      Complete;
      if Assigned(Callback) then
        Callback(Self);
      Reset;
    end;
end;

{ TLccTaskControllerQuery }

procedure TLccTaskControllerQuery.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionControllerQuery(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);  // There is no reply from this
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end else
  begin
    Error(1, 'Unable to resolve an Alias');
  end;
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
        if Assigned(Callback) then
          Callback(Self);
        Reset;
      end;
end;

{ TLccTaskControllerRelease }

procedure TLccTaskControllerRelease.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionControllerRelease(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, ControllerNode);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);  // There is no reply from this
    Complete;
    if Assigned(Callback) then
      Callback(Self);
    Reset;
  end else
  begin
    Error(1, 'Unable to resolve an Alias');
  end;
end;

{ TLccTaskControllerAttach }

procedure TLccTaskControllerAttach.Start(ATimeout: Integer);
var
  Alias: Word;
begin
  inherited Start(ATimeout);

  Alias := AliasServer.FindAlias(Target);
  if Alias <> 0 then
  begin
    WorkerMessage.LoadTractionControllerAssign(OwnerNode.NodeID, OwnerNode.AliasID, Target, Alias, OwnerNode.NodeID);
    OwnerNode.SendMessage(WorkerMessage, OwnerNode);
  end;
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
                   if Assigned(Callback) then
                     Callback(Self);
                   Reset;
                 end;
         TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                begin
                   Error(TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN, 'Train Refused to connect to the throttle');
                   if Assigned(Callback) then
                     Callback(Self);
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
                if Assigned(Callback) then
                  Callback(Self);
                Reset;
              end
            end;
         end;
    end
  end;
end;

{ TAssignedTrainState }

constructor TAssignedTrainState.Create;
begin
  FWorkerMessage := TLccMessage.Create;
end;

destructor TAssignedTrainState.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;


procedure TAssignedTrainState.AcceptSearchCriteriaPending;
begin
  FSearchCriteria := SearchCriteriaPending;
  FSearchCriteriaPending := 0;
end;

procedure TAssignedTrainState.ClearTrain;
begin
  FTrain.NodeID := NULL_NODE_ID;
  FTrain.Alias := 0;
  FSearchCriteria := 0;
  FSearchCriteriaPending := 0;
end;

function TAssignedTrainState.IsAssigned: Boolean;
begin
  Result := not NullNodeID(FTrain.NodeID) or (FTrain.Alias <> 0)
end;

function TAssignedTrainState.IsEqual(ATestNodeID: TNodeID; ATestNodeAlias: Word): Boolean;
begin
  Result := EqualNodeID(ATestNodeID, FTrain.NodeID, False) or (ATestNodeAlias = FTrain.Alias);
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

function TLccTrainController.IsTrainAssigned: Boolean;
begin
  Result := AssignedTrain.IsAssigned;
end;

constructor TLccTrainController.Create(AOwner: TComponent; CdiXML: string);
var
  i: Integer;
begin
  inherited Create(AOwner, CdiXML);

  FAssignedTrain := TAssignedTrainState.Create;
  AssignedTrain.Owner := Self;

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

function TLccTrainController.ControllerAssign(ATrainID: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskControllerAttach.IsIdle then
  begin
    TaskControllerAttach.Callback := ACallback;
    TaskControllerAttach.Target := ATrainID;
    TaskControllerAttach.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ControllerRelease(ATrainID: TNodeID): Boolean;
begin
  Result := False;
  if TaskControllerRelease.IsIdle then
  begin
    TaskControllerRelease.Target := ATrainID;
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

function TLccTrainController.ListenerAttach(Listener, AListenerToAttach: TNodeID; ReverseDir, LinkF0, LinkFn, Hide: Boolean; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskListenerAttach.IsIdle then
  begin
    TaskListenerAttach.ReverseDir := ReverseDir;
    TaskListenerAttach.LinkF0 := LinkF0;
    TaskListenerAttach.FLinkFn := LinkFn;
    TaskListenerAttach.Hidden := Hide;
    TaskListenerAttach.Listener := AListenerToAttach;
    TaskListenerAttach.Target := Listener;
    TaskListenerAttach.Callback := ACallback;
    TaskListenerAttach.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ListenerDetach(Listener, AListenerToDetach: TNodeID): Boolean;
begin
  Result := False;
  if TaskListenerDetach.IsIdle then
  begin
    TaskListenerDetach.Listener := AListenerToDetach;
    TaskListenerDetach.Target := Listener;
    TaskListenerDetach.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ListenerQueryAtIndex(Listener: TNodeID; ListenerIndex: Byte; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskListenerQueryAtIndex.IsIdle then
  begin
    TaskListenerQueryAtIndex.Target := Listener;
    TaskListenerQueryAtIndex.Callback := ACallback;
    TaskListenerQueryAtIndex.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ListenerQueryCount(Listener: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskListenerQueryCount.IsIdle then
  begin
    TaskListenerQueryCount.Target := Listener;
    TaskListenerQueryCount.Callback := ACallback;
    TaskListenerQueryCount.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ManagementReserveTrain(Listener: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskManagementReserveTrain.IsIdle then
  begin
    TaskManagementReserveTrain.Target := Listener;
    TaskManagementReserveTrain.Callback := ACallback;
    TaskManagementReserveTrain.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ManagementReleaseTrain(Listener: TNodeID): Boolean;
begin
  Result := False;
  if TaskManagementReleaseTrain.IsIdle then
  begin
    TaskManagementReleaseTrain.Target := Listener;
    TaskManagementReleaseTrain.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.ManagementNoOp(Listener: TNodeID; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskManagementNoOp.IsIdle then
  begin
    TaskManagementNoOp.Target := Listener;
    TaskManagementNoOp.Callback := ACallback;
    TaskManagementNoOp.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.SetSpeedDir(Listener: TNodeID; Speed: single): Boolean;
begin
  Result := False;
  if TaskSetSpeedDir.IsIdle then
  begin
    TaskSetSpeedDir.Target := Listener;
    TaskSetSpeedDir.Speed := Speed;
    TaskSetSpeedDir.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.SetSpeedDir(Listener: TNodeID; Speed: Integer; Forward: Boolean): Boolean;
begin
  if not Forward then Speed := -Speed;
  Result := SetSpeedDir(Listener, Speed);
end;

function TLccTrainController.SetFunction(Listener: TNodeID; FunctionAddress: DWord; Value: Word): Boolean;
begin
  Result := False;
  if TaskSetFunctions[FunctionAddress].IsIdle then
  begin
    TaskSetFunctions[FunctionAddress].Address := FunctionAddress;
    TaskSetFunctions[FunctionAddress].Value := Value;
    TaskSetFunctions[FunctionAddress].Target := Listener;
    TaskSetFunctions[FunctionAddress].Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.EmergencyStop(Listener: TNodeID): Boolean;
begin
  Result := False;
  if TaskEmergencyStop.IsIdle then
  begin
    TaskEmergencyStop.Target := Listener;
    TaskEmergencyStop.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.QuerySpeedDir(Listener: TNodeID;
  ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskQuerySpeed.IsIdle then
  begin
    TaskQuerySpeed.Target := Listener;
    TaskQuerySpeed.Callback := ACallback;
    TaskQuerySpeed.Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.QueryFunction(Listener: TNodeID; FunctionAddress: DWord; ACallback: TOnTaskCallback): Boolean;
begin
  Result := False;
  if TaskQueryFunctions[FunctionAddress].IsIdle then
  begin
    TaskQueryFunctions[FunctionAddress].Address := FunctionAddress;
    TaskQueryFunctions[FunctionAddress].Target := Listener;
    TaskQueryFunctions[FunctionAddress].Callback := ACallback;
    TaskQueryFunctions[FunctionAddress].Start(TIMEOUT_TASK_MESSAGES);
    Result := True;
  end;
end;

function TLccTrainController.GetCdiFile: string;
begin
  Result := CDI_XML_CONTROLLER;
end;

end.

