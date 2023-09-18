unit lcc_node_traindatabase;

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
  lcc_protocol_utilities,
  lcc_defines,
  lcc_node_messages,
  lcc_utilities,
  lcc_node,
  lcc_base_classes,
  lcc_alias_server,
  lcc_train_server;


type
  TLccEngineSearchAndAllocateTrain = class;
  TLccEngineSearchTrain = class;
  TLccEngineRetrieveListenerInfo = class;


  TOnLccEngineSearchAndAllocateTrainCallback = procedure(EngineAllocateTrain: TLccEngineSearchAndAllocateTrain; AEngineSearchTrain: TLccEngineSearchTrain) of object;
  TLccEngineSearchTrainCallback = procedure(EngineSearchTrain: TLccEngineSearchTrain; ASearchTrain: TLccSearchTrainObject) of object;
  TOnLccEngineRetrieveListenerInfo = procedure(EngineListenerInfo: TLccEngineRetrieveListenerInfo) of object;


  { TLccEngineSearchTrain }

  TLccEngineSearchTrain = class(TLccEngineBase)
  private
    FCallback: TLccEngineSearchTrainCallback;
    FSearchCriteriaPending: DWORD;
    FSearchTrain: TLccSearchTrainObject;
  protected
    property Callback: TLccEngineSearchTrainCallback read FCallback write FCallback;
    property SearchCriteriaPending: DWORD read FSearchCriteriaPending;
    procedure HandleProducerIdentified(SourceMessage: TLccMessage);
  public
    property SearchTrain: TLccSearchTrainObject read FSearchTrain write FSearchTrain;
    constructor Create(AnOwner: TLccNode); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Reset; override;
    procedure Process(SourceMessage: TLccMessage); override;
    procedure Assign(DccAddress: Word; AnIsLongAddress: Boolean; ASpeedStep: TLccDccSpeedStep; ACallback: TLccEngineSearchTrainCallback); overload;
    procedure Assign(ASearchString: string; AnIsLongAddress: Boolean; ASpeedStep: TLccDccSpeedStep; ACallback: TLccEngineSearchTrainCallback); overload;
    procedure Assign(ASearchString: string; ATrackProtocolFlags: Word; ACallback: TLccEngineSearchTrainCallback); overload;
    procedure Assign(ASearchString: string; ASearchCriteria: DWORD; ACallback: TLccEngineSearchTrainCallback); overload;
  end;

  { TLccEngineSearchAndAllocateTrain }

  TLccEngineSearchAndAllocateTrain = class(TLccEngineBase)
  private
    FCallback: TOnLccEngineSearchAndAllocateTrainCallback;
    FEngineSearchTrain: TLccEngineSearchTrain;
  protected
    property Callback: TOnLccEngineSearchAndAllocateTrainCallback read FCallback write FCallback;
    procedure SearchTrainCallback(AnEngineSearchTrain: TLccEngineSearchTrain; ASearchTrain: TLccSearchTrainObject);
  public
    property EngineSearchTrain: TLccEngineSearchTrain read FEngineSearchTrain write FEngineSearchTrain;

    constructor Create(AnOwner: TLccNode); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Stop; override;
    procedure Reset; override;
    procedure Process(SourceMessage: TLccMessage); override;
    procedure Assign(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnLccEngineSearchAndAllocateTrainCallback); overload;
    procedure Assign(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnLccEngineSearchAndAllocateTrainCallback); overload;
    procedure Assign(SearchString: string; TrackProtocolFlags: Word; ACallback: TOnLccEngineSearchAndAllocateTrainCallback); overload;
  end;

  { TLccEngineRetrieveListenerInfo }

  TLccEngineRetrieveListenerInfo = class(TLccEngineBase)
  private
    FListenerList: TList;
    FCallback: TOnLccEngineRetrieveListenerInfo;
    FTargetNodeID: TNodeID;
    FTargetAlias: Word;
    FQueryNodeIndex: Byte;
    FQueryNodeCount: Byte;
    function GetListener(Index: Integer): TLccListenerObject;
    function GetCount: Byte;
  protected
    property ListenerList: TList read FListenerList write FListenerList;
    property Callback: TOnLccEngineRetrieveListenerInfo read FCallback write FCallback;
    property TargetNodeID: TNodeID read FTargetNodeID write FTargetNodeID;
    property TargetAlias: Word read FTargetAlias write FTargetAlias;
    property QueryNodeCount: Byte read FQueryNodeCount write FQueryNodeCount;
    property QueryNodeIndex: Byte read FQueryNodeIndex write FQueryNodeIndex;

    procedure ClearListeners;
    procedure QueryNextListener;

  public
    property Count: Byte read GetCount;
    property Listeners[Index: Integer]: TLccListenerObject read GetListener;

    constructor Create(AnOwner: TLccNode); override;
    destructor Destroy; override;
    procedure Start; override;
    procedure Reset; override;
    procedure Process(SourceMessage: TLccMessage); override;
    procedure Assign(ATargetNodeID: TNodeID; ATargetAliasID: Word; ACallback: TOnLccEngineRetrieveListenerInfo); // AMemorySpace = MSI_xxxx constants
  end;

  { TLccTractionServerNode }

  TLccTractionServerNode = class(TLccNode)
  private
    FEngineSearchAndAllocateTrain: TLccEngineSearchAndAllocateTrain;
    FEngineSearchTrain: TLccEngineSearchTrain;
    FTractionServer: TLccTractionServer;
    FEngineListenerInfo: TLccEngineRetrieveListenerInfo;
  protected
    procedure DoMemorySpaceReadEngineDone(MemoryReadEngine: TLccEngineMemorySpaceAccess); override;
  public
    property EngineSearchAndAllocateTrain: TLccEngineSearchAndAllocateTrain read FEngineSearchAndAllocateTrain write FEngineSearchAndAllocateTrain;
    property EngineCreateTrain: TLccEngineSearchTrain read FEngineSearchTrain write FEngineSearchTrain;
    property EngineListenerInfo: TLccEngineRetrieveListenerInfo read FEngineListenerInfo write FEngineListenerInfo;
    property TractionServer: TLccTractionServer read FTractionServer;

    constructor Create(ANodeManager: {$IFDEF LCC_DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean); override;
    destructor Destroy; override;
    function ProcessMessageLCC(SourceMessage: TLccMessage): Boolean; override;
  end;
implementation


{ TLccEngineRetrieveListenerInfo }

procedure TLccEngineRetrieveListenerInfo.Assign(ATargetNodeID: TNodeID; ATargetAliasID: Word; ACallback: TOnLccEngineRetrieveListenerInfo);
begin
  FCallback := ACallback;
  FTargetAlias := ATargetAliasID;
  FTargetNodeID := ATargetNodeID;
end;

procedure TLccEngineRetrieveListenerInfo.ClearListeners;
var
  i: Integer;
begin
  try
    for i := 0 to FListenerList.Count - 1 do
      TObject(ListenerList[i]).Free;
  finally
    ListenerList.Clear;
  end;
end;

constructor TLccEngineRetrieveListenerInfo.Create(AnOwner: TLccNode);
begin
  inherited;
  FListenerList := TList.Create;
end;

destructor TLccEngineRetrieveListenerInfo.Destroy;
begin
  ClearListeners;
  FreeAndNil(FListenerList);
  inherited;
end;

function TLccEngineRetrieveListenerInfo.GetCount: Byte;
begin
  Result := ListenerList.Count;
end;

function TLccEngineRetrieveListenerInfo.GetListener(Index: Integer): TLccListenerObject;
begin
  Result := TLccListenerObject(ListenerList[Index])
end;

procedure TLccEngineRetrieveListenerInfo.Process(SourceMessage: TLccMessage);
var
  LocalListener: TLccListenerObject;
  LocalMapping: TLccAliasMapping;
begin
  if EqualNodeID(TargetNodeID, SourceMessage.SourceID, False) then
  begin
    case SourceMessage.MTI of
      MTI_TRACTION_REPLY :
        begin
          case SourceMessage.DataArray[0] of
            TRACTION_LISTENER_CONFIG :
              begin
                case SourceMessage.DataArray[1] of
                  TRACTION_LISTENER_CONFIG_QUERY  :
                    begin
                      if SourceMessage.DataCount = 1 then
                      begin
                        QueryNodeCount := SourceMessage.TractionExtractListenerQueryNodeCountReply;
                        QueryNodeIndex := 0;
                        if QueryNodeCount > 0 then
                          QueryNextListener;
                      end else
                      begin
                        LocalListener := TLccListenerObject.Create;
                        LocalListener.NodeIdentification.NodeID := SourceMessage.TractionExtractListenerIDReply;
                        if OwnerNode.GridConnect then
                        begin
                          LocalMapping := AliasServer.FindMapping(LocalListener.NodeIdentification.NodeID);
                          Assert(Assigned(LocalMapping), 'Unable to find Mapping:  TLccEngineListenerInfo.Process(SourceMessage: TLccMessage);');
                          LocalListener.NodeIdentification.Alias := LocalMapping.NodeAlias;
                        end;
                        LocalListener.Flags := SourceMessage.TractionExtractListenerQueryNodeFlagsReply;
                        LocalListener.ListIndex := SourceMessage.TractionExtractListenerQueryNodeIndexReply;
                        ListenerList.Add(LocalListener);
                        QueryNextListener;
                      end;
                    end;
                end;
              end;
          end;
        end;
    end
  end;
end;

procedure TLccEngineRetrieveListenerInfo.QueryNextListener;
begin
  if QueryNodeIndex < QueryNodeCount then
  begin
    WorkerMessage.LoadTractionListenerQuery(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias, QueryNodeIndex);
    OwnerNode.SendMessageFunc(OwnerNode, WorkerMessage);
    Inc(FQueryNodeIndex);
  end else
  begin
    EngineStateComplete;
    Callback(Self);
  end;
end;

procedure TLccEngineRetrieveListenerInfo.Reset;
begin
  inherited;
  ClearListeners;
end;

procedure TLccEngineRetrieveListenerInfo.Start;
begin
  inherited;
  WorkerMessage.LoadTractionListenerQueryCount(OwnerNode.NodeID, OwnerNode.AliasID, TargetNodeID, TargetAlias);
  OwnerNode.SendMessageFunc(OwnerNode, WorkerMessage);
end;

{ TLccEngineSearchTrain }

procedure TLccEngineSearchTrain.HandleProducerIdentified(SourceMessage: TLccMessage);
var
  AliasMapping: TLccAliasMapping;
begin
  if SourceMessage.TractionIsSearchEvent and (SourceMessage.TractionSearchExtractSearchData = SearchCriteriaPending) then
  begin
    if OwnerNode.GridConnect then
    begin
      AliasMapping := AliasServer.FindMapping(SourceMessage.CAN.SourceAlias);
      SearchTrain.NodeIdentification.AssignID(AliasMapping.NodeID, AliasMapping.NodeAlias);
    end else
      SearchTrain.NodeIdentification.AssignID(SourceMessage.SourceID, SourceMessage.CAN.SourceAlias);
    SearchTrain.SearchCriteria := SearchCriteriaPending;
    Callback(Self, SearchTrain)
  end;
end;

constructor TLccEngineSearchTrain.Create(AnOwner: TLccNode);
begin
  inherited Create(AnOwner);
  FSearchTrain := TLccSearchTrainObject.Create;
end;

destructor TLccEngineSearchTrain.Destroy;
begin
  FreeAndNil(FSearchTrain);
  inherited Destroy;
end;

procedure TLccEngineSearchTrain.Start;
begin
  inherited Start;
  // Search for that train... the reply to this is handled in the Process Message MTI_PRODUCER_IDENTIFIED_XXXXX
  WorkerMessage.LoadTractionSearch(OwnerNode.NodeID, OwnerNode.AliasID, SearchCriteriaPending);
  OwnerNode.SendMessageFunc(OwnerNode, WorkerMessage);
end;

procedure TLccEngineSearchTrain.Reset;
begin
  inherited Reset;
  FSearchCriteriaPending := 0;
  FCallback := nil;
end;

procedure TLccEngineSearchTrain.Process(SourceMessage: TLccMessage);
begin
  if EngineState = lesRunning then
  begin
    case SourceMessage.MTI of
       MTI_PRODUCER_IDENTIFIED_CLEAR   : HandleProducerIdentified(SourceMessage);
       MTI_PRODUCER_IDENTIFIED_SET     : HandleProducerIdentified(SourceMessage);
       MTI_PRODUCER_IDENTIFIED_UNKNOWN : HandleProducerIdentified(SourceMessage);
    end;
  end;
end;

procedure TLccEngineSearchTrain.Assign(DccAddress: Word;
  AnIsLongAddress: Boolean; ASpeedStep: TLccDccSpeedStep;
  ACallback: TLccEngineSearchTrainCallback);
begin
  Assign(IntToStr(DccAddress), AnIsLongAddress, ASpeedStep, ACallback);
end;

procedure TLccEngineSearchTrain.Assign(ASearchString: string; AnIsLongAddress: Boolean; ASpeedStep: TLccDccSpeedStep; ACallback: TLccEngineSearchTrainCallback);
var
  TrackProtocolFlags: Word;
begin
  TrackProtocolFlags := TRACTION_SEARCH_TARGET_ADDRESS_MATCH or TRACTION_SEARCH_ALLOCATE_FORCE or TRACTION_SEARCH_TYPE_EXACT_MATCH or
                        TRACTION_SEARCH_TRACK_PROTOCOL_GROUP_DCC_ONLY;
  if AnIsLongAddress then
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_LONG
  else
    TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ADDRESS_DEFAULT;
  case ASpeedStep of
     ldssDefault : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_ANY_SPEED_STEP;
     ldss14      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_14_SPEED_STEP;
     ldss28      : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_28_SPEED_STEP;
     ldss128     : TrackProtocolFlags := TrackProtocolFlags or TRACTION_SEARCH_TRACK_PROTOCOL_DCC_128_SPEED_STEP;
  end;
  Assign(ASearchString, TrackProtocolFlags, ACallback);
end;

procedure TLccEngineSearchTrain.Assign(ASearchString: string; ATrackProtocolFlags: Word; ACallback: TLccEngineSearchTrainCallback);
var
  LocalSearchCriteria: DWORD;
begin
  LocalSearchCriteria := 0;
  WorkerMessage.TractionSearchEncodeSearchString(ASearchString, ATrackProtocolFlags, LocalSearchCriteria);
  Assign(ASearchString, LocalSearchCriteria, ACallback);
end;

procedure TLccEngineSearchTrain.Assign(ASearchString: string; ASearchCriteria: DWORD; ACallback: TLccEngineSearchTrainCallback);
begin
  FCallback := ACallback;
  FSearchCriteriaPending := ASearchCriteria;
end;

{ TLccEngineSearchAndAllocateTrain }

procedure TLccEngineSearchAndAllocateTrain.SearchTrainCallback(AnEngineSearchTrain: TLccEngineSearchTrain; ASearchTrain: TLccSearchTrainObject);
begin
  if AnEngineSearchTrain.EngineState = lesComplete then
  begin
    // AliasMapping was done in the Search Engine
    WorkerMessage.LoadTractionControllerAssign(OwnerNode.NodeID, OwnerNode.AliasID, ASearchTrain.NodeIdentification.NodeID, ASearchTrain.NodeIdentification.Alias, OwnerNode.NodeID);
    OwnerNode.SendMessageFunc(Self, WorkerMessage);
  end else
  begin  // Problem
  //  EngineState := AnEngineSearchTrain.EngineState;
    ErrorCode := AnEngineSearchTrain.ErrorCode;
    EngineStateError;
    Callback(Self, EngineSearchTrain)
  end;
end;

constructor TLccEngineSearchAndAllocateTrain.Create(AnOwner: TLccNode);
begin
  inherited Create(AnOwner);
  FEngineSearchTrain := TLccEngineSearchTrain.Create(AnOwner)
end;

destructor TLccEngineSearchAndAllocateTrain.Destroy;
begin
  FreeAndNil(FEngineSearchTrain);
  inherited Destroy;
end;

procedure TLccEngineSearchAndAllocateTrain.Start;
begin
  inherited Start;
  EngineSearchTrain.Start;
end;

procedure TLccEngineSearchAndAllocateTrain.Stop;
begin
  inherited Stop;
  EngineSearchTrain.Stop;
end;

procedure TLccEngineSearchAndAllocateTrain.Reset;
begin
  inherited Reset;
  EngineSearchTrain.Reset;
  FCallback := nil;
end;

procedure TLccEngineSearchAndAllocateTrain.Process(SourceMessage: TLccMessage);
begin
  if EngineSearchTrain.EngineState = lesRunning then
    EngineSearchTrain.Process(SourceMessage);
  if (EngineState = lesRunning) and (EngineSearchTrain.EngineState = lesComplete) then
  begin
    if EqualNodeID(EngineSearchTrain.SearchTrain.NodeIdentification.NodeID, SourceMessage.SourceID, False) then
    begin
      case SourceMessage.MTI of
        MTI_TRACTION_REPLY :
          begin
            case SourceMessage.DataArray[0] of
              TRACTION_CONTROLLER_CONFIG :
                begin
                  case SourceMessage.DataArray[1] of
                    TRACTION_CONTROLLER_CONFIG_ASSIGN  :
                      begin
                        case SourceMessage.TractionExtractControllerAssignResult of
                          TRACTION_CONTROLLER_CONFIG_REPLY_OK :
                            begin
                              EngineStateComplete;
                              Callback(Self, EngineSearchTrain)
                            end;
                          TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN :
                            begin
                              ErrorCode := ENGINE_ERROR_TRACTION_CONTROLLER_CONFIG_ASSIGN_REPLY_REFUSE_TRAIN;
                              EngineStateError;
                              Callback(Self, EngineSearchTrain)
                            end
                      end;
                  end;
                end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TLccEngineSearchAndAllocateTrain.Assign(DccAddress: Word; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnLccEngineSearchAndAllocateTrainCallback);
begin
  Callback := ACallback;
  EngineSearchTrain.Assign(DccAddress, IsLongAddress, SpeedSteps, {$IFNDEF LCC_DELPHI}@{$ENDIF}SearchTrainCallback);
end;

procedure TLccEngineSearchAndAllocateTrain.Assign(SearchString: string; IsLongAddress: Boolean; SpeedSteps: TLccDccSpeedStep; ACallback: TOnLccEngineSearchAndAllocateTrainCallback);
begin
  Callback := ACallback;
  EngineSearchTrain.Assign(SearchString, IsLongAddress, SpeedSteps, {$IFNDEF LCC_DELPHI}@{$ENDIF}SearchTrainCallback);
end;

procedure TLccEngineSearchAndAllocateTrain.Assign(SearchString: string; TrackProtocolFlags: Word; ACallback: TOnLccEngineSearchAndAllocateTrainCallback);
begin
  Callback := ACallback;
  EngineSearchTrain.Assign(SearchString, TrackProtocolFlags, {$IFNDEF LCC_DELPHI}@{$ENDIF}SearchTrainCallback);
end;

{ TLccTractionServerNode }

procedure TLccTractionServerNode.DoMemorySpaceReadEngineDone(MemoryReadEngine: TLccEngineMemorySpaceAccess);
var
  TractionObject: TLccTractionObject;
begin
  inherited DoMemorySpaceReadEngineDone(MemoryReadEngine);
  if TractionServer.Enabled then
  begin
    if MemoryReadEngine.MemorySpace = MSI_CDI then
    begin
      TractionObject := TractionServer.Find(MemoryReadEngine.TargetNodeID);
      if Assigned(TractionObject) then
      begin
        if MemoryReadEngine.EngineState = lesComplete then
        begin
          TractionObject.NodeCDI.CDI := MemoryReadEngine.StreamAsString;
          TractionObject.NodeCDI.Implemented := True;
        end else
        begin
          TractionObject.NodeCDI.CDI := '';  // Error
          TractionObject.NodeCDI.Implemented := False;
        end;
      end;
    end;
  end;
end;

constructor TLccTractionServerNode.Create(ANodeManager: {$IFDEF LCC_DELPHI}TComponent{$ELSE}TObject{$ENDIF}; CdiXML: string; GridConnectLink: Boolean);
begin
  inherited Create(ANodeManager, CdiXML, GridConnectLink);
  FTractionServer := TLccTractionServer.Create(GridConnectLink);
  FEngineSearchAndAllocateTrain := TLccEngineSearchAndAllocateTrain.Create(Self);
  FEngineSearchTrain := TLccEngineSearchTrain.Create(Self);
  FEngineListenerInfo := TLccEngineRetrieveListenerInfo.Create(Self);
end;

destructor TLccTractionServerNode.Destroy;
begin
  FreeAndNil(FEngineSearchTrain);
  FreeAndNil(FEngineSearchAndAllocateTrain);
  FreeAndNil(FTractionServer);
  FreeAndNil(FEngineListenerInfo);
  inherited Destroy;
end;

function TLccTractionServerNode.ProcessMessageLCC(SourceMessage: TLccMessage): Boolean;
begin
  Result := inherited ProcessMessageLCC(SourceMessage);
  if not Result then
  begin
    if TractionServer.Enabled then
      TractionServer.ProcessMessageLCC(Self, SourceMessage);
    EngineSearchAndAllocateTrain.Process(SourceMessage);
    EngineCreateTrain.Process(SourceMessage);
  end;
end;

end.



