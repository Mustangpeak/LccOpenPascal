unit lcc_alias_server;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses

  Classes,
  SysUtils,
  {$IFNDEF LCC_FPC}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  {$IFDEF WEB_APP}
    generics.collections,
  {$ENDIF}
  lcc_defines,
  lcc_utilities;

type

  // When a LccMessage is validating if the nodes it references is missing a mapping
  // in the Alias Server it calls back to the TReceiveMessageAliasServerThread with
  // a request to send out a request for mapping of this ID pair.  Either could be
  // null depending on what info exists in the message

  TLccMessageRequestMappingCallback = procedure(NodeID: TNodeID; AliasID: Word) of object;


  { TLccAliasMapping }

  TLccAliasMapping = class(TObject)
  private
    FAbandonCount: Integer;
    FMarkedForDeletion: Boolean;   // These are for the Event notification system.  It is called through a timer
    FMarkedForInsertion: Boolean;  // to call the main threads event handlers to notify the app of insertion or deletion
    FNodeAlias: Word;
    FNodeID: TNodeID;
  protected
    property AbandonCount: Integer read FAbandonCount write FAbandonCount;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
    property MarkedForDeletion: Boolean read FMarkedForDeletion write FMarkedForDeletion;
    property MarkedForInsertion: Boolean read FMarkedForInsertion write FMarkedForInsertion;

    procedure AssignID(ANodeID: TNodeID; AnAlias: Word);
    function Clone: TLccAliasMapping;
    function Compare(TestMapping: TLccAliasMapping): Boolean;
    function CompareEitherOr(TestMapping: TLccAliasMapping): Boolean;
  end;


  // List of TLccAliasMapping objects that define all the mappings for any node referenced
  // within a LccMessage (Source, Destination, any NodeID's carried in payload
  { TLccMessageAliasMappingList }

  TLccMessageAliasMappingList = class(TList)

  end;


  { TLccAliasServer }

  TLccAliasServer = class(TObject)

  private
    FMappingList: TThreadList;          // Valid Mappings
    FMappingRequestList: TThreadList;   // Mappings that have only one ID and waiting for a message to come in to fill them out and move them to the MappingList
    FWorkerMappingRequestList: TList;

  protected
    procedure FlushOldMappingWithAlias(AnAlias: Word);

  public
    property MappingList: TThreadList read FMappingList write FMappingList;
    property MappingRequestList: TThreadList read FMappingRequestList write FMappingRequestList;
    property WorkerMappingRequestList: TList read FWorkerMappingRequestList write FWorkerMappingRequestList;


    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearMappingRequests;
    procedure ClearWorkerMappingRequests;
    function Count: Integer;
    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID; AnAliasID: Word): TLccAliasMapping; overload;
    function AddMapping(ANodeID: TNodeID; AnAlias: Word): TLccAliasMapping;

    // These are for internal virtual nodes to deal with Alias messages.  If the Receive Message Thread finds a message with a node that is not mapped that could be a
    // node that is internal to this application.  That said there needs to be a mechinism for the receive thread to function the same as if it was an external node by
    procedure AddMappingRequest(ANodeID: TNodeID; AnAlias: Word);
    function MarkForRemovalByAlias(AnAlias: Word): TLccAliasMapping;
    procedure MappingRequestLiveTimeIncreaseAndDeleteAbandoned(MaxAbandonTimeCount: Word);
  end;

var
  AliasServer: TLccAliasServer;

implementation

uses
  lcc_base_classes;

{ TLccAliasMapping }

procedure TLccAliasMapping.AssignID(ANodeID: TNodeID; AnAlias: Word);
begin
  FNodeID := ANodeID;
  FNodeAlias := AnAlias;
end;

function TLccAliasMapping.Clone: TLccAliasMapping;
begin
  Result := TLccAliasMapping.Create;
  Result.AssignID(NodeID, NodeAlias);
  Result.AbandonCount := 0;
end;

function TLccAliasMapping.Compare(TestMapping: TLccAliasMapping): Boolean;
begin
  Result := (TestMapping.NodeAlias = NodeAlias) and (TestMapping.NodeID[0] = NodeID[0]) and (TestMapping.NodeID[0] = NodeID[0])
end;

function TLccAliasMapping.CompareEitherOr(TestMapping: TLccAliasMapping): Boolean;
begin
  Result := (TestMapping.NodeAlias = NodeAlias) or ((TestMapping.NodeID[0] = NodeID[0]) and (TestMapping.NodeID[0] = NodeID[0]))
end;

{ TLccAliasServer }

procedure TLccAliasServer.FlushOldMappingWithAlias(AnAlias: Word);
var
  i: Integer;
  List: TList;
begin
  // Remove any old mappings with is alias
  List := MappingList.LockList;
  try
    for i := List.Count - 1 downto 0 do
    begin
      if TLccAliasMapping(List[i]).NodeAlias = AnAlias then
      begin
        TObject(List[i]).Free;
        List.Delete(i);
      end;
    end;
  finally
    MappingList.UnLockList
  end;
end;

constructor TLccAliasServer.Create;
begin
  inherited Create;
  FMappingList := TThreadList.Create;
  FMappingRequestList := TThreadList.Create;
  FWorkerMappingRequestList := TList.Create;
end;

destructor TLccAliasServer.Destroy;
begin
  Clear;
  ClearMappingRequests;
  ClearWorkerMappingRequests;
  FreeAndNil(FMappingList);
  FreeAndNil(FMappingRequestList);
  FreeAndNil(FWorkerMappingRequestList);
  inherited Destroy;
end;

procedure TLccAliasServer.Clear;
var
  i: Integer;
  List: TList;
begin
  List := MappingList.LockList;
  try
  for i := 0 to List.Count - 1 do
    TObject(List[i]).Free
  finally
    List.Clear;
    MappingList.UnlockList;
  end;
end;

procedure TLccAliasServer.ClearMappingRequests;
var
  LocalObj: TLccNodeIdentificationObject;
  i: Integer;
  List: TList;
begin
  List := MappingRequestList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      LocalObj := TLccNodeIdentificationObject( List[i]);
      LocalObj.Free;
    end;
  finally
    List.Clear;
    MappingRequestList.UnlockList;
  end;
end;

procedure TLccAliasServer.ClearWorkerMappingRequests;
var
  LocalObj: TLccNodeIdentificationObject;
  i: Integer;
begin
  try
    for i := 0 to WorkerMappingRequestList.Count - 1 do
    begin
      LocalObj := TLccNodeIdentificationObject( WorkerMappingRequestList[i]);
      LocalObj.Free;
    end;
  finally
    WorkerMappingRequestList.Clear;
  end;
end;

function TLccAliasServer.Count: Integer;
var
  List: TList;
begin
  List := MappingList.LockList;
  try
    Result := List.Count;
  finally
     MappingList.UnlockList;
  end;
end;

function TLccAliasServer.FindMapping(AnAliasID: Word): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
  List: TList;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  List := MappingList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      TestMapping := TLccAliasMapping(List.Items[i]);
      if TestMapping.NodeAlias = AnAliasID then
      begin
        Result := TestMapping;
        Break;
      end;
    end;
  finally
     MappingList.UnlockList;
  end;
end;

function TLccAliasServer.FindMapping(ANodeID: TNodeID): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
  List: TList;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  List := MappingList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      TestMapping := TLccAliasMapping(List.Items[i]);
      if (TestMapping.NodeID[0] = ANodeID[0]) and (TestMapping.NodeID[1] = ANodeID[1]) then
      begin
        Result := TestMapping;
        Break;
      end;
    end;
  finally
     MappingList.UnlockList;
  end;
end;

function TLccAliasServer.FindMapping(ANodeID: TNodeID; AnAliasID: Word): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
  List: TList;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  List := MappingList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      TestMapping := TLccAliasMapping(List.Items[i]);
      if EqualNodeID(TestMapping.NodeID, ANodeID, False) or (TestMapping.NodeAlias = AnAliasID) then
      begin
        Result := TestMapping;
        Break;
      end;
    end;
  finally
     MappingList.UnlockList;
  end;
end;

function TLccAliasServer.AddMapping(ANodeID: TNodeID; AnAlias: Word): TLccAliasMapping;
begin
  Assert(AnAlias <> 0, 'Alias = 0 in TLccAliasServer.AddMapping');
  Assert(not NullNodeID(ANodeID), 'NodeID = NULL_ID in TLccAliasServer.AddMapping');

  MappingList.LockList;
  try
    Result := FindMapping(AnAlias);
    if not Assigned(Result) then
    begin
      FlushOldMappingWithAlias(AnAlias);
      Result := TLccAliasMapping.Create;
      Result.NodeID := ANodeID;
      Result.NodeAlias := AnAlias;
      Result.MarkedForInsertion := True;
      MappingList.Add(Result);
      {$IFDEF LOG_MAPPING}DebugLn('Adding Mapping: 0x' + IntToHex(Result.NodeAlias, 4) + '; ' + NodeIDToString(Result.NodeID, True));{$ENDIF}
    end;
  finally
    MappingList.UnlockList;
  end;
end;

procedure TLccAliasServer.AddMappingRequest(ANodeID: TNodeID; AnAlias: Word);
var
  LocalObj: TLccNodeIdentificationObject;
  i: Integer;
  List: TList;
  Duplicate: Boolean;
begin
  // Someone is asking for a Mapping Request, first we see if it is a duplicate
  Duplicate := False;
  List := MappingRequestList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      LocalObj := TLccNodeIdentificationObject( List[i]);
      if (not NullNodeID(ANodeID) and ((ANodeID[0] = LocalObj.NodeID[0]) and (ANodeID[1] = LocalObj.NodeID[1]))) or (AnAlias = LocalObj.Alias) then
      begin
        Duplicate := True;
        Break;
      end;
    end;
  finally
    MappingRequestList.UnlockList;
  end;

  // Nope not a duplicate (as of before we unlocked the list) so create a new request object and add it to the list (that call is thread safe)
  if not Duplicate then
  begin
    LocalObj := TLccNodeIdentificationObject.Create;
    LocalObj.AssignID(ANodeID, AnAlias);
    MappingRequestList.Add(LocalObj);
    {$IFDEF LOG_MAPPING}DebugLn('New Mapping Request Added: 0x' + IntToHex(LocalObj.Alias, 4) + '; ' + NodeIDToString(LocalObj.NodeID, True));{$ENDIF}
  end;
end;

function TLccAliasServer.MarkForRemovalByAlias(AnAlias: Word): TLccAliasMapping;
begin
  MappingList.LockList;
  try
    Result := FindMapping(AnAlias);
    if Assigned(Result) then
    begin
      Result.MarkedForDeletion := True;
      {$IFDEF LOG_MAPPING}DebugLn('Marked for Deletion Mapping: 0x' + IntToHex(Result.NodeAlias, 4) + '; ' + NodeIDToString(Result.NodeID, True));{$ENDIF}
    end;
  finally
    MappingList.UnlockList;
  end;
end;

procedure TLccAliasServer.MappingRequestLiveTimeIncreaseAndDeleteAbandoned(MaxAbandonTimeCount: Word);
var
  LocalNodeIdentificationObject: TLccNodeIdentificationObject;
  i: Integer;
  List: TList;
begin
  List := MappingRequestList.LockList;
  try
    for i := List.Count - 1 downto 0 do
    begin
      LocalNodeIdentificationObject := TLccNodeIdentificationObject( List[i]);
      LocalNodeIdentificationObject.AbandonCount := LocalNodeIdentificationObject.AbandonCount + 1;

      if LocalNodeIdentificationObject.AbandonCount > MaxAbandonTimeCount then
      begin
        {$IFDEF LOG_MAPPING}DebugLn('Mapping Request Deleted: 0x' + IntToHex(LocalNodeIdentificationObject.Alias, 4) + '; ' + NodeIDToString(LocalNodeIdentificationObject.NodeID, True));{$ENDIF}
        LocalNodeIdentificationObject.Free;
        List.Delete(i);
      end;
    end;
  finally
    MappingRequestList.UnlockList;
  end;
end;

initialization
  AliasServer := TLccAliasServer.Create;

finalization;
  AliasServer.Clear;
  FreeAndNil(AliasServer);

end.

