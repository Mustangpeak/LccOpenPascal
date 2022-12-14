unit lcc_alias_server;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ..\lcc_compilers.inc}

{.$DEFINE DISABLE_STATE_TIMEOUTS_FOR_DEBUG}

uses

  Classes,
  SysUtils,
  {$IFDEF FPC}
    contnrs,
    {$IFNDEF FPC_CONSOLE_APP}
      ExtCtrls,
    {$ENDIF}
  {$ELSE}
    System.Types,
    FMX.Types,
    System.Generics.Collections,
  {$ENDIF}
  lcc_defines,
  lcc_utilities;

type

  { TLccAliasMapping }

  TLccAliasMapping = class(TObject)
  private
    FMarkedForDeletion: Boolean;   // These are for the Event notification system.  It is called through a timer
    FMarkedForInsertion: Boolean;  // to call the main threads event handlers to notify the app of insertion or deletion
    FNodeAlias: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
    property MarkedForDeletion: Boolean read FMarkedForDeletion write FMarkedForDeletion;
    property MarkedForInsertion: Boolean read FMarkedForInsertion write FMarkedForInsertion;
  end;


  { TLccAliasServer }

  TLccAliasServer = class(TObject)

  private
    FMappingList: TThreadList;
    FMappingRequestList: TThreadList;

  public
    property MappingList: TThreadList read FMappingList write FMappingList;
    property MappingRequestList: TThreadList read FMappingRequestList write FMappingRequestList;


    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    procedure ClearMappingRequests;
    function Count: Integer;
    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID; AnAliasID: Word): TLccAliasMapping; overload;
    function AddMapping(ANodeID: TNodeID; AnAlias: Word): TLccAliasMapping;
    procedure AddMappingRequest(ANodeID: TNodeID; AnAlias: Word);
    function MarkForRemovalByAlias(AnAlias: Word): TLccAliasMapping;
    procedure RemoveMappingRequest(ANodeID: TNodeID; AnAlias: Word);
    procedure MappingRequestLiveTimeIncrease(MaxAbandonTimeCount: Word);
    {$IFDEF WriteLnDebug}procedure WriteMapping(AComment: string; AMapping: TLccAliasMapping); {$ENDIF}
  end;

var
  AliasServer: TLccAliasServer;

implementation

uses
  lcc_node_messages;

{ TLccAliasServer }

constructor TLccAliasServer.Create;
begin
  inherited Create;
  FMappingList := TThreadList.Create;
  FMappingRequestList := TThreadList.Create;
end;

destructor TLccAliasServer.Destroy;
begin
  Clear;
  ClearMappingRequests;
  FreeAndNil(FMappingList);
  FreeAndNil(FMappingRequestList);
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
  Duplicate: Boolean;
begin
  Duplicate := False;
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

function TLccAliasServer.Count: Integer;
var
  List: TList;
begin
  Result := 0;
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
  Result := nil;

  Assert(AnAlias <> 0, 'Alias = 0 in TLccAliasServer.AddMapping');
  Assert(not NullNodeID(ANodeID), 'NodeID = NULL_ID in TLccAliasServer.AddMapping');

  MappingList.LockList;
  try
    Result := FindMapping(AnAlias);
    if not Assigned(Result) then
    begin
      Result := TLccAliasMapping.Create;
      Result.NodeID := ANodeID;
      Result.NodeAlias := AnAlias;
      Result.MarkedForInsertion := True;
      MappingList.Add(Result);
      {$IFDEF WriteLnDebug}WriteMapping('New Mapping', Result);{$ENDIF}
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
  Duplicate := False;
  List := MappingRequestList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      LocalObj := TLccNodeIdentificationObject( List[i]);
      if ((ANodeID[0] = LocalObj.NodeID[0]) and (ANodeID[1] = LocalObj.NodeID[1])) or (AnAlias = LocalObj.Alias) then
      begin
        Duplicate := True;
        Break;
      end;
    end;
  finally
    MappingRequestList.UnlockList;
  end;

  if not Duplicate then
  begin
    LocalObj := TLccNodeIdentificationObject.Create;
    LocalObj.AssignID(ANodeID, AnAlias);
    MappingRequestList.Add(LocalObj);
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
      {$IFDEF WriteLnDebug}WriteMapping('Marked for Deletion Mapping', Result);{$ENDIF}
    end;
  finally
    MappingList.UnlockList;
  end;
end;

procedure TLccAliasServer.RemoveMappingRequest(ANodeID: TNodeID; AnAlias: Word);
var
  i: Integer;
  List: TList;
  LocalObj: TLccNodeIdentificationObject;
begin
  List := MappingRequestList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      LocalObj := TLccNodeIdentificationObject( List[i]);
      if ((ANodeID[0] = LocalObj.NodeID[0]) and (ANodeID[1] = LocalObj.NodeID[1])) or (AnAlias = LocalObj.Alias) then
      begin
        LocalObj.Free;
        List.Delete(i);
        Break;
      end;
    end;
  finally
    MappingRequestList.UnlockList;
  end;
end;

procedure TLccAliasServer.MappingRequestLiveTimeIncrease(MaxAbandonTimeCount: Word);
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
      LocalObj.AbandonCount := LocalObj.AbandonCount + 1;

      if LocalObj.AbandonCount > MaxAbandonTimeCount then
      begin
        LocalObj.Free;
        List[i] := nil;
      end;
    end;

    for i := List.Count - 1 downto 0 do
    begin
      if List[i] = nil then
        List.Delete(i);
    end;

  finally
    MappingRequestList.UnlockList;
  end;
end;

{$IFDEF WriteLnDebug}
procedure TLccAliasServer.WriteMapping(AComment: string; AMapping: TLccAliasMapping);
begin
  if Assigned(AMapping) then
    WriteLn(AComment + '- Alias 0x' + IntToHex(AMapping.NodeAlias, 4) + ' ID: ' + NodeIDToString(AMapping.NodeID, True))
  else
    WriteLn(AComment + ' Nill Mapping')
end;
{$ENDIF}

initialization
  AliasServer := TLccAliasServer.Create;

finalization;
  AliasServer.Clear;
  FreeAndNil(AliasServer);

end.

