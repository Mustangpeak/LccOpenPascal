unit lcc_alias_server;


// NOTE FOR USAGE .... ALWAYS GUARD BAND REPLIES FROM FINDALIAS()... it is possible
// that a node reset has come in right after the the message you are handling (recall
// that is all done in the thread) and may remove that mapping before you try to
// access it.. this is the way it has to be because the requirement is that you can't
// send any messages to a node that sent an AMR after 50ms.

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
    FInternalNode: Boolean;
    FNodeAlias: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
    // True if this node is managed by the NodeManager else it is some node on the network
    property InternalNode: Boolean read FInternalNode write FInternalNode;

    procedure AssignID(ANodeID: TNodeID; AnAlias: Word);
    function Clone: TLccAliasMapping;
    function Compare(TestMapping: TLccAliasMapping): Boolean;
    function CompareEitherOr(TestMapping: TLccAliasMapping): Boolean;
  end;


  { TLccAliasServer }

  TLccAliasServer = class(TObject)

  private
    FMappingList: TThreadList;          // Valid Mappings

  protected
    property MappingList: TThreadList read FMappingList write FMappingList;

  public

    constructor Create;
    destructor Destroy; override;

    // Clears the mapping, can choose if you want to keep the internal nodes (owned by the Node Manager)
    procedure Clear(ForceInternalNodes: Boolean);
    // How many mappings there are
    function Count: Integer;
    // Find a mapping based on an ID or Alias
    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    function FindAlias(ANodeID: TNodeID): Word; overload;  // Returns the Alias or 0
    // Finds either the ID or Alias associated with mappings and deletes them if found
    procedure FlushOldMapping(ANodeID: TNodeID; AnAlias: Word);
    // Add a new mapping
    function AddMapping(ANodeID: TNodeID; AnAlias: Word; AnInternalNode: Boolean): TLccAliasMapping;
    // Remove a mapping, can choose if you want to keep the internal nodes (owned by the Node Manager)
    procedure RemoveMapping(AnAliasID: Word; ForceInternalNodes: Boolean);
    // For debug or other interest
    procedure ReadMappingsToStringList(AStringList: TStringList);
  end;

var
  AliasServer: TLccAliasServer;

implementation


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

procedure TLccAliasServer.FlushOldMapping(ANodeID: TNodeID; AnAlias: Word);
var
  i: Integer;
  List: TList;
begin
  // Remove any old mappings with is alias
  List := MappingList.LockList;
  try
    for i := List.Count - 1 downto 0 do
    begin
      if (TLccAliasMapping(List[i]).NodeAlias = AnAlias) or EqualNodeID(ANodeID, TLccAliasMapping(List[i]).NodeID, False) then
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
end;

destructor TLccAliasServer.Destroy;
begin
  Clear(True);
  FreeAndNil(FMappingList);
  inherited Destroy;
end;

procedure TLccAliasServer.Clear(ForceInternalNodes: Boolean);
var
  i: Integer;
  List: TList;
begin
  List := MappingList.LockList;
  try
  for i := List.Count - 1 downto 0 do
  begin
    if not TLccAliasMapping( List[i]).InternalNode or ForceInternalNodes then
    begin
      TObject(List[i]).Free;
      List.Delete(i);
    end;
  end;
  finally
    MappingList.UnlockList;
  end;
end;

function TLccAliasServer.Count: Integer;
begin
  Result := ThreadListCount(MappingList);
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

function TLccAliasServer.FindAlias(ANodeID: TNodeID): Word;
var
  Mapping: TLccAliasMapping;
begin
  Result := 0;
  Mapping := FindMapping(ANodeID);
  if Assigned(Mapping) then
    Result := Mapping.NodeAlias;
end;

function TLccAliasServer.AddMapping(ANodeID: TNodeID; AnAlias: Word;
  AnInternalNode: Boolean): TLccAliasMapping;
begin
  Assert(AnAlias <> 0, 'Alias = 0 in TLccAliasServer.AddMapping');
  Assert(not NullNodeID(ANodeID), 'NodeID = NULL_ID in TLccAliasServer.AddMapping');

  MappingList.LockList;
  try
    Result := FindMapping(AnAlias);
    if not Assigned(Result) then
    begin
      FlushOldMapping(ANodeID, AnAlias);
      Result := TLccAliasMapping.Create;
      Result.NodeID := ANodeID;
      Result.NodeAlias := AnAlias;
      Result.InternalNode := AnInternalNode;
      MappingList.Add(Result);
      {$IFDEF LOG_MAPPING}DebugLn('Adding Mapping: 0x' + IntToHex(Result.NodeAlias, 4) + '; ' + NodeIDToString(Result.NodeID, True));{$ENDIF}
    end;
  finally
    MappingList.UnlockList;
  end;
end;

procedure TLccAliasServer.RemoveMapping(AnAliasID: Word; ForceInternalNodes: Boolean);
var
  i: Integer;
  List: TList;
  Mapping: TLccAliasMapping;
begin
  List := MappingList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      Mapping := TLccAliasMapping( List[i]);
      if AnAliasID = Mapping.NodeAlias then
      begin
        if not Mapping.InternalNode or ForceInternalNodes then
        begin
          {$IFDEF LOG_MAPPING}DebugLn('Mapping Deleted: 0x' + IntToHex(Mapping.NodeAlias, 4) + '; ' + NodeIDToString(Mapping.NodeID, True));{$ENDIF}
          Mapping.Free;
          List.Delete(i);
        end;
        Break;
      end;
    end;
  finally
    MappingList.UnlockList;
  end;
end;

procedure TLccAliasServer.ReadMappingsToStringList(AStringList: TStringList);
var
  i: Integer;
  List: TList;
  Mapping: TLccAliasMapping;
begin
  AStringList.Clear;
  List := MappingList.LockList;
  try
    for i := 0 to List.Count - 1 do
    begin
      Mapping := TLccAliasMapping( List[i]);
      if Mapping.InternalNode then
        AStringList.Add( 'NodeID: ' + NodeIDToString( Mapping.NodeID, True) + '  Alias: ' + IntToHex(Mapping.NodeAlias) + ' [Internal]')
      else
        AStringList.Add( 'NodeID: ' + NodeIDToString( Mapping.NodeID, True) + '  Alias: ' + IntToHex(Mapping.NodeAlias) + ' [External]');
    end;
  finally
    MappingList.UnlockList;
  end;
end;

initialization
  AliasServer := TLccAliasServer.Create;

finalization;
  FreeAndNil(AliasServer);

end.

