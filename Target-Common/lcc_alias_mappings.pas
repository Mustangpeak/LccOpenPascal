unit lcc_alias_mappings;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ..\lcc_compilers.inc}

{.$DEFINE DISABLE_STATE_TIMEOUTS_FOR_DEBUG}

uses
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
  lcc_node_messages;



//Is seems like this should all be able to be handled in the SendMessage engine.... if the Alias is needed can''t that engine get the Alias if needed?.....

type
  TLccAliasMapping = class(TObject)
  private
    FNodeAlias: Word;
    FNodeID: TNodeID;
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property NodeAlias: Word read FNodeAlias write FNodeAlias;
  end;

  { TLccAliasMappingServer }

  TLccAliasMappingServer = class(TObject)

  private
    {$IFDEF DELPHI}
    FMappingList: TObjectList<TLccAliasMapping>;      // List of TLccAliasMapping Objects
    FInProcessMessageList: TObjectList<TLccMessage>;  // List of TLccMessages that are in waiting for a valid AliasMapping before processing
    {$ELSE}
    FMappingList: TObjectList;
    FInProcessMessageList: TObjectList;
    {$ENDIF}
    FWorkerMessage: TLccMessage;

  protected
    {$IFDEF DELPHI}
    property MappingList: TObjectList<TLccAliasMapping> read FMappingList write FMappingList;
    property InProcessMessageList: TObjectList<TLccMessage> read FInProcessMessageList write FInProcessMessageList;
    {$ELSE}
    property MappingList: TObjectList read FMappingList write FMappingList;
    property InProcessMessageList: TObjectList read FInProcessMessageList write FInProcessMessageList;
    {$ENDIF}

    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

  public
    constructor Create;
    destructor Destroy; override;

    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    procedure FlushInProcessMessages;
    function AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
    function RemoveMapping(AnAlias: Word; FreeMapping: Boolean): TLccAliasMapping;
  end;

implementation

{ TLccAliasMappingServer }

constructor TLccAliasMappingServer.Create;
begin
  inherited Create;
  {$IFDEF DELPHI}
  FMappingList := TObjectList<TLccAliasMapping>.Create(True);
  FInProcessMessageList := TObjectList<TLccMessage>.Create(True);
  {$ELSE}
  FMappingList := TObjectList.Create(True);
  FInProcessMessageList := TObjectList.Create(True);
  {$ENDIF}
  WorkerMessage := TLccMessage.Create;
end;

destructor TLccAliasMappingServer.Destroy;
begin
  FreeAndNil(FMappingList);
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FInProcessMessageList);
  inherited Destroy;
end;

function TLccAliasMappingServer.FindMapping(AnAliasID: Word): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  for i := 0 to MappingList.Count - 1 do
  begin
    TestMapping := MappingList.Items[i] as TLccAliasMapping;
    if TestMapping.NodeAlias = AnAliasID then
    begin
      Result := TestMapping;
      Break;
    end;
  end;
end;

function TLccAliasMappingServer.FindMapping(ANodeID: TNodeID): TLccAliasMapping;
var
  i: Integer;
  TestMapping: TLccAliasMapping;
begin
  Result := nil;                      // Needs to Sort then do a binary search here eventually
  for i := 0 to MappingList.Count - 1 do
  begin
    TestMapping := MappingList.Items[i] as TLccAliasMapping;
    if (TestMapping.NodeID[0] = ANodeID[0]) and (TestMapping.NodeID[1] = ANodeID[1]) then
    begin
      Result := TestMapping;
      Break;
    end;
  end;
end;

procedure TLccAliasMappingServer.FlushInProcessMessages;
begin
  InProcessMessageList.Clear;
end;

function TLccAliasMappingServer.AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
begin
  Result := FindMapping(AnAlias);
  if not Assigned(Result) then
  begin
    Result := TLccAliasMapping.Create;
    Result.NodeID := AnID;
    Result.NodeAlias := AnAlias;
    MappingList.Add(Result);
  end;
end;

function TLccAliasMappingServer.RemoveMapping(AnAlias: Word; FreeMapping: Boolean): TLccAliasMapping;
var
  TestMapping: TLccAliasMapping;
  WasOwnsObject: Boolean;
begin
  Result := nil;
  TestMapping := FindMapping(AnAlias);
  if Assigned(TestMapping) then
  begin
    WasOwnsObject := MappingList.OwnsObjects;
    if not FreeMapping then
      MappingList.OwnsObjects := False;
    MappingList.Remove(TestMapping);
    MappingList.OwnsObjects := WasOwnsObject;
    Result := TestMapping;
  end;
end;

end.

