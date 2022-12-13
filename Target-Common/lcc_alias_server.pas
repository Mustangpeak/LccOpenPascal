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

  public
    property MappingList: TThreadList read FMappingList write FMappingList;


    constructor Create;
    destructor Destroy; override;

    procedure Clear;
    function Count: Integer;
    function FindMapping(AnAliasID: Word): TLccAliasMapping; overload;
    function FindMapping(ANodeID: TNodeID): TLccAliasMapping; overload;
    function AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
    function MarkForRemovalByAlias(AnAlias: Word): TLccAliasMapping;
    {$IFDEF WriteLnDebug}procedure WriteMapping(AComment: string; AMapping: TLccAliasMapping); {$ENDIF}
  end;

var
  AliasServer: TLccAliasServer;

implementation

{ TLccAliasServer }

constructor TLccAliasServer.Create;
begin
  inherited Create;
  FMappingList := TThreadList.Create;
end;

destructor TLccAliasServer.Destroy;
begin
  Clear;
  FreeAndNil(FMappingList);
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

function TLccAliasServer.AddMapping(AnAlias: Word; AnID: TNodeID): TLccAliasMapping;
begin
  Result := nil;

  Assert(AnAlias <> 0, 'Alias = 0 in TLccAliasServer.AddMapping');
  Assert(not NullNodeID(AnID), 'NodeID = NULL_ID in TLccAliasServer.AddMapping');

  MappingList.LockList;
  try
    Result := FindMapping(AnAlias);
    if not Assigned(Result) then
    begin
      Result := TLccAliasMapping.Create;
      Result.NodeID := AnID;
      Result.NodeAlias := AnAlias;
      Result.MarkedForInsertion := True;
      MappingList.Add(Result);
      {$IFDEF WriteLnDebug}WriteMapping('New Mapping', Result);{$ENDIF}
    end;
  finally
    MappingList.UnlockList;
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

