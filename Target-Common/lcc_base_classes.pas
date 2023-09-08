unit lcc_base_classes;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

uses
  Classes,
  SysUtils,
  lcc_alias_server,
  lcc_defines;


type

  // Base struture that contains the two types of LCC node identifiers, the long
  // globally unique ID or NodeID and the local Alias (if running CAN/Gridconnect)

  { TLccNodeIdentificationObject }

  TLccNodeIdentificationObject = class
  private
    FActive: Boolean;
    FAlias: Word;
    FNodeID: TNodeID;
    FAbandonCount: Integer;
    function GetValid: Boolean;  // Valid means one or the other is non zero, not that is is mapped
  public
    property NodeID: TNodeID read FNodeID write FNodeID;
    property Alias: Word read FAlias write FAlias;
    property Active: Boolean read FActive write FActive;
    property AbandonCount: Integer read FAbandonCount write FAbandonCount;
    property Valid: Boolean read GetValid;

    procedure AssignID(ANodeID: TNodeID; AnAlias: Word); overload;
    procedure AssignID(ANodeIdentification: TLccNodeIdentificationObject); overload;
    function Clone: TLccNodeIdentificationObject;
    function Compare(TestObject: TLccNodeIdentificationObject): Boolean; overload;
    function Compare(TestMapping: TLccAliasMapping): Boolean overload;
    function CompareEitherOr(TestObject: TLccNodeIdentificationObject): Boolean; overload;
    function CompareEitherOr(TestMapping: TLccAliasMapping): Boolean; overload;
  end;



  // List that manages multiple Node Identification ID pairs

  { TLccNodeIdentificationObjectList }

  TLccNodeIdentificationObjectList = class(TList)
  private
    function GetDestination: TLccNodeIdentificationObject;
    function GetIdentification(Index: Integer): TLccNodeIdentificationObject;
    function GetSource: TLccNodeIdentificationObject;
    procedure SetIdentification(Index: Integer; AValue: TLccNodeIdentificationObject);
  public
    property NodeIdentification[Index: Integer]: TLccNodeIdentificationObject read GetIdentification write SetIdentification; default;
    property Source: TLccNodeIdentificationObject read GetSource;
    property Destination: TLccNodeIdentificationObject read GetDestination;

    constructor Create(AutoCreateSourceDestination: Boolean = True);
    destructor Destroy; override;
    procedure Clear; override;

    // Returns true if all the Identifications have been filled in or are not active anyway
    function IdentificationsValid: Boolean;
    // Tests if the passed Identification is already in the list
    function IsDuplicate(DestinationObject: TLccNodeIdentificationObject): Boolean; overload;
    function IsDuplicate(ANodeID: TNodeID; AnAlias: Word): Boolean; overload;
    // Updates the list with the new mapping that is passed
    function ProcessNewMapping(NewMapping: TLccAliasMapping): Boolean;
    // Removes any Identification objects that match the mapping
    procedure RemoveIdentification(AliasMapping: TLccAliasMapping);
    // Clears the list other than the first 2 (source/destination) which it just zeros and sets to defaults
    procedure ClearIdentifications(AutoCreateSourceDestination: Boolean = True);
    // Returns True if the mapping that is going away is associated with any alias in this list
    function LogOut(AnAlias: Word): Boolean;
    // Adds and sets the properities of a TLccNodeIdentificationObject
    function AddNodeIdentificationObject(ANodeID: TNodeID; AnAlias: Word): TLccNodeIdentificationObject;
    //
    function RetryCountMaxedOut(ATestCount: Integer): Boolean;
  end;


  { TLccNodeIdentificationBaseObject }

  TLccNodeIdentificationBaseObject = class
  private
    FNodeIdentification: TLccNodeIdentificationObject;
  public
    property NodeIdentification: TLccNodeIdentificationObject read FNodeIdentification write FNodeIdentification;

    constructor Create; virtual;
    destructor Destroy; override;
    function Clone: TLccNodeIdentificationBaseObject;
  end;

  { TLccSearchTrainObject }

  TLccSearchTrainObject = class(TLccNodeIdentificationBaseObject)
  private
    FSearchCriteria: DWord;
  public
    property SearchCriteria: DWord read FSearchCriteria write FSearchCriteria;
    function Clone: TLccSearchTrainObject;
  end;

  { TLccListenerObject }
  TLccListenerObject = class(TLccNodeIdentificationBaseObject)
  private
    FChildren: TList;
    FFlags: DWord;
    FListIndex: Integer;
    FParentListener: TLccListenerObject;
    function GetChildCount: Integer;
    function GetFLinkFn: Boolean;
    function GetHidden: Boolean;
    function GetIsRoot: Boolean;
    function GetLinkF0: Boolean;
    function GetParent: TLccListenerObject;
    function GetReverseDir: Boolean;
  protected
    property Children: TList read FChildren write FChildren;
    property ParentListener: TLccListenerObject read FParentListener write FParentListener;
  public
    property Flags: DWord read FFlags write FFlags;
    property ListIndex: Integer read FListIndex write FListIndex;
    property Hidden: Boolean read GetHidden;
    property ReverseDir: Boolean read GetReverseDir;
    property LinkF0: Boolean read GetLinkF0;
    property LinkFn: Boolean read GetFLinkFn;

    property ChildCount: Integer read GetChildCount;
    property Parent: TLccListenerObject read GetParent;
    property IsRoot: Boolean read GetIsRoot;

    constructor Create; override;
    destructor Destroy; override;
    procedure AddChild(NewChild: TLccListenerObject); overload;
    function AddChild(ANodeID: TNodeID; AnAlias: Word; AFlags: Byte): TLccListenerObject; overload;
    function AddChild(NodeIdentificationObject:  TLccNodeIdentificationObject; AFlags: Byte): TLccListenerObject; overload;
    function Clone: TLccListenerObject;
  end;



implementation

{ TLccNodeIdentificationBaseObject }

destructor TLccNodeIdentificationBaseObject.Destroy;
begin
  FreeAndNil(FNodeIdentification);
  inherited;
end;

function TLccNodeIdentificationBaseObject.Clone: TLccNodeIdentificationBaseObject;
begin
  Result := TLccNodeIdentificationBaseObject.Create;
  Result.NodeIdentification.AssignID(NodeIdentification);
end;

constructor TLccNodeIdentificationBaseObject.Create;
begin
  FNodeIdentification := TLccNodeIdentificationObject.Create;
end;


{ TLccNodeIdentificationObject }

function TLccNodeIdentificationObject.GetValid: Boolean;
begin
  Result := (Alias <> 0) or ((NodeID[0] <> 0) or (NodeID[1] <> 0))
end;

procedure TLccNodeIdentificationObject.AssignID(ANodeID: TNodeID; AnAlias: Word);
begin
  Alias := AnAlias;
  NodeID := ANodeID;
  FActive := True;
end;

procedure TLccNodeIdentificationObject.AssignID(ANodeIdentification: TLccNodeIdentificationObject);
begin
  NodeID := ANodeIdentification.NodeID;
  Alias := ANodeIdentification.Alias;
end;

function TLccNodeIdentificationObject.Clone: TLccNodeIdentificationObject;
begin
  Result := TLccNodeIdentificationObject.Create;
  Result.NodeID := NodeID;
  Result.Alias := Alias;
  Result.Active := Active;
  Result.AbandonCount := 0;
end;

function TLccNodeIdentificationObject.Compare(TestObject: TLccNodeIdentificationObject): Boolean;
begin
  Result := (TestObject.Alias = Alias) and (TestObject.NodeID[0] = NodeID[0]) and (TestObject.NodeID[0] = NodeID[0])
end;

function TLccNodeIdentificationObject.Compare(TestMapping: TLccAliasMapping): Boolean;
begin
  Result := (TestMapping.NodeAlias = Alias) and (TestMapping.NodeID[0] = NodeID[0]) and (TestMapping.NodeID[0] = NodeID[0])
end;

function TLccNodeIdentificationObject.CompareEitherOr(TestObject: TLccNodeIdentificationObject): Boolean;
begin
  Result := (TestObject.Alias = Alias) or ((TestObject.NodeID[0] = NodeID[0]) and (TestObject.NodeID[0] = NodeID[0]))
end;

function TLccNodeIdentificationObject.CompareEitherOr(TestMapping: TLccAliasMapping): Boolean;
begin
  Result := (TestMapping.NodeAlias = Alias) or ((TestMapping.NodeID[0] = NodeID[0]) and (TestMapping.NodeID[0] = NodeID[0]))
end;


{ TLccNodeIdentificationObjectList }

function TLccNodeIdentificationObjectList.GetDestination: TLccNodeIdentificationObject;
begin
  Result := TLccNodeIdentificationObject(Items[1]);
end;

function TLccNodeIdentificationObjectList.GetIdentification(Index: Integer): TLccNodeIdentificationObject;
begin
  Result := nil;
  if Index < Count then
    Result := TLccNodeIdentificationObject(Items[Index]);
end;

function TLccNodeIdentificationObjectList.GetSource: TLccNodeIdentificationObject;
begin
  Result := TLccNodeIdentificationObject(Items[0]);
end;

procedure TLccNodeIdentificationObjectList.SetIdentification(Index: Integer; AValue: TLccNodeIdentificationObject);
begin
  if Index < Count then
  begin
    TObject(Items[Index]).Free;
    Items[Index] := AValue;
  end;
end;

constructor TLccNodeIdentificationObjectList.Create(AutoCreateSourceDestination: Boolean);
begin
  inherited Create;
  if AutoCreateSourceDestination then
  begin
    Add(TLccNodeIdentificationObject.Create);  // Create the Source
    Add(TLccNodeIdentificationObject.Create);  // Create the Destination
  end
end;

destructor TLccNodeIdentificationObjectList.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TLccNodeIdentificationObjectList.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to Count - 1 do
    begin
      TObject(Items[i]).Free
    end;
  finally
    inherited Clear;
  end;
end;

function TLccNodeIdentificationObjectList.IdentificationsValid: Boolean;
var
  i: Integer;
  TempValid: Boolean;
begin
  TempValid := True;
  for i := 0 to Count - 1 do
  begin
    if NodeIdentification[i].Active then
      if not NodeIdentification[i].Valid then
      begin
        TempValid := False;
        Break
      end;
  end;
  Result := TempValid;
end;

function TLccNodeIdentificationObjectList.IsDuplicate(DestinationObject: TLccNodeIdentificationObject): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    if DestinationObject.CompareEitherOr(NodeIdentification[i]) then
    begin
      Result := True;
      Break
    end;
  end;
end;

function TLccNodeIdentificationObjectList.IsDuplicate(ANodeID: TNodeID; AnAlias: Word): Boolean;
var
  i: Integer;
  NodeIdentificationObject: TLccNodeIdentificationObject;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    NodeIdentificationObject := TLccNodeIdentificationObject( Items[i]);
    if (NodeIdentificationObject.NodeID[0] = ANodeID[0]) and (NodeIdentificationObject.NodeID[1] = ANodeID[1]) and (NodeIdentificationObject.Alias = AnAlias) then
    begin
      Result := True;
      Break
    end;
  end;
end;

function TLccNodeIdentificationObjectList.ProcessNewMapping(NewMapping: TLccAliasMapping): Boolean;
var
  i: Integer;
begin
  Result := False;
  if Assigned(NewMapping) then
    begin
    for i := 0 to Count - 1 do
    begin
      if NodeIdentification[i].Active and NodeIdentification[i].CompareEitherOr(NewMapping) then
        NodeIdentification[i].AssignID(NewMapping.NodeID, NewMapping.NodeAlias);
    end;
    Result := IdentificationsValid;  // This is the cheap and easy way... may update
  end;
end;

procedure TLccNodeIdentificationObjectList.RemoveIdentification(
  AliasMapping: TLccAliasMapping);
var
  i: Integer;
begin
  for i := 0 to Count - 1 do
  begin
    if NodeIdentification[i].Compare(AliasMapping) then
    begin
      NodeIdentification[i].Free;
      Delete(i);
      Break
    end;
  end;
end;

procedure TLccNodeIdentificationObjectList.ClearIdentifications(AutoCreateSourceDestination: Boolean);
begin
  Clear;
  if AutoCreateSourceDestination then
  begin
    Add(TLccNodeIdentificationObject.Create);  // Create the Source
    Add(TLccNodeIdentificationObject.Create);  // Create the Destination
  end;
end;

function TLccNodeIdentificationObjectList.LogOut(AnAlias: Word): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 0 to Count - 1 do
  begin
    Result := TLccNodeIdentificationObject(Items[i]).Alias = AnAlias;
    if Result then
      Break
  end;
end;

function TLccNodeIdentificationObjectList.AddNodeIdentificationObject(ANodeID: TNodeID; AnAlias: Word): TLccNodeIdentificationObject;
begin
  Result := TLccNodeIdentificationObject.Create;
  Result.AssignID(ANodeID, AnAlias);
  Add(Result)
end;

function TLccNodeIdentificationObjectList.RetryCountMaxedOut(ATestCount: Integer): Boolean;
var
  i: Integer;
begin
  Result := True;

  for i := 0 to Count - 1 do
  begin
    if TLccNodeIdentificationObject( Items[i]).Valid then
    begin
      if TLccNodeIdentificationObject( Items[i]).AbandonCount < ATestCount then
      begin
        Result := False;
        Break
      end;
    end;
  end;
end;

{ TLccListenerObject }

function TLccListenerObject.GetFLinkFn: Boolean;
begin
  Result := Flags and TRACTION_LISTENER_FLAG_LINK_FN = TRACTION_LISTENER_FLAG_LINK_FN;
end;

function TLccListenerObject.GetChildCount: Integer;
begin
  Result := Children.Count;
end;

function TLccListenerObject.GetHidden: Boolean;
begin
  Result := Flags and TRACTION_LISTENER_FLAG_HIDDEN = TRACTION_LISTENER_FLAG_HIDDEN;
end;

function TLccListenerObject.GetIsRoot: Boolean;
begin
  Result := ParentListener = nil;
end;

function TLccListenerObject.GetLinkF0: Boolean;
begin
  Result := Flags and TRACTION_LISTENER_FLAG_LINK_F0 = TRACTION_LISTENER_FLAG_LINK_F0;
end;

function TLccListenerObject.GetParent: TLccListenerObject;
begin
  Result := ParentListener;
end;

function TLccListenerObject.GetReverseDir: Boolean;
begin
  Result := Flags and TRACTION_LISTENER_FLAG_REVERSE_DIR = TRACTION_LISTENER_FLAG_REVERSE_DIR;
end;

constructor TLccListenerObject.Create;
begin
  inherited Create;
  Children := TList.Create;
end;

destructor TLccListenerObject.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

procedure TLccListenerObject.AddChild(NewChild: TLccListenerObject);
begin
  NewChild.ParentListener := Self;
  Children.Add(NewChild);
end;

function TLccListenerObject.AddChild(ANodeID: TNodeID; AnAlias: Word; AFlags: Byte): TLccListenerObject;
begin
  Result := TLccListenerObject.Create;
  Result.NodeIdentification.AssignID(ANodeID, AnAlias);
  Result.Flags := AFlags;
  AddChild(Result);
end;

function TLccListenerObject.AddChild(NodeIdentificationObject: TLccNodeIdentificationObject; AFlags: Byte): TLccListenerObject;
begin
  Result := TLccListenerObject.Create;
  Result.NodeIdentification.AssignID(NodeIdentificationObject);
  Result.Flags := AFlags;
  AddChild(Result);
end;

function TLccListenerObject.Clone: TLccListenerObject;
begin
  Result := TLccListenerObject.Create;
  Result.NodeIdentification.AssignID(NodeIdentification);
  Result.Flags := Flags;
end;

{ TLccSearchTrainObject }

function TLccSearchTrainObject.Clone: TLccSearchTrainObject;
begin
  Result := TLccSearchTrainObject.Create;
  Result.NodeIdentification.AssignID(NodeIdentification);
  Result.SearchCriteria := SearchCriteria;
end;

end.

