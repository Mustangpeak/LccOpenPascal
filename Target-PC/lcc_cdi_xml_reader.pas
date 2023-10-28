unit lcc_cdi_xml_reader;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

 uses
  {$IFDEF LCC_DELPHI}
  System.Classes,
  System.SysUtils,
  {$ELSE}
  {$ENDIF}
  lcc_defines,
  lcc_utilities,
  lcc_node,
  lcc_xmlutilities;


type

  TLccCdiGroup = class;
  TLccCdiElementInt = class;
  TLccCdiElementStr = class;
  TLccCdiElementEventId = class;
  TLccCdiMapRelationList = class;


   // Holds the Property/Value pairs for a Map type Configuration

  { TLccCdiMapRelation }

  TLccCdiMapRelation = class
  private
    FProp: LccDOMString;     // Think of this as the "Key" for the relation, often an integer but does not need to be could be any datatype
    FValue: LccDOMString;    // Think of this as the "human readable" part of the relation that maps to a "Key"
  public
    constructor Create(AValue, AProperty: LccDOMString);
    property Value: LccDOMString read FValue write FValue;
    property Prop: LccDOMString read FProp write FProp;
  end;

  // Holds TMapRelations for a Map type Configuration
  { TLccCdiMapRelationList }

  TLccCdiMapRelationList = class
  private
    FList: TList;
    function GetCount: Integer;
    function GetRelation(Index: Integer): TLccCdiMapRelation;
    procedure SetRelation(Index: Integer; AValue: TLccCdiMapRelation);
  protected
    property List: TList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Relation: TLccCdiMapRelation);
    procedure AddRelation( AValue, AProperty: LccDOMString);
    procedure ClearList;
    property Count: Integer read GetCount;
    property Relations[Index: Integer]: TLccCdiMapRelation read GetRelation write SetRelation; default;
  end;

  //----------------------------------------------------------------------------
  // Core Objects
  //----------------------------------------------------------------------------

  {TLccCidElement}
  TLccCidCore = class
  private
    FChildElements: TList;
    FMemorySpace: Byte;
    FAddressAbsolute: Int64;
    FLevel: Integer;
    FEngineMemoryAccess: TLccTaskMemorySpaceAccess;
    FAutoLoadConfigMem: Boolean;
    FName: LccDOMString;
    FDescription: LccDOMString;
    function GetChildElement(Index: Integer): TLccCidCore;
    function GetChildElementCount: Integer;
  protected
    property MemorySpace: Byte read FMemorySpace write FMemorySpace;
    property AddressAbsolute: Int64 read FAddressAbsolute write FAddressAbsolute;
    property ChildElements: TList read FChildElements write FChildElements;
  public
    property Name: LccDOMString read FName write FName;
    property Description: LccDOMString read FDescription write FDescription;
    property AutoLoadConfigMem: Boolean read FAutoLoadConfigMem;
    property ChildElement[Index: Integer]: TLccCidCore read GetChildElement;
    property ChildElementCount: Integer read GetChildElementCount;
    property Level: Integer read FLevel;
    property EngineMemoryAccess: TLccTaskMemorySpaceAccess read FEngineMemoryAccess write FEngineMemoryAccess;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure ClearChildElements;
  end;

  {TLccCdiElement}
  TLccCdiElementCore = class(TLccCidCore)
  private
    FSize: Int64;    // Size of the Data for the Element
    FMapRelations: TLccCdiMapRelationList;  // Any Map Relations (predefined values)
  protected
    procedure EnumerateMapping(Node: TLccXmlNode);
    procedure EnumerateElement(Node: TLccXmlNode);
    procedure CallbackReadConfigurationMemory(ATask: TLccTaskBase); virtual; abstract;
    procedure CallbackWriteConfigurationMemory(ATask: TLccTaskBase); virtual; abstract;
  public
    property AddressAbsolute;          // The Address to read/write in the configuration memory for this Element
    property MemorySpace;              // What Memeory Space to read/write to
    property MapRelations: TLccCdiMapRelationList read FMapRelations;
    property Size: Int64 read FSize write FSize;

    constructor Create; override;
    destructor Destroy; override;
  end;

  //----------------------------------------------------------------------------
  // Xml Element Objects
  //----------------------------------------------------------------------------

  {TLccCdiIdentification}
  TLccCdiIdentification = class(TLccCidCore)
  public

  end;

  {TLccCdiSegment}
  TLccCdiSegment = class(TLccCidCore)
  public
    property AddressAbsolute;
    property MemorySpace;

    procedure AddGroup(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64);
    procedure AddElementInt(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64);
    procedure AddElementStr(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64);
    procedure AddElementEventId(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64);

  end;

  {TLccCdiAcdi}
  TLccCdiAcdi = class(TLccCidCore)
  public
  end;

 {TLccCdiGroup}
  TLccCdiGroup = class(TLccCdiSegment) // Specialized Segment
  private
    FReplicationCount: Integer;  // How many times this group is replicated
    FRepeatName: LccDOMString;   // Label that is repeated in each instance with the index attached (today starts at 0)
    FRepeatStartIndex: Integer;  // Anticipting the future where we can define where the index starts
  protected
    procedure EnumerateGroup(GroupNode: TLccXmlNode; var CurrentAddressAbsolute: Int64; ReplicationNumber: Integer);
  public
    property RepeatName: LccDOMString read FRepeatName write FRepeatName;
    property RepeatStartIndex: Integer read FRepeatStartIndex write FRepeatStartIndex;
    property AddressAbsolute;
    property ReplicationCount: Integer read FReplicationCount write FReplicationCount;
  end;

  {TLccCdiElementInt}
  TLccCdiElementInt = class(TLccCdiElementCore)
  private
    FElementInt: Integer;
    FMax: Integer;
    FMinValid: Boolean;
    FMin: Integer;
    FValue: Integer;
    FMaxValid: Boolean;
    FDefaultValue: Integer;
    FDefaultValueValid: Boolean;
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetValue(const Value: Integer);
    procedure SetDefaultValue(const Value: Integer);
    function GetValueAsStr: LccDOMString;
    function GetValueAsHex: LccDOMString;
    procedure SetValueAsStr(const Value: LccDOMString);
    procedure SetValueAsHex(const Value: LccDOMString);
  protected
    procedure CallbackReadConfigurationMemory(ATask: TLccTaskBase); override;
    procedure CallbackWriteConfigurationMemory(ATask: TLccTaskBase); override;
  public
    property ElementInt: Integer read FElementInt write FElementInt;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property DefaultValue: Integer read FDefaultValue write SetDefaultValue;
    property MinValid: Boolean read FMinValid;
    property MaxValid: Boolean read FMaxValid;
    property DefaultValueValid: Boolean read FDefaultValueValid;
    property Value: Integer read FValue write SetValue;
    property ValueAsStr: LccDOMString read GetValueAsStr write SetValueAsStr;
    property ValueAsHex: LccDOMString read GetValueAsHex write SetValueAsHex;
  end;

  {TLccCdiElementStr}
  TLccCdiElementStr = class(TLccCdiElementCore)
  private
    FElementStr: LccDOMString;
  protected
    procedure CallbackReadConfigurationMemory(ATask: TLccTaskBase); override;
    procedure CallbackWriteConfigurationMemory(ATask: TLccTaskBase); override;
  public
    property ElementStr: LccDOMString read FElementStr write FElementStr;
  end;

  {TLccCdiElementEventId}
  TLccCdiElementEventId = class(TLccCdiElementCore)
  private
    FElementEventId: TEventID;
    function GetElementEventIdAsStr: LccDOMString;
    function GetElementEventIdAsStrWithDots: LccDOMString;
    procedure SetElementEventIDAsStr(const Value: LccDOMString);
  protected
    procedure CallbackReadConfigurationMemory(ATask: TLccTaskBase); override;
    procedure CallbackWriteConfigurationMemory(ATask: TLccTaskBase); override;
  public
    property ElementEventId: TEventID read FElementEventId write FElementEventId;
    property ElementEventIdAsStr: LccDOMString read GetElementEventIdAsStr write SetElementEventIDAsStr;
    property ElementEventIdAsStrWithDots: LccDOMString read GetElementEventIdAsStrWithDots;

    constructor Create; override;
  end;

  {TLccCdiRoot}
  TLccCdiRoot = class(TLccCidCore)
  private
    FXMLDocument: TLccXmlDocument;
  protected
    procedure EnumerateIdentification(IdentificationNode: TLccXmlNode);
    procedure EnumerateSegment(SegmentNode: TLccXmlNode);
  public
    property XMLDocument: TLccXmlDocument read FXMLDocument write FXMLDocument;

    constructor Create(ANode: TLccNode); reintroduce;
    destructor Destroy; override;
    function AddSegment(SegmentNode: TLccXmlNode): TLccCdiSegment;
    procedure AddAcdi;
    procedure AddIdentification(IdentificationNode: TLccXmlNode);
    function BuildTree(DoAutoLoadConfigMem: Boolean): Boolean;
    function LoadFromFile(FilePath: string): Boolean;
    function LoadFromCDI(CDIFile: string): Boolean;
    procedure SetTargetNode(ANodeID: TNodeID; ANodeAlias: Word);
  end;

implementation

{ TLccCidCore }

procedure TLccCidCore.ClearChildElements;
var
  i: Integer;
begin
  try
    for i := 0 to ChildElements.Count - 1 do
      TObject( ChildElements[i]).Free;
  finally
    ChildElements.Clear;
  end;

end;

constructor TLccCidCore.Create;
begin
  FChildElements := TList.Create;
end;

destructor TLccCidCore.Destroy;
begin
  ClearChildElements;
  ChildElements.Free;
  inherited;
end;

function TLccCidCore.GetChildElement(Index: Integer): TLccCidCore;
begin
  Result := TLccCidCore( ChildElements[Index]);
end;

function TLccCidCore.GetChildElementCount: Integer;
begin
  Result := ChildElements.Count;
end;


{ TLccCdiSegment }

procedure TLccCdiSegment.AddElementEventId(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64);
var
  AnElementEventId: TLccCdiElementEventId;
begin
  AnElementEventId := TLccCdiElementEventId.Create;
  AnElementEventId.MemorySpace := MemorySpace;

  if XmlAttributeExists(Node, 'offset') then
    CurrentAddressAbsolute := CurrentAddressAbsolute + StrToInt( XmlAttributeRead(Node, 'offset'));
  if XmlAttributeExists(Node, 'size') then
    AnElementEventId.Size := StrToInt( XmlAttributeRead(Node, 'size'));

  AnElementEventId.AddressAbsolute := CurrentAddressAbsolute;
  Inc(CurrentAddressAbsolute, SizeOf(AnElementEventId.ElementEventId));
  AnElementEventId.EngineMemoryAccess := EngineMemoryAccess;
  AnElementEventId.FAutoLoadConfigMem := AutoLoadConfigMem;
  AnElementEventId.FLevel := Level + 1;
  ChildElements.Add(AnElementEventId);

  if Assigned(EngineMemoryAccess) and AutoLoadConfigMem then
    EngineMemoryAccess.Assign(
      lems_Read,
      AnElementEventId.MemorySpace,
      False,
      AnElementEventId.AddressAbsolute,
      AnElementEventId.AddressAbsolute + AnElementEventId.Size,
      True,
      EngineMemoryAccess.TargetNodeID,
      EngineMemoryAccess.TargetAlias,
      {$IFNDEF LCC_DELPHI}@{$ENDIF}AnElementEventId.CallbackReadConfigurationMemory);

  AnElementEventId.EnumerateElement(Node);   // no child can modify the address as "offset" is not defined for defined children
end;

procedure TLccCdiSegment.AddElementInt(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64);
var
  AnElementInt: TLccCdiElementInt;
begin
  AnElementInt := TLccCdiElementInt.Create;
  AnElementInt.MemorySpace := MemorySpace;

  // Handle the possible attributes
  if XmlAttributeExists(Node, 'offset') then
    CurrentAddressAbsolute := CurrentAddressAbsolute + StrToInt( XmlAttributeRead(Node, 'offset'));
  if XmlAttributeExists(Node, 'size') then
    AnElementInt.Size := StrToInt( XmlAttributeRead(Node, 'size')) else AnElementInt.Size := 1;    // Default
  if XmlAttributeExists(Node, 'min') then
    AnElementInt.Min := StrToInt( XmlAttributeRead(Node, 'min'));
  if XmlAttributeExists(Node, 'max') then
    AnElementInt.Max := StrToInt( XmlAttributeRead(Node, 'max'));
  if XmlAttributeExists(Node, 'default') then
    AnElementInt.DefaultValue := StrToInt( XmlAttributeRead(Node, 'default'));

  AnElementInt.AddressAbsolute := CurrentAddressAbsolute;
  Inc(CurrentAddressAbsolute, AnElementInt.Size);
  AnElementInt.EngineMemoryAccess := EngineMemoryAccess;
  AnElementInt.FAutoLoadConfigMem := AutoLoadConfigMem;
  AnElementInt.FLevel := Level + 1;
  ChildElements.Add(AnElementInt);

  if Assigned(EngineMemoryAccess) and AutoLoadConfigMem then
    EngineMemoryAccess.Assign(
      lems_Read,
      AnElementInt.MemorySpace,
      False,
      AnElementInt.AddressAbsolute,
      AnElementInt.AddressAbsolute + AnElementInt.Size,
      True,
      EngineMemoryAccess.TargetNodeID,
      EngineMemoryAccess.TargetAlias,
      {$IFNDEF LCC_DELPHI}@{$ENDIF}AnElementInt.CallbackReadConfigurationMemory);

  AnElementInt.EnumerateElement(Node);   // no child can modify the address as "offset" is not defined for defined children
end;

procedure TLccCdiSegment.AddElementStr(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64);
var
  AnElementStr: TLccCdiElementStr;
begin
  AnElementStr := TLccCdiElementStr.Create;
  AnElementStr.MemorySpace := MemorySpace;

  if XmlAttributeExists(Node, 'offset') then
    CurrentAddressAbsolute := CurrentAddressAbsolute + StrToInt( XmlAttributeRead(Node, 'offset'));
  if XmlAttributeExists(Node, 'size') then
    AnElementStr.Size := StrToInt( XmlAttributeRead(Node, 'size'));

  AnElementStr.AddressAbsolute := CurrentAddressAbsolute;
  Inc(CurrentAddressAbsolute, AnElementStr.Size);
  AnElementStr.EngineMemoryAccess := EngineMemoryAccess;
  AnElementStr.FAutoLoadConfigMem := AutoLoadConfigMem;
  AnElementStr.FLevel := Level + 1;
  ChildElements.Add(AnElementStr);

  if Assigned(EngineMemoryAccess) and AutoLoadConfigMem then
    EngineMemoryAccess.Assign(
      lems_Read,
      AnElementStr.MemorySpace,
      False,
      AnElementStr.AddressAbsolute,
      AnElementStr.AddressAbsolute + AnElementStr.Size,
      True,
      EngineMemoryAccess.TargetNodeID,
      EngineMemoryAccess.TargetAlias,
      {$IFNDEF LCC_DELPHI}@{$ENDIF}AnElementStr.CallbackReadConfigurationMemory);

  AnElementStr.EnumerateElement(Node);   // no child can modify the address as "offset" is not defined for defined children
end;

procedure TLccCdiSegment.AddGroup(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64);
var
  AGroup: TLccCdiGroup;
//  i: Integer;
begin
  // Group is complicated.  It can modify the current Address Point, have attribute and have elements
  AGroup := TLccCdiGroup.Create;
  // Everyone gets the Address Space of the Segment propagated to them
  AGroup.MemorySpace := MemorySpace;
  // "offset" attribute is optional to modify the address pointer
  if XmlAttributeExists(Node, 'offset') then
    CurrentAddressAbsolute := CurrentAddressAbsolute + StrToInt( XmlAttributeRead(Node, 'offset'));
  // "replication" attribute is optional to duplicate this group N times
  if XmlAttributeExists(Node, 'replication') then
    AGroup.ReplicationCount := StrToInt( XmlAttributeRead(Node, 'replication'));

  // Add the Group to the list
  AGroup.EngineMemoryAccess := EngineMemoryAccess;
  AGroup.FAutoLoadConfigMem := AutoLoadConfigMem;
  AGroup.FLevel := Level + 1;
  ChildElements.Add(AGroup);

 // if AGroup.ReplicationCount > 0 then
 // begin
 //   for i := 1 to AGroup.ReplicationCount do
 //     AGroup.EnumerateGroup(Node, CurrentAddressAbsolute, i);
 // end else
    AGroup.EnumerateGroup(Node, CurrentAddressAbsolute, 0);
end;

{ TLccCdiElement }

procedure TLccCdiElementCore.EnumerateMapping(Node: TLccXmlNode);
var
  ChildNode: TLccXmlNode;
  PropertyStr, ValueStr, ChildNodeName: LccDOMString;
begin
   // These children can't have attributes that modify address pointers or anything
   if Assigned(Node) then
   begin
     ChildNode := XmlFirstChild(Node);
     while Assigned(ChildNode) do
     begin
       ChildNodeName := XmlNodeName(ChildNode);
       if ChildNodeName = 'name' then
       begin end                               // Today I don't know what to do with these... have proposal out to remove them
       else
       if ChildNodeName = 'description' then
       begin end                              // Today I don't know what to do with these... have proposal out to remove them
       else
       if ChildNodeName = 'relation' then
       begin
         PropertyStr := XmlNodeFindChildNodeTextContent(ChildNode, 'property');
         ValueStr := XmlNodeFindChildNodeTextContent(ChildNode, 'value');
         MapRelations.AddRelation(ValueStr, PropertyStr);
       end;
       ChildNode := XmlNextSiblingNode(ChildNode)
     end;
   end;
end;

constructor TLccCdiElementCore.Create;
begin
  inherited Create;
  FMapRelations := TLccCdiMapRelationList.Create;
end;

destructor TLccCdiElementCore.Destroy;
begin
  MapRelations.Free;
  inherited;
end;

procedure TLccCdiElementCore.EnumerateElement(Node: TLccXmlNode);
var
  ChildNode: TLccXmlNode;
  ChildNodeName: LccDOMString;
begin
  ChildNode := XmlFirstChild(Node);
  while Assigned(ChildNode) do
  begin
    ChildNodeName := XmlNodeName(ChildNode);
    if ChildNodeName = 'name' then
      Name := XmlNodeTextContent(ChildNode)
    else
    if ChildNodeName = 'description' then
      Description := XmlNodeTextContent(ChildNode)
     else
    if ChildNodeName = 'map' then
      EnumerateMapping(ChildNode);

    ChildNode := XmlNextSiblingNode(ChildNode);
  end;

end;


{ TLccCdiGroup }

procedure TLccCdiGroup.EnumerateGroup(GroupNode: TLccXmlNode; var CurrentAddressAbsolute: Int64; ReplicationNumber: Integer);
var
  GroupChild: TLccXmlNode;
  GroupChildName: LccDOMString;
begin
  // Enumerate the Group's child Elements, the attributes have already been handled by here
  GroupChild := XmlFirstChild(GroupNode);
  while Assigned(GroupChild) do
  begin
    GroupChildName := XmlNodeName(GroupChild);
    if GroupChildName = 'name' then                // "origin" attributes not allowed by the schema so don't need to pass the address pointer
      Name := XmlNodeTextContent(GroupChild)
    else
    if GroupChildName = 'description' then         // "origin" attributes not allowed by the schema so don't need to pass the address pointer
      Description := XmlNodeTextContent(GroupChild)
    else
    if GroupChildName = 'repname' then             // "origin" attributes not allowed by the schema so don't need to pass the address pointer
      RepeatName  := XmlNodeTextContent(GroupChild)
    else
    if GroupChildName = 'group' then
      AddGroup(GroupChild, CurrentAddressAbsolute)
    else
    if GroupChildName = 'int' then
      AddElementInt(GroupChild, CurrentAddressAbsolute)
    else
    if GroupChildName = 'string' then
      AddElementStr(GroupChild, CurrentAddressAbsolute)
    else
    if GroupChildName = 'eventid' then
      AddElementEventId(GroupChild, CurrentAddressAbsolute);

    GroupChild := XmlNextSiblingNode(GroupChild);
  end;
end;

{ TLccCdiRoot }

procedure TLccCdiRoot.AddAcdi;
var
  Acdi: TLccCdiAcdi;
begin
  Acdi := TLccCdiAcdi.Create;
  Acdi.EngineMemoryAccess := EngineMemoryAccess;
  ChildElements.Add(Acdi);
end;

procedure TLccCdiRoot.AddIdentification(IdentificationNode: TLccXmlNode);
var
  Identification: TLccCdiIdentification;
begin
  Identification := TLccCdiIdentification.Create;
  Identification.EngineMemoryAccess := EngineMemoryAccess;
  ChildElements.Add(Identification);
end;

function TLccCdiRoot.AddSegment(SegmentNode: TLccXmlNode): TLccCdiSegment;
begin
  Result := TLccCdiSegment.Create;
  Result.MemorySpace := StrToInt( XmlAttributeRead(SegmentNode, 'space'));
  // "origin" is optional
  if XmlAttributeExists(SegmentNode, 'origin') then
    Result.AddressAbsolute := StrToInt( XmlAttributeRead(SegmentNode, 'origin'))
  else
    Result.AddressAbsolute := 0;

  Result.FLevel := Level + 1;
  Result.EngineMemoryAccess := EngineMemoryAccess;
  ChildElements.Add(Result);
end;

function TLccCdiRoot.BuildTree(DoAutoLoadConfigMem: Boolean): Boolean;
var
  Root, RootChildNode: TLccXmlNode;
  RootChildName : LccDOMString;
begin
  Result := False;
  ClearChildElements;
  FAutoLoadConfigMem := DoAutoLoadConfigMem;
  if Assigned(XMLDocument) then
  begin
    Root := XmlFindRootNode(XMLDocument, 'cdi');
    if Assigned(Root) then
    begin
      RootChildNode := XmlFirstChild(Root);
      while Assigned(RootChildNode) do
      begin
        RootChildName := XmlNodeName(RootChildNode);
        if RootChildName = 'identification' then
          EnumerateIdentification(RootChildNode)
        else
        if RootChildName = 'segment' then
          EnumerateSegment(RootChildNode);

        RootChildNode := XmlNextSiblingNode(RootChildNode);
      end;
    end;
  end;
end;

constructor TLccCdiRoot.Create(ANode: TLccNode);
begin
  inherited Create;
  EngineMemoryAccess := TLccTaskMemorySpaceAccess.Create(ANode)
end;

destructor TLccCdiRoot.Destroy;
begin
  ClearChildElements;
  XmlFreeDocument(FXMLDocument);
  EngineMemoryAccess.Abort;
  FreeAndNil(FEngineMemoryAccess);
  inherited;
end;

procedure TLccCdiRoot.EnumerateIdentification(IdentificationNode: TLccXmlNode);
begin
  AddIdentification(IdentificationNode);
end;

procedure TLccCdiRoot.EnumerateSegment(SegmentNode: TLccXmlNode);
var
  SegmentChild: TLccXmlNode;
  SegmentChildName : LccDOMString;
  Segment: TLccCdiSegment;
begin
  // "space" Attribute must exists, it's required
  if XmlAttributeExists(SegmentNode, 'space') then
  begin
    // Required Attribute
    Segment := AddSegment(SegmentNode);
    Segment.EngineMemoryAccess := EngineMemoryAccess;
    Segment.FAutoLoadConfigMem := AutoLoadConfigMem;

    // enumerate the segment the Segment
    SegmentChild := XmlFirstChild(SegmentNode);
    while Assigned(SegmentChild) do
    begin
      SegmentChildName := XmlNodeName(SegmentChild);
      if SegmentChildName = 'name' then
        Segment.Name := XmlNodeTextContent(SegmentChild)               // "origin" attributes not allowed by the schema so don't need to pass the address pointer
      else
      if SegmentChildName = 'description' then
        Segment.Description := XmlNodeTextContent(SegmentChild)            // "origin" attributes not allowed by the schema so don't need to pass the address pointer
      else
      if SegmentChildName = 'group' then
        Segment.AddGroup(SegmentChild, FAddressAbsolute)
      else
      if SegmentChildName = 'int' then
        Segment.AddElementInt(SegmentChild, FAddressAbsolute)
      else
      if SegmentChildName = 'string' then
        Segment.AddElementStr(SegmentChild, FAddressAbsolute)
      else
      if SegmentChildName = 'eventid' then
        Segment.AddElementEventId(SegmentChild, FAddressAbsolute);

      SegmentChild := XmlNextSiblingNode(SegmentChild);
    end;
  end;
end;

function TLccCdiRoot.LoadFromCDI(CDIFile: string): Boolean;
begin
  XmlFreeDocument(FXMLDocument);
  XMLDocument := XmlLoadFromText(CDIFile);
  Result := Assigned(XMLDocument);
end;

function TLccCdiRoot.LoadFromFile(FilePath: string): Boolean;
var
  FileStream: TFileStream;
begin
  if FileExists(FilePath) then
  begin
    FileStream := TFileStream.Create(FilePath, fmOpenRead or fmShareDenyWrite);
    try
      XMLDocument := XmlLoadFromStream(FileStream)
    finally
      FileStream.Free;
    end;
  end;
  Result := Assigned(XMLDocument);
end;

procedure TLccCdiRoot.SetTargetNode(ANodeID: TNodeID; ANodeAlias: Word);
begin
  EngineMemoryAccess.FlushMemorySpaceQueue;
  EngineMemoryAccess.TargetNodeID := ANodeID;
  EngineMemoryAccess.TargetAlias := ANodeAlias;
end;

{ TLccCdiElementInt }

procedure TLccCdiElementInt.CallbackReadConfigurationMemory(ATask: TLccTaskBase);
begin
  if ATask.TaskState = lesComplete then
    ElementInt := (ATask as TLccTaskMemorySpaceAccess).StreamAsInt;
end;

procedure TLccCdiElementInt.CallbackWriteConfigurationMemory(ATask: TLccTaskBase);
begin

end;

function TLccCdiElementInt.GetValueAsHex: LccDOMString;
begin
  Result := IntToHex(Value);
end;

function TLccCdiElementInt.GetValueAsStr: LccDOMString;
begin
  Result := IntToStr(Value);
end;

procedure TLccCdiElementInt.SetDefaultValue(const Value: Integer);
begin
  FDefaultValue := Value;
  FDefaultValueValid := True;
end;

procedure TLccCdiElementInt.SetMax(const Value: Integer);
begin
  FMax := Value;
  FMaxValid := True;
end;

procedure TLccCdiElementInt.SetMin(const Value: Integer);
begin
  FMin := Value;
  FMinValid := True;
end;

procedure TLccCdiElementInt.SetValue(const Value: Integer);
begin
  FValue := Value;
end;

procedure TLccCdiElementInt.SetValueAsHex(const Value: LccDOMString);
begin
  if Pos('$', Value) = 0 then
    FValue := StrToInt('$' + Value)
  else
    FValue := StrToInt(Value)
end;

procedure TLccCdiElementInt.SetValueAsStr(const Value: LccDOMString);
begin
  FValue := StrToInt(Value)
end;

{ TLccCdiElementEventId }

procedure TLccCdiElementEventId.CallbackReadConfigurationMemory(ATask: TLccTaskBase);
begin
  if ATask.TaskState = lesComplete then
    ElementEventId := (ATask as TLccTaskMemorySpaceAccess).StreamAsEventID;
end;

procedure TLccCdiElementEventId.CallbackWriteConfigurationMemory(ATask: TLccTaskBase);
begin

end;

constructor TLccCdiElementEventId.Create;
begin
  inherited Create;
  FSize := 8;
end;

function TLccCdiElementEventId.GetElementEventIdAsStr: LccDOMString;
begin
  Result := EventIDToString(ElementEventId, False)
end;

function TLccCdiElementEventId.GetElementEventIdAsStrWithDots: LccDOMString;
begin
  Result := EventIDToString(ElementEventId, True)
end;

procedure TLccCdiElementEventId.SetElementEventIDAsStr(const Value: LccDOMString);
begin
  FElementEventId := StrToEventID(Value)
end;

{ TLccCdiMapRelation }

constructor TLccCdiMapRelation.Create(AValue, AProperty: LccDOMString);
begin
  inherited Create;
  Value := AValue;
  Prop := AProperty
end;

{ TLccCdiMapRelationList }

procedure TLccCdiMapRelationList.Add(Relation: TLccCdiMapRelation);
begin
  List.Add(Relation);
end;

procedure TLccCdiMapRelationList.AddRelation(AValue, AProperty: LccDOMString);
begin
  List.Add( TLccCdiMapRelation.Create( AValue, AProperty));
end;

procedure TLccCdiMapRelationList.ClearList;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    List.Clear;
  end;
end;

constructor TLccCdiMapRelationList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TLccCdiMapRelationList.Destroy;
begin
  ClearList;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TLccCdiMapRelationList.GetCount: Integer;
begin
  Result := List.Count;
end;

function TLccCdiMapRelationList.GetRelation(Index: Integer): TLccCdiMapRelation;
begin
  Result := TLccCdiMapRelation( List[Index])
end;

procedure TLccCdiMapRelationList.SetRelation(Index: Integer; AValue: TLccCdiMapRelation);
begin
  List[Index] := AValue
end;

{ TLccCdiElementStr }

procedure TLccCdiElementStr.CallbackReadConfigurationMemory(ATask: TLccTaskBase);
begin
  if ATask.TaskState = lesComplete then
    ElementStr := (ATask as TLccTaskMemorySpaceAccess).StreamAsString;
end;

procedure TLccCdiElementStr.CallbackWriteConfigurationMemory(ATask: TLccTaskBase);
begin

end;

end.
