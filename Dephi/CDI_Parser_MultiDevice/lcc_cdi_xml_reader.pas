unit lcc_cdi_xml_reader;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ../lcc_compilers.inc}

 uses
  {$IFDEF DELPHI}
  System.Classes,
  System.SysUtils,
  {$ELSE}
  {$ENDIF}
  lcc_defines,
  lcc_utilities,
  lcc_xmlutilities;


type

  TLccCdiGroup = class;
  TLccCdiElementInt = class;
  TLccCdiElementStr = class;
  TLccCdiElementEventId = class;
  TLccCdiMapRelationList = class;
  TLccCdiLabelName = class;
  TLccCdiLabelDescription = class;

  //----------------------------------------------------------------------------
  // Core Objects
  //----------------------------------------------------------------------------

  {TLccCidElement}
  TLccCidCore = class
  private
    FChildElements: TList;
    FAddressSpace: Byte;
    FAddressAbsolute: Int64;
    function GetChildElement(Index: Integer): TLccCidCore;
    function GetChildElementCount: Integer;
  protected
    property AddressSpace: Byte read FAddressSpace write FAddressSpace;
    property AddressAbsolute: Int64 read FAddressAbsolute write FAddressAbsolute;
    property ChildElements: TList read FChildElements write FChildElements;
  public
    property ChildElement[Index: Integer]: TLccCidCore read GetChildElement;
    property ChildElementCount: Integer read GetChildElementCount;

    constructor Create; virtual;
    destructor Destroy; override;
    procedure ClearChildElements;
    function ValidChildObject(AChildObject: TLccCidCore): Boolean; virtual; abstract;
  end;

  {TLccCdiLabel}
  TLccCdiLabelCore = class(TLccCidCore)
  private
    FLabelStr: string;
  public
    property AddressSpace;

    property LabelStr: string read FLabelStr write FLabelStr;
    function ValidChildObject(AChildObject: TLccCidCore): Boolean; override;
  end;

  {TLccCdiElement}
  TLccCdiElementCore = class(TLccCidCore)
  private
    FAddress: Int64;
    FSize: Int64;
    FMapRelations: TLccCdiMapRelationList;
  protected
    procedure AddMapping(Node: TLccXmlNode);
  public
    property AddressAbsolute;
    property AddressSpace;
    property Address: Int64 read FAddress write FAddress;
    property MapRelations: TLccCdiMapRelationList read FMapRelations;
    property Size: Int64 read FSize write FSize;

    constructor Create; override;
    destructor Destroy; override;
    function AddName(Node: TLccXmlNode): TLccCdiLabelName;
    function AddDescription(Node: TLccXmlNode): TLccCdiLabelDescription;
    function ValidChildObject(AChildObject: TLccCidCore): Boolean; override;
  end;

  //----------------------------------------------------------------------------
  // Xml Element Objects
  //----------------------------------------------------------------------------

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


  {TLccCdiLabelName}
  TLccCdiLabelName = class(TLccCdiLabelCore)
  public
    property AddressAbsolute;
  end;

  {TLccCdiLabelDescription}
  TLccCdiLabelDescription = class(TLccCdiLabelCore)
  public
    property AddressAbsolute;
  end;

  {TLccCdiRepeatName}
  TLccCdiRepeatName = class(TLccCdiLabelCore)
  public

  end;

  {TLccCdiIdentification}
  TLccCdiIdentification = class(TLccCidCore)
  public
    function ValidChildObject(AChildObject: TLccCidCore): Boolean; override;
  end;

  {TLccCdiSegment}
  TLccCdiSegment = class(TLccCidCore)
  public
    property AddressAbsolute;
    property AddressSpace;

    function AddName(Node: TLccXmlNode): TLccCdiLabelName;
    function AddDescription(Node: TLccXmlNode): TLccCdiLabelDescription;
    function AddGroup(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64): TLccCdiGroup;
    function AddElementInt(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64): TLccCdiElementInt;
    function AddElementStr(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64): TLccCdiElementStr;
    function AddElementEventId(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64): TLccCdiElementEventId;
    function ValidChildObject(AChildObject: TLccCidCore): Boolean; override;
  end;

  {TLccCdiAcdi}
  TLccCdiAcdi = class(TLccCidCore)
  public
    function ValidChildObject(AChildObject: TLccCidCore): Boolean; override;
  end;

 {TLccCdiGroup}
  TLccCdiGroup = class(TLccCdiSegment)
  private
    FReplicationCount: Integer;    // Specialized Segment
  public
    property AddressAbsolute;
    property ReplicationCount: Integer read FReplicationCount write FReplicationCount;

    function AddRepeatName(Node: TLccXmlNode): TLccCdiRepeatName;
    function ValidChildObject(AChildObject: TLccCidCore): Boolean; override;
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
  public
    property ElementInt: Integer read FElementInt write FElementInt;
    property Min: Integer read FMin write SetMin;
    property Max: Integer read FMax write SetMax;
    property DefaultValue: Integer read FDefaultValue write SetDefaultValue;
    property MinValid: Boolean read FMinValid;
    property MaxValid: Boolean read FMaxValid;
    property DefaultValueValid: Boolean read FDefaultValueValid;
    property Value: Integer read FValue write SetValue;
  end;

  {TLccCdiElementStr}
  TLccCdiElementStr = class(TLccCdiElementCore)
  private
    FElementStr: string;
  public
    property ElementStr: string read FElementStr write FElementStr;
  end;

  {TLccCdiElementEventId}
  TLccCdiElementEventId = class(TLccCdiElementCore)
  private
    FElementEventId: TEventID;
  public
    property ElementEventId: TEventID read FElementEventId write FElementEventId;

    constructor Create; override;
  end;

  {TLccCdiRoot}
  TLccCdiRoot = class(TLccCidCore)
  private
    FXMLDocument: TLccXmlDocument;
  public
    property XMLDocument: TLccXmlDocument read FXMLDocument write FXMLDocument;

    destructor Destroy; override;
    function AddSegment(AnAddressSpace: Byte): TLccCdiSegment;
    function AddAcdi: TLccCdiAcdi;
    function AddIdentification: TLccCdiIdentification;
    function ValidChildObject(AChildObject: TLccCidCore): Boolean; override;
    function LoadFromFile(FilePath: string): Boolean;
    function LoadFromCDI(CDIFile: string): Boolean;
    function BuildTree: Boolean;
  end;

implementation

{ TLccCidElementObject }

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

function TLccCdiSegment.AddDescription(Node: TLccXmlNode): TLccCdiLabelDescription;
begin
  Result := TLccCdiLabelDescription.Create;
  Result.LabelStr := XmlNodeTextContent(Node);

  Result.AddressSpace := AddressSpace;
  ChildElements.Add(Result);
end;

function TLccCdiSegment.AddElementEventId(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64): TLccCdiElementEventId;
var
  ChildNode: TLccXmlNode;
  ChildNodeName: string;
  Name: TLccCdiLabelName;
  Description: TLccCdiLabelDescription;
begin
  Result := TLccCdiElementEventId.Create;
//  Result.ElementEventId := StrToEventID(XmlNodeName(Node));

  if XmlAttributeExists(Node, 'offset') then
    CurrentAddressAbsolute := CurrentAddressAbsolute + StrToInt( XmlAttributeRead(Node, 'offset'));
  if XmlAttributeExists(Node, 'size') then
    Result.Size := StrToInt( XmlAttributeRead(Node, 'size'));

  ChildNode := XmlFirstChild(Node);
  while Assigned(ChildNode) do
  begin
    ChildNodeName := XmlNodeName(ChildNode);
    if ChildNodeName = 'name' then
      Name := Result.AddName(ChildNode)
    else
    if ChildNodeName = 'description' then
      Description := Result.AddDescription(ChildNode)
    else
    if ChildNodeName = 'map' then
      Result.AddMapping(ChildNode);

    ChildNode := XmlNextSiblingNode(ChildNode);
  end;

  Result.AddressSpace := AddressSpace;
  Result.AddressAbsolute := CurrentAddressAbsolute;
  Inc(CurrentAddressAbsolute, SizeOf(Result.ElementEventId));
  ChildElements.Add(Result);
end;

function TLccCdiSegment.AddElementInt(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64): TLccCdiElementInt;
var
  ChildNode: TLccXmlNode;
  ChildNodeName: string;
  Name: TLccCdiLabelName;
  Description: TLccCdiLabelDescription;
begin
  Result := TLccCdiElementInt.Create;
//  Result.ElementInt := StrToInt64(XmlNodeTextContent(Node));

  if XmlAttributeExists(Node, 'offset') then
    CurrentAddressAbsolute := CurrentAddressAbsolute + StrToInt( XmlAttributeRead(Node, 'offset'));
  if XmlAttributeExists(Node, 'size') then
    Result.Size := StrToInt( XmlAttributeRead(Node, 'size'));
  if XmlAttributeExists(Node, 'min') then
    Result.Min := StrToInt( XmlAttributeRead(Node, 'min'));
  if XmlAttributeExists(Node, 'max') then
    Result.Max := StrToInt( XmlAttributeRead(Node, 'max'));
  if XmlAttributeExists(Node, 'default') then
    Result.DefaultValue := StrToInt( XmlAttributeRead(Node, 'default'));


  ChildNode := XmlFirstChild(Node);
  while Assigned(ChildNode) do
  begin
    ChildNodeName := XmlNodeName(ChildNode);
    if ChildNodeName = 'name' then
      Name := Result.AddName(ChildNode)
    else
    if ChildNodeName = 'description' then
      Description := Result.AddDescription(ChildNode)
    else
    if ChildNodeName = 'map' then
      Result.AddMapping(ChildNode);

    ChildNode := XmlNextSiblingNode(ChildNode);
  end;

  Result.AddressAbsolute := CurrentAddressAbsolute;
  Result.AddressSpace := AddressSpace;
  Inc(CurrentAddressAbsolute, Result.Size);
  ChildElements.Add(Result);
end;

function TLccCdiSegment.AddElementStr(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64): TLccCdiElementStr;
var
  ChildNode: TLccXmlNode;
  ChildNodeName: string;
  Name: TLccCdiLabelName;
  Description: TLccCdiLabelDescription;
begin
  Result := TLccCdiElementStr.Create;

  if XmlAttributeExists(Node, 'offset') then
    CurrentAddressAbsolute := CurrentAddressAbsolute + StrToInt( XmlAttributeRead(Node, 'offset'));
  if XmlAttributeExists(Node, 'size') then
    Result.Size := StrToInt( XmlAttributeRead(Node, 'size'));

  ChildNode := XmlFirstChild(Node);
  while Assigned(ChildNode) do
  begin
    ChildNodeName := XmlNodeName(ChildNode);
    if ChildNodeName = 'name' then
      Name := Result.AddName(ChildNode)
    else
    if ChildNodeName = 'description' then
      Description := Result.AddDescription(ChildNode)
     else
    if ChildNodeName = 'map' then
      Result.AddMapping(ChildNode);

    ChildNode := XmlNextSiblingNode(ChildNode);
  end;

  Result.AddressAbsolute := CurrentAddressAbsolute;
  Result.AddressSpace := AddressSpace;
  Inc(CurrentAddressAbsolute, Result.Size);
  ChildElements.Add(Result);
end;

function TLccCdiSegment.AddGroup(Node: TLccXmlNode; var CurrentAddressAbsolute: Int64): TLccCdiGroup;
var
  GroupChild: TLccXmlNode;
  GroupChildName: string;
  Identification: TLccCdiIdentification;
  Name: TLccCdiLabelName;
  Group: TLccCdiGroup;
  Description: TLccCdiLabelDescription;
  RepName: TLccCdiRepeatName;
  Int: TLccCdiElementInt;
  Str: TLccCdiElementStr;
  EventId: TLccCdiElementEventId;
begin
  Result := TLccCdiGroup.Create;
  Result.AddressSpace := AddressSpace;
  if XmlAttributeExists(Node, 'offset') then
    CurrentAddressAbsolute := CurrentAddressAbsolute + StrToInt( XmlAttributeRead(Node, 'offset'));
  if XmlAttributeExists(Node, 'replication') then
    Result.ReplicationCount := StrToInt( XmlAttributeRead(Node, 'replication'));

  // Handle the Group
  GroupChild := XmlFirstChild(Node);
  while Assigned(GroupChild) do
  begin
    GroupChildName := XmlNodeName(GroupChild);
    if GroupChildName = 'name' then
      Name := Result.AddName(GroupChild)
    else
    if GroupChildName = 'description' then
      Description := Result.AddDescription(GroupChild)
    else
    if GroupChildName = 'repname' then
      RepName := Result.AddRepeatName(GroupChild)
    else
    if GroupChildName = 'group' then
      Group := Result.AddGroup(GroupChild, FAddressAbsolute)
    else
    if GroupChildName = 'int' then
      Int := Result.AddElementInt(GroupChild, FAddressAbsolute)
    else
    if GroupChildName = 'string' then
      Str := Result.AddElementStr(GroupChild, FAddressAbsolute)
    else
    if GroupChildName = 'eventid' then
      EventId := Result.AddElementEventId(GroupChild, FAddressAbsolute);

    GroupChild := XmlNextSiblingNode(GroupChild);
  end;

  ChildElements.Add(Result);
end;

function TLccCdiSegment.AddName(Node: TLccXmlNode): TLccCdiLabelName;
begin
  Result := TLccCdiLabelName.Create;
  Result.LabelStr := XmlNodeTextContent(Node);
  Result.AddressSpace := AddressSpace;
  ChildElements.Add(Result);
end;

function TLccCdiSegment.ValidChildObject(AChildObject: TLccCidCore): Boolean;
begin
  Result :=  (AChildObject is TLccCdiLabelName) or
             (AChildObject is TLccCdiLabelDescription) or
             (AChildObject is TLccCdiGroup) or
             (AChildObject is TLccCdiElementCore)
end;

{ TLccCdiElement }

function TLccCdiElementCore.AddDescription(Node: TLccXmlNode): TLccCdiLabelDescription;
begin
  Result := TLccCdiLabelDescription.Create;
  Result.LabelStr := XmlNodeTextContent(Node);
  Result.AddressSpace := AddressSpace;
  ChildElements.Add(Result);
end;

procedure TLccCdiElementCore.AddMapping(Node: TLccXmlNode);
var
  MapChild: TLccXmlNode;
  PropertyStr, ValueStr, ElementName: LccDOMString;
begin
   if Assigned(Node) then
   begin
     MapChild := XmlFirstChild(Node);
     while Assigned(MapChild) do
     begin
       PropertyStr := XmlNodeFindChildNodeTextContent(MapChild, 'property');
       ValueStr := XmlNodeFindChildNodeTextContent(MapChild, 'value');
       if( PropertyStr <> '') and (ValueStr <> '') then
         MapRelations.AddRelation(ValueStr, PropertyStr);
       MapChild := XmlNextSiblingNode(MapChild)
     end;
   end;
end;

function TLccCdiElementCore.AddName(Node: TLccXmlNode): TLccCdiLabelName;
begin
  Result := TLccCdiLabelName.Create;
  Result.LabelStr := XmlNodeTextContent(Node);
  Result.AddressSpace := AddressSpace;
  ChildElements.Add(Result);
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

function TLccCdiElementCore.ValidChildObject(AChildObject: TLccCidCore): Boolean;
begin
 Result := (AChildObject is TLccCdiLabelName) or
           (AChildObject is TLccCdiLabelDescription)
end;

{ TLccCdiAcdi }

function TLccCdiAcdi.ValidChildObject(AChildObject: TLccCidCore): Boolean;
begin
  // Nothing can be a child
  Result := False;
end;

{ TLccCdiGroup }

function TLccCdiGroup.AddRepeatName(Node: TLccXmlNode): TLccCdiRepeatName;
begin
  Result := TLccCdiRepeatName.Create;
  Result.LabelStr := XmlNodeTextContent(Node);
  Result.AddressSpace := AddressSpace;
  ChildElements.Add(Result);
end;

function TLccCdiGroup.ValidChildObject(AChildObject: TLccCidCore): Boolean;
begin
   Result :=  (AChildObject is TLccCdiLabelName) or
              (AChildObject is TLccCdiLabelDescription) or
              (AChildObject is TLccCdiGroup) or
              (AChildObject is TLccCdiElementCore) or
              (AChildObject is TLccCdiRepeatName)
end;

{ TLccCdiRoot }

function TLccCdiRoot.AddAcdi: TLccCdiAcdi;
begin
  Result := TLccCdiAcdi.Create;
  ChildElements.Add(Result);
end;

function TLccCdiRoot.AddIdentification: TLccCdiIdentification;
begin
  Result := TLccCdiIdentification.Create;
  ChildElements.Add(Result);
end;

function TLccCdiRoot.AddSegment(AnAddressSpace: Byte): TLccCdiSegment;
begin
  Result := TLccCdiSegment.Create;
  Result.AddressSpace := AnAddressSpace;
  ChildElements.Add(Result);
end;

function TLccCdiRoot.BuildTree: Boolean;
var
  Root, RootChildNode, SegmentChildNode: TLccXmlNode;
  RootChildName, SegmentChildName : string;
  Identification: TLccCdiIdentification;
  Segment: TLccCdiSegment;
  Name: TLccCdiLabelName;
  Group: TLccCdiGroup;
  Description: TLccCdiLabelDescription;
  Int: TLccCdiElementInt;
  Str: TLccCdiElementStr;
  EventId: TLccCdiElementEventId;
begin
  Result := False;
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
        begin
          Identification := AddIdentification;
        end else
        if RootChildName = 'segment' then
        begin
          if XmlAttributeExists(RootChildNode, 'space') then
          begin
            // Required Attribute
            Segment := AddSegment( StrToInt( XmlAttributeRead(RootChildNode, 'space')));
            if XmlAttributeExists(RootChildNode, 'origin') then
              Segment.AddressAbsolute := StrToInt( XmlAttributeRead(RootChildNode, 'origin'))
            else
              Segment.AddressAbsolute := 0;

            // Handle the Segment
            SegmentChildNode := XmlFirstChild(RootChildNode);
            while Assigned(SegmentChildNode) do
            begin
              SegmentChildName := XmlNodeName(SegmentChildNode);
              if SegmentChildName = 'name' then
                Name := Segment.AddName(SegmentChildNode)
              else
              if SegmentChildName = 'description' then
                Description := Segment.AddDescription(SegmentChildNode)
              else
              if SegmentChildName = 'group' then
                Group := Segment.AddGroup(SegmentChildNode, FAddressAbsolute)
              else
              if SegmentChildName = 'int' then
                Int := Segment.AddElementInt(SegmentChildNode, FAddressAbsolute)
              else
              if SegmentChildName = 'string' then
                Str := Segment.AddElementStr(SegmentChildNode, FAddressAbsolute)
              else
              if SegmentChildName = 'eventid' then
                EventId := Segment.AddElementEventId(SegmentChildNode, FAddressAbsolute);

              SegmentChildNode := XmlNextSiblingNode(SegmentChildNode);
            end;
          end;
        end;
        RootChildNode := XmlNextSiblingNode(RootChildNode);
      end;
    end;
  end;
end;

destructor TLccCdiRoot.Destroy;
begin
  ClearChildElements;
  XmlFreeDocument(FXMLDocument);
  inherited;
end;

function TLccCdiRoot.LoadFromCDI(CDIFile: string): Boolean;
begin
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

function TLccCdiRoot.ValidChildObject(AChildObject: TLccCidCore): Boolean;
begin
  Result := (AChildObject is TLccCdiSegment) or
            (AChildObject is TLccCdiAcdi) or
            (AChildObject is TLccCdiIdentification)
end;

{ TLccCdiIdentification }

function TLccCdiIdentification.ValidChildObject(AChildObject: TLccCidCore): Boolean;
begin
  Result :=  (AChildObject is TLccCdiLabelName) or
             (AChildObject is TLccCdiLabelDescription);  // TODO
end;

{ TLccCdiLabelCore }

function TLccCdiLabelCore.ValidChildObject(AChildObject: TLccCidCore): Boolean;
begin
  Result := False; // No one can be a child
end;

{ TLccCdiElementInt }

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

{ TLccCdiElementEventId }

constructor TLccCdiElementEventId.Create;
begin
  inherited Create;
  FSize := 8;
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

end.
