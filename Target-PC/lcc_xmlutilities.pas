unit lcc_xmlutilities;

{$I ..\lcc_compilers.inc}

{$IFDEF LCC_FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface


uses
  Classes,
  {$IFDEF LCC_FPC}
    DOM,
    XMLRead,
    XMLWrite,
  {$ELSE}
    Xml.XMLDoc,
    Xml.xmldom,
    Xml.XMLIntf,
  {$ENDIF}
  SysUtils;


type
  TLccXmlNode ={$IFDEF LCC_FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
  TLccXmlDocument = {$IFDEF LCC_FPC}TXMLDocument{$ELSE}IXMLDocument{$ENDIF};
  TLccXmlAttribute = {$IFDEF LCC_FPC}TDOMAttr{$ELSE}IXMLNode{$ENDIF};
  LccDOMString = {$IFDEF LCC_FPC}DOMString{$ELSE}DOMString{$ENDIF};

// Document functions
function XmlLoadFromFile(FilePath: string): TLccXmlDocument;
function XmlLoadFromStream(Stream: TStream): TLccXmlDocument;
function XmlLoadFromText(XMLText: string): TLccXmlDocument;
function BuildConfigurationDocument(CdiXMLFilePath: string): TLccXmlDocument;
procedure XmlFreeDocument(var XmlDoc: TLccXmlDocument);
function XmlCreateEmptyDocument: TLccXmlDocument;
procedure XmlWriteToFile(FilePath: string; XmlDoc: TLccXmlDocument);
function XmlCreateChildNode(XmlDoc: TLccXmlDocument; ParentNode: TLccXmlNode; Element, Content: domString): TLccXmlNode;
function XmlCreateRootNode(XmlDoc: TLccXmlDocument; Element, Content: domString): TLccXmlNode;
// Find functions
function XmlFindChildNode(XmlNode: TLccXmlNode; Name: domString): TLccXmlNode;
function XmlFirstChild(XmlNode: TLccXmlNode): TLccXmlNode;
function XmlFindRootNode(XmlDoc: TLccXmlDocument; RootName: domString): TLccXmlNode;
function XmlNextSiblingNode(XmlNode: TLccXmlNode): TLccXmlNode;
// Element value (name) functions
function XmlFirstChildValue(XmlNode: TLccXmlNode): domString;
function XmlNextSiblingValue(XmlNode: TLccXmlNode): domString;
function XmlNodeName(XmlNode: TLccXmlNode): domString;
// Element content (text) functions
function XmlNodeTextContent(XmlNode: TLccXmlNode): domString;
procedure XmlNodeSetTextContent(XmlNode: TLccXmlNode; Text: domString);
procedure XmlNodeSetFirstLevelTextContent(XMLDoc: TLccXmlDocument; RootElement, ChildElement, Content: domString; Force: Boolean); overload;
procedure XmlNodeSetFirstLevelTextContent(FilePath, RootElement, ChildElement, Content: domString; Force: Boolean); overload;
function XmlNodeFindChildNodeTextContent(TopLevelElement: TLccXmlNode; ChildNodeName: LccDOMString): LccDOMString;
// Attribute functions
procedure XmlAttributeCreateAndSet(XmlDoc: TLccXmlDocument; TargetNode: TLccXmlNode; Attribute, Content: domString);
procedure XmlAttributeForce(XmlDoc: TLccXmlDocument; TargetNode: TLccXmlNode; Attribute, Content: domString);
function XmlAttributeRead(TargetNode: TLccXmlNode; Attribute: domString): domString;
function XmlAttributeExists(TargetNode: TLccXmlNode; Attribute: domString): Boolean;
procedure XmlAttributeRemove(TargetNode: TLccXmlNode; Attribute: domString);

implementation

procedure XmlAttributeForce(XmlDoc: TLccXmlDocument; TargetNode: TLccXmlNode; Attribute, Content: domString);
{$IFDEF LCC_FPC}
var
  AttributeNode: TLccXmlNode;
{$ENDIF}
begin
  {$IFDEF LCC_FPC}
  if Assigned( TargetNode.Attributes) then
  begin
    AttributeNode := TargetNode.Attributes.GetNamedItem(Attribute);
    if Assigned(AttributeNode) then
      XmlNodeSetTextContent(AttributeNode, Content)
    else
      XmlAttributeCreateAndSet(XmlDoc, TargetNode, Attribute, Content);
  end;
  {$ELSE}
    TargetNode.SetAttributeNS(Attribute, '', Content)
  {$ENDIF}
end;

function XmlAttributeRead(TargetNode: TLccXmlNode; Attribute: domString): domString;
var
  Node: TLccXmlNode;
begin
  Result := '';
  Node := nil;
  {$IFDEF LCC_FPC}
  if Assigned( TargetNode.Attributes) then
    Node := TargetNode.Attributes.GetNamedItem(Attribute);
  if Assigned(Node) then
    Result := Node.NodeValue;
  {$ELSE}
  if TargetNode.HasAttribute(Attribute) then
    Result := TargetNode.Attributes[Attribute];
  {$ENDIF}
end;

function XmlAttributeExists(TargetNode: TLccXmlNode; Attribute: domString): Boolean;
begin
  {$IFDEF LCC_FPC}
  if Assigned( TargetNode.Attributes) then
    Result := Assigned(TargetNode.Attributes.GetNamedItem(Attribute));
  {$ELSE}
  Result := TargetNode.HasAttribute(Attribute)
  {$ENDIF}
end;

procedure XmlAttributeRemove(TargetNode: TLccXmlNode; Attribute: domString);
{$IFNDEF LCC_FPC}
var
  Node: IXMLNode;
{$ENDIF}
begin
  {$IFDEF LCC_FPC}
  if Assigned( TargetNode.Attributes) then
    if Assigned( TargetNode.Attributes.GetNamedItem(Attribute)) then
      TargetNode.Attributes.RemoveNamedItem(Attribute);
  {$ELSE}
  Node := TargetNode.AttributeNodes.FindNode(Attribute);
  if Assigned(Node) then
    TargetNode.AttributeNodes.Remove(Node)
  {$ENDIF}
end;

function XmlLoadFromStream(Stream: TStream): TLccXmlDocument;
begin
  Stream.Position := 0;
  {$IFDEF LCC_FPC}
  Result := nil;
  ReadXMLFile(Result, Stream);
  {$ELSE}
  Result := XmlCreateEmptyDocument;
  Result.LoadFromStream(Stream);
  {$ENDIF}
end;

function XmlLoadFromText(XMLText: string): TLccXmlDocument;
{$IFDEF LCC_FPC}
var
  Stream: TMemoryStream;
  i: Integer;
{$ENDIF}
begin
  {$IFDEF LCC_FPC}
  Result := nil;
  Stream := TMemoryStream.Create;
  try
    for i := 1 to Length(XMLText) do
      Stream.Write(AnsiChar(XMLText[i]), 1);
    Stream.Position := 0;
    ReadXMLFile(Result, Stream);
  finally
    Stream.Free
  end;
  {$ELSE}
  Result := XmlCreateEmptyDocument;
  Result.LoadFromXML(XMLText);

//  Result := Xml.XMLDoc.LoadXMLData(XMLText);
  {$ENDIF}
end;

function BuildConfigurationDocument(CdiXMLFilePath: string): TLccXmlDocument;
  procedure RunCdi(ChildNode: TLccXmlNode; var CurrentAddress: Integer);
  var
    Attrib: domString;
    ReplicationCount, i: Integer;
  begin
     while Assigned(ChildNode) do
     begin
       XmlAttributeRemove(ChildNode, 'offset');   // Remove any "offset" attribute not used
   //    XmlAttributeForce(Result, ChildNode, 'testing', 'wow');
       if ChildNode.NodeName = 'group' then    // If it is a group then recurse into it.
       begin
         XmlAttributeForce(Result, ChildNode, 'origin', domString( IntToStr(CurrentAddress)));
         ReplicationCount := 1;
         Attrib := XmlAttributeRead(ChildNode, 'replication');
         if Attrib <> '' then
           ReplicationCount := StrToInt( string( Attrib));
         for i := 0 to ReplicationCount-1 do
           RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'int' then
       begin
         Attrib := XmlAttributeRead(ChildNode, 'size');
         if Attrib <> '' then
           CurrentAddress := CurrentAddress + StrToInt( string( Attrib))
         else
           Inc(CurrentAddress, 1);
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'domString' then
       begin
         Attrib := XmlAttributeRead(ChildNode, 'size');
         if Attrib <> '' then
           CurrentAddress := CurrentAddress + StrToInt( string( Attrib));  // else broken
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'eventid' then
       begin
         Inc(CurrentAddress, 8);
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'bit' then
       begin
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'map' then
       begin
         RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end else
       if ChildNode.NodeName = 'relation' then
       begin
          RunCdi( XmlFirstChild(ChildNode), CurrentAddress);
       end;
       ChildNode := XmlNextSiblingNode(ChildNode);
     end;
  end;
var
  RootNode, SegmentNode: TLccXmlNode;
  CurrentAddress: Integer;
begin
  Result := XmlLoadFromFile(CdiXmlFilePath);
  if Assigned(Result) then
  begin
    CurrentAddress := 0;
    RootNode := XmlFindRootNode(Result, 'ndi');
    if Assigned(RootNode) then
    begin
      SegmentNode := XmlFindChildNode(RootNode, 'segment');
      while Assigned(SegmentNode) do              // Run all the Segements in the file
      begin
        XmlAttributeForce(Result, SegmentNode, 'origin', domString( IntToStr(CurrentAddress)));
        XmlAttributeRemove(SegmentNode, 'offset');
        // From here on it can be recursive down into groups, etc...
        RunCdi(XmlFirstChild(SegmentNode), CurrentAddress);
        SegmentNode := XmlNextSiblingNode(SegmentNode);
      end;
    end;
  end;
end;

procedure XmlFreeDocument(var XmlDoc: TLccXmlDocument);
begin
  {$IFDEF LCC_FPC}
  FreeAndNil(XmlDoc)
  {$ELSE}
     // Is and interface and will free itself
  {$ENDIF}
end;

function XmlCreateEmptyDocument: TLccXmlDocument;
begin
  {$IFDEF LCC_FPC}
  Result := TXMLDocument.Create;
  {$ELSE}
  Result := NewXMLDocument();
  Result.Encoding := 'UTF-8';
  Result.Options := [doNodeAutoIndent];
  {$ENDIF}
end;

procedure XmlWriteToFile(FilePath: string; XmlDoc: TLccXmlDocument);
begin
  {$IFDEF LCC_FPC}
  WriteXMLFile(XmlDoc, FilePath);
  {$ELSE}
  XmlDoc.SaveToFile(FilePath)
  {$ENDIF}
end;

function XmlCreateChildNode(XmlDoc: TLccXmlDocument; ParentNode: TLccXmlNode; Element, Content: domString): TLccXmlNode;
begin
  {$IFDEF LCC_FPC}
  Result := XmlDoc.CreateElement(Element);
  ParentNode.AppendChild(Result);
  if Content <> '' then
    Result.TextContent := Content;
  {$ELSE}
  Result := ParentNode.AddChild(Element);
  if Content <> '' then
    Result.Text := Content
  {$ENDIF}
end;

function XmlCreateRootNode(XmlDoc: TLccXmlDocument; Element, Content: domString): TLccXmlNode;
begin
  {$IFDEF LCC_FPC}
  Result := XmlDoc.CreateElement(Element);
  XmlDoc.AppendChild(Result);
  Result.TextContent := Content;
  {$ELSE}
  Result := XmlDoc.AddChild(Element);
  Result.Text := Content;
  {$ENDIF}
end;

function XmlLoadFromFile(FilePath: string): TLccXmlDocument;
begin
  {$IFDEF LCC_FPC}
  Result := nil;
  ReadXMLFile(Result, FilePath);
  {$ELSE}
  Result := TXMLDocument.Create(nil) as IXMLDocument;
  Result.LoadFromFile(FilePath);
  {$ENDIF}
end;

function XmlFindChildNode(XmlNode: TLccXmlNode; Name: domString): TLccXmlNode;
begin
  Result := nil;
  if Assigned(XmlNode) then
    Result := XmlNode.{$IFNDEF LCC_FPC}ChildNodes.{$ENDIF}FindNode(Name)
end;

function XmlFirstChild(XmlNode: TLccXmlNode): TLccXmlNode;
begin
  Result := XmlNode.{$IFDEF LCC_FPC}FirstChild{$ELSE}ChildNodes.First{$ENDIF}
end;

function XmlFirstChildValue(XmlNode: TLccXmlNode): domString;
var
  Child: {$IFDEF LCC_FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
begin
  Result := '';
  Child := XmlFirstChild(XmlNode);
  if Assigned(Child) then
    Result := Child.NodeValue;
end;

function XmlNextSiblingValue(XmlNode: TLccXmlNode): domString;
var
  Sibling: {$IFDEF LCC_FPC}TDOMNode{$ELSE}IXMLNode{$ENDIF};
begin
  Result := '';
  Sibling := XmlNextSiblingNode(XmlNode);
  if Assigned(Sibling) then
    Result := Sibling.NodeValue;
end;

function XmlNodeName(XmlNode: TLccXmlNode): domString;
begin
  Result := XmlNode.NodeName;
end;

function XmlNodeTextContent(XmlNode: TLccXmlNode): domString;
begin
  Result := '';
  if Assigned(XmlNode) then
    Result := XmlNode.{$IFDEF LCC_FPC}TextContent{$ELSE}Text{$ENDIF}
end;

procedure XmlNodeSetTextContent(XmlNode: TLccXmlNode; Text: domString);
begin
  XmlNode.{$IFDEF LCC_FPC}TextContent{$ELSE}Text{$ENDIF} := Text;
end;

procedure XmlNodeSetFirstLevelTextContent(FilePath, RootElement, ChildElement, Content: domString; Force: Boolean);
var
  XMLDoc: TLccXmlDocument;
begin
  // Does not Force the FilePath and and a new XML file
  XMLDoc := XmlLoadFromFile(string( FilePath));
  if Assigned(XMLDoc) then
  begin
    XmlNodeSetFirstLevelTextContent(XMLDoc, RootElement, ChildElement, Content, Force);
    XmlWriteToFile(string(FilePath), XMLDoc)
  end;
end;

procedure XmlNodeSetFirstLevelTextContent(XMLDoc: TLccXmlDocument; RootElement, ChildElement, Content: domString; Force: Boolean);
var
  RootNode, ChildNode: TLccXmlNode;
begin
  if Assigned(XMLDoc) then
  begin
    RootNode := XmlFindRootNode(XMLDoc, RootElement);
    if not Assigned(RootNode) and Force then
      RootNode := XmlCreateRootNode(XMLDoc, RootElement, '');
    if Assigned(RootNode) then
    begin
      ChildNode := XmlFindChildNode(RootNode, ChildElement);
      if not Assigned(ChildNode) and Force then
        XmlCreateChildNode(XMLDoc, RootNode, ChildElement, Content)
      else
        XmlNodeSetTextContent(ChildNode, Content)
    end;
  end;
end;

function XmlNodeFindChildNodeTextContent(TopLevelElement: TLccXmlNode;
  ChildNodeName: LccDOMString): LccDOMString;
begin
  Result := XmlNodeTextContent(XmlFindChildNode(TopLevelElement, ChildNodeName));
end;

function XmlNextSiblingNode(XmlNode: TLccXmlNode): TLccXmlNode;
begin
  Result := XmlNode.NextSibling;
end;

procedure XmlAttributeCreateAndSet(XmlDoc: TLccXmlDocument; TargetNode: TLccXmlNode; Attribute, Content: domString);
{$IFDEF LCC_FPC}
var
  AttributeNode: TLccXmlAttribute;
{$ENDIF}
begin
  {$IFDEF LCC_FPC}
  AttributeNode := XmlDoc.CreateAttribute(Attribute);
  XmlNodeSetTextContent(AttributeNode, Content);
  TargetNode.Attributes.SetNamedItem(AttributeNode);
  {$ELSE}
  TargetNode.SetAttributeNS(Attribute, '', Content);
  {$ENDIF}
end;

function XmlFindRootNode(XmlDoc: TLccXmlDocument; RootName: domString): TLccXmlNode;
begin
  {$IFDEF LCC_FPC}
  Result := XmlFindChildNode(XmlDoc, RootName);
  {$ELSE}
  Result := XmlDoc.ChildNodes.FindNode(RootName);
  {$ENDIF}
end;

end.
