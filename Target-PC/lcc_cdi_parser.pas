 unit lcc_cdi_parser;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ../lcc_compilers.inc}

{.$DEFINE PRINT_MEM_LOCATIONS}

uses
  Classes,
  SysUtils,
  {$IFDEF FPC}
    {$IFNDEF FPC_CONSOLE_APP}
      ExtCtrls,
      ComCtrls,
      StdCtrls,
      Forms,
      Buttons,
      Controls,
      Spin,
      Graphics,
      Types,
    {$ENDIF}
    contnrs,
  {$ELSE}
    System.Types,
    FMX.Types,
    FMX.Layouts,
    FMX.TabControl,
    FMX.StdCtrls,
    FMX.SpinBox,
    FMX.Edit,
    FMX.Listbox,
    FMX.controls,
    FMX.ImgList,
    FMX.Objects,
    FMX.Graphics,
    System.UITypes,
    FMX.Forms,
    System.Generics.Collections,
  {$ENDIF}
  lcc_defines,
  lcc_node_messages,
  lcc_node_manager,
  lcc_utilities,
  lcc_node,
  lcc_xmlutilities;

const
  BASE_EDITOR_HEIGHT = 22;
  BASE_BUTTON_HEIGHT = 22;
  BASE_BUTTON_WIDTH = 80;
  TAB_RIGHT_MARGIN = 28;


  // Default Label Left side Margins
  LABEL_MARGIN_INDENT = 4;
  LABEL_DELTA_INDENT = 8;
  LABEL_DELTA_INDENT_DOUBLE = LABEL_DELTA_INDENT + LABEL_DELTA_INDENT;



type
  {$IFDEF FPC}
    TLccPanel = TPanel;
    TLccTabControl = TPageControl;
    TLccTabSheet = TTabSheet;
    TLccControl = TControl;
    TLccSpeedButton = TButton;
    TLccLabel = TLabel;
    TLccSpinEdit = TSpinEdit;
    TLccEdit = TEdit;
    TLccComboBox = TComboBox;
    TLccImageList = TImageList;
    TLccImage = TImage;
    TLccBitmap = TBitmap;
  {$ELSE}
    TLccPanel = TLayout;
    TLccTabControl = TTabControl;
    TLccTabSheet = TTabItem;
    TLccControl = TControl;
    TLccSpeedButton = TButton;
    TLccLabel = TLabel;
    TLccSpinEdit = TSpinBox;
    TLccEdit = TEdit;
    TLccComboBox = TComboBox;
    TLccImageList = TImageList;
    TLccImage = TImage;
    TLccBitmap = TBitmap;
  {$ENDIF}



  TConfigMemState = (ocs_Current, ocs_Unknown);
  TConfigDataDirection = (cdd_Read, cdd_Write);
  TParserSerializer = (ps_InsertRead, ps_InsertWrite, ps_RemoveRead, ps_RemoveWrite);

  TParserNotificationEvent = procedure(Sender: TObject; Notify: TParserSerializer) of object;

  TLccCdiParser = class;

  // Holds the Property/Value pairs for a Map type Configuration
  { TMapRelation }

  TMapRelation = class
  private
    FProp: LccDOMString;     // Think of this as the "Key" for the relation, often an integer but does not need to be could be any datatype
    FValue: LccDOMString;    // Think of this as the "human readable" part of the relation that maps to a "Key"
  public
    constructor Create(AValue, AProperty: LccDOMString);
    property Value: LccDOMString read FValue write FValue;
    property Prop: LccDOMString read FProp write FProp;
  end;

  // Holds TMapRelations for a Map type Configuration
  { TMapRelationList }

  TMapRelationList = class
  private
    FList: TList;
    function GetCount: Integer;
    function GetRelation(Index: Integer): TMapRelation;
    procedure SetRelation(Index: Integer; AValue: TMapRelation);
  protected
    property List: TList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Relation: TMapRelation);
    procedure AddRelation( AValue, AProperty: LccDOMString);
    procedure ClearList;
    property Count: Integer read GetCount;
    property Relations[Index: Integer]: TMapRelation read GetRelation write SetRelation;
  end;

  { TDataElementConfig }

  TDataElementConfig = class
  private
    FDataBit: Byte;
    FDataDirection: TConfigDataDirection;  // Read or Write
    FDataEvent: TNodeID;
    FDataInteger: Integer;
    FDataString: ansistring;
  public
    property DataDirection: TConfigDataDirection read FDataDirection write FDataDirection;
    property DataString: ansistring read FDataString write FDataString;
    property DataInteger: Integer read FDataInteger write FDataInteger;
    property DataEvent: TNodeID read FDataEvent write FDataEvent;
    property DataBit: Byte read FDataBit write FDataBit;
  end;

  // Contains information that defines what the configuration info for an element
  { TDataElementInformation }

  TDataElementInformation = class
  private
    FCompareMemorySpaceButton: TLccSpeedButton;
    FConfigData: TDataElementConfig;                                // What is the Datatype associated with this Memory Configuration location
    FIntDefault: Int64;                                          // If an Integer Type the default value
    FIntMax: Int64;                                              // If an Integer Type the Max value
    FIntMin: Int64;                                              // If an Integer Type the Min value
    FMemoryAddressPointer: Int64;                                // What is memory address pointer we use to access this memory location within the MemorySpace
    FMemoryAllocation: DWord;                                    // How many bytes of memory does this data type allocate
    FDataType: TLccConfigDataType;                               // What datatype is this associated with (cdt_String, cdt_Int, cdt_EventID, cdt_Bit)
    FMapList: TMapRelationList;                                  // Enumeration of the defined values for this data type (may not be any) and the Key associated with each defined value
    FOnMemChangeState: TNotifyEvent;                             // Called when MemState changes in the UI from the user interacting with it
    FMemState: TConfigMemState;                                  // Tracks the state of the actual memory stored in the Node compared to the UI interface for the Element (unknown, saved, unsaved, etc)
    FReadMemorySpaceButton: TLccSpeedButton;
    FWriteMemorySpaceButton: TLccSpeedButton;
    procedure SetMemState(AValue: TConfigMemState);
  public
    constructor Create(ADataType: TLccConfigDataType);
    destructor Destroy; override;
    property ConfigData: TDataElementConfig read FConfigData write FConfigData;
    property IntMin: Int64 read FIntMin write FIntMin;
    property IntMax: Int64 read FIntMax write FIntMax;
    property IntDefault: Int64 read FIntDefault write FIntDefault;
    property MemoryAddressPointer: Int64 read FMemoryAddressPointer write FMemoryAddressPointer;
    property MemoryAllocation: DWord read FMemoryAllocation write FMemoryAllocation;
    property MemState: TConfigMemState read FMemState write SetMemState;
    property DataType: TLccConfigDataType read FDataType write FDataType;
    property MapList: TMapRelationList read FMapList write FMapList;
    property OnMemChangeState: TNotifyEvent read FOnMemChangeState write FOnMemChangeState;
    property ReadMemorySpaceButton: TLccSpeedButton read FReadMemorySpaceButton write FReadMemorySpaceButton;
    property WriteMemorySpaceButton: TLccSpeedButton read FWriteMemorySpaceButton write FWriteMemorySpaceButton;
    property CompareMemorySpaceButton: TLccSpeedButton read FCompareMemorySpaceButton write FCompareMemorySpaceButton;
  end;

   { TLccOpenPascalSpinEdit }

  TLccOpenPascalSpinEdit = class(TLccSpinEdit)
  private
    FDataElementInformation: TDataElementInformation;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property DataElementInformation: TDataElementInformation read FDataElementInformation write FDataElementInformation;
 end;

  { TLccOpenPascalEdit }

  TLccOpenPascalEdit = class(TLccEdit)
  private
    FDataElementInformation: TDataElementInformation;
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property DataElementInformation: TDataElementInformation read FDataElementInformation write FDataElementInformation;
  end;

  { TLccOpenPascalComboBox }

  TLccOpenPascalComboBox = class(TLccComboBox)
  private
    FDataElementInformation: TDataElementInformation;
  protected

  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property DataElementInformation: TDataElementInformation read FDataElementInformation write FDataElementInformation;
  end;


  { TLccOpenPascalTabSheet }

  TLccOpenPascalTabSheet = class(TLccTabSheet)
  private
    FScrollingWindowContentsPanel: TLccPanel;
  public
    property ScrollingWindowContentsPanel: TLccPanel read FScrollingWindowContentsPanel write FScrollingWindowContentsPanel;
  end;


  { TLccCdiParserSerializer }

  TLccCdiParserSerializer = class
  private
    FDataElementInformation: TDataElementInformation;
    FDataElementInformationList: TList;
    FOnNotification: TParserNotificationEvent;
    FOwnerParser: TLccCdiParser;
    FWorkerMessage: TLccMessage;
  protected
    procedure DoNotification(Notify: TParserSerializer); virtual;
    function PackCVReads(CV_Start: DWord): Byte;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRead(AConfigInfo: TDataElementInformation);    // THIS SHOULD BE ONLY ONE ADD SO THIS STAYS SERIALIZED
    procedure AddWrite(AConfigInfo: TDataElementInformation);
    procedure Clear;
    procedure SendNext;
    procedure Remove(AConfigInfo: TDataElementInformation);

    property DataElementInformation: TDataElementInformation read FDataElementInformation write FDataElementInformation;
    property DataElementInformationList: TList read FDataElementInformationList write FDataElementInformationList;
    property OwnerParser: TLccCdiParser read FOwnerParser write FOwnerParser;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
    property OnNotification: TParserNotificationEvent read FOnNotification write FOnNotification;
  end;


  // - Application Form
  //   - ..... Controls
  //     - CDI Background Control (Passed to CDI Parser)
  //       - TPanel (background CDI Parser builds the UI on
  //         - TScrollBox - [alClient]
  //           - TPanel [akLeft, akRight, akTop] Bottom is dynamically set to include all child controls
  //             - TLabel (....) [alTop]
  //             - TPanel [alTop] Height set based on the initial heights of the below before setting Alignments
  //               - TButton (Read) [alRight]
  //               - TButton (Write) [alRight]
  //               - TEdit [alClient]
  //         - TPanel (Footer with Global Read/Write buttons) [alBottom]
  //           - TButton (Read All)
  //           - TButton ([Write All)
  //           - TButton (Cancel)
  { TLccCdiParser }

  TLccCdiParser = class(TLccCdiParserBase)
  private
    FAutoReadOnTabChange: Boolean;
    FFooterBkGnd: TLccPanel;
    FGlobalButtonReadPage: TButton;
    FGlobalButtonStop: TButton;
    FGlobalButtonWritePage: TButton;
    FCVBlockRead: Word;
    FMarkedToStop: Boolean;
    FMarkedToStopIsStopping: Boolean;
    FNodeManager: TLccNodeManager;
    FLccNode: TLccNode;
    FOnAfterReadPage: TNotifyEvent;
    FOnAfterWritePage: TNotifyEvent;
    FOnBuildInterfaceComplete: TNotifyEvent;
    FOnClearInterface: TNotifyEvent;
    FApplicationPanel: TLccPanel;
    FGlobalButtonBkGnd: TLccPanel;
    FParserFrame: TLccPanel;
    FPrintMemOffset: Boolean;
    FSerializer: TLccCdiParserSerializer;
    FShowCompareBtn: Boolean;
    FShowReadBtn: Boolean;
    FShowWriteBtn: Boolean;
    FSuppressNameAndDescription: Boolean;
    FTabControl: TLccTabControl;
    FWorkerMessage: TLccMessage;
    function GetCVBlockRead: Word;
    procedure SetCVBlockRead(AValue: Word);
  protected
    // Top level Tab
    function CreateTab(ATabControl: TLccTabControl; ACaption: LccDOMString): TLccPanel;

    // These are the base elements that need to be alTop aligned in the tabs
    function CreateBaseEditorLayout(ParentControl: TLccPanel; DataElementInformation: TDataElementInformation): TLccPanel;
    function CreateLabel(ParentControl: TLccPanel; ACaption: LccDOMString; Indent: Single; Bold: Boolean): TLccLabel;
    function CreateSpacer(ParentControl: TLccPanel): TLccLabel;

    // These all live within the base elements that need to be alTop aligned
    function CreateButton(AContainerParent: TLccPanel; ACaption: LccDOMString; OnClickFunc: TNotifyEvent; Enable: Boolean; AWidth: single; AnAlign: {$IFDEF DELPHI}TAlignLayout{$ELSE}TAlign{$ENDIF}): TLccSpeedButton;
    procedure CreateSpinEditLayout(ParentControl: TLccPanel; Indent: Integer; DataElementInformation: TDataElementInformation);
    procedure CreateEditLayout(ParentControl: TLccPanel; Indent: Integer; DataElementInformation: TDataElementInformation);
    procedure CreateComboBoxListLayout(ParentControl: TLccPanel; Indent: Integer; DataElementInformation: TDataElementInformation);

    // Top level processes
    function ProcessIdentificationTab(ATabControl: TLccTabControl; IdentificationElement: TLccXmlNode; Indent: Integer): TLccPanel;
    function ProcessSegmentTab(ATabControl: TLccTabControl; SegmentElement: TLccXmlNode; Indent: Integer): TLccPanel;

    // Midtier processes that may need to create UI elements or call processes that do
    procedure ProcessElementForUISegment(ParentControl: TLccPanel; Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer; AddressSpace: Byte);
    procedure ProcessElementForUIGroup(ParentControl: TLccPanel; Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer; AddressSpace: Byte);
    procedure ProcessElementForUIString(ParentControl: TLccPanel; Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer; AddressSpace: Byte);
    procedure ProcessElementForUIInt(ParentControl: TLccPanel; Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer; AddressSpace: Byte);
    procedure ProcessElementForUIEventID(ParentControl: TLccPanel; Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer; AddressSpace: Byte);
    procedure ProcessElementForUIDataElement(ParentControl: TLccPanel; Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer; AddressSpace: Byte; DataElementInformation: TDataElementInformation);

    // Lowest level process that does not create UI elements directly
    procedure ProcessElementForUIMap(Element: TLccXmlNode; var MemoryAddressPointer: Int64; DataElementInformation: TDataElementInformation);

    procedure OnParserFrameResize(Sender: TObject);
    procedure ResizeActiveTabScrollingWindowFrame;


    // Methods to look at below...............

    procedure DoAfterReadPage(Sender: TObject); virtual;
    procedure DoAfterWritePage(Sender: TObject); virtual;
    procedure DoButtonReadPageClick(Sender: TObject); virtual;
    procedure DoButtonWritePageClick(Sender: TObject); virtual;
    procedure DoButtonStopClick(Sender: TObject); virtual;
    procedure DoSpeedButtonCompareClick(Sender: TObject); virtual;
    procedure DoSpeedButtonReadClick(Sender: TObject); virtual;
    procedure DoSpeedButtonWriteClick(Sender: TObject); virtual;
    procedure DoClearInterface; virtual;
    procedure DoBuildInterfaceComplete; virtual;
    function ExtractElementItem(ParentElement: TLccXmlNode; Item: string; var ItemStr: LccDOMString): Boolean;
    function ExtractElementAttribute(Element: TLccXmlNode; AttributeName: LccDOMString; var AttributeStr: LccDOMString): Boolean;
    function FindControlByAddress(Address: DWord): TLccControl;
    function FindActivePage: TLccTabSheet;
    function IsMemorySpace(Segment: TLccXmlNode; MemorySpace: Byte): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnSpinEditChange(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
    procedure OnComboBoxChange(Sender: TObject);
    procedure OnPageControlChange(Sender: TObject);
    procedure OnSerializerNotification(Sender: TObject; Notify: TParserSerializer);
    function SpeedButtonToConfigInfo(Sender: TObject): TDataElementInformation;
    // ...............



    property ApplicationPanel: TLccPanel read FApplicationPanel;     // Application Passed Control to build in
    property ParserFrame: TLccPanel read FParserFrame write FParserFrame;  // alClient aligned to the Application Panel used as our drawing canvas
    property GlobalButtonBkGnd: TLccPanel read FGlobalButtonBkGnd write FGlobalButtonBkGnd; // alBottom aligned to the Parser Frame
    property TabControl: TLccTabControl read FTabControl write FTabControl; // alClient aligned in the Parser Frame, sibling to the GlobalButtonBkgnd

    // In the GlobalButtonBkGnd
    property GlobalButtonReadPage: TButton read FGlobalButtonReadPage write FGlobalButtonReadPage;
    property GlobalButtonWritePage: TButton read FGlobalButtonWritePage write FGlobalButtonWritePage;
    property GlobalButtonStop: TButton read FGlobalButtonStop write FGlobalButtonStop;


    // Methods to look at below...............

    property MarkedToStop: Boolean read FMarkedToStop write FMarkedToStop;
    property MarkedToStopIsStopping: Boolean read FMarkedToStopIsStopping write FMarkedToStopIsStopping;
    property Serializer: TLccCdiParserSerializer read FSerializer write FSerializer;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Build_CDI_Interface(AnLccNode: TLccNode; ParentControl: TLccPanel; CDI: TLccXmlDocument): TLccPanel;
    procedure Clear_CDI_Interface(ClearLccNode: Boolean);
    procedure DoConfigMemReadReply(ANode: TObject); override;
    procedure DoConfigMemWriteReply(ANode: TObject); override;
    procedure NotifyLccNodeDestroy(LccNode: TObject); override;
    procedure SetNodeManager(ANodeManager: TObject); override;

    property LccNode: TLccNode read FLccNode;
    property AutoReadOnTabChange: Boolean read FAutoReadOnTabChange write FAutoReadOnTabChange;
    property CVBlockRead: Word read GetCVBlockRead write SetCVBlockRead;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property PrintMemOffset: Boolean read FPrintMemOffset write FPrintMemOffset;
    property ShowReadBtn: Boolean read FShowReadBtn write FShowReadBtn;
    property ShowWriteBtn: Boolean read FShowWriteBtn write FShowWriteBtn;
    property ShowCompareBtn: Boolean read FShowCompareBtn write FShowCompareBtn;
    property SuppressNameAndDescription: Boolean read FSuppressNameAndDescription write FSuppressNameAndDescription;
    property OnAfterWritePage: TNotifyEvent read FOnAfterWritePage write FOnAfterWritePage;
    property OnAfterReadPage: TNotifyEvent read FOnAfterReadPage write FOnAfterReadPage;
    property OnClearInterface: TNotifyEvent read FOnClearInterface write FOnClearInterface;
    property OnBuildInterfaceComplete: TNotifyEvent read FOnBuildInterfaceComplete write FOnBuildInterfaceComplete;
  end;


implementation

type
  TLccNodeManagerHack = class(TLccNodeManager)
  end;


{ TLccCdiParserSerializer }

procedure TLccCdiParserSerializer.DoNotification(Notify: TParserSerializer);
begin
  if Assigned(OnNotification) then
    OnNotification(Self, Notify);
end;

function TLccCdiParserSerializer.PackCVReads(CV_Start: DWord): Byte;
var
  Next: TDataElementInformation;
  CV_Next: DWord;
  i: Integer;
  Found: Boolean;
begin
  Result := 1;
  CV_Next := CV_Start + 1;
  repeat
    Found := False;
    i := 0; // Index 0 is what is being used at the base address
    while (i < DataElementInformationList.Count) and not Found do
    begin
      Next := TDataElementInformation( DataElementInformationList[i]);
      if Next.MemoryAddressPointer = CV_Next then
      begin
        Inc(Result);
        Inc(CV_Next);
        Found := True;
      end else
        Inc(i);
    end;

    if Found then
      DataElementInformationList.Delete(i);            // We don't own these, they belong to the Controls

  until not Found or (Result = OwnerParser.CVBlockRead);
end;

constructor TLccCdiParserSerializer.Create;
begin
  inherited Create;
  FDataElementInformation := nil;
  FDataElementInformationList := TList.Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TLccCdiParserSerializer.Destroy;
begin
  FreeAndNil(FDataElementInformationList);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

procedure TLccCdiParserSerializer.AddRead(AConfigInfo: TDataElementInformation);
begin
  if Assigned(AConfigInfo) then
  begin
    AConfigInfo.ConfigData.DataDirection := cdd_Read;
    DataElementInformationList.Add(AConfigInfo);
    DoNotification(ps_InsertRead);
  end;
end;

procedure TLccCdiParserSerializer.AddWrite(AConfigInfo: TDataElementInformation);
begin
  if Assigned(AConfigInfo) then
  begin
    AConfigInfo.ConfigData.DataDirection := cdd_Write;
    DataElementInformationList.Add(AConfigInfo);
    DoNotification(ps_InsertWrite);
  end;
end;

procedure TLccCdiParserSerializer.Clear;
var
  i: Integer;
begin
  for i := 0 to DataElementInformationList.Count - 1 do
  begin
    if TDataElementInformation(DataElementInformationList[i]).ConfigData.DataDirection = cdd_Read then
      DoNotification(ps_RemoveRead)
    else
      DoNotification(ps_RemoveWrite);
  end;
  DataElementInformationList.Clear;
  DataElementInformation := nil;
end;

procedure TLccCdiParserSerializer.SendNext;
var
  SequencialCVReads: Byte;
begin

  {
  if (DataElementInformationList.Count > 0) and not Assigned(DataElementInformation) and Assigned(OwnerParser.NodeManager.RootNode) then
  begin
    DataElementInformation := TDataElementInformation( DataElementInformationList[0]);
    DataElementInformationList.Delete(0);
    if Assigned(OwnerParser) then
    begin
      if DataElementInformation.ConfigData.DataDirection = cdd_Read then
      begin    // Reply will return on OwnerParser.DoConfigMemReadReply(...);
        if (DataElementInformation.MemoryAddressPointer and $FF000000 = $FF000000) and (OwnerParser.CVBlockRead > 1) then
        begin
           SequencialCVReads := PackCVReads(DataElementInformation.MemoryAddressPointer);
           OwnerParser.LccNode.ConfigurationMem.Initialize(DataElementInformation.MemoryAddressPointer, MSI_CONFIG, SequencialCVReads, DataElementInformation.DataType);
           WorkerMessage.LoadConfigMemRead(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, DataElementInformation.MemoryAddressPointer, SequencialCVReads);
           TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
        end else
        begin
          OwnerParser.LccNode.ConfigurationMem.Initialize(DataElementInformation.MemoryAddressPointer, MSI_CONFIG, DataElementInformation.MemoryAllocation, DataElementInformation.DataType);
          WorkerMessage.LoadConfigMemRead(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, DataElementInformation.MemoryAddressPointer, DataElementInformation.MemoryAllocation);
          TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
        end;
      end else
      if DataElementInformation.ConfigData.DataDirection = cdd_Write then
      begin    // Reply MIGHT return on OwnerParser.DoConfigMemWriteReply(...);  Nodes are NOT required to send this
        case DataElementInformation.DataType of
          cdt_Int :
            begin
              OwnerParser.LccNode.ConfigurationMem.Initialize(DataElementInformation.MemoryAddressPointer, MSI_CONFIG, DataElementInformation.MemoryAllocation, DataElementInformation.DataType);
              WorkerMessage.LoadConfigMemWriteInteger(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, DataElementInformation.MemoryAddressPointer, DataElementInformation.MemoryAllocation, DataElementInformation.ConfigData.DataInteger);
              TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
            end;
          cdt_String :
            begin
              OwnerParser.LccNode.ConfigurationMem.Initialize(DataElementInformation.MemoryAddressPointer, MSI_CONFIG, DataElementInformation.MemoryAllocation, DataElementInformation.DataType);
              WorkerMessage.LoadConfigMemWriteString(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, DataElementInformation.MemoryAddressPointer, DataElementInformation.ConfigData.DataString);
              TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
            end;
        end;
      end;
    end;
  end;       }
end;

procedure TLccCdiParserSerializer.Remove(AConfigInfo: TDataElementInformation);
begin
  if AConfigInfo = DataElementInformation then
  begin
    DataElementInformation := nil;
    if AConfigInfo.ConfigData.DataDirection = cdd_Read then
      DoNotification(ps_RemoveRead)
    else
      DoNotification(ps_RemoveWrite);
    SendNext;
  end;
end;


{ TMapRelation }

constructor TMapRelation.Create(AValue, AProperty: LccDOMString);
begin
  inherited Create;
  Value := AValue;
  Prop := AProperty
end;

{ TMapRelationList }

function TMapRelationList.GetRelation(Index: Integer): TMapRelation;
begin
  Result := TMapRelation( List[Index])
end;

function TMapRelationList.GetCount: Integer;
begin
  Result := List.Count;
end;


procedure TMapRelationList.SetRelation(Index: Integer; AValue: TMapRelation);
begin
  List[Index] := AValue
end;

constructor TMapRelationList.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMapRelationList.Destroy;
begin
  ClearList;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMapRelationList.Add(Relation: TMapRelation);
begin
  List.Add(Relation);
end;

procedure TMapRelationList.AddRelation(AValue, AProperty: LccDOMString);
begin
  List.Add( TMapRelation.Create( AValue, AProperty));
end;

procedure TMapRelationList.ClearList;
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

{ TLccOpenPascalComboBox }

constructor TLccOpenPascalComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataElementInformation := nil;
end;

destructor TLccOpenPascalComboBox.Destroy;
begin
  FreeAndNil( FDataElementInformation);
  inherited Destroy;
end;

{ TLccOpenPascalEdit }

constructor TLccOpenPascalEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataElementInformation := nil;
end;

destructor TLccOpenPascalEdit.Destroy;
begin
  FreeAndNil( FDataElementInformation);
  inherited Destroy;
end;

{ TLccOpenPascalSpinEdit }

procedure TLccOpenPascalSpinEdit.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TLccBitmap;
begin
 { case DataElementInformation.MemState of
    ocs_Current :
      begin
        ImageIndex := ImageIndexStateCurrent;
        StateImage.Picture.Clear;
      end;
    ocs_Unknown :
      begin
        ImageIndex := ImageIndexStateUnknown;
        Bitmap := TLccBitmap.Create;
        Bitmap.Width := ImageList16x16.Width;
        Bitmap.Height := ImageList16x16.Height;
        ImageList16x16.GetBitmap(ImageIndex, Bitmap);
        StateImage.Picture.Graphic := Bitmap;
        Bitmap.Free;
      end;
  end;      }
end;

constructor TLccOpenPascalSpinEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FDataElementInformation := nil;  // created during building of window as we don't know what control we need (if the element has a map or not) early enough
end;

destructor TLccOpenPascalSpinEdit.Destroy;
begin
  FreeAndNil( FDataElementInformation);
  inherited Destroy;
end;

{ TLccCdiParser }

function TLccCdiParser.CreateButton(AContainerParent: TLccPanel;
  ACaption: LccDOMString; OnClickFunc: TNotifyEvent; Enable: Boolean;
  AWidth: single;  AnAlign: {$IFDEF DELPHI}TAlignLayout{$ELSE}TAlign{$ENDIF}): TLccSpeedButton;
begin
  Result := TLccSpeedButton.Create(AContainerParent);
  Result.Width := {$IFDEF DELPHI}AWidth{$ELSE}Trunc(AWidth){$ENDIF};
  Result.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := string( ACaption);

  Result.OnClick := OnClickFunc;
  Result.Enabled := Enable;
  Result.Align := AnAlign;
  {$IFDEF DELPHI}
  Result.Padding.Left := 4;
  Result.Padding.Right := 4;
  Result.Padding.Top := 4;
  Result.Padding.Bottom := 4;
  {$ELSE}
  Result.BorderSpacing.Around := 2;      // Maybe a bug in this...
  {$ENDIF};
  Result.Height := BASE_BUTTON_HEIGHT;
  Result.Parent := AContainerParent;
end;

function TLccCdiParser.CreateLabel(ParentControl: TLccPanel;
  ACaption: LccDOMString; Indent: Single; Bold: Boolean): TLccLabel;
begin
  if ACaption <> '' then
  begin
    Result := TLccLabel.Create(ParentControl);
    {$IFDEF DELPHI}Result.StyledSettings := Result.StyledSettings-[TStyledSetting.Style];{$ENDIF}
    if Bold then
      {$IFDEF DELPHI}Result.TextSettings.Font.Style := [TFontStyle.fsBold]{$ELSE}Result.Font.Style := [fsBold]{$ENDIF};
    Result.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := string( ACaption);
    Result.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ParentControl.Height;   // Make sure it stacks in the right order;
    Result.Align := {$IFDEF DELPHI}TAlignLayout.Top{$ELSE}alTop{$ENDIF};
    Result.WordWrap := True;
    {$IFDEF DELPHI}Result.Margins.Left := Indent;{$ELSE}Result.BorderSpacing.Left := Trunc(Indent);{$ENDIF}
    Result.Parent := ParentControl;
  end;
end;

function TLccCdiParser.CreateSpacer(ParentControl: TLccPanel): TLccLabel;
begin
  Result := CreateLabel(ParentControl, ' ', 0, False);
end;

function TLccCdiParser.CreateTab(ATabControl: TLccTabControl; ACaption: LccDOMString): TLccPanel;
var
  LocalTab: TLccOpenPascalTabSheet;
  LocalScrollBox: TScrollBox;
begin
  if ACaption = '' then
    ACaption := 'Unknown';
  {$IFDEF DELPHI}
  // Create new Tab and connect it to the TabControl
  LocalTab := TLccOpenPascalTabSheet.Create(ATabControl);
  LocalTab.Text := ACaption;
  LocalTab.Parent := ATabControl;
  ATabControl.ActiveTab := LocalTab;   // Need to select the Tab so all components are dropped when created so the Align work right (alTop)

  // Create the ScrollBox and put it in the Tab
  LocalScrollBox := TScrollBox.Create(LocalTab);
  LocalScrollBox.Parent := LocalTab;
  LocalScrollBox.Align := TAlignLayout.Client;
  LocalScrollBox.Padding.Left := 4;
  LocalScrollBox.Padding.Right := 4;
  LocalScrollBox.Padding.Top := 4;
  LocalScrollBox.Padding.Bottom := 4;


  Result := TLccPanel.Create(LocalScrollBox);
 // Result.Anchors := [TAnchorKind.akLeft, TAnchorKind.akRight, TAnchorKind.akTop];
  Result.Position.X := 0;
  Result.Width := LocalScrollBox.Width - 20;
  Result.Parent := LocalScrollBox;
  Result.Height := 16384; /// TODO: Temp
  LocalTab.ScrollingWindowContentsPanel := Result;
  {$ELSE}
  // Create new Tab and connect it to the TabControl

  LocalTab := TLccOpenPascalTabSheet.Create(Self);
  LocalTab.PageControl := ATabControl;

  LocalTab.Caption := string( ACaption);
  ATabControl.ActivePage := LocalTab;   // Need to select the Tab so all components are dropped when created so the Align work right (alTop)

  // Create the ScrollBox and put it in the Tab
  LocalScrollBox := TScrollBox.Create(LocalTab);
  LocalScrollBox.Parent := LocalTab;
  LocalScrollBox.Align := alClient;
  LocalScrollBox.BorderSpacing.Around := 4;
  LocalScrollBox.AutoScroll := True;

  LocalScrollBox.VertScrollBar.Tracking := True;
  LocalScrollBox.HorzScrollBar.Tracking := True;

  Result := TLccPanel.Create(LocalScrollBox);
 // Result.Anchors := [akLeft, akRight, akTop];
  Result.Left := 0;
  Result.Width := LocalScrollBox.Width - TAB_RIGHT_MARGIN;
  Result.Parent := LocalScrollBox;
  Result.Height := 16384;                // TODO: TEMP
  LocalTab.ScrollingWindowContentsPanel := Result;
  {$ENDIF}
end;

function TLccCdiParser.ProcessIdentificationTab(ATabControl: TLccTabControl;
  IdentificationElement: TLccXmlNode; Indent: Integer): TLccPanel;
var
  IdentificationElementChild: TLccXmlNode;
  ItemStr: LccDOMString;
  LastLabel: TLccLabel;
begin
  ItemStr := '';
  if Assigned(IdentificationElement) then
  begin

    // Add a tab to place the Identification information on
    Result := CreateTab(ATabControl, 'Identification');

    // Space on the Top
    CreateSpacer(Result);

    if ExtractElementItem(IdentificationElement, 'name', ItemStr) then
      CreateLabel(Result, ItemStr, Indent, True);

    if ExtractElementItem(IdentificationElement, 'description', ItemStr) then
      CreateLabel(Result, ItemStr, Indent + LABEL_DELTA_INDENT, False);


    // Handle the manufacturer
    CreateLabel(Result, 'Manufacturer', 0, True);

    IdentificationElementChild := XmlFindChildNode(IdentificationElement, 'manufacturer');
    if Assigned(IdentificationElementChild) then
      CreateLabel(Result, ItemStr + XmlNodeTextContent(IdentificationElementChild), Indent + LABEL_DELTA_INDENT, False)
    else
      CreateSpacer(Result);


    // Handle the model number
    CreateLabel(Result, 'Model: ', 0, True);
    IdentificationElementChild := XmlFindChildNode(IdentificationElement, 'model');
    if Assigned(IdentificationElementChild) then
      CreateLabel(Result, ItemStr + XmlNodeTextContent(IdentificationElementChild), Indent + LABEL_DELTA_INDENT, False)
    else
      CreateSpacer(Result);

    // Handle the Hardware Version
    CreateLabel(Result, 'Hardware Version: ', 0, True);
    IdentificationElementChild := XmlFindChildNode(IdentificationElement, 'hardwareVersion');
    if Assigned(IdentificationElementChild) then
      CreateLabel(Result, ItemStr + XmlNodeTextContent(IdentificationElementChild), Indent + LABEL_DELTA_INDENT, False)
    else
      CreateSpacer(Result);

    // Handle the Software Version
    CreateLabel(Result, 'Software Version: ', 0, True);
    IdentificationElementChild := XmlFindChildNode(IdentificationElement, 'softwareVersion');
    if Assigned(IdentificationElementChild) then
      CreateLabel(Result, ItemStr + XmlNodeTextContent(IdentificationElementChild), Indent + LABEL_DELTA_INDENT, False)
    else
      CreateSpacer(Result);

    // Space on the bottom
    LastLabel := CreateSpacer(Result);
    {$IFDEF DELPHI}
    Result.Height := LastLabel.Position.Y + LastLabel.Height;
    {$ELSE}
    Result.Height := LastLabel.Top + LastLabel.Height;
    {$ENDIF}

  end;
end;

function TLccCdiParser.ProcessSegmentTab(ATabControl: TLccTabControl;
  SegmentElement: TLccXmlNode; Indent: Integer): TLccPanel;
var
  MemoryAddressPointer: Int64;
  ElementString: LccDOMString;
  AddressSpace: DWord;
  LastLabel: TLccLabel;
  i: Integer;
begin
  ElementString := '';

  // Only a Segment can have an 'origin' to start the address mapping and it can not have an 'offset'
  if XmlAttributeExists(SegmentElement, 'origin') then
    MemoryAddressPointer := StrToInt64(string( XmlAttributeRead(SegmentElement, 'origin')))
  else
    MemoryAddressPointer := 0;

  // Read in the Address Space.. this is required
  if XmlAttributeExists(SegmentElement, 'space') then
    AddressSpace := StrToUInt(string( XmlAttributeRead(SegmentElement, 'space')));

  // Create the tab and background panel to add to
  ExtractElementItem(SegmentElement, 'name', ElementString);
  Result := CreateTab(ATabControl, ElementString);


  // Time to build the UI for this segment
  Result.Visible := False;
  ProcessElementForUISegment(Result, SegmentElement, MemoryAddressPointer, Indent, AddressSpace);

  // Space on the bottom
  LastLabel := CreateSpacer(Result);
  {$IFDEF DELPHI}
  Result.Height := LastLabel.Position.Y + LastLabel.Height;
  Result.Visible := True;
  {$ELSE}
  // Quirkiness of FPC...
  Result.Visible := True;
  Result.Visible := False;
  for i := Result.ControlCount - 1 downto 0 do
    Result.Controls[i].Top := 0;
  Result.Visible := True;

  if Result.ControlCount > 0 then
    Result.Height := Result.Controls[Result.ControlCount - 1].Top + Result.Controls[Result.ControlCount - 1].Height;
  {$ENDIF}


end;

function TLccCdiParser.CreateBaseEditorLayout(ParentControl: TLccPanel;
  DataElementInformation: TDataElementInformation): TLccPanel;
begin
  Result := TLccPanel.Create(ParentControl);
  {$IFNDEF DELPHI}Result.Caption := '';{$ENDIF};
  Result.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ParentControl.Height;   // Make sure it stacks in the right order;
  Result.Align := {$IFDEF DELPHI}TAlignLayout.Top{$ELSE}alTop{$ENDIF};
  {$IFNDEF DELPHI}Result.BevelOuter := bvNone;{$ENDIF}
  Result.Parent := ParentControl;

  DataElementInformation.ReadMemorySpaceButton := CreateButton(Result, 'Read', {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonReadClick, False, BASE_BUTTON_WIDTH, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  DataElementInformation.WriteMemorySpaceButton := CreateButton(Result, 'Write', {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonWriteClick, False, BASE_BUTTON_WIDTH, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  DataElementInformation.CompareMemorySpaceButton := CreateButton(Result, 'Compare', {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonCompareClick, False, BASE_BUTTON_WIDTH, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});

  DataElementInformation.ReadMemorySpaceButton.Visible := ShowReadBtn;
  DataElementInformation.WriteMemorySpaceButton.Visible := ShowWriteBtn;
  DataElementInformation.CompareMemorySpaceButton.Visible := ShowCompareBtn;

  Result.Height := BASE_EDITOR_HEIGHT;          ;
end;

procedure TLccCdiParser.CreateSpinEditLayout(ParentControl: TLccPanel;
  Indent: Integer; DataElementInformation: TDataElementInformation);
var
  SpinEdit: TLccOpenPascalEdit;
  ContainerPanel: TLccPanel;
begin
  {$IFDEF PRINT_MEM_LOCATIONS}
    CreateLabel(ParentControl, 'Offset: ' + LccDOMString( IntToStr(MemoryStartPointer)), Indent, False);
    CreateLabel(ParentControl, 'Size: ' + LccDOMString( IntToStr(DataElementInformation.MemoryAllocation)), Indent, False);
  {$ENDIF}

  ContainerPanel := CreateBaseEditorLayout(ParentControl, DataElementInformation);

  SpinEdit := TLccOpenPascalEdit.Create(ContainerPanel);
  SpinEdit.Parent := ContainerPanel;
  SpinEdit.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  {$IFDEF DELPHI}SpinEdit.Margins.Left := Indent;{$ELSE}SpinEdit.BorderSpacing.Left := Trunc(Indent);{$ENDIF}
  {$IFDEF DELPHI}SpinEdit.Margins.Right := Indent;{$ELSE}SpinEdit.BorderSpacing.Right := Trunc(Indent);{$ENDIF}


 // SpinEdit.MaxValue := Info.in;


end;

procedure TLccCdiParser.CreateEditLayout(ParentControl: TLccPanel;
  Indent: Integer; DataElementInformation: TDataElementInformation);
var
  EditBox: TLccOpenPascalEdit;
  ContainerPanel: TLccPanel;
begin
  {$IFDEF PRINT_MEM_LOCATIONS}
    CreateLabel(ParentControl, 'Offset: ' + LccDOMString( IntToStr(MemoryStartPointer)), Indent, False);
    CreateLabel(ParentControl, 'Size: ' + LccDOMString( IntToStr(DataElementInformation.MemoryAllocation)), Indent, False);
  {$ENDIF}

  ContainerPanel := CreateBaseEditorLayout(ParentControl, DataElementInformation);

  EditBox := TLccOpenPascalEdit.Create(ContainerPanel);
  EditBox.Parent := ContainerPanel;
  EditBox.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  {$IFDEF DELPHI}EditBox.Margins.Left := Indent;{$ELSE}EditBox.BorderSpacing.Left := Trunc(Indent);{$ENDIF}
  {$IFDEF DELPHI}EditBox.Margins.Right := Indent;{$ELSE}EditBox.BorderSpacing.Right := Trunc(Indent);{$ENDIF}
end;

procedure TLccCdiParser.CreateComboBoxListLayout(ParentControl: TLccPanel;
  Indent: Integer; DataElementInformation: TDataElementInformation);
var
  ContainerPanel: TLccPanel;
  ComboBox: TLccOpenPascalComboBox;
  i: Integer;
begin
  {$IFDEF PRINT_MEM_LOCATIONS}
    CreateLabel(ParentControl, 'Offset: ' + LccDOMString( IntToStr(MemoryStartPointer)), Indent, False);
    CreateLabel(ParentControl, 'Size: ' + LccDOMString( IntToStr(DataElementInformation.MemoryAllocation)), Indent, False);
  {$ENDIF}

  ContainerPanel := CreateBaseEditorLayout(ParentControl, DataElementInformation);

  ComboBox := TLccOpenPascalComboBox.Create(ContainerPanel);
  ComboBox.Parent := ContainerPanel;
  {$IFNDEF DELPHI}ComboBox.Style := csDropDownList;{$ENDIF}
  ComboBox.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  {$IFDEF DELPHI}ComboBox.Margins.Left := Indent;{$ELSE}ComboBox.BorderSpacing.Left := Trunc(Indent);{$ENDIF}
  {$IFDEF DELPHI}ComboBox.Margins.Right := Indent;{$ELSE}ComboBox.BorderSpacing.Right := Trunc(Indent);{$ENDIF}
  for i := 0 to DataElementInformation.MapList.Count - 1 do
    ComboBox.Items.Add(string( TMapRelation(DataElementInformation.MapList.Relations[i]).Value));
  ComboBox.ItemIndex := 0;

  ComboBox.DataElementInformation := DataElementInformation;
end;

procedure TLccCdiParser.DoAfterReadPage(Sender: TObject);
begin
  if Assigned(OnAfterReadPage) then
    OnAfterReadPage(Self)
end;

procedure TLccCdiParser.DoAfterWritePage(Sender: TObject);
begin
  if Assigned(OnAfterWritePage) then
    OnAfterWritePage(Self)
end;

procedure TLccCdiParser.DoClearInterface;
begin
  if Assigned(OnClearInterface) then
    OnClearInterface(Self);
end;

procedure TLccCdiParser.DoConfigMemReadReply(ANode: TObject);

{

    procedure LoadConfigMemToControl(TempNode: TLccOwnedNode; Control: TComponent; DataOffset: Integer);
    var
      TempConfigInfo: TDataElementInformation;
    begin
      if Assigned(Control) then
      begin
        TempConfigInfo := nil;
        if Control is TLccOpenPascalSpinEdit then
        begin
          if TempNode.ConfigurationMem.DataType = cdt_Int then
          begin
            if TempNode.ConfigurationMem.Address and $FF000000 = $FF000000 then
              TLccOpenPascalSpinEdit(Control).Value := TempNode.ConfigurationMem.DataRaw[DataOffset]
            else
              TLccOpenPascalSpinEdit(Control).Value := TempNode.ConfigurationMem.DataTypeInteger;
            TLccOpenPascalSpinEdit(Control).OnChange(TLccOpenPascalSpinEdit(Control));
            TempConfigInfo := TLccOpenPascalSpinEdit(Control).DataElementInformation;
            TempConfigInfo.MemState := ocs_Current;
            Serializer.Remove(TempConfigInfo);
          end;
        end else
        if Control is TLccOpenPascalEdit then
        begin
          if TempNode.ConfigurationMem.DataType = cdt_String then
          begin
            TLccOpenPascalEdit(Control).Text := TempNode.ConfigurationMem.DataTypeString;
            TLccOpenPascalEdit(Control).OnChange(TLccOpenPascalEdit(Control));
            TempConfigInfo := TLccOpenPascalEdit(Control).DataElementInformation;
            TempConfigInfo.MemState := ocs_Current;
            Serializer.Remove(TempConfigInfo);
          end;
        end else
        if Control is TLccOpenPascalComboBox then
        begin
          if TempNode.ConfigurationMem.DataType = cdt_Int then
          begin
            TLccOpenPascalComboBox(Control).ItemIndex := TempNode.ConfigurationMem.DataTypeInteger;
            TLccOpenPascalComboBox(Control).OnChange(TLccOpenPascalComboBox(Control));
            TempConfigInfo := TLccOpenPascalComboBox(Control).DataElementInformation;
            TempConfigInfo.MemState := ocs_Current;
            Serializer.Remove(TempConfigInfo);
          end;
        end;
      end;
    end;        }

var
//  TempNode: TLccOwnedNode;

  i: Integer;

begin
{  Assert(not (ANode is TLccOwnedNode), 'Not a TLccNode');

  TempNode := TLccOwnedNode( ANode);
  if TempNode = LccNode then       // Make sure the passed node is the same one we called on for the config memory
  begin
    if TempNode.ConfigurationMem.Address and $FF000000 = $FF000000 then
    begin
      for i := TempNode.ConfigurationMem.DataCount - 1 downto 0 do             // Do this backward to ensure we don't delete the initial ConfigMem and force a new block to be read
        LoadConfigMemToControl(TempNode, FindControlByAddress(TempNode.ConfigurationMem.Address + i), i)
    end else
      LoadConfigMemToControl(TempNode, FindControlByAddress(TempNode.ConfigurationMem.Address), 0)
  end;  }
end;

procedure TLccCdiParser.DoConfigMemWriteReply(ANode: TObject);
var
  Control: TLccControl;
 // TempNode: TLccOwnedNode;
  TempConfigInfo: TDataElementInformation;
begin

  {
  Assert(not (ANode is TLccOwnedNode), 'Not a TLccNode');

  TempNode := TLccOwnedNode( ANode);
  if TempNode = LccNode then       // Make sure the passed node is the same one we called on for the config memory
  begin
    TempConfigInfo := nil;
    Control := FindControlByAddress(TempNode.ConfigurationMem.Address);
    if Assigned(Control) then
    begin
      if Control is TLccOpenPascalSpinEdit then
      begin
        if TempNode.ConfigurationMem.DataType = cdt_Int then
        begin
          TLccOpenPascalSpinEdit(Control).DataElementInformation.MemState := ocs_Current;
          TempConfigInfo := TLccOpenPascalSpinEdit(Control).DataElementInformation;
        end;
      end else
      if Control is TLccOpenPascalEdit then
      begin
        if TempNode.ConfigurationMem.DataType = cdt_String then
        begin
          TLccOpenPascalEdit(Control).DataElementInformation.MemState := ocs_Current;
          TempConfigInfo := TLccOpenPascalEdit(Control).DataElementInformation;
        end;
      end else
      if Control is TLccOpenPascalComboBox then
      begin
        if TempNode.ConfigurationMem.DataType = cdt_Int then
        begin
          TLccOpenPascalComboBox(Control).DataElementInformation.MemState := ocs_Current;
          TempConfigInfo := TLccOpenPascalComboBox(Control).DataElementInformation;
        end;
      end;
    end;
    Serializer.Remove(TempConfigInfo);
  end;   }
end;

procedure TLccCdiParser.DoSpeedButtonCompareClick(Sender: TObject);
var
  ConfigInfo: TDataElementInformation;
begin
  if Assigned(FLccNode) then
  begin
    ConfigInfo := SpeedButtonToConfigInfo(Sender);
    if Assigned(ConfigInfo) then
    begin
      // Reply will return on DoConfigMemReply(...);
    end;
  end;
end;

procedure TLccCdiParser.DoSpeedButtonReadClick(Sender: TObject);
begin
  if Assigned(FLccNode) and Assigned(NodeManager) then
  begin
    Serializer.AddRead(SpeedButtonToConfigInfo(Sender));
    Serializer.SendNext;
  end;
end;

procedure TLccCdiParser.DoSpeedButtonWriteClick(Sender: TObject);
begin
  if Assigned(FLccNode) and Assigned(NodeManager) then
  begin
    Serializer.AddWrite(SpeedButtonToConfigInfo(Sender));
    Serializer.SendNext;
  end;
end;

procedure TLccCdiParser.DoBuildInterfaceComplete;
begin
  if Assigned(OnBuildInterfaceComplete) then
    OnBuildInterfaceComplete(Self);
end;

procedure TLccCdiParser.DoButtonReadPageClick(Sender: TObject);

  procedure RunSheet(Component: TComponent);
  var
    i: Integer;
  begin
    for i := 0 to Component.ComponentCount - 1 do
      RunSheet(Component.Components[i]);

    if Component is TLccOpenPascalSpinEdit then
      Serializer.AddRead(TLccOpenPascalSpinEdit( Component).DataElementInformation)
    else
    if Component is TLccOpenPascalEdit then
      Serializer.AddRead(TLccOpenPascalEdit( Component).DataElementInformation)
    else
    if Component is TLccOpenPascalComboBox then
      Serializer.AddRead(TLccOpenPascalComboBox( Component).DataElementInformation)
  end;

var
  TabSheet: TLccTabSheet;
begin
  TabSheet := FindActivePage;
  if Assigned(TabSheet) then
    RunSheet(TabSheet);
  Serializer.SendNext;
end;

procedure TLccCdiParser.DoButtonStopClick(Sender: TObject);
begin
  MarkedToStop := True;
end;

procedure TLccCdiParser.DoButtonWritePageClick(Sender: TObject);
  procedure RunSheet(Component: TComponent);
  var
    i: Integer;
  begin
    for i := 0 to Component.ComponentCount - 1 do
      RunSheet(Component.Components[i]);

    if Component is TLccOpenPascalSpinEdit then
      Serializer.AddWrite(TLccOpenPascalSpinEdit( Component).DataElementInformation)
    else
    if Component is TLccOpenPascalEdit then
      Serializer.AddWrite(TLccOpenPascalEdit( Component).DataElementInformation)
    else
    if Component is TLccOpenPascalComboBox then
      Serializer.AddWrite(TLccOpenPascalComboBox( Component).DataElementInformation)
  end;

var
  TabSheet: TLccTabSheet;
begin
  TabSheet := FindActivePage;
  if Assigned(TabSheet) then
    RunSheet(TabSheet);
  Serializer.SendNext;
end;

function TLccCdiParser.ExtractElementItem(ParentElement: TLccXmlNode; Item: string;var ItemStr: LccDOMString): Boolean;
var
  Node: TLccXmlNode;
begin
  Result := False;
  ItemStr := '';
  Node := XmlFindChildNode(ParentElement, LccDOMString(Item));
  if Assigned(Node) then
  begin
    Result := True;
    ItemStr := XmlNodeTextContent(Node);
  end;
end;

function TLccCdiParser.ExtractElementAttribute(Element: TLccXmlNode;
  AttributeName: LccDOMString; var AttributeStr: LccDOMString): Boolean;
begin
  AttributeStr := '';
  Result := XmlAttributeExists(Element, LccDOMString(AttributeName));
  if Result then
   AttributeStr := XmlAttributeRead(Element, LccDOMString(AttributeName));
end;

function TLccCdiParser.FindControlByAddress(Address: DWord): TLccControl;

  function SearchComponents(AComponent: TComponent): TComponent;
  var
    i: Integer;
  begin
    Result := nil;
    for i := 0 to AComponent.ComponentCount - 1 do
    begin
      Result := SearchComponents(AComponent.Components[i]);
      if Assigned(Result) then
        Break
    end;

    if not Assigned(Result) then
    begin
      if AComponent is TLccOpenPascalSpinEdit then
      begin
        if TLccOpenPascalSpinEdit( AComponent).DataElementInformation.MemoryAddressPointer = Address then
          Result := AComponent
      end else
      if AComponent is TLccOpenPascalEdit then
      begin
        if TLccOpenPascalEdit( AComponent).DataElementInformation.MemoryAddressPointer = Address then
          Result := AComponent
      end else
      if AComponent is TLccOpenPascalComboBox then
      begin
        if TLccOpenPascalComboBox( AComponent).DataElementInformation.MemoryAddressPointer = Address then
          Result := AComponent
      end
    end
  end;

begin
  Result := nil;
  if Assigned(ApplicationPanel) then
    Result := SearchComponents(ApplicationPanel) as TLccControl;
end;

function TLccCdiParser.GetCVBlockRead: Word;
begin
  Result := FCVBlockRead;
end;

function TLccCdiParser.FindActivePage: TLccTabSheet;
var
  i: Integer;
  PageControl: TLccTabControl;
begin
  Result := nil;
  if Assigned(ApplicationPanel) then
  begin
    for i := 0 to ApplicationPanel.ComponentCount - 1 do
    begin
      if ApplicationPanel.Components[i] is TLccTabControl then
      begin
        PageControl := TLccTabControl( ApplicationPanel.Components[i]);
        Result := PageControl.{$IFDEF DELPHI}ActiveTab{$ELSE}ActivePage{$ENDIF};
      end;
    end;
  end;
end;

function TLccCdiParser.IsMemorySpace(Segment: TLccXmlNode; MemorySpace: Byte): Boolean;
begin
  Result := False;
  if XmlAttributeExists(Segment, 'space') then
    Result := StrToInt( String(XmlAttributeRead(Segment, 'space'))) = MemorySpace;
end;

procedure TLccCdiParser.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if AComponent is TLccNodeManager then
  begin
 //    case Operation of
 //      opInsert : TLccNodeManager(AComponent).CdiParser := Self;
 //      opRemove : TLccNodeManager(AComponent).CdiParser := nil;
  //   end;
  end;
end;

procedure TLccCdiParser.ProcessElementForUISegment(ParentControl: TLccPanel;
  Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer;
  AddressSpace: Byte);
var
  Group_Child, Map_Child: TLccXmlNode;
  TempStr: LccDOMString;
  ReplicationCount, i: Integer;
  ElementMemoryAllocated: DWord;
  ChildElement: TLccXmlNode;
begin
 if Assigned(Element) then
 begin
   TempStr := '';

   // Update the Address Pointer if necessary
   MemoryAddressPointer := 0;   // Note that 'offset' is not valid in a Segment so reset it just in case
   if XmlAttributeExists(Element, 'origin') then
     MemoryAddressPointer := MemoryAddressPointer + StrToInt64(string( XmlAttributeRead(Element, 'origin')));

   ChildElement := XmlFirstChild(Element);
   while Assigned(ChildElement) do
   begin
     if ChildElement.NodeName = 'name' then
       CreateLabel(ParentControl, XmlNodeTextContent(ChildElement), Indent + LABEL_DELTA_INDENT, True)
     else
     if ChildElement.NodeName = 'description' then
       CreateLabel(ParentControl, XmlNodeTextContent(ChildElement), Indent + LABEL_DELTA_INDENT_DOUBLE, False)
     else
     if ChildElement.NodeName = 'group' then
       ProcessElementForUIGroup(ParentControl, ChildElement, MemoryAddressPointer, Indent + LABEL_DELTA_INDENT_DOUBLE, AddressSpace)
     else
     if ChildElement.NodeName = 'string' then
       ProcessElementForUIString(ParentControl, ChildElement, MemoryAddressPointer, Indent + LABEL_DELTA_INDENT_DOUBLE, AddressSpace)
     else
     if ChildElement.NodeName = 'int' then
       ProcessElementForUIInt(ParentControl, ChildElement, MemoryAddressPointer, Indent + LABEL_DELTA_INDENT_DOUBLE, AddressSpace)
     else
     if ChildElement.NodeName = 'eventid' then
       ProcessElementForUIEventID(ParentControl, ChildElement, MemoryAddressPointer, Indent + LABEL_DELTA_INDENT_DOUBLE, AddressSpace) ;

     ChildElement := ChildElement.NextSibling;
   end;
 end;
end;

procedure TLccCdiParser.ProcessElementForUIGroup(ParentControl: TLccPanel;
  Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer;
  AddressSpace: Byte);
var
  Replications, i: Integer;
  ChildElement: TLccXmlNode;
  NameDone, DescriptionDone: Boolean;
begin
  if Assigned(Element) then
  begin
    Replications := 1;
    NameDone := False;
    DescriptionDone := False;

    // Looks for replications
    if XmlAttributeExists(Element, 'replication') then
      Replications := StrToInt( string( XmlAttributeRead(Element, 'replication')));

    // Update the Address Offset if necessary
    if XmlAttributeExists(Element, 'offset') then
      MemoryAddressPointer := MemoryAddressPointer + StrToInt64(string( XmlAttributeRead(Element, 'offset')));

    for i := 0 to Replications - 1 do
    begin
      ChildElement := XmlFirstChild(Element);
      while Assigned(ChildElement) do
      begin
        if ChildElement.NodeName = 'name' then
        begin
          if not NameDone then
          begin
            CreateLabel(ParentControl, XmlNodeTextContent(ChildElement), Indent, True);
            NameDone := True;
          end;
        end else
        if ChildElement.NodeName = 'description' then
        begin
          if not DescriptionDone then
          begin
            CreateLabel(ParentControl, XmlNodeTextContent(ChildElement), Indent + LABEL_DELTA_INDENT, False);
            DescriptionDone := True;
          end;
        end else
        if ChildElement.NodeName = 'repname' then
          CreateLabel(ParentControl, XmlNodeTextContent(ChildElement) + ' ' + LccDOMString(IntToStr(i)), Indent, False)
        else
        if ChildElement.NodeName = 'group' then
          ProcessElementForUIGroup(ParentControl, ChildElement, MemoryAddressPointer, Indent + LABEL_DELTA_INDENT_DOUBLE, AddressSpace)
        else
        if ChildElement.NodeName = 'string' then
          ProcessElementForUIString(ParentControl, ChildElement, MemoryAddressPointer, Indent + LABEL_DELTA_INDENT_DOUBLE, AddressSpace)
        else
        if ChildElement.NodeName = 'int' then
          ProcessElementForUIInt(ParentControl, ChildElement, MemoryAddressPointer, Indent + LABEL_DELTA_INDENT_DOUBLE, AddressSpace)
        else
         if ChildElement.NodeName = 'eventid' then
           ProcessElementForUIEventID(ParentControl, ChildElement, MemoryAddressPointer, Indent + LABEL_DELTA_INDENT_DOUBLE, AddressSpace);

        ChildElement := ChildElement.NextSibling;
      end;
    end;
  end;
end;

procedure TLccCdiParser.ProcessElementForUIString(ParentControl: TLccPanel;
  Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer;
  AddressSpace: Byte);
var
  DataElementInformation: TDataElementInformation;
begin
  if Assigned(Element) then
  begin
    DataElementInformation := TDataElementInformation.Create(cdt_String);
    ProcessElementForUIDataElement(ParentControl, Element, MemoryAddressPointer, Indent, AddressSpace, DataElementInformation);
    if DataElementInformation.MapList.Count > 0 then
      CreateComboBoxListLayout(ParentControl, Indent, DataElementInformation)
    else
      CreateEditLayout(ParentControl, Indent, DataElementInformation);
  end;
end;

procedure TLccCdiParser.ProcessElementForUIInt(ParentControl: TLccPanel;
  Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer;
  AddressSpace: Byte);
var
  DataElementInformation: TDataElementInformation;
  ChildElement: TLccXmlNode;
begin
  if Assigned(Element) then
  begin
    DataElementInformation := TDataElementInformation.Create(cdt_Int);
    ProcessElementForUIDataElement(ParentControl, Element, MemoryAddressPointer, Indent, AddressSpace, DataElementInformation);

    ChildElement := XmlFirstChild(Element);
    while Assigned(ChildElement) do
    begin
      if ChildElement.NodeName = 'min' then
        DataElementInformation.IntMin := StrToInt64(string( XmlNodeTextContent(ChildElement)))
      else
      if ChildElement.NodeName = 'max' then
        DataElementInformation.IntMax := StrToInt64(string( XmlNodeTextContent(ChildElement)))
      else
      if ChildElement.NodeName = 'default' then
        DataElementInformation.IntDefault := StrToInt64(string( XmlNodeTextContent(ChildElement)));

      ChildElement := ChildElement.NextSibling;
    end;


    if DataElementInformation.MapList.Count > 0 then
      CreateComboBoxListLayout(ParentControl, Indent, DataElementInformation)
    else
      CreateSpinEditLayout(ParentControl, Indent, DataElementInformation)
  end;
end;

procedure TLccCdiParser.ProcessElementForUIEventID(ParentControl: TLccPanel;
  Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer;
  AddressSpace: Byte);
var
  DataElementInformation: TDataElementInformation;
begin
  if Assigned(Element) then
  begin
    DataElementInformation := TDataElementInformation.Create(cdt_EventID);
    ProcessElementForUIDataElement(ParentControl, Element, MemoryAddressPointer, Indent, AddressSpace, DataElementInformation);

    if DataElementInformation.MapList.Count > 0 then
      CreateComboBoxListLayout(ParentControl, Indent, DataElementInformation)
    else
      CreateEditLayout(ParentControl, Indent, DataElementInformation);
  end;
end;

procedure TLccCdiParser.ProcessElementForUIMap(Element: TLccXmlNode;
  var MemoryAddressPointer: Int64;
  DataElementInformation: TDataElementInformation);
var
  ChildElement: TLccXmlNode;
  PropertyStr, ValueStr: LccDOMString;
begin
   if Assigned(Element) then
   begin
     // Update the Address Offset if necessary
     if XmlAttributeExists(Element, 'offset') then
       MemoryAddressPointer := MemoryAddressPointer + StrToInt64(string( XmlAttributeRead(Element, 'offset')));

     ChildElement := XmlFirstChild(Element);
     while Assigned(ChildElement) do
     begin
       if ChildElement.NodeName = 'relation' then
       begin
         PropertyStr := XmlNodeFindChildNodeTextContent(ChildElement, 'property');
         ValueStr := XmlNodeFindChildNodeTextContent(ChildElement, 'value');
         DataElementInformation.MapList.AddRelation(ValueStr, PropertyStr);
       end;
       ChildElement := ChildElement.NextSibling;
     end;

   end;
end;

procedure TLccCdiParser.ProcessElementForUIDataElement(
  ParentControl: TLccPanel; Element: TLccXmlNode;
  var MemoryAddressPointer: Int64; Indent: Integer; AddressSpace: Byte;
  DataElementInformation: TDataElementInformation);
var
  ChildElement: TLccXmlNode;
begin
  if Assigned(Element) then
  begin
    // Update the DataElement Information on Memory Size (valid for Strings and Integers only, EventIDs will not have this)
    if XmlAttributeExists(Element, 'size') then
      DataElementInformation.MemoryAllocation := StrToInt(string( XmlAttributeRead(Element, 'size')));

    // Update the Address Offset if necessary and update the DataElement Information for this Data Element
    if XmlAttributeExists(Element, 'offset') then
      MemoryAddressPointer := MemoryAddressPointer + StrToInt64(string( XmlAttributeRead(Element, 'offset')));

    // At this point the Memory pointer is valid for the DataElement
    DataElementInformation.MemoryAddressPointer := MemoryAddressPointer;

    ChildElement := XmlFirstChild(Element);
    while Assigned(ChildElement) do
    begin
      if ChildElement.NodeName = 'name' then
        CreateLabel(ParentControl, XmlNodeTextContent(ChildElement), Indent + LABEL_DELTA_INDENT, True)
      else
      if ChildElement.NodeName = 'description' then
        CreateLabel(ParentControl, XmlNodeTextContent(ChildElement), Indent + LABEL_DELTA_INDENT_DOUBLE, False)
      else
      if ChildElement.NodeName = 'map' then
        ProcessElementForUIMap(ChildElement, MemoryAddressPointer, DataElementInformation);
      ChildElement := ChildElement.NextSibling;
    end;

   end;
end;

procedure TLccCdiParser.SetCVBlockRead(AValue: Word);
begin
  FCVBlockRead := AValue;
end;

procedure TLccCdiParser.SetNodeManager(ANodeManager: TObject);
begin
  NodeManager := ANodeManager as TLccNodeManager;
end;

function TLccCdiParser.SpeedButtonToConfigInfo(Sender: TObject): TDataElementInformation;
begin
  if TLccSpeedButton(Sender).Owner is TLccOpenPascalSpinEdit then
    Result := TLccOpenPascalSpinEdit( TLccSpeedButton(Sender).Owner).DataElementInformation
  else
  if TLccSpeedButton(Sender).Owner is TLccOpenPascalEdit then
    Result := TLccOpenPascalEdit( TLccSpeedButton(Sender).Owner).DataElementInformation
  else
  if TLccSpeedButton(Sender).Owner is TLccOpenPascalComboBox then
    Result := TLccOpenPascalComboBox( TLccSpeedButton(Sender).Owner).DataElementInformation
  else
   Result := nil;
end;

procedure TLccCdiParser.OnSpinEditChange(Sender: TObject);
begin
  (Sender as TLccOpenPascalSpinEdit).DataElementInformation.MemState := ocs_Unknown;
  (Sender as TLccOpenPascalSpinEdit).DataElementInformation.ConfigData.DataInteger := Round((Sender as TLccOpenPascalSpinEdit).Value);
end;

procedure TLccCdiParser.OnEditChange(Sender: TObject);
begin
  (Sender as TLccOpenPascalEdit).DataElementInformation.MemState := ocs_Unknown;
  (Sender as TLccOpenPascalEdit).DataElementInformation.ConfigData.DataString := AnsiString( (Sender as TLccOpenPascalEdit).Text);
end;

procedure TLccCdiParser.OnComboBoxChange(Sender: TObject);
begin
 (Sender as TLccOpenPascalComboBox).DataElementInformation.MemState := ocs_Unknown;
 (Sender as TLccOpenPascalComboBox).DataElementInformation.ConfigData.DataInteger := (Sender as TLccOpenPascalComboBox).ItemIndex;
end;

procedure TLccCdiParser.OnPageControlChange(Sender: TObject);
begin
  ResizeActiveTabScrollingWindowFrame;

  Serializer.Clear;
  if AutoReadOnTabChange then
    DoButtonReadPageClick(Sender);
end;

procedure TLccCdiParser.OnSerializerNotification(Sender: TObject; Notify: TParserSerializer);
begin
  if Assigned(ApplicationPanel) then
  begin

    if MarkedToStop and not MarkedToStopIsStopping then
    begin
      MarkedToStopIsStopping := True;
      Serializer.Clear;
    end;

    if Serializer.DataElementInformationList.Count > 0 then
    begin
      GlobalButtonReadPage.Enabled := False;
      GlobalButtonWritePage.Enabled := False;
      GlobalButtonStop.Enabled := True;
    end else
    begin
      GlobalButtonReadPage.Enabled := True;
      GlobalButtonWritePage.Enabled := True;
      GlobalButtonStop.Enabled := False;
      MarkedToStop := False;
      MarkedToStopIsStopping := False;
      if Notify = ps_RemoveRead then
        DoAfterReadPage(Self)
      else
        DoAfterWritePage(Self);
    end;
//    StatusPanel.Text := 'Remaining: ' + IntToStr(Serializer.DataElementInformationList.Count);
  end;
end;

procedure TLccCdiParser.OnParserFrameResize(Sender: TObject);
begin
  ResizeActiveTabScrollingWindowFrame;
end;

procedure TLccCdiParser.ResizeActiveTabScrollingWindowFrame;
var
  TabSheet: TLccOpenPascalTabSheet;
begin
  {$IFDEF DELPHI}
  TabSheet := TabControl.ActiveTab as TLccOpenPascalTabSheet;
  TabSheet.ScrollingWindowContentsPanel.Width := TabSheet.TabControl.Width - TAB_RIGHT_MARGIN;
  {$ELSE}
  TabSheet := TabControl.ActivePage as TLccOpenPascalTabSheet;
  TabSheet.ScrollingWindowContentsPanel.Width := TabSheet.Width - TAB_RIGHT_MARGIN;
  {$ENDIF}
end;

constructor TLccCdiParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FShowReadBtn := True;
  FShowWriteBtn := True;
  FShowCompareBtn := True;
  FSuppressNameAndDescription := False;
  FPrintMemOffset := False;
  FWorkerMessage := TLccMessage.Create;
  FSerializer := TLccCdiParserSerializer.Create;
  Serializer.OwnerParser := Self;
  CVBlockRead := 1;
end;

destructor TLccCdiParser.Destroy;
begin
 // if Assigned(NodeManager) then
 //   NodeManager.CdiParser := nil;
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FSerializer);
  inherited Destroy;
end;

function TLccCdiParser.Build_CDI_Interface(AnLccNode: TLccNode; ParentControl: TLccPanel; CDI: TLccXmlDocument): TLccPanel;
const
  BUTTON_HEIGHT = 40;
var
  CdiRootElement, CdiRootElementChild: TLccXmlNode;
begin

  // - Application Form
  //   - ..... Controls
  //     - CDI Background Control (Passed to CDI Parser)
  //       - TPanel (background CDI Parser builds the UI on
  //         - TScrollBox - [alClient]
  //           - TPanel [akLeft, akRight, akTop] Bottom is dynamically set to include all child controls
  //             - TLabel (....) [alTop]
  //             - TPanel [alTop] Height set based on the initial heights of the below before setting Alignments
  //               - TButton (Read) [alRight]
  //               - TButton (Write) [alRight]
  //               - TEdit [alClient]
  //         - TPanel (Footer with Global Read/Write buttons) [alBottom]
  //           - TButton (Read All)
  //           - TButton ([Write All)
  //           - TButton (Cancel)


  FLccNode := AnLccNode;
  FApplicationPanel := ParentControl;
  Serializer.OnNotification := {$IFNDEF DELPHI}@{$ENDIF}OnSerializerNotification;
  Clear_CDI_Interface(False);

  // Background that holds everything and is passed back as the child of the ParentControl
  ParserFrame := TLccPanel.Create(ParentControl);
  ParserFrame.Parent := ParentControl;
  ParserFrame.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  ParserFrame.OnResize := {$IFNDEF DELPHI}@{$ENDIF}OnParserFrameResize;
  Result := ParserFrame;

  // Bottom aligned Panel to hold the Read/Write all Buttons
  GlobalButtonBkGnd := TLccPanel.Create(ParserFrame);
  GlobalButtonBkGnd.Align := {$IFDEF DELPHI}TAlignLayout.Bottom{$ELSE}alBottom{$ENDIF};
  {$IFNDEF DELPHI}GlobalButtonBkGnd.BevelOuter := bvNone;{$ENDIF}
  GlobalButtonBkGnd.Parent := ParserFrame;
  GlobalButtonBkGnd.Height := BUTTON_HEIGHT;
  CreateButton(GlobalButtonBkGnd, 'Read All', {$IFNDEF DELPHI}@{$ENDIF}DoButtonReadPageClick, False, (GlobalButtonBkGnd.Width/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  CreateButton(GlobalButtonBkGnd, 'Write All', {$IFNDEF DELPHI}@{$ENDIF}DoButtonWritePageClick, False, (GlobalButtonBkGnd.Width/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  CreateButton(GlobalButtonBkGnd, 'Abort', {$IFNDEF DELPHI}@{$ENDIF}DoButtonStopClick, False, (GlobalButtonBkGnd.Width/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});

  // TabControl that is client aligned with the FooterBkGnd
  TabControl := TLccTabControl.Create(ParentControl);
  TabControl.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  TabControl.Parent := ParserFrame;

  CdiRootElement := XmlFindRootNode(CDI, 'cdi');
  if Assigned(CdiRootElement) then
  begin
    CdiRootElementChild := XmlFirstChild(CdiRootElement);
    while Assigned(CdiRootElementChild) do
    begin
      if CdiRootElementChild.NodeName = 'segment' then
        ProcessSegmentTab(TabControl, CdiRootElementChild, LABEL_MARGIN_INDENT)
      else
      if CdiRootElementChild.NodeName = 'identification' then  // Handle the Identification block
        ProcessIdentificationTab(TabControl, CdiRootElementChild, LABEL_MARGIN_INDENT);

      CdiRootElementChild := CdiRootElementChild.NextSibling;
    end;

    // Allow the controls to be built so Change event are not fired the first time a tab is selected
    {$IFDEF DELPHI}
    TabControl.TabIndex := TabControl.TabCount - 1;
    TabControl.TabIndex := 0;
    {$ELSE}
    TabControl.ActivePageIndex := TabControl.PageCount - 1;
    TabControl.ActivePageIndex := 0;
    {$ENDIF}

  end;

  TabControl.OnChange := {$IFNDEF DELPHI}@{$ENDIF}OnPageControlChange;
  OnPageControlChange(Result);
  DoBuildInterfaceComplete;
end;

procedure TLccCdiParser.Clear_CDI_Interface(ClearLccNode: Boolean);
var
  i: Integer;
begin
  Serializer.Clear;
  DoClearInterface;
  if Assigned(TabControl) then
  begin
    TabControl.Enabled := False;  // Speed it up
    TabControl.Visible := False;
  end;

  if Assigned(ApplicationPanel) then
  begin
    {$IFDEF DELPHI}
    for i := ApplicationPanel.ControlsCount - 1 downto 0 do
    {$ELSE}
    for i := ApplicationPanel.ControlCount - 1 downto 0 do
    {$ENDIF}
      ApplicationPanel.Controls[i].Free;
  end;
  GlobalButtonStop := nil;
  GlobalButtonWritePage := nil;
  GlobalButtonReadPage := nil;
  FApplicationPanel := nil;
  FParserFrame := nil;
  FGlobalButtonBkGnd := nil;
  FTabControl := nil;
  if ClearLccNode then
    FLccNode := nil;
end;

procedure TLccCdiParser.NotifyLccNodeDestroy(LccNode: TObject);
begin
  if LccNode = FLccNode then
    Clear_CDI_Interface(True);
end;


{ TDataElementInformation }

procedure TDataElementInformation.SetMemState(AValue: TConfigMemState);
begin
  if AValue <> FMemState then
  begin
    FMemState:=AValue;
    if Assigned(OnMemChangeState) then
      OnMemChangeState(Self);
  end;
end;

constructor TDataElementInformation.Create(ADataType: TLccConfigDataType);
begin
  inherited Create;
  FMemState := ocs_Unknown;
  FDataType := ADataType;
  MapList := TMapRelationList.Create;
  FConfigData := TDataElementConfig.Create;
end;

destructor TDataElementInformation.Destroy;
begin
   FreeAndNil( FMapList);
   FreeAndNil(FConfigData);
  inherited Destroy;
end;


end.


