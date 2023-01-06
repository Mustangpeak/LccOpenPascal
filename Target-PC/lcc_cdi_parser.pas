 unit lcc_cdi_parser;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ../lcc_compilers.inc}

{$DEFINE PRINT_MEM_LOCATIONS}

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
  lcc_base_classes,
  lcc_xmlutilities;

const
  BASE_EDITOR_HEIGHT = 22;
  BASE_BUTTON_HEIGHT = 22;
  BASE_BUTTON_WIDTH = 80;
  BASE_BUTTON_WIDTH_SINGLE_CHAR = 22;
  TAB_RIGHT_MARGIN = 28;
  COMMON_MARGIN = 4;


  // Default Label Left side Margins
  LABEL_MARGIN_INDENT = 4;
  LABEL_DELTA_INDENT = 8;
  LABEL_DELTA_INDENT_DOUBLE = LABEL_DELTA_INDENT + LABEL_DELTA_INDENT;



type
  TLccOpenPascalSpeedButton = class;

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
    TLccTextBox = TPanel;
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
    TLccTextBox = TLabel;
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
    FCompareMemorySpaceButton: TLccOpenPascalSpeedButton;
    FConfigData: TDataElementConfig;                             // What is the Datatype associated with this Memory Configuration location
    FIntDefault: Int64;                                          // If an Integer Type the default value
    FIntMax: Int64;                                              // If an Integer Type the Max value
    FIntMin: Int64;                                              // If an Integer Type the Min value
    FMemoryAddressPointer: Int64;                                // What is memory address pointer we use to access this memory location within the MemorySpace
    FMemoryAllocation: DWord;                                    // How many bytes of memory does this data type allocate
    FDataType: TLccConfigDataType;                               // What datatype is this associated with (cdt_String, cdt_Int, cdt_EventID, cdt_Bit)
    FMapList: TMapRelationList;                                  // Enumeration of the defined values for this data type (may not be any) and the Key associated with each defined value
    FOnMemChangeState: TNotifyEvent;                             // Called when MemState changes in the UI from the user interacting with it
    FMemState: TConfigMemState;                                  // Tracks the state of the actual memory stored in the Node compared to the UI interface for the Element (unknown, saved, unsaved, etc)
    FReadMemorySpaceButton: TLccOpenPascalSpeedButton;
    FWriteMemorySpaceButton: TLccOpenPascalSpeedButton;
    FEditControl: TObject;
    procedure SetMemState(AValue: TConfigMemState);
    function GetMemoryAddressPointerHi: Int64;
  public
    constructor Create(ADataType: TLccConfigDataType);
    destructor Destroy; override;
    property ConfigData: TDataElementConfig read FConfigData write FConfigData;
    property IntMin: Int64 read FIntMin write FIntMin;
    property IntMax: Int64 read FIntMax write FIntMax;
    property IntDefault: Int64 read FIntDefault write FIntDefault;
    property MemoryAddressPointer: Int64 read FMemoryAddressPointer write FMemoryAddressPointer;
    property MemoryAddressPointerHi: Int64 read GetMemoryAddressPointerHi;
    property MemoryAllocation: DWord read FMemoryAllocation write FMemoryAllocation;
    property MemState: TConfigMemState read FMemState write SetMemState;
    property DataType: TLccConfigDataType read FDataType write FDataType;
    property MapList: TMapRelationList read FMapList write FMapList;
    property OnMemChangeState: TNotifyEvent read FOnMemChangeState write FOnMemChangeState;
    property ReadMemorySpaceButton: TLccOpenPascalSpeedButton read FReadMemorySpaceButton write FReadMemorySpaceButton;
    property WriteMemorySpaceButton: TLccOpenPascalSpeedButton read FWriteMemorySpaceButton write FWriteMemorySpaceButton;
    property CompareMemorySpaceButton: TLccOpenPascalSpeedButton read FCompareMemorySpaceButton write FCompareMemorySpaceButton;
    property EditControl: TObject read FEditControl write FEditControl;
  end;

   { TLccOpenPascalSpinEdit }

  TLccOpenPascalSpinEdit = class(TLccSpinEdit)
  private
    FDataElementInformation: TDataElementInformation;
  protected

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

  { TLccOpenPascalSpeedButton }

  TLccOpenPascalSpeedButton = class(TLccSpeedButton)
  private
    FEditObject: TObject;
  public
    property EditObject: TObject read FEditObject write FEditObject;
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
    FEngineMemorySpaceAccess: TLccEngineMemorySpaceAccess;
    FGlobalButtonReadPage: TLccOpenPascalSpeedButton;
    FGlobalButtonStop: TLccOpenPascalSpeedButton;
    FGlobalButtonWritePage: TLccOpenPascalSpeedButton;
    FCVBlockRead: Word;
    FMarkedToStop: Boolean;
    FMarkedToStopIsStopping: Boolean;
    FLccNode: TLccNode;
    FOnAfterReadPage: TNotifyEvent;
    FOnAfterWritePage: TNotifyEvent;
    FOnBuildInterfaceComplete: TNotifyEvent;
    FOnClearInterface: TNotifyEvent;
    FApplicationPanel: TLccTextBox;
    FGlobalButtonBkGnd: TLccPanel;
    FParserFrame: TLccPanel;
    FPrintMemOffset: Boolean;
    FShowCompareBtn: Boolean;
    FShowReadBtn: Boolean;
    FShowWriteBtn: Boolean;
    FSuppressNameAndDescription: Boolean;
    FTabControl: TLccTabControl;
    FTargetNodeIdentification: TLccNodeIdentificationObject;
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
    function CreateButton(AContainerParent: TLccPanel; ACaption: LccDOMString; OnClickFunc: TNotifyEvent; Enable: Boolean; AWidth: single; AnAlign: {$IFDEF DELPHI}TAlignLayout{$ELSE}TAlign{$ENDIF}): TLccOpenPascalSpeedButton;
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

    function ExtractStringFromElementUI(DataElementInformation: TDataElementInformation; var AString: string): Boolean;
    function ExtractIntFromElementUI(DataElementInformation: TDataElementInformation; var AnInteger: Integer): Boolean;
    function ExtractEventIDFromElementUI(DataElementInformation: TDataElementInformation; var AnEventID: TEventID): Boolean;


    // Read Button presse handlers
    procedure OnButtonCompareClick(Sender: TObject); virtual;
    procedure OnButtonReadClick(Sender: TObject); virtual;
    procedure OnButtonWriteClick(Sender: TObject); virtual;
    // Read Page Button presse handlers
    procedure DoButtonReadPageClick(Sender: TObject); virtual;
    procedure DoButtonWritePageClick(Sender: TObject); virtual;
    procedure DoButtonStopClick(Sender: TObject); virtual;

    procedure OnEngineMemorySpaceAccessCallback(EngineMemorySpaceAccess: TLccEngineMemorySpaceAccess);

    // Control Event Handler
    procedure OnPageControlChange(Sender: TObject);

    // Utility methods
    function ButtonToDataElementInformation(Sender: TObject): TDataElementInformation;
    function ExtractElementItem(ParentElement: TLccXmlNode; Item: string; var ItemStr: LccDOMString): Boolean;
    function FindControlByMemoryAddressPointer(Address: DWord): TLccControl;
    function FindActivePage: TLccTabSheet;
    procedure OnParserFrameResize(Sender: TObject);
    procedure ResizeActiveTabScrollingWindowFrame;


    // Methods to look at below...............

    procedure DoAfterReadPage(Sender: TObject); virtual;
    procedure DoAfterWritePage(Sender: TObject); virtual;


    procedure DoClearInterface; virtual;
    procedure DoBuildInterfaceComplete; virtual;

    procedure OnSpinEditChange(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
    procedure OnComboBoxChange(Sender: TObject);

    // ...............



    property ApplicationPanel: TLccTextBox read FApplicationPanel;     // Application Passed Control to build in
    property ParserFrame: TLccPanel read FParserFrame write FParserFrame;  // alClient aligned to the Application Panel used as our drawing canvas
    property GlobalButtonBkGnd: TLccPanel read FGlobalButtonBkGnd write FGlobalButtonBkGnd; // alBottom aligned to the Parser Frame
    property TabControl: TLccTabControl read FTabControl write FTabControl; // alClient aligned in the Parser Frame, sibling to the GlobalButtonBkgnd
    property EngineMemorySpaceAccess: TLccEngineMemorySpaceAccess read FEngineMemorySpaceAccess write FEngineMemorySpaceAccess;
    property TargetNodeIdentification: TLccNodeIdentificationObject read FTargetNodeIdentification write FTargetNodeIdentification;

    // Located in the GlobalButtonBkGnd frame
    property GlobalButtonReadPage: TLccOpenPascalSpeedButton read FGlobalButtonReadPage write FGlobalButtonReadPage;
    property GlobalButtonWritePage: TLccOpenPascalSpeedButton read FGlobalButtonWritePage write FGlobalButtonWritePage;
    property GlobalButtonStop: TLccOpenPascalSpeedButton read FGlobalButtonStop write FGlobalButtonStop;


    // Properties to look at below...............

    property MarkedToStop: Boolean read FMarkedToStop write FMarkedToStop;
    property MarkedToStopIsStopping: Boolean read FMarkedToStopIsStopping write FMarkedToStopIsStopping;
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function Build_CDI_Interface(AnLccNode: TLccNode; ATargetNode: TLccNodeIdentificationObject; ParentControl: TLccTextBox; CDI: TLccXmlDocument): TLccPanel; overload;
    function Build_CDI_Interface(AnLccNode: TLccNode; ATargetNode: TLccNodeIdentificationObject; ParentControl: TLccTextBox; CDI: String): TLccPanel; overload;
    procedure Clear_CDI_Interface(ClearLccNode: Boolean);
    procedure DoConfigMemReadReply(ANode: TObject); override;
    procedure DoConfigMemWriteReply(ANode: TObject); override;
    procedure NotifyLccNodeDestroy(LccNode: TObject); override;

    property LccNode: TLccNode read FLccNode;
    property AutoReadOnTabChange: Boolean read FAutoReadOnTabChange write FAutoReadOnTabChange;
    property CVBlockRead: Word read GetCVBlockRead write SetCVBlockRead;
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
  AWidth: single; AnAlign: {$IFDEF DELPHI}TAlignLayout{$ELSE}TAlign{$ENDIF}): TLccOpenPascalSpeedButton;
begin
  Result := TLccOpenPascalSpeedButton.Create(AContainerParent);
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
  Result.Enabled := True;
end;

function TLccCdiParser.CreateLabel(ParentControl: TLccPanel; ACaption: LccDOMString; Indent: Single; Bold: Boolean): TLccLabel;
begin
  Result := nil;
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

function TLccCdiParser.ProcessSegmentTab(ATabControl: TLccTabControl;
  SegmentElement: TLccXmlNode; Indent: Integer): TLccPanel;
var
  MemoryAddressPointer: Int64;
  ElementString: LccDOMString;
  AddressSpace: DWord;
  {$IFDEF DELPHI}
  LastLabel: TLccLabel;
  {$ELSE}
  i: Integer;
  {$ENDIF}
begin
  ElementString := '';

  // Only a Segment can have an 'origin' to start the address mapping and it can not have an 'offset'
  if XmlAttributeExists(SegmentElement, 'origin') then
    MemoryAddressPointer := StrToInt64(string( XmlAttributeRead(SegmentElement, 'origin')))
  else
    MemoryAddressPointer := 0;

  // Read in the Address Space.. this is required
  AddressSpace := 0;
  if XmlAttributeExists(SegmentElement, 'space') then
    AddressSpace := StrToUInt(string( XmlAttributeRead(SegmentElement, 'space')));

  // Create the tab and background panel to add to
  ExtractElementItem(SegmentElement, 'name', ElementString);
  Result := CreateTab(ATabControl, ElementString);


  // Time to build the UI for this segment
  Result.Visible := False;
  ProcessElementForUISegment(Result, SegmentElement, MemoryAddressPointer, Indent, AddressSpace);

  // Space on the bottom
  {$IFDEF DELPHI}
  LastLabel := CreateSpacer(Result);
  Result.Height := LastLabel.Position.Y + LastLabel.Height;
  Result.Visible := True;
  {$ELSE}
  // Quirkiness of FPC...
  CreateSpacer(Result);

  Result.Visible := True;
  Result.Visible := False;
  for i := Result.ControlCount - 1 downto 0 do
    Result.Controls[i].Top := 0;
  Result.Visible := True;

  if Result.ControlCount > 0 then
    Result.Height := Result.Controls[Result.ControlCount - 1].Top + Result.Controls[Result.ControlCount - 1].Height;
  {$ENDIF}


end;

function TLccCdiParser.CreateBaseEditorLayout(ParentControl: TLccPanel; DataElementInformation: TDataElementInformation): TLccPanel;
var
  ButtonWidth: Integer;
  ReadStr, WriteStr, CompareStr: LccDOMString;
begin
  Result := TLccPanel.Create(ParentControl);
  {$IFNDEF DELPHI}Result.Caption := '';{$ENDIF};
  Result.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ParentControl.Height;   // Make sure it stacks in the right order;
  Result.Align := {$IFDEF DELPHI}TAlignLayout.Top{$ELSE}alTop{$ENDIF};
  {$IFNDEF DELPHI}Result.BevelOuter := bvNone;{$ENDIF}
  Result.Parent := ParentControl;


  if (3*BASE_BUTTON_WIDTH) >  Trunc(ParentControl.Width / 3) then
  begin
    ButtonWidth := BASE_BUTTON_WIDTH_SINGLE_CHAR;
    ReadStr := 'R';
    WriteStr := 'W';
    CompareStr := 'C';
  end else
  begin
    ButtonWidth := BASE_BUTTON_WIDTH;
    ReadStr := 'Read';
    WriteStr := 'Write';
    CompareStr := 'Compare';
  end;

  DataElementInformation.ReadMemorySpaceButton := CreateButton(Result, ReadStr, {$IFNDEF DELPHI}@{$ENDIF}OnButtonReadClick, False, ButtonWidth, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  DataElementInformation.WriteMemorySpaceButton := CreateButton(Result, WriteStr, {$IFNDEF DELPHI}@{$ENDIF}OnButtonWriteClick, False, ButtonWidth, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  DataElementInformation.CompareMemorySpaceButton := CreateButton(Result, CompareStr, {$IFNDEF DELPHI}@{$ENDIF}OnButtonCompareClick, False, ButtonWidth, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});

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
  ContainerPanel := CreateBaseEditorLayout(ParentControl, DataElementInformation);

  SpinEdit := TLccOpenPascalEdit.Create(ContainerPanel);
  SpinEdit.DataElementInformation := DataElementInformation;
  SpinEdit.Parent := ContainerPanel;
  SpinEdit.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  {$IFDEF DELPHI}SpinEdit.Margins.Left := Indent;{$ELSE}SpinEdit.BorderSpacing.Left := Trunc(Indent);{$ENDIF}
  {$IFDEF DELPHI}SpinEdit.Margins.Right := COMMON_MARGIN;{$ELSE}SpinEdit.BorderSpacing.Right := COMMON_MARGIN;{$ENDIF}

  // Put a link to the editor to the buttons associated with it
  DataElementInformation.ReadMemorySpaceButton.EditObject := SpinEdit;
  DataElementInformation.WriteMemorySpaceButton.EditObject := SpinEdit;
  DataElementInformation.CompareMemorySpaceButton.EditObject := SpinEdit;
  DataElementInformation.EditControl := SpinEdit;

 // SpinEdit.MaxValue := Info.in;


end;

procedure TLccCdiParser.CreateEditLayout(ParentControl: TLccPanel;
  Indent: Integer; DataElementInformation: TDataElementInformation);
var
  EditBox: TLccOpenPascalEdit;
  ContainerPanel: TLccPanel;
begin
  ContainerPanel := CreateBaseEditorLayout(ParentControl, DataElementInformation);

  EditBox := TLccOpenPascalEdit.Create(ContainerPanel);
  EditBox.DataElementInformation := DataElementInformation;
  EditBox.Parent := ContainerPanel;
  EditBox.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  {$IFDEF DELPHI}EditBox.Margins.Left := Indent;{$ELSE}EditBox.BorderSpacing.Left := Trunc(Indent);{$ENDIF}
  {$IFDEF DELPHI}EditBox.Margins.Right := COMMON_MARGIN;{$ELSE}EditBox.BorderSpacing.Right := COMMON_MARGIN;{$ENDIF}

    // Put a link to the editor to the buttons associated with it
  DataElementInformation.ReadMemorySpaceButton.EditObject := EditBox;
  DataElementInformation.WriteMemorySpaceButton.EditObject := EditBox;
  DataElementInformation.CompareMemorySpaceButton.EditObject := EditBox;
  DataElementInformation.EditControl := EditBox;
end;

procedure TLccCdiParser.CreateComboBoxListLayout(ParentControl: TLccPanel;
  Indent: Integer; DataElementInformation: TDataElementInformation);
var
  ContainerPanel: TLccPanel;
  ComboBox: TLccOpenPascalComboBox;
  i: Integer;
begin
  ContainerPanel := CreateBaseEditorLayout(ParentControl, DataElementInformation);

  ComboBox := TLccOpenPascalComboBox.Create(ContainerPanel);
  ComboBox.DataElementInformation := DataElementInformation;
  ComboBox.Parent := ContainerPanel;
  {$IFNDEF DELPHI}ComboBox.Style := csDropDownList;{$ENDIF}
  ComboBox.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  {$IFDEF DELPHI}ComboBox.Margins.Left := Indent;{$ELSE}ComboBox.BorderSpacing.Left := Trunc(Indent);{$ENDIF}
  {$IFDEF DELPHI}ComboBox.Margins.Right := COMMON_MARGIN;{$ELSE}ComboBox.BorderSpacing.Right := COMMON_MARGIN;{$ENDIF}
  for i := 0 to DataElementInformation.MapList.Count - 1 do
    ComboBox.Items.Add(string( TMapRelation(DataElementInformation.MapList.Relations[i]).Value));
  ComboBox.ItemIndex := 0;

  ComboBox.DataElementInformation := DataElementInformation;

  // Put a link to the editor to the buttons associated with it
  DataElementInformation.ReadMemorySpaceButton.EditObject := ComboBox;
  DataElementInformation.WriteMemorySpaceButton.EditObject := ComboBox;
  DataElementInformation.CompareMemorySpaceButton.EditObject := ComboBox;
  DataElementInformation.EditControl := ComboBox;
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

//var
//  TempNode: TLccOwnedNode;

 // i: Integer;

begin
{  Assert(not (ANode is TLccOwnedNode), 'Not a TLccNode');

  TempNode := TLccOwnedNode( ANode);
  if TempNode = LccNode then       // Make sure the passed node is the same one we called on for the config memory
  begin
    if TempNode.ConfigurationMem.Address and $FF000000 = $FF000000 then
    begin
      for i := TempNode.ConfigurationMem.DataCount - 1 downto 0 do             // Do this backward to ensure we don't delete the initial ConfigMem and force a new block to be read
        LoadConfigMemToControl(TempNode, FindControlByMemoryAddressPointer(TempNode.ConfigurationMem.Address + i), i)
    end else
      LoadConfigMemToControl(TempNode, FindControlByMemoryAddressPointer(TempNode.ConfigurationMem.Address), 0)
  end;  }
end;

procedure TLccCdiParser.DoConfigMemWriteReply(ANode: TObject);
//var
 // Control: TLccControl;
 // TempNode: TLccOwnedNode;
 // TempConfigInfo: TDataElementInformation;
begin

  {
  Assert(not (ANode is TLccOwnedNode), 'Not a TLccNode');

  TempNode := TLccOwnedNode( ANode);
  if TempNode = LccNode then       // Make sure the passed node is the same one we called on for the config memory
  begin
    TempConfigInfo := nil;
    Control := FindControlByMemoryAddressPointer(TempNode.ConfigurationMem.Address);
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

procedure TLccCdiParser.OnButtonCompareClick(Sender: TObject);
var
  ConfigInfo: TDataElementInformation;
begin
  if Assigned(FLccNode) then
  begin
    ConfigInfo := ButtonToDataElementInformation(Sender);
    if Assigned(ConfigInfo) then
    begin
      // Reply will return on DoConfigMemReply(...);
    end;
  end;
end;

procedure TLccCdiParser.OnButtonReadClick(Sender: TObject);
var
  Info: TDataElementInformation;
begin
  Info :=  ButtonToDataElementInformation(Sender);
  if Assigned(Info) then
  begin
    EngineMemorySpaceAccess.Reset;
    case Info.DataType of
      cdt_String   : EngineMemorySpaceAccess.Assign(lems_Read, MSI_CONFIG, True, Info.MemoryAddressPointer, Info.MemoryAddressPointerHi, True, TargetNodeIdentification.NodeID, TargetNodeIdentification.Alias, {$IFNDEF DELPHI}@{$ENDIF}OnEngineMemorySpaceAccessCallback);
      cdt_Int      : EngineMemorySpaceAccess.Assign(lems_Read, MSI_CONFIG, False, Info.MemoryAddressPointer, Info.MemoryAddressPointerHi, True, TargetNodeIdentification.NodeID, TargetNodeIdentification.Alias, {$IFNDEF DELPHI}@{$ENDIF}OnEngineMemorySpaceAccessCallback);
      cdt_EventID  : EngineMemorySpaceAccess.Assign(lems_Read, MSI_CONFIG, False, Info.MemoryAddressPointer, Info.MemoryAddressPointerHi, True, TargetNodeIdentification.NodeID, TargetNodeIdentification.Alias, {$IFNDEF DELPHI}@{$ENDIF}OnEngineMemorySpaceAccessCallback);
      cdt_Bit      : begin end;
    end;
    EngineMemorySpaceAccess.TagObject := Info;
    EngineMemorySpaceAccess.Start;
  end;
end;

procedure TLccCdiParser.OnButtonWriteClick(Sender: TObject);
var
  Info: TDataElementInformation;
  AString: String;
  AnInteger: Integer;
  AnEventID: TEventID;
begin
  Info :=  ButtonToDataElementInformation(Sender);
  if Assigned(Info) then
  begin
    EngineMemorySpaceAccess.Reset;
    case Info.DataType of
      cdt_String   : begin
                       AString := '';
                       if ExtractStringFromElementUI(Info, AString) then
                       begin
                         EngineMemorySpaceAccess.Assign(lems_Write, MSI_CONFIG, True, Info.MemoryAddressPointer, Info.MemoryAddressPointerHi, True, TargetNodeIdentification.NodeID, TargetNodeIdentification.Alias, {$IFNDEF DELPHI}@{$ENDIF}OnEngineMemorySpaceAccessCallback);
                         EngineMemorySpaceAccess.StringToStream := AnsiString(AString);
                         EngineMemorySpaceAccess.Start;
                       end;
                     end;

      cdt_Int      : begin
                       AnInteger := 0;
                       if ExtractIntFromElementUI(Info, AnInteger) then
                       begin
                         EngineMemorySpaceAccess.Assign(lems_Write, MSI_CONFIG, False, Info.MemoryAddressPointer, Info.MemoryAddressPointerHi, True, TargetNodeIdentification.NodeID, TargetNodeIdentification.Alias, {$IFNDEF DELPHI}@{$ENDIF}OnEngineMemorySpaceAccessCallback);
                         EngineMemorySpaceAccess.IntToStream[Info.MemoryAllocation] := AnInteger;
                         EngineMemorySpaceAccess.Start;
                       end;
                     end;

      cdt_EventID  : begin
                       AnEventID := NULL_EVENT_ID;
                       if ExtractEventIDFromElementUI(Info, AnEventID) then
                       begin
                         EngineMemorySpaceAccess.Assign(lems_Write, MSI_CONFIG, False, Info.MemoryAddressPointer, Info.MemoryAddressPointerHi, True, TargetNodeIdentification.NodeID, TargetNodeIdentification.Alias, {$IFNDEF DELPHI}@{$ENDIF}OnEngineMemorySpaceAccessCallback);
                         EngineMemorySpaceAccess.EventIDToStream := AnEventID;
                         EngineMemorySpaceAccess.Start;
                       end;
                     end;

      cdt_Bit      : begin end;
    end;
    EngineMemorySpaceAccess.TagObject := Info;
    EngineMemorySpaceAccess.Start;
  end;
end;

procedure TLccCdiParser.DoBuildInterfaceComplete;
begin
  if Assigned(OnBuildInterfaceComplete) then
    OnBuildInterfaceComplete(Self);
end;

procedure TLccCdiParser.DoButtonReadPageClick(Sender: TObject);
 {
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
  TabSheet: TLccTabSheet;   }
begin
{  TabSheet := FindActivePage;
  if Assigned(TabSheet) then
    RunSheet(TabSheet);
  Serializer.SendNext;  }
end;

procedure TLccCdiParser.DoButtonStopClick(Sender: TObject);
begin
  MarkedToStop := True;
end;

procedure TLccCdiParser.OnEngineMemorySpaceAccessCallback(EngineMemorySpaceAccess: TLccEngineMemorySpaceAccess);
var
  Info: TDataElementInformation;
  Edit: TLccEdit;
  SpinEdit: TLccSpinEdit;
  ComboBox: TLccComboBox;
begin
  if EngineMemorySpaceAccess.ReadWrite = lems_Read then
  begin // Read completed
    Info := EngineMemorySpaceAccess.TagObject as TDataElementInformation;
    if Info.EditControl is TLccEdit then
    begin
      Edit := Info.EditControl as TLccEdit;
      Edit.Text := string( EngineMemorySpaceAccess.StreamAsString);
    end else
    if Info.EditControl is TLccSpinEdit then
    begin
      SpinEdit := Info.EditControl as TLccSpinEdit;
      SpinEdit.Value := EngineMemorySpaceAccess.StreamAsInt;
    end else
    if Info.EditControl is TLccComboBox then
    begin
      ComboBox := Info.EditControl as TLccComboBox;
      {$IFDEF DELPHI}
      if ComboBox.ItemIndex > -1 then
      begin

      end
      {$ELSE}
      Combobox.Text := EventIDToString(EngineMemorySpaceAccess.StreamAsEventID, True);
      {$ENDIF}
    end;
  end else
  begin // Write completed
     // do something interesting here.....
     beep;

  end;
end;

procedure TLccCdiParser.DoButtonWritePageClick(Sender: TObject);
  {
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
  TabSheet: TLccTabSheet;   }
begin
 { TabSheet := FindActivePage;
  if Assigned(TabSheet) then
    RunSheet(TabSheet);
  Serializer.SendNext;    }
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

function TLccCdiParser.FindControlByMemoryAddressPointer(Address: DWord): TLccControl;

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
        if (AComponent as TLccOpenPascalSpinEdit).DataElementInformation.MemoryAddressPointer = Address then
          Result := AComponent
      end else
      if AComponent is TLccOpenPascalEdit then
      begin
        if (AComponent as TLccOpenPascalEdit).DataElementInformation.MemoryAddressPointer = Address then
          Result := AComponent
      end else
      if AComponent is TLccOpenPascalComboBox then
      begin
        if (AComponent as TLccOpenPascalComboBox).DataElementInformation.MemoryAddressPointer = Address then
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

procedure TLccCdiParser.ProcessElementForUISegment(ParentControl: TLccPanel;
  Element: TLccXmlNode; var MemoryAddressPointer: Int64; Indent: Integer;
  AddressSpace: Byte);
var
  ChildElement: TLccXmlNode;
begin
 if Assigned(Element) then
 begin
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

function TLccCdiParser.ExtractStringFromElementUI(DataElementInformation: TDataElementInformation; var AString: string): Boolean;
var
  LocalEdit: TLccEdit;
  LocalComboBox: TLccComboBox;
begin
  Result := False;
  AString := '';
  if DataElementInformation.EditControl is TLccEdit then
  begin
    LocalEdit := DataElementInformation.EditControl as TLccEdit;
    AString := LocalEdit.Text;
    Result := True;
  end else
  if DataElementInformation.EditControl is TLccComboBox then
  begin
    LocalComboBox := DataElementInformation.EditControl as TLccComboBox;
    if LocalComboBox.ItemIndex > -1 then
    begin
      AString := LocalComboBox.Items[LocalComboBox.ItemIndex];
      Result := True;
    end;
  end
  // SpinEdit is Integer only
end;

function TLccCdiParser.ExtractIntFromElementUI(DataElementInformation: TDataElementInformation; var AnInteger: Integer): Boolean;
var
  LocalEdit: TLccEdit;
  LocalComboBox: TLccComboBox;
  LocalSpinEdit: TLccSpinEdit;
  LocalText: String;
begin
  Result := False;
  AnInteger := 0;
  if DataElementInformation.EditControl is TLccEdit then
  begin
    LocalEdit := DataElementInformation.EditControl as TLccEdit;
    LocalText := LocalEdit.Text;
    if TryStrToInt(LocalText, AnInteger) then
      Result := True;
  end else
  if DataElementInformation.EditControl is TLccComboBox then
  begin
    LocalComboBox := DataElementInformation.EditControl as TLccComboBox;
    if LocalComboBox.ItemIndex > -1 then
    begin
      LocalText := LocalComboBox.Items[LocalComboBox.ItemIndex];
      if TryStrToInt(LocalText, AnInteger) then
        Result := True;
    end;
  end else
  if DataElementInformation.EditControl is TLccSpinEdit then
  begin
     LocalSpinEdit := DataElementInformation.EditControl as TLccSpinEdit;
     AnInteger := Trunc(LocalSpinEdit.Value);
     Result := True;
  end;

  // See if the Min/Max/Default have been used
  if DataElementInformation.IntDefault < High(Int64) then
  begin  // Not sure what to do with default....
  end;
  if DataElementInformation.IntMin < High(Int64) then
    Result := AnInteger >= DataElementInformation.IntMin;
  if DataElementInformation.IntMax < High(Int64) then
    Result := AnInteger <= DataElementInformation.IntMax;
end;

function TLccCdiParser.ExtractEventIDFromElementUI(DataElementInformation: TDataElementInformation; var AnEventID: TEventID): Boolean;
var
  LocalEdit: TLccEdit;
  LocalComboBox: TLccComboBox;
begin
  Result := False;
  AnEventID := NULL_EVENT_ID;
  if DataElementInformation.EditControl is TLccEdit then
  begin
    LocalEdit := DataElementInformation.EditControl as TLccEdit;
    if ValidateEventIDString(LocalEdit.Text) then
    begin
      AnEventID := StrToEventID(LocalEdit.Text);
      Result := True;
    end;
  end else
  if DataElementInformation.EditControl is TLccComboBox then
  begin
    LocalComboBox := DataElementInformation.EditControl as TLccComboBox;
    if LocalComboBox.ItemIndex > -1 then
    begin
      if ValidateEventIDString(LocalComboBox.Items[LocalComboBox.ItemIndex]) then
      begin
        AnEventID := StrToEventID( LocalComboBox.Items[LocalComboBox.ItemIndex]);
        Result := True;
      end;
    end;
  end
  // SpinEdit is Integer only
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
    // Update the Address Offset if necessary and update the DataElement Information for this Data Element
    if XmlAttributeExists(Element, 'offset') then
      MemoryAddressPointer := MemoryAddressPointer + StrToInt64(string( XmlAttributeRead(Element, 'offset')));

    // Done updating it with 'offset' (if available)...
    DataElementInformation.MemoryAddressPointer := MemoryAddressPointer;

    case DataElementInformation.DataType of
      cdt_String,
      cdt_Int :
        begin
          // Update the DataElement Information on Memory Size (valid for Strings and Integers only, EventIDs will not have this)
          if XmlAttributeExists(Element, 'size') then
            DataElementInformation.MemoryAllocation := StrToInt(string( XmlAttributeRead(Element, 'size')));
        end;
      cdt_EventID :
        begin
          DataElementInformation.MemoryAllocation := 8;
        end;
      cdt_Bit :
        begin
          // Not defined in the released standard yet
        end;
    end;

    {$IFDEF PRINT_MEM_LOCATIONS}
      CreateLabel(ParentControl, 'Address: ' + LccDOMString( IntToStr(DataElementInformation.MemoryAddressPointer)), Indent, False);
      CreateLabel(ParentControl, 'Size: ' + LccDOMString( IntToStr(DataElementInformation.MemoryAllocation)), Indent, False);
    {$ENDIF}

    // Jump past this memeory space
    Inc(MemoryAddressPointer, DataElementInformation.MemoryAllocation);

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


function TLccCdiParser.ButtonToDataElementInformation(Sender: TObject): TDataElementInformation;
var
  LocalSpeedButton: TLccOpenPascalSpeedButton;
begin
  Result := nil;
  if Sender is TLccOpenPascalSpeedButton then
  begin
    LocalSpeedButton := Sender as TLccOpenPascalSpeedButton;
    if LocalSpeedButton.EditObject is TLccOpenPascalSpinEdit then
      Result := (LocalSpeedButton.EditObject as TLccOpenPascalSpinEdit).DataElementInformation;
    if LocalSpeedButton.EditObject is TLccOpenPascalEdit then
      Result := (LocalSpeedButton.EditObject as TLccOpenPascalEdit).DataElementInformation;
    if LocalSpeedButton.EditObject is TLccOpenPascalComboBox then
      Result := (LocalSpeedButton.EditObject as TLccOpenPascalComboBox).DataElementInformation;
  end;
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

  if AutoReadOnTabChange then
    DoButtonReadPageClick(Sender);
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
  FTargetNodeIdentification := TLccNodeIdentificationObject.Create;
  CVBlockRead := 1;
end;

destructor TLccCdiParser.Destroy;
begin
 // if Assigned(NodeManager) then
 //   NodeManager.CdiParser := nil;
  FreeAndNil(FWorkerMessage);
  FreeAndNil(FEngineMemorySpaceAccess);
  FreeAndNil(FTargetNodeIdentification);
  inherited Destroy;
end;

function TLccCdiParser.Build_CDI_Interface(AnLccNode: TLccNode; ATargetNode: TLccNodeIdentificationObject; ParentControl: TLccTextBox; CDI: TLccXmlDocument): TLccPanel;
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


  Clear_CDI_Interface(False);
  FLccNode := AnLccNode;
  TargetNodeIdentification.AssignID(ATargetNode);
  // So we can read write the memory spaces
  FEngineMemorySpaceAccess := TLccEngineMemorySpaceAccess.Create(LccNode);
  LccNode.RegisterEngine(EngineMemorySpaceAccess);

  FApplicationPanel := ParentControl;

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
  GlobalButtonReadPage := CreateButton(GlobalButtonBkGnd, 'Read All', {$IFNDEF DELPHI}@{$ENDIF}DoButtonReadPageClick, False, (GlobalButtonBkGnd.Width/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  GlobalButtonWritePage := CreateButton(GlobalButtonBkGnd, 'Write All', {$IFNDEF DELPHI}@{$ENDIF}DoButtonWritePageClick, False, (GlobalButtonBkGnd.Width/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  GlobalButtonStop := CreateButton(GlobalButtonBkGnd, 'Abort', {$IFNDEF DELPHI}@{$ENDIF}DoButtonStopClick, False, (GlobalButtonBkGnd.Width/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});

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

function TLccCdiParser.Build_CDI_Interface(AnLccNode: TLccNode; ATargetNode: TLccNodeIdentificationObject; ParentControl: TLccTextBox; CDI: String): TLccPanel;
var
  XML: TLccXmlDocument;
begin
  Result := nil;
  if CDI <> '' then
  begin
    XML := XmlLoadFromText(CDI);
    try
      Result := Build_CDI_Interface(AnLccNode, ATargetNode, ParentControl, XML);
    finally
      XmlFreeDocument(XML);
    end;
  end;
end;

procedure TLccCdiParser.Clear_CDI_Interface(ClearLccNode: Boolean);
begin
  if Assigned(LccNode) and Assigned(EngineMemorySpaceAccess) then
  begin
    EngineMemorySpaceAccess.Reset;
    LccNode.UnRegisterEngine(EngineMemorySpaceAccess);
    FreeAndNil(FEngineMemorySpaceAccess);
  end;

  DoClearInterface;
  if Assigned(TabControl) then
  begin
    TabControl.Enabled := False;  // Speed it up
    TabControl.Visible := False;
  end;

  if Assigned(ParserFrame) then
  begin
    ParserFrame.Parent := nil;
    FreeAndNil(FParserFrame);
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
  FIntDefault := High(Int64);
  FIntMin := High(Int64);
  FIntMax := High(Int64);
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


function TDataElementInformation.GetMemoryAddressPointerHi: Int64;
begin
  Result := MemoryAddressPointer + MemoryAllocation;
end;

end.


