 unit lcc_cdi_parser;

{$IFDEF FPC}
{$mode objfpc}{$H+}
{$ENDIF}

interface

{$I ../lcc_compilers.inc}

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
  CV_BUTTON_WIDTH = 80;


type
  {$IFDEF FPC}
    TLccPanel = TPanel;
    TLccTabControl = TPageControl;
    TLccTabSheet = TTabSheet;
    TLccControl = TControl;
    TLccSpeedButton = TSpeedButton;
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
    TLccSpeedButton = TSpeedButton;
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
    FProp: string;
    FValue: string;
  public
    constructor Create( AValue, AProperty: string);
    property Value: string read FValue write FValue;
    property Prop: string read FProp write FProp;
  end;

  // Holds TMapRelations for a Map type Configuration
  { TMap }

  TMap = class(TList)
  private
    FList: TList;
    function GetRelation(Index: Integer): TMapRelation;
    procedure SetRelation(Index: Integer; AValue: TMapRelation);
  protected
    property List: TList read FList write FList;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Add(Relation: TMapRelation);
    procedure AddRelation( AValue, AProperty: string);
    procedure ClearList;
    function FindMapByValue(AValue: string): TMapRelation;
    function FindMapByProperty(AProperty: string): TMapRelation;
    property Relations[Index: Integer]: TMapRelation read GetRelation write SetRelation;
  end;

  { TConfigData }

  TConfigData = class
  private
    FDataBit: Byte;
    FDataDirection: TConfigDataDirection;
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
  { TConfigInfo }

  TConfigInfo = class
  private
    FConfigData: TConfigData;
    FMemAddress: DWord;                                                         // Address of the Configurtion memory space (as extracted from the XML file)
    FMemSize: DWord;                                                            // Size of the Configuration Memory space (as extracted from the XML file)
    FDataType: TLccConfigDataType;                                             // Defines the Configuration Memory space type (cdt_String, cdt_Int, cdt_EventID, cdt_Bit)
    FMapList: TMap;                                                             // List of the possible values/user names if the Configuration Memory space is a Map.  This should be used as index of the comboxbox may not be right in all cases
    FOnMemChangeState: TNotifyEvent;                                            // Called when MemState changes
    FMemState: TConfigMemState;                                                 // Tracks the state of the Configruation Memory with the UI for the Element (unknown, saved, unsaved, etc)
    procedure SetMemState(AValue: TConfigMemState);
  public
    constructor Create(MemOffset, MemSize: DWord; ADataType: TLccConfigDataType);
    destructor Destroy; override;
    property ConfigData: TConfigData read FConfigData write FConfigData;
    property MemAddress: DWord read FMemAddress write FMemAddress;
    property MemSize: DWord read FMemSize write FMemSize;
    property MemState: TConfigMemState read FMemState write SetMemState;
    property DataType: TLccConfigDataType read FDataType write FDataType;
    property MapList: TMap read FMapList write FMapList;
    property OnMemChangeState: TNotifyEvent read FOnMemChangeState write FOnMemChangeState;
  end;

  // UI control to edit Integer or bit type Configuration meory
  { TLccOpenPascalSpinEdit }

  TLccOpenPascalSpinEdit = class(TLccSpinEdit)
  private
    FCompareCVSpeedButton: TLccSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageIndexRead: Integer;
    FImageIndexStateCurrent: Integer;
    FImageIndexStateUnknown: Integer;
    FImageIndexWrite: Integer;
    FImageList16x16: TLccImageList;
    FReadCVSpeedButton: TLccSpeedButton;
    FStateImage: TLccImage;
    FWriteCVSpeedButton: TLccSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TLccSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TLccSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TLccSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TLccImageList read FImageList16x16 write FImageList16x16;
    property ImageIndexStateCurrent: Integer read FImageIndexStateCurrent write FImageIndexStateCurrent;
    property ImageIndexStateUnknown: Integer read FImageIndexStateUnknown write FImageIndexStateUnknown;
    property ImageIndexRead: Integer read FImageIndexRead write FImageIndexRead;
    property ImageIndexWrite: Integer read FImageIndexWrite write FImageIndexWrite;
    property StateImage: TLccImage read FStateImage write FStateImage;
  end;

  // UI control to edit String type Configuration meory
  { TLccOpenPascalEdit }

  TLccOpenPascalEdit = class(TLccEdit)
  private
    FCompareCVSpeedButton: TLccSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageIndexRead: Integer;
    FImageIndexStateCurrent: Integer;
    FImageIndexStateUnknown: Integer;
    FImageIndexWrite: Integer;
    FImageList16x16: TLccImageList;
    FReadCVSpeedButton: TLccSpeedButton;
    FStateImage: TLccImage;
    FWriteCVSpeedButton: TLccSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TLccSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TLccSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TLccSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TLccImageList read FImageList16x16 write FImageList16x16;
    property ImageIndexStateCurrent: Integer read FImageIndexStateCurrent write FImageIndexStateCurrent;
    property ImageIndexStateUnknown: Integer read FImageIndexStateUnknown write FImageIndexStateUnknown;
    property ImageIndexRead: Integer read FImageIndexRead write FImageIndexRead;
    property ImageIndexWrite: Integer read FImageIndexWrite write FImageIndexWrite;
    property StateImage: TLccImage read FStateImage write FStateImage;
  end;

  // UI control to edit Max type Configuration meory
  { TLccOpenPascalComboBox }

  TLccOpenPascalComboBox = class(TLccComboBox)
  private
    FCompareCVSpeedButton: TLccSpeedButton;
    FConfigInfo: TConfigInfo;
    FImageIndexRead: Integer;
    FImageIndexStateCurrent: Integer;
    FImageIndexStateUnknown: Integer;
    FImageIndexWrite: Integer;
    FImageList16x16: TLccImageList;
    FReadCVSpeedButton: TLccSpeedButton;
    FStateImage: TLccImage;
    FWriteCVSpeedButton: TLccSpeedButton;
  protected
    procedure OnDrawImageState(Sender: TObject);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ReadCVSpeedButton: TLccSpeedButton read FReadCVSpeedButton write FReadCVSpeedButton;
    property WriteCVSpeedButton: TLccSpeedButton read FWriteCVSpeedButton write FWriteCVSpeedButton;
    property CompareCVSpeedButton: TLccSpeedButton read FCompareCVSpeedButton write FCompareCVSpeedButton;
    property ImageList16x16: TLccImageList read FImageList16x16 write FImageList16x16;
    property ImageIndexStateCurrent: Integer read FImageIndexStateCurrent write FImageIndexStateCurrent;
    property ImageIndexStateUnknown: Integer read FImageIndexStateUnknown write FImageIndexStateUnknown;
    property ImageIndexRead: Integer read FImageIndexRead write FImageIndexRead;
    property ImageIndexWrite: Integer read FImageIndexWrite write FImageIndexWrite;
    property StateImage: TLccImage read FStateImage write FStateImage;
  end;

  { TLccCdiParserSerializer }

  TLccCdiParserSerializer = class
  private
    FConfigInfo: TConfigInfo;
    FConfigInfoList: TList;
    FOnNotification: TParserNotificationEvent;
    FOwnerParser: TLccCdiParser;
    FWorkerMessage: TLccMessage;
  protected
    procedure DoNotification(Notify: TParserSerializer); virtual;
    function PackCVReads(CV_Start: DWord): Byte;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddRead(AConfigInfo: TConfigInfo);    // THIS SHOULD BE ONLY ONE ADD SO THIS STAYS SERIALIZED
    procedure AddWrite(AConfigInfo: TConfigInfo);
    procedure Clear;
    procedure SendNext;
    procedure Remove(AConfigInfo: TConfigInfo);

    property ConfigInfo: TConfigInfo read FConfigInfo write FConfigInfo;
    property ConfigInfoList: TList read FConfigInfoList write FConfigInfoList;
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
    FButtonReadPage: TButton;
    FButtonStop: TButton;
    FButtonWritePage: TButton;
    FCVBlockRead: Word;
    FImageIndexRead: Integer;
    FImageIndexStateCurrent: Integer;
    FImageIndexStateUnknown: Integer;
    FImageIndexWrite: Integer;
    FImageList16x16: TLccImageList;
    FMarkedToStop: Boolean;
    FMarkedToStopIsStopping: Boolean;
    FNodeManager: TLccNodeManager;
    FLccNode: TLccNode;
    FOnAfterReadPage: TNotifyEvent;
    FOnAfterWritePage: TNotifyEvent;
    FOnBuildInterfaceComplete: TNotifyEvent;
    FOnClearInterface: TNotifyEvent;
    FPallet: TLccPanel;
    FPalletButtons: TLccPanel;
    FPrintMemOffset: Boolean;
    FSerializer: TLccCdiParserSerializer;
    FShowReadBtn: Boolean;
    FShowWriteBtn: Boolean;
    FSuppressNameAndDescription: Boolean;
    FWorkerMessage: TLccMessage;
    function GetCVBlockRead: Word;
    procedure SetCVBlockRead(AValue: Word);
  protected
    function AddTabWithScrollBoxClientArea(PageControl: TLccTabControl; ACaption: string): TScrollBox;
    procedure AddLabel(ParentControl: TScrollBox; ACaption: string; var ControlOffset: Integer; ControlMargin, Indent: Integer; Bold: Boolean);
    procedure AddSpinEdit(ParentControl: TScrollBox; Element: TLccXmlNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean; ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
    procedure AddEdit(ParentControl: TScrollBox; Element: TLccXmlNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean; ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
    procedure AddComboBoxList(ParentControl: TScrollBox; Element: TLccXmlNode; var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean; ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
    procedure AddPalletButtons(ParentControl: TLccPanel);
    procedure AddSpeedButtonGlyph(SpeedButton: TLccSpeedButton; ImageListIndex: Integer);
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
    function ExtractElementItem(Element: TLccXmlNode; Item: string; var ItemStr: string): Boolean;
    function ExtractElementAttribute(Element: TLccXmlNode; AttributeName: string; var AttributeStr: string): Boolean;
    function FindControlByAddress(Address: DWord): TLccControl;
    function FindActivePage: TLccTabSheet;
    function IsMemorySpace(Segment: TLccXmlNode; MemorySpace: Byte): Boolean;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OnSpinEditChange(Sender: TObject);
    procedure OnEditChange(Sender: TObject);
    procedure OnComboBoxChange(Sender: TObject);
    procedure OnPageControlChange(Sender: TObject);
    procedure OnSerializerNotification(Sender: TObject; Notify: TParserSerializer);
    procedure OnBkGndResize(Sender: TObject);
    procedure ProcessElementForUI(ParentControl: TScrollBox; Element: TLccXmlNode; var MemOffset: DWord; var ControlOffset: Integer; Indent: Integer; DoSuppressNameAndDescription: Boolean; DoPrintMemOffset: Boolean; ShowReadBtn, ShowWriteBtn: Boolean);
    function SpeedButtonToConfigInfo(Sender: TObject): TConfigInfo;
    procedure UpdateMemOffsetJump(Element: TLccXmlNode; var MemOffset: DWord);
    function UpdateMemOffsetSize(Element: TLccXmlNode): DWord;

    property Pallet: TLccPanel read FPallet;
    property PalletButtons: TLccPanel read FPalletButtons write FPalletButtons;
    property ButtonReadPage: TButton read FButtonReadPage write FButtonReadPage;
    property ButtonWritePage: TButton read FButtonWritePage write FButtonWritePage;
    property ButtonStop: TButton read FButtonStop write FButtonStop;
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
    property ImageList16x16: TLccImageList read FImageList16x16 write FImageList16x16;
    property ImageIndexStateCurrent: Integer read FImageIndexStateCurrent write FImageIndexStateCurrent;
    property ImageIndexStateUnknown: Integer read FImageIndexStateUnknown write FImageIndexStateUnknown;
    property ImageIndexRead: Integer read FImageIndexRead write FImageIndexRead;
    property ImageIndexWrite: Integer read FImageIndexWrite write FImageIndexWrite;
    property NodeManager: TLccNodeManager read FNodeManager write FNodeManager;
    property PrintMemOffset: Boolean read FPrintMemOffset write FPrintMemOffset;
    property ShowReadBtn: Boolean read FShowReadBtn write FShowReadBtn;
    property ShowWriteBtn: Boolean read FShowWriteBtn write FShowWriteBtn;
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
  Next: TConfigInfo;
  CV_Next: DWord;
  i: Integer;
  Found: Boolean;
begin
  Result := 1;
  CV_Next := CV_Start + 1;
  repeat
    Found := False;
    i := 0; // Index 0 is what is being used at the base address
    while (i < ConfigInfoList.Count) and not Found do
    begin
      Next := TConfigInfo( ConfigInfoList[i]);
      if Next.MemAddress = CV_Next then
      begin
        Inc(Result);
        Inc(CV_Next);
        Found := True;
      end else
        Inc(i);
    end;

    if Found then
      ConfigInfoList.Delete(i);            // We don't own these, they belong to the Controls

  until not Found or (Result = OwnerParser.CVBlockRead);
end;

constructor TLccCdiParserSerializer.Create;
begin
  inherited Create;
  FConfigInfo := nil;
  FConfigInfoList := TList.Create;
  FWorkerMessage := TLccMessage.Create;
end;

destructor TLccCdiParserSerializer.Destroy;
begin
  FreeAndNil(FConfigInfoList);
  FreeAndNil(FWorkerMessage);
  inherited Destroy;
end;

procedure TLccCdiParserSerializer.AddRead(AConfigInfo: TConfigInfo);
begin
  if Assigned(AConfigInfo) then
  begin
    AConfigInfo.ConfigData.DataDirection := cdd_Read;
    ConfigInfoList.Add(AConfigInfo);
    DoNotification(ps_InsertRead);
  end;
end;

procedure TLccCdiParserSerializer.AddWrite(AConfigInfo: TConfigInfo);
begin
  if Assigned(AConfigInfo) then
  begin
    AConfigInfo.ConfigData.DataDirection := cdd_Write;
    ConfigInfoList.Add(AConfigInfo);
    DoNotification(ps_InsertWrite);
  end;
end;

procedure TLccCdiParserSerializer.Clear;
var
  i: Integer;
begin
  for i := 0 to ConfigInfoList.Count - 1 do
  begin
    if TConfigInfo(ConfigInfoList[i]).ConfigData.DataDirection = cdd_Read then
      DoNotification(ps_RemoveRead)
    else
      DoNotification(ps_RemoveWrite);
  end;
  ConfigInfoList.Clear;
  ConfigInfo := nil;
end;

procedure TLccCdiParserSerializer.SendNext;
var
  SequencialCVReads: Byte;
begin

  {
  if (ConfigInfoList.Count > 0) and not Assigned(ConfigInfo) and Assigned(OwnerParser.NodeManager.RootNode) then
  begin
    ConfigInfo := TConfigInfo( ConfigInfoList[0]);
    ConfigInfoList.Delete(0);
    if Assigned(OwnerParser) then
    begin
      if ConfigInfo.ConfigData.DataDirection = cdd_Read then
      begin    // Reply will return on OwnerParser.DoConfigMemReadReply(...);
        if (ConfigInfo.MemAddress and $FF000000 = $FF000000) and (OwnerParser.CVBlockRead > 1) then
        begin
           SequencialCVReads := PackCVReads(ConfigInfo.MemAddress);
           OwnerParser.LccNode.ConfigurationMem.Initialize(ConfigInfo.MemAddress, MSI_CONFIG, SequencialCVReads, ConfigInfo.DataType);
           WorkerMessage.LoadConfigMemRead(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, ConfigInfo.MemAddress, SequencialCVReads);
           TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
        end else
        begin
          OwnerParser.LccNode.ConfigurationMem.Initialize(ConfigInfo.MemAddress, MSI_CONFIG, ConfigInfo.MemSize, ConfigInfo.DataType);
          WorkerMessage.LoadConfigMemRead(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, ConfigInfo.MemAddress, ConfigInfo.MemSize);
          TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
        end;
      end else
      if ConfigInfo.ConfigData.DataDirection = cdd_Write then
      begin    // Reply MIGHT return on OwnerParser.DoConfigMemWriteReply(...);  Nodes are NOT required to send this
        case ConfigInfo.DataType of
          cdt_Int :
            begin
              OwnerParser.LccNode.ConfigurationMem.Initialize(ConfigInfo.MemAddress, MSI_CONFIG, ConfigInfo.MemSize, ConfigInfo.DataType);
              WorkerMessage.LoadConfigMemWriteInteger(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, ConfigInfo.MemAddress, ConfigInfo.MemSize, ConfigInfo.ConfigData.DataInteger);
              TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
            end;
          cdt_String :
            begin
              OwnerParser.LccNode.ConfigurationMem.Initialize(ConfigInfo.MemAddress, MSI_CONFIG, ConfigInfo.MemSize, ConfigInfo.DataType);
              WorkerMessage.LoadConfigMemWriteString(OwnerParser.NodeManager.RootNode.NodeID, OwnerParser.NodeManager.RootNode.AliasID, OwnerParser.LccNode.NodeID, OwnerParser.LccNode.AliasID, MSI_CONFIG, ConfigInfo.MemAddress, ConfigInfo.ConfigData.DataString);
              TLccNodeManagerHack( OwnerParser.NodeManager).DoRequestMessageSend(WorkerMessage);
            end;
        end;
      end;
    end;
  end;       }
end;

procedure TLccCdiParserSerializer.Remove(AConfigInfo: TConfigInfo);
begin
  if AConfigInfo = ConfigInfo then
  begin
    ConfigInfo := nil;
    if AConfigInfo.ConfigData.DataDirection = cdd_Read then
      DoNotification(ps_RemoveRead)
    else
      DoNotification(ps_RemoveWrite);
    SendNext;
  end;
end;


{ TMapRelation }

constructor TMapRelation.Create(AValue, AProperty: string);
begin
  inherited Create;
  Value := AValue;
  Prop := AProperty
end;

{ TMap }

function TMap.GetRelation(Index: Integer): TMapRelation;
begin
  Result := TMapRelation( List[Index])
end;


procedure TMap.SetRelation(Index: Integer; AValue: TMapRelation);
begin
  List[Index] := AValue
end;

constructor TMap.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TMap.Destroy;
begin
  ClearList;
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TMap.Add(Relation: TMapRelation);
begin
  List.Add(Relation);
end;

procedure TMap.AddRelation(AValue, AProperty: string);
begin
  List.Add( TMapRelation.Create( AValue, AProperty));
end;

procedure TMap.ClearList;
var
  i: Integer;
begin
  try
    for i := 0 to List.Count - 1 do
      TObject( List[i]).Free;
  finally
    Clear;
  end;
end;

function TMap.FindMapByValue(AValue: string): TMapRelation;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  AValue := LowerCase( AValue);
  while not Assigned(Result) and (i < List.Count) do
  begin
    if LowerCase( Relations[i].Value) = AValue then
      Result := Relations[i];
    Inc(i)
  end;
end;

function TMap.FindMapByProperty(AProperty: string): TMapRelation;
var
  i: Integer;
begin
  Result := nil;
  i := 0;
  AProperty := LowerCase( AProperty);
  while not Assigned(Result) and (i < List.Count) do
  begin
    if LowerCase( Relations[i].Prop) = AProperty then
      Result := Relations[i];
    Inc(i)
  end;
end;

{ TLccOpenPascalComboBox }

procedure TLccOpenPascalComboBox.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TLccBitmap;
begin

{
  case ConfigInfo.MemState of
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
  end;
  }
end;

constructor TLccOpenPascalComboBox.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
  ReadCVSpeedButton := TLccSpeedButton.Create(Self);
  WriteCVSpeedButton := TLccSpeedButton.Create(Self);
  CompareCVSpeedButton := TLccSpeedButton.Create(Self);
  StateImage := TLccImage.Create(Self);
  FImageList16x16 := nil;
end;

destructor TLccOpenPascalComboBox.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TLccOpenPascalEdit }

procedure TLccOpenPascalEdit.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TLccBitmap;
begin
{  case ConfigInfo.MemState of
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
  end;}
end;

constructor TLccOpenPascalEdit.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  FConfigInfo := nil;
  ReadCVSpeedButton := TLccSpeedButton.Create(Self);
  WriteCVSpeedButton := TLccSpeedButton.Create(Self);
  CompareCVSpeedButton := TLccSpeedButton.Create(Self);
  StateImage := TLccImage.Create(Self);
  ImageList16x16 := nil;
end;

destructor TLccOpenPascalEdit.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TLccOpenPascalSpinEdit }

procedure TLccOpenPascalSpinEdit.OnDrawImageState(Sender: TObject);
var
  ImageIndex: Integer;
  Bitmap: TLccBitmap;
begin
 { case ConfigInfo.MemState of
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
  FConfigInfo := nil;
  ReadCVSpeedButton := TLccSpeedButton.Create(Self);
  WriteCVSpeedButton := TLccSpeedButton.Create(Self);
  CompareCVSpeedButton := TLccSpeedButton.Create(Self);
  StateImage := TLccImage.Create(Self);
  ImageList16x16 := nil;
end;

destructor TLccOpenPascalSpinEdit.Destroy;
begin
  FreeAndNil( FConfigInfo);
  inherited Destroy;
end;

{ TLccCdiParser }

procedure TLccCdiParser.AddSpeedButtonGlyph(SpeedButton: TLccSpeedButton; ImageListIndex: Integer);
var
  Bitmap: TLccBitmap;
begin
{  Bitmap := TLccBitmap.Create;
  Bitmap.Width := ImageList16x16.Width;
  Bitmap.Height := ImageList16x16.Height;
  ImageList16x16.GetBitmap(ImageListIndex, Bitmap);
  SpeedButton.Glyph.Assign(Bitmap);
  Bitmap.Free;      }
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
      TempConfigInfo: TConfigInfo;
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
            TempConfigInfo := TLccOpenPascalSpinEdit(Control).ConfigInfo;
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
            TempConfigInfo := TLccOpenPascalEdit(Control).ConfigInfo;
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
            TempConfigInfo := TLccOpenPascalComboBox(Control).ConfigInfo;
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
  TempConfigInfo: TConfigInfo;
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
          TLccOpenPascalSpinEdit(Control).ConfigInfo.MemState := ocs_Current;
          TempConfigInfo := TLccOpenPascalSpinEdit(Control).ConfigInfo;
        end;
      end else
      if Control is TLccOpenPascalEdit then
      begin
        if TempNode.ConfigurationMem.DataType = cdt_String then
        begin
          TLccOpenPascalEdit(Control).ConfigInfo.MemState := ocs_Current;
          TempConfigInfo := TLccOpenPascalEdit(Control).ConfigInfo;
        end;
      end else
      if Control is TLccOpenPascalComboBox then
      begin
        if TempNode.ConfigurationMem.DataType = cdt_Int then
        begin
          TLccOpenPascalComboBox(Control).ConfigInfo.MemState := ocs_Current;
          TempConfigInfo := TLccOpenPascalComboBox(Control).ConfigInfo;
        end;
      end;
    end;
    Serializer.Remove(TempConfigInfo);
  end;   }
end;

procedure TLccCdiParser.DoSpeedButtonCompareClick(Sender: TObject);
var
  ConfigInfo: TConfigInfo;
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
      Serializer.AddRead(TLccOpenPascalSpinEdit( Component).ConfigInfo)
    else
    if Component is TLccOpenPascalEdit then
      Serializer.AddRead(TLccOpenPascalEdit( Component).ConfigInfo)
    else
    if Component is TLccOpenPascalComboBox then
      Serializer.AddRead(TLccOpenPascalComboBox( Component).ConfigInfo)
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
      Serializer.AddWrite(TLccOpenPascalSpinEdit( Component).ConfigInfo)
    else
    if Component is TLccOpenPascalEdit then
      Serializer.AddWrite(TLccOpenPascalEdit( Component).ConfigInfo)
    else
    if Component is TLccOpenPascalComboBox then
      Serializer.AddWrite(TLccOpenPascalComboBox( Component).ConfigInfo)
  end;

var
  TabSheet: TLccTabSheet;
begin
  TabSheet := FindActivePage;
  if Assigned(TabSheet) then
    RunSheet(TabSheet);
  Serializer.SendNext;
end;

function TLccCdiParser.ExtractElementItem(Element: TLccXmlNode; Item: string; var ItemStr: string): Boolean;
var
  Node: TLccXmlNode;
begin
  Result := False;
  ItemStr := '';
  Node := XmlFindChildNode(Element, LccDOMString(Item));
  if Assigned(Node) then
  begin
    Result := True;
    ItemStr := String(XmlNodeTextContent(Node));
  end;
end;

function TLccCdiParser.ExtractElementAttribute(Element: TLccXmlNode; AttributeName: string; var AttributeStr: string): Boolean;
begin
  AttributeStr := '';
  Result := XmlAttributeExists(Element, LccDOMString(AttributeName));
  if Result then
   AttributeStr := String( XmlAttributeRead(Element, LccDOMString(AttributeName)));
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
        if TLccOpenPascalSpinEdit( AComponent).ConfigInfo.MemAddress = Address then
          Result := AComponent
      end else
      if AComponent is TLccOpenPascalEdit then
      begin
        if TLccOpenPascalEdit( AComponent).ConfigInfo.MemAddress = Address then
          Result := AComponent
      end else
      if AComponent is TLccOpenPascalComboBox then
      begin
        if TLccOpenPascalComboBox( AComponent).ConfigInfo.MemAddress = Address then
          Result := AComponent
      end
    end
  end;

begin
  if Assigned(Pallet) then
    Result := SearchComponents(Pallet) as TLccControl;
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
  if Assigned(Pallet) then
  begin
    for i := 0 to Pallet.ComponentCount - 1 do
    begin
      if Pallet.Components[i] is TLccTabControl then
      begin
        PageControl := TLccTabControl( Pallet.Components[i]);
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

procedure TLccCdiParser.UpdateMemOffsetJump(Element: TLccXmlNode; var MemOffset: DWord);
var
  i: Integer;
begin
  if XmlAttributeExists(Element, 'origin') then
    MemOffset := StrToInt64( String(XmlAttributeRead(Element, 'origin')))
  else
  if XmlAttributeExists(Element, 'offset') then
    MemOffset := StrToInt64( String(XmlAttributeRead(Element, 'offset')));
end;

function TLccCdiParser.UpdateMemOffsetSize(Element: TLccXmlNode): DWord;
var
  OffsetModified: Boolean;
  TempStr: string;
begin
  OffsetModified := False;
  TempStr := '';

  if ExtractElementAttribute(Element, 'size', TempStr) then
  begin
    Result := StrToInt64( TempStr);
    if Element.NodeName = 'bit' then
    begin
      Result := (Result div 8);
      if Result mod 8 <> 0 then
        Inc(Result);
    end;
    OffsetModified := True;
  end;

  if not OffsetModified then
  begin
    if LowerCase( Element.NodeName) = 'int' then
      Result := 1
    else
    if LowerCase( Element.NodeName) = 'eventid' then
      Result := 8
    else
  end;
end;

function TLccCdiParser.AddTabWithScrollBoxClientArea(PageControl: TLccTabControl; ACaption: string): TScrollBox;
var
  Tab: TLccTabSheet;
begin
  {$IFDEF DELPHI}
  Tab := TLccTabSheet.Create(PageControl);
  Tab.Parent := PageControl;
  Tab.Text := ACaption;
  Result := TScrollBox.Create(Tab);
  Result.Align := TAlignLayout.Client;
  Result.Padding.Left := 8;
  Result.Padding.Right := 8;
  Result.Padding.Top := 8;
  Result.Padding.Bottom := 8;
  Result.Parent := Tab;
  {$ELSE}
  Tab := PageControl.AddTabSheet;
  Tab.Caption := ACaption;
  Result := TScrollBox.Create(Tab);
  Result.Align := alClient;
  Result.BorderSpacing.Around := 8;
  Result.VertScrollBar.Tracking := True;
  Result.HorzScrollBar.Tracking := True;
  Result.Parent := Tab;
  {$ENDIF}
end;

procedure TLccCdiParser.AddLabel(ParentControl: TScrollBox; ACaption: string;
  var ControlOffset: Integer; ControlMargin, Indent: Integer; Bold: Boolean);
var
  ALabel: TLccLabel;
begin
  {$IFDEF DELPHI}
  ALabel := TLccLabel.Create(ParentControl);
  ALabel.Text := ACaption;
  ALabel.Position.Y := ControlOffset;
  ALabel.Position.X := Indent;
  if Bold then
    ALabel.Font.Style := [TFontStyle.fsBold];
  ALabel.Parent := ParentControl;
  ControlOffset := Round(ControlOffset + ALabel.Height + ControlMargin);
  {$ELSE}
  ALabel := TLccLabel.Create(ParentControl);
  ALabel.Caption := ACaption;
  ALabel.Top := ControlOffset;
  ALabel.Left := Indent;
  if Bold then
    ALabel.Font.Style := [fsBold];
  ALabel.WordWrap := True;
  ALabel.Parent := ParentControl;
  ControlOffset := ControlOffset + ALabel.Height + ControlMargin;
  {$ENDIF}
end;

procedure TLccCdiParser.AddPalletButtons(ParentControl: TLccPanel);
const
  BUTTON_MARGIN = 4;
  BUTTON_WIDTH = 120;
  BUTTON_HEIGHT = 22;
  STATUS_HEIGHT = 20;
var
  X, Y: Integer;
begin
  {$IFDEF DELPHI}
  PalletButtons := TLccPanel.Create(ParentControl);
  PalletButtons.Align := TAlignLayout.Bottom;
  PalletButtons.Height := STATUS_HEIGHT + BUTTON_HEIGHT + BUTTON_HEIGHT + BUTTON_MARGIN * 4;
  PalletButtons.Parent := ParentControl;

  ButtonWritePage := TButton.Create(PalletButtons);
  ButtonWritePage.Width := BUTTON_WIDTH;
  ButtonWritePage.Height := BUTTON_HEIGHT;
  ButtonWritePage.Text := 'Write Page';
  ButtonWritePage.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop];
  X := Round(PalletButtons.Width - BUTTON_MARGIN - BUTTON_WIDTH);
  ButtonWritePage.Position.X := X;
  Y := BUTTON_MARGIN;
  ButtonWritePage.Position.Y := Y;
  ButtonWritePage.OnClick := DoButtonWritePageClick;
  ButtonWritePage.Parent := PalletButtons;

  ButtonReadPage := TButton.Create(PalletButtons);
  ButtonReadPage.Width := BUTTON_WIDTH;
  ButtonReadPage.Height := BUTTON_HEIGHT;
  ButtonReadPage.Text := 'Read Page';
  ButtonReadPage.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop];
  ButtonReadPage.Position.X := X - BUTTON_MARGIN - BUTTON_WIDTH;
  ButtonReadPage.Position.Y := Y;
  ButtonReadPage.OnClick := DoButtonReadPageClick;
  ButtonReadPage.Parent := PalletButtons;

  ButtonStop := TButton.Create(PalletButtons);
  ButtonStop.Width := BUTTON_WIDTH;
  ButtonStop.Height := BUTTON_HEIGHT;
  ButtonStop.Text := 'Stop Action';
  ButtonStop.Anchors := [TAnchorKind.akRight, TAnchorKind.akTop];
  X := Round(PalletButtons.Width - BUTTON_MARGIN - BUTTON_WIDTH);
  ButtonStop.Position.X := X;
  Y := BUTTON_MARGIN + BUTTON_MARGIN + BUTTON_HEIGHT;
  ButtonStop.Position.Y := Y;
  ButtonStop.OnClick := DoButtonStopClick;
  ButtonStop.Enabled := False;
  ButtonStop.Parent := PalletButtons;
  {$ELSE}
  PalletButtons := TLccPanel.Create(ParentControl);
  PalletButtons.Caption := '';
  PalletButtons.Align := alBottom;
  PalletButtons.Height := STATUS_HEIGHT + BUTTON_HEIGHT + BUTTON_HEIGHT + BUTTON_MARGIN * 4;
  PalletButtons.BevelOuter := bvNone;
  PalletButtons.Parent := ParentControl;

  ButtonWritePage := TButton.Create(PalletButtons);
  ButtonWritePage.Width := BUTTON_WIDTH;
  ButtonWritePage.Height := BUTTON_HEIGHT;
  ButtonWritePage.Caption := 'Write Page';
  ButtonWritePage.Anchors := [akRight, akTop];
  X := PalletButtons.Width - BUTTON_MARGIN - BUTTON_WIDTH;
  ButtonWritePage.Left := X;
  Y := BUTTON_MARGIN;
  ButtonWritePage.Top := Y;
  ButtonWritePage.OnClick := @DoButtonWritePageClick;
  ButtonWritePage.Parent := PalletButtons;

  ButtonReadPage := TButton.Create(PalletButtons);
  ButtonReadPage.Width := BUTTON_WIDTH;
  ButtonReadPage.Height := BUTTON_HEIGHT;
  ButtonReadPage.Caption := 'Read Page';
  ButtonReadPage.Anchors := [akRight, akTop];
  ButtonReadPage.Left := X - BUTTON_MARGIN - BUTTON_WIDTH;
  ButtonReadPage.Top := Y;
  ButtonReadPage.OnClick := @DoButtonReadPageClick;
  ButtonReadPage.Parent := PalletButtons;

  ButtonStop := TButton.Create(PalletButtons);
  ButtonStop.Width := BUTTON_WIDTH;
  ButtonStop.Height := BUTTON_HEIGHT;
  ButtonStop.Caption := 'Stop Action';
  ButtonStop.Anchors := [akRight, akTop];
  X := PalletButtons.Width - BUTTON_MARGIN - BUTTON_WIDTH;
  ButtonStop.Left := X;
  Y := BUTTON_MARGIN + BUTTON_MARGIN + BUTTON_HEIGHT;
  ButtonStop.Top := Y;
  ButtonStop.OnClick := @DoButtonStopClick;
  ButtonStop.Enabled := False;
  ButtonStop.Parent := PalletButtons;
  {$ENDIF}
end;

procedure TLccCdiParser.AddSpinEdit(ParentControl: TScrollBox;
  Element: TLccXmlNode; var ControlOffset: Integer; ControlMargin,
  Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean;
  ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
var
  ASpinEdit: TLccOpenPascalSpinEdit;
  TempStr: string;
  ButtonLeft: Integer;
begin
  TempStr := '';

  // Debug Printing
  if DoPrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Create the SpinEdit
  ASpinEdit := TLccOpenPascalSpinEdit.Create(ParentControl);
  ASpinEdit.Width := 120;
  ASpinEdit.{$IFDEF DELPHI}Max{$ELSE}MaxValue{$ENDIF} := MaxInt;

  // Extract special modifiers
  if ExtractElementItem(Element, 'min', TempStr) then
    ASpinEdit.{$IFDEF DELPHI}Min{$ELSE}MinValue{$ENDIF} := StrToInt(TempStr);
  if ExtractElementItem(Element, 'max', TempStr) then
    ASpinEdit.{$IFDEF DELPHI}Max{$ELSE}MaxValue{$ENDIF} := StrToInt(TempStr);
  if ExtractElementItem(Element, 'default', TempStr) then
    ASpinEdit.Value := StrToInt(TempStr);
  ASpinEdit.Text := '';

  // Look for descripive names and descriptions to print
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  Inc(Indent, 8);

  // Create the ConfigInfo Struture
  if ElementType = 'int' then
    ASpinEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
  else
  if ElementType = 'bit' then
  begin
    ASpinEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
    ASpinEdit.{$IFDEF DELPHI}Max{$ELSE}MaxValue{$ENDIF} := 1;
    ASpinEdit.{$IFDEF DELPHI}Min{$ELSE}MinValue{$ENDIF} := 0;
  end;
  ASpinEdit.ConfigInfo.OnMemChangeState := {$IFNDEF DELPHI}@{$ENDIF}ASpinEdit.OnDrawImageState;

  // Create the Control Window
  ASpinEdit.StateImage.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := Indent;
  ASpinEdit.StateImage.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ControlOffset;
  ASpinEdit.StateImage.Width := 20; // ImageList16x16.Width;
  ASpinEdit.StateImage.Height := 20; //ImageList16x16.Height;
  ASpinEdit.ImageList16x16 := ImageList16x16;
  ASpinEdit.ImageIndexStateCurrent := ImageIndexStateCurrent;
  ASpinEdit.ImageIndexStateUnknown := ImageIndexStateUnknown;
  ASpinEdit.ImageIndexRead := ImageIndexRead;
  ASpinEdit.ImageIndexWrite := ImageIndexWrite;
  ASpinEdit.OnDrawImageState(ASpinEdit.ConfigInfo);
  ASpinEdit.StateImage.Parent := ParentControl;

  ASpinEdit.Top := ControlOffset;
  ASpinEdit.Left := ASpinEdit.StateImage.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} + ASpinEdit.StateImage.Width + 8;
  ASpinEdit.Parent := ParentControl;
  ASpinEdit.OnChange :=  {$IFNDEF DELPHI}@{$ENDIF}OnSpinEditChange;

  ButtonLeft := Round(ASpinEdit.Left + ASpinEdit.Width + 4);
  if ShowReadBtn then
  begin
    ASpinEdit.ReadCVSpeedButton.Visible := True;
    ASpinEdit.ReadCVSpeedButton.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := ButtonLeft;
    ASpinEdit.ReadCVSpeedButton.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ASpinEdit.Top;
    ASpinEdit.ReadCVSpeedButton.Height := ASpinEdit.Height;
    ASpinEdit.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
    ASpinEdit.ReadCVSpeedButton.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := 'Read';
    ASpinEdit.ReadCVSpeedButton.OnClick :=  {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonReadClick;
    AddSpeedButtonGlyph(ASpinEdit.ReadCVSpeedButton, ImageIndexRead);
    ASpinEdit.ReadCVSpeedButton.Parent := ParentControl;
    ButtonLeft := Round(ButtonLeft + ASpinEdit.ReadCVSpeedButton.Width);
  end else
   ASpinEdit.ReadCVSpeedButton.Visible := False;

  if ShowWriteBtn then
  begin
    ASpinEdit.WriteCVSpeedButton.Visible := True;
    ASpinEdit.WriteCVSpeedButton.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := ButtonLeft;
    ASpinEdit.WriteCVSpeedButton.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ASpinEdit.Top;
    ASpinEdit.WriteCVSpeedButton.Height := ASpinEdit.Height;
    ASpinEdit.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
    ASpinEdit.WriteCVSpeedButton.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := 'Write';
    ASpinEdit.WriteCVSpeedButton.OnClick :=  {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonWriteClick;
    AddSpeedButtonGlyph(ASpinEdit.WriteCVSpeedButton, ImageIndexWrite);
    ASpinEdit.WriteCVSpeedButton.Parent := ParentControl;
    ButtonLeft := Round(ButtonLeft + ASpinEdit.WriteCVSpeedButton.Width);
  end else
    ASpinEdit.WriteCVSpeedButton.Visible := False;

{  ASpinEdit.CompareCVSpeedButton.Left := ASpinEdit.WriteCVSpeedButton.Left + ASpinEdit.WriteCVSpeedButton.Width + 4;
  ASpinEdit.CompareCVSpeedButton.Top := ASpinEdit.Top;
  ASpinEdit.CompareCVSpeedButton.Height := ASpinEdit.Height;
  ASpinEdit.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
  ASpinEdit.CompareCVSpeedButton.Caption := 'Compare';
  ASpinEdit.CompareCVSpeedButton.OnClick := @DoSpeedButtonCompareClick;
  ASpinEdit.CompareCVSpeedButton.Parent := ParentControl;    }

  // Update the Control Offsets
  ControlOffset := Round(ControlOffset + ASpinEdit.Height + ControlMargin);
end;

procedure TLccCdiParser.AddEdit(ParentControl: TScrollBox; Element: TLccXmlNode;
  var ControlOffset: Integer; ControlMargin, Indent: Integer; MemOffset,
  MemSize: DWord; DoPrintMemOffset: Boolean; ElementType: string; ShowReadBtn,
  ShowWriteBtn: Boolean);
var
  AnEdit: TLccOpenPascalEdit;
  TempStr: string;
  i, ButtonLeft, ButtonWidthTotal: Integer;
  Size: TSize;
begin
  TempStr := '';

  // Debug Printing
  if DoPrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Create the Edit
  AnEdit := TLccOpenPascalEdit.Create(ParentControl);
  AnEdit.Left := Round(Indent + {ImageList16x16.Width} 0 + 8);    // Need valid to deal with setting the width

  // Look for descripive names and descriptions to print
  if ExtractElementItem(Element, 'name', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
  if ExtractElementItem(Element, 'description', TempStr) then
    AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);

  // Create the ConfigInfo Struture
  if ElementType = 'eventid' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_EventID)
  else
  if ElementType = 'int' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
  else
  if ElementType = 'string' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_String)
  else
  if ElementType = 'bit' then
    AnEdit.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
  AnEdit.ConfigInfo.OnMemChangeState := {$IFNDEF DELPHI}@{$ENDIF}AnEdit.OnDrawImageState;

  // Calculate the Width of the control needed
  for i := 0 to MemSize - 1 do
    TempStr := TempStr + 'Y';
  {$IFDEF DELPHI}
  Size.cx := Round( (Screen.ActiveForm as TCustomForm).Canvas.TextWidth(TempStr));
  Size.cy := Round( (Screen.ActiveForm as TCustomForm).Canvas.TextHeight(TempStr));
  {$ELSE}
  Size := Application.MainForm.Canvas.TextExtent(TempStr);
  {$ENDIF}
  if ElementType = 'eventid' then
    AnEdit.Width := Round( Size.cx * 3.2)
  else
    AnEdit.Width := Round( Size.cx * 1.2);

  ButtonWidthTotal := 0;
  if ShowReadBtn then
    ButtonWidthTotal := ButtonWidthTotal + CV_BUTTON_WIDTH + 4;
  if ShowWriteBtn then
    ButtonWidthTotal := ButtonWidthTotal + CV_BUTTON_WIDTH + 4;

  {$IFDEF DELPHI}
  if AnEdit.Left + AnEdit.Width + ButtonWidthTotal + 32 > TControl( ParentControl.Parent).Size.Width then      // The ScrollWindow can be wider than the view
    AnEdit.Width := Round( TControl( ParentControl.Parent).Size.Width - ButtonWidthTotal + 32);
  {$ELSE}
  if AnEdit.Left + AnEdit.Width + ButtonWidthTotal + 32 > ParentControl.Parent.Width then      // The ScrollWindow can be wider than the view
    AnEdit.Width := Round(ParentControl.Parent.Width - ButtonWidthTotal + 32);
  {$ENDIF}
  // Create the Control Window
  AnEdit.StateImage.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := Indent;
  AnEdit.StateImage.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ControlOffset;
  AnEdit.StateImage.Width := 0;   // ImageList16x16.Width;
  AnEdit.StateImage.Height := 0;  //ImageList16x16.Height;
  AnEdit.ImageList16x16 := ImageList16x16;
  AnEdit.ImageIndexStateCurrent := ImageIndexStateCurrent;
  AnEdit.ImageIndexStateUnknown := ImageIndexStateUnknown;
  AnEdit.ImageIndexRead := ImageIndexRead;
  AnEdit.ImageIndexWrite := ImageIndexWrite;
  AnEdit.OnDrawImageState(AnEdit.ConfigInfo);
 // AnEdit.Anchors := [akLeft, akRight, akTop];
  AnEdit.StateImage.Parent := ParentControl;

  AnEdit.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ControlOffset;
  AnEdit.Parent := ParentControl;
  AnEdit.OnChange := {$IFNDEF DELPHI}@{$ENDIF}OnEditChange;

  ButtonLeft := Round(AnEdit.Left + AnEdit.Width + 4);
  if ShowReadBtn then
  begin
    AnEdit.ReadCVSpeedButton.Visible := True;
    AnEdit.ReadCVSpeedButton.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := ButtonLeft;
    AnEdit.ReadCVSpeedButton.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := AnEdit.Top;
    AnEdit.ReadCVSpeedButton.Height := AnEdit.Height;
    AnEdit.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AnEdit.ReadCVSpeedButton.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := 'Read';
    AnEdit.ReadCVSpeedButton.OnClick := {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonReadClick;
    AddSpeedButtonGlyph(AnEdit.ReadCVSpeedButton, ImageIndexRead);
  //  AnEdit.ReadCVSpeedButton.Anchors := [akRight, akTop];
    AnEdit.ReadCVSpeedButton.Parent := ParentControl;
    ButtonLeft := Round( ButtonLeft + AnEdit.ReadCVSpeedButton.Width + 4);
  end else
    AnEdit.ReadCVSpeedButton.Visible := True;

  if ShowWriteBtn then
  begin
    AnEdit.WriteCVSpeedButton.Visible := True;
    AnEdit.WriteCVSpeedButton.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := ButtonLeft;
    AnEdit.WriteCVSpeedButton.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := AnEdit.Top;
    AnEdit.WriteCVSpeedButton.Height := AnEdit.Height;
    AnEdit.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AnEdit.WriteCVSpeedButton.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := 'Write';
    AnEdit.WriteCVSpeedButton.OnClick := {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonWriteClick;
    AddSpeedButtonGlyph(AnEdit.WriteCVSpeedButton, ImageIndexWrite);
   // AnEdit.WriteCVSpeedButton.Anchors := [akRight, akTop];
    AnEdit.WriteCVSpeedButton.Parent := ParentControl;
    ButtonLeft := Round(ButtonLeft + AnEdit.WriteCVSpeedButton.Width + 4);
  end else
    AnEdit.WriteCVSpeedButton.Visible := False;

{  AnEdit.CompareCVSpeedButton.Left := AnEdit.WriteCVSpeedButton.Left + AnEdit.WriteCVSpeedButton.Width + 4;
  AnEdit.CompareCVSpeedButton.Top := AnEdit.Top;
  AnEdit.CompareCVSpeedButton.Height := AnEdit.Height;
  AnEdit.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
  AnEdit.CompareCVSpeedButton.Caption := 'Compare';
  AnEdit.CompareCVSpeedButton.OnClick := @DoSpeedButtonCompareClick;
  AnEdit.CompareCVSpeedButton.Parent := ParentControl; }

  // Update the Control Offsets
  ControlOffset := Round(ControlOffset + AnEdit.Height + ControlMargin);
end;

procedure TLccCdiParser.AddComboBoxList(ParentControl: TScrollBox;
  Element: TLccXmlNode; var ControlOffset: Integer; ControlMargin,
  Indent: Integer; MemOffset, MemSize: DWord; DoPrintMemOffset: Boolean;
  ElementType: string; ShowReadBtn, ShowWriteBtn: Boolean);
var
  AComboBoxList: TLccOpenPascalComboBox;
  TempStr, LongestStr, ValueStr, PropertyStr: string;
  MapNode, ChildNode: TLccXmlNode;
  DoIndent: Boolean;
  Size: TSize;
  ButtonLeft: Integer;
begin
  TempStr := '';

  // Debug Printing
  if DoPrintMemOffset then
  begin
    AddLabel(ParentControl, 'Offset: ' + IntToStr(MemOffset), ControlOffset, 2, Indent, False);
    AddLabel(ParentControl, 'Size: ' + IntToStr(MemSize), ControlOffset, 2, Indent, False);
  end;

  // Find the map for the element
  MapNode := XmlFindChildNode(Element, 'map');

  // A ComboBox is only used for a map element
  if MapNode <> nil then
  begin
    // Create the ComboBox
    AComboBoxList := TLccOpenPascalComboBox.Create(ParentControl);
    {$IFNDEF DELPHI}
    AComboBoxList.AutoSize := True;
    AComboBoxList.Style := csDropDownList;
    {$ENDIF}

    // Look for descripive names and descriptions to print
    if ExtractElementItem(Element, 'name', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
    if ExtractElementItem(Element, 'description', TempStr) then
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
    Inc(Indent, 8);

    // The map can have a name and description too, look for them and print
    DoIndent := False;
    if ExtractElementItem(MapNode, 'name', TempStr) then
    begin
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
      DoIndent := True;
    end;
    if ExtractElementItem(MapNode, 'description', TempStr) then
    begin
      AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, False);
      DoIndent := True
    end;

    // If there were map descriptions then indent the following deeper than the descriptions
    if DoIndent then
      Inc(Indent, 8);

    // Create the ConfigInfo Struture
    if ElementType = 'eventid' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_EventID)
    else
    if ElementType = 'int' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_Int)
    else
    if ElementType = 'string' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_String)
    else
    if ElementType = 'bit' then
      AComboBoxList.ConfigInfo := TConfigInfo.Create(MemOffset, MemSize, cdt_bit);
    AComboBoxList.ConfigInfo.OnMemChangeState := {$IFNDEF DELPHI}@{$ENDIF}AComboBoxList.OnDrawImageState;

    // Run the children of the map looking for its relations
    LongestStr := '';
    ChildNode := XmlFirstChild(MapNode);
    while ChildNode <> nil do
    begin
      if LowerCase( ChildNode.NodeName) = 'relation' then
      begin
        // Found a relation
        PropertyStr := '';
        ValueStr := '';
        // Look for the value
        if ExtractElementItem(ChildNode, 'value', ValueStr) then
        begin
          // Found the value add it to the Listbox
          AComboBoxList.Items.Add(ValueStr);
          // Track the longest string so the control width can be set later
          if Length(ValueStr) > Length(LongestStr) then
            LongestStr := ValueStr;
        end;
        PropertyStr := '';
        // Look for the property
        ExtractElementItem(ChildNode, 'property', PropertyStr);
        // Create a list of relations for later use in the ComboBox
        AComboBoxList.ConfigInfo.MapList.AddRelation(ValueStr, PropertyStr);
      end;
      ChildNode := ChildNode.NextSibling;
    end;

    // Deselect any relation so it is clear it is not a valid value yet (need to read the config memory to select the correct one)
    AComboBoxList.ItemIndex := -1;

    // Calculate the correct size to display all the text
    {$IFDEF DELPHI}
    Size.cx := Round( (Screen.ActiveForm as TCustomForm).Canvas.TextWidth(LongestStr));
    Size.cy := Round( (Screen.ActiveForm as TCustomForm).Canvas.TextHeight(LongestStr));
    {$ELSE}
    Size := Application.MainForm.Canvas.TextExtent(LongestStr);
    {$ENDIF}
    AComboBoxList.Width := Round( Size.cx + 50);

    // Create the Control Window
    AComboBoxList.StateImage.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := Indent;
    AComboBoxList.StateImage.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := ControlOffset;
    AComboBoxList.StateImage.Width := 0; //ImageList16x16.Width;
    AComboBoxList.StateImage.Height := 0; //ImageList16x16.Height;
    AComboBoxList.ImageList16x16 := ImageList16x16;
    AComboBoxList.ImageIndexStateCurrent := ImageIndexStateCurrent;
    AComboBoxList.ImageIndexStateUnknown := ImageIndexStateUnknown;
    AComboBoxList.ImageIndexRead := ImageIndexRead;
    AComboBoxList.ImageIndexWrite := ImageIndexWrite;
    AComboBoxList.OnDrawImageState(AComboBoxList.ConfigInfo);
    AComboBoxList.StateImage.Parent := ParentControl;

    AComboBoxList.Top := ControlOffset;
    AComboBoxList.Left := AComboBoxList.StateImage.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} + AComboBoxList.StateImage.Width + 8;
    AComboBoxList.Parent := ParentControl;
    AComboBoxList.OnChange := {$IFNDEF DELPHI}@{$ENDIF}OnComboBoxChange;

    ButtonLeft := Round(AComboBoxList.Left + AComboBoxList.Width + 4);
    if ShowReadBtn then
    begin
      AComboBoxList.ReadCVSpeedButton.Visible := True;
      AComboBoxList.ReadCVSpeedButton.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := ButtonLeft;
      AComboBoxList.ReadCVSpeedButton.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := AComboBoxList.Top;
      AComboBoxList.ReadCVSpeedButton.Height := AComboBoxList.Height;
      AComboBoxList.ReadCVSpeedButton.Width := CV_BUTTON_WIDTH;
      AComboBoxList.ReadCVSpeedButton.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := 'Read';
      AComboBoxList.ReadCVSpeedButton.OnClick := {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonReadClick;
      AddSpeedButtonGlyph(AComboBoxList.ReadCVSpeedButton, ImageIndexRead);
      AComboBoxList.ReadCVSpeedButton.Parent := ParentControl;
      ButtonLeft := Round(ButtonLeft + AComboBoxList.ReadCVSpeedButton.Width + 4);
    end else
      AComboBoxList.ReadCVSpeedButton.Visible := False;

    if ShowWriteBtn then
    begin
      AComboBoxList.WriteCVSpeedButton.Visible := True;
      AComboBoxList.WriteCVSpeedButton.{$IFDEF DELPHI}Position.X{$ELSE}Left{$ENDIF} := ButtonLeft;
      AComboBoxList.WriteCVSpeedButton.{$IFDEF DELPHI}Position.Y{$ELSE}Top{$ENDIF} := AComboBoxList.Top;
      AComboBoxList.WriteCVSpeedButton.Height := AComboBoxList.Height;
      AComboBoxList.WriteCVSpeedButton.Width := CV_BUTTON_WIDTH;
      AComboBoxList.WriteCVSpeedButton.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := 'Write';
      AComboBoxList.WriteCVSpeedButton.OnClick := {$IFNDEF DELPHI}@{$ENDIF}DoSpeedButtonWriteClick;
      AddSpeedButtonGlyph(AComboBoxList.WriteCVSpeedButton, ImageIndexWrite);
      AComboBoxList.WriteCVSpeedButton.Parent := ParentControl;
      ButtonLeft := Round(ButtonLeft + AComboBoxList.WriteCVSpeedButton.Width + 4);
    end else
      AComboBoxList.WriteCVSpeedButton.Visible := False;

 {   AComboBoxList.CompareCVSpeedButton.Left := AComboBoxList.WriteCVSpeedButton.Left + AComboBoxList.WriteCVSpeedButton.Width + 4;
    AComboBoxList.CompareCVSpeedButton.Top := AComboBoxList.Top;
    AComboBoxList.CompareCVSpeedButton.Height := AComboBoxList.Height;
    AComboBoxList.CompareCVSpeedButton.Width := CV_BUTTON_WIDTH;
    AComboBoxList.CompareCVSpeedButton.Caption := 'Compare';
    AComboBoxList.CompareCVSpeedButton.OnClick := @DoSpeedButtonCompareClick;
    AComboBoxList.CompareCVSpeedButton.Parent := ParentControl;       }

    // Update the Control Offsets
    ControlOffset := Round(ControlOffset + AComboBoxList.Height + ControlMargin);
  end;
end;

procedure TLccCdiParser.ProcessElementForUI(ParentControl: TScrollBox;
  Element: TLccXmlNode; var MemOffset: DWord; var ControlOffset: Integer;
  Indent: Integer; DoSuppressNameAndDescription: Boolean;
  DoPrintMemOffset: Boolean; ShowReadBtn, ShowWriteBtn: Boolean);
var
  Group_Child, Map_Child: TLccXmlNode;
  TempStr: string;
  ReplicationCount, i: Integer;
  MemSize: DWord;
begin
 if Element <> nil then
 begin
   TempStr := '';

   // Test for a Group segment
   if LowerCase( Element.NodeName) = 'group' then
   begin
     // If it is a group then run into the group
     Inc(Indent, 8);

     // Group may override the Offset
     UpdateMemOffsetJump(Element, MemOffset);

     // Look for descripive names and descriptions to print
     if ExtractElementItem(Element, 'name', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent, True);
     if ExtractElementItem(Element, 'description', TempStr) then
       AddLabel(ParentControl, TempStr, ControlOffset, 2, Indent + 4, False);

     // Look for replications
     if ExtractElementAttribute(Element, 'replication', TempStr) then
       ReplicationCount := StrToInt(TempStr)
     else
       ReplicationCount := 1;
     ExtractElementItem(Element, 'repname', TempStr);   // Is only one repeated name allowed?  Appears to be with the XML tool.

     // Run through the replicated group (if there was no replication then this is set to 1)
     for i := 1 to ReplicationCount do
     begin
       // TempStr contains the repeated name so print it with the iteration number
       if TempStr <> '' then
         AddLabel(ParentControl, TempStr + ' ' + IntToStr(i), ControlOffset, 2, Indent + 8, False);

       // Run through each of the children of the group calling this method to process them
       Group_Child := XmlFirstChild(Element);
        while Group_Child <> nil do
       begin
         ProcessElementForUI(ParentControl, Group_Child, MemOffset, ControlOffset, Indent + 16, True, DoPrintMemOffset, ShowReadBtn, ShowWriteBtn);
         Group_Child := Group_Child.NextSibling;
       end;
     end;
   end else
   begin
     // It is not a group
     if (LowerCase(Element.NodeName) = 'name') or (LowerCase(Element.NodeName) = 'description') then
     begin
       // It is a descriptive block so print it
       if not DoSuppressNameAndDescription then
         AddLabel(ParentControl, string(XmlNodeTextContent(Element)), ControlOffset, 2, Indent, False);
     end else
     if LowerCase(Element.NodeName) = 'int' then
     begin
       // It is an Integer which may have a memory modifier as well as a size
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);

       // If it has a map then create a ComboListBox to handle it else use a Spin Edit
       Map_Child := XmlFindChildNode(Element, 'map');
       if Map_Child = nil then
         AddSpinEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, string(Element.NodeName), ShowReadBtn, ShowWriteBtn)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, string(Element.NodeName), ShowReadBtn, ShowWriteBtn);

       // Update the Control Offset
       Inc(MemOffset, MemSize);
     end else
     if LowerCase(Element.NodeName) = 'bit' then
     begin
       // It is an Bit which may have a memory modifier as well as a size
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);

       // Think a bit MUST have a map, not sure what the alternative would look like
       Map_Child := XmlFindChildNode(Element, 'map');
       if Map_Child = nil then
         AddSpinEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, string(Element.NodeName), ShowReadBtn, ShowWriteBtn)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, string(Element.NodeName), ShowReadBtn, ShowWriteBtn);

       // Update the Control Offset
       Inc(MemOffset, MemSize);
     end else
     if (LowerCase(Element.NodeName) = 'string') or (LowerCase(Element.NodeName) = 'eventid') then
     begin
       UpdateMemOffsetJump(Element, MemOffset);
       MemSize := UpdateMemOffsetSize(Element);
       Map_Child := XmlFindChildNode(Element, 'map');
       if Map_Child = nil then
         AddEdit(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, string(Element.NodeName), ShowReadBtn, ShowWriteBtn)
       else
         AddComboBoxList(ParentControl, Element, ControlOffset, 4, Indent + 4, MemOffset, MemSize, DoPrintMemOffset, string(Element.NodeName), ShowReadBtn, ShowWriteBtn);

       // Update the Control Offset
       Inc(MemOffset, MemSize);
     end else
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

function TLccCdiParser.SpeedButtonToConfigInfo(Sender: TObject): TConfigInfo;
begin
  if TLccSpeedButton(Sender).Owner is TLccOpenPascalSpinEdit then
    Result := TLccOpenPascalSpinEdit( TLccSpeedButton(Sender).Owner).ConfigInfo
  else
  if TLccSpeedButton(Sender).Owner is TLccOpenPascalEdit then
    Result := TLccOpenPascalEdit( TLccSpeedButton(Sender).Owner).ConfigInfo
  else
  if TLccSpeedButton(Sender).Owner is TLccOpenPascalComboBox then
    Result := TLccOpenPascalComboBox( TLccSpeedButton(Sender).Owner).ConfigInfo
  else
   Result := nil;
end;

procedure TLccCdiParser.OnSpinEditChange(Sender: TObject);
begin
  (Sender as TLccOpenPascalSpinEdit).ConfigInfo.MemState := ocs_Unknown;
  (Sender as TLccOpenPascalSpinEdit).ConfigInfo.ConfigData.DataInteger := Round((Sender as TLccOpenPascalSpinEdit).Value);
end;

procedure TLccCdiParser.OnEditChange(Sender: TObject);
begin
  (Sender as TLccOpenPascalEdit).ConfigInfo.MemState := ocs_Unknown;
  (Sender as TLccOpenPascalEdit).ConfigInfo.ConfigData.DataString := (Sender as TLccOpenPascalEdit).Text;
end;

procedure TLccCdiParser.OnComboBoxChange(Sender: TObject);
begin
 (Sender as TLccOpenPascalComboBox).ConfigInfo.MemState := ocs_Unknown;
 (Sender as TLccOpenPascalComboBox).ConfigInfo.ConfigData.DataInteger := (Sender as TLccOpenPascalComboBox).ItemIndex;
end;

procedure TLccCdiParser.OnPageControlChange(Sender: TObject);
begin
  Serializer.Clear;
  if AutoReadOnTabChange then
    DoButtonReadPageClick(Sender);
end;

procedure TLccCdiParser.OnSerializerNotification(Sender: TObject; Notify: TParserSerializer);
begin
  if Assigned(Pallet) then
  begin

    if MarkedToStop and not MarkedToStopIsStopping then
    begin
      MarkedToStopIsStopping := True;
      Serializer.Clear;
    end;

    if Serializer.ConfigInfoList.Count > 0 then
    begin
      ButtonReadPage.Enabled := False;
      ButtonWritePage.Enabled := False;
      ButtonStop.Enabled := True;
    end else
    begin
      ButtonReadPage.Enabled := True;
      ButtonWritePage.Enabled := True;
      ButtonStop.Enabled := False;
      MarkedToStop := False;
      MarkedToStopIsStopping := False;
      if Notify = ps_RemoveRead then
        DoAfterReadPage(Self)
      else
        DoAfterWritePage(Self);
    end;
//    StatusPanel.Text := 'Remaining: ' + IntToStr(Serializer.ConfigInfoList.Count);
  end;
end;

procedure TLccCdiParser.OnBkGndResize(Sender: TObject);
begin

end;

constructor TLccCdiParser.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ImageList16x16 := nil;
  FShowReadBtn := True;
  FShowWriteBtn := True;
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


    function CreateTab(
      APageControl: TLccTabControl;
      ACaption: String
      ): TLccPanel;
    var
      LocalTab: TLccTabSheet;
      LocalScrollBox: TScrollBox;
    begin
      {$IFDEF DELPHI}
      // Create new Tab and connect it to the TabControl
      LocalTab := TLccTabSheet.Create(APageControl);
      LocalTab.Text := ACaption;
      LocalTab.Parent := APageControl;

      // Create the ScrollBox and put it in the Tab
      LocalScrollBox := TScrollBox.Create(Tab);
      LocalScrollBox.Parent := LocalTab;
      LocalScrollBox.Align := TAlignLayout.Client;
      LocalScrollBox.Padding.Left := 4;
      LocalScrollBox.Padding.Right := 4;
      LocalScrollBox.Padding.Top := 4;
      LocalScrollBox.Padding.Bottom := 4;

      Result := TLccPanel.Create(LocalTab);
      Result.Anchors := [TAnchorKind.akRight, TAnchorKind.akRight, TAnchorKind.akTop];
      Result.Position.X;
      Result.Width := LocalScrollBox.Width;
      Result.Parent := LocalScrollBox;

      {$ELSE}
      // Create new Tab and connect it to the TabControl
      LocalTab := APageControl.AddTabSheet;
      LocalTab.Caption := ACaption;

      // Create the ScrollBox and put it in the Tab
      LocalScrollBox := TScrollBox.Create(LocalTab);
      LocalScrollBox.Parent := LocalTab;
      LocalScrollBox.Align := alClient;
      LocalScrollBox.BorderSpacing.Around := 4;
      LocalScrollBox.AutoScroll := True;

      LocalScrollBox.VertScrollBar.Tracking := True;
      LocalScrollBox.HorzScrollBar.Tracking := True;

      Result := TLccPanel.Create(LocalTab);
      Result.Anchors := [akLeft, akRight, akTop];
      Result.Left := 0;
      Result.Width := LocalScrollBox.Width;
      Result.Height := 1000;                // TODO: TEMP
      Result.Parent := LocalScrollBox;
      {$ENDIF}
    end;

    function CreateButton(
      AContainerParent: TLccPanel;
      ACaption: String;
      OnClickFunc: TNotifyEvent;
      Enable: Boolean;
      AWidth: single;
      {$IFDEF DELPHI}TAlignLayout{$ELSE}AnAlign: TAlign{$ENDIF}
        ): TLccSpeedButton;
    begin
      Result := TLccSpeedButton.Create(AContainerParent);
      Result.Width := {$IFDEF DELPHI}AWidth{$ELSE}Trunc(AWidth){$ENDIF};
      Result.Caption := ACaption;
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
      Result.Parent := AContainerParent;
    end;

    function CreateLabel(
      ParentControl: TLccPanel;
      ACaption: String;
      Indent: Single;
      Bold: Boolean
      ): TLccLabel;
    begin
      Result := TLccLabel.Create(ParentControl);
      Result.{$IFDEF DELPHI}Text{$ELSE}Caption{$ENDIF} := ACaption;
      Result.Top := ParentControl.Height;   // Make sure it stacks in the right order;
      Result.Align := {$IFDEF DELPHI}TAlignLayout.Top{$ELSE}alTop{$ENDIF};
      if Bold then
        Result.Font.Style := {$IFDEF DELPHI}[TFontStyle.fsBold]{$ELSE}[fsBold]{$ENDIF};
      Result.WordWrap := True;
      {$IFDEF DELPHI}
      Result.Padding.Left := Indent;
      {$ELSE}
      Result.BorderSpacing.Left := Trunc(Indent);
      {$ENDIF}
      Result.Parent := ParentControl;
    end;

    function CreateSpacer(
      ParentControl: TLccPanel
    ): TLccLabel;
    begin
      Result := CreateLabel(ParentControl, '', 0, False);
    end;

const
  IDENTIFICATION_INDENT = 16;
  BUTTON_HEIGHT = 40;
var
  CDI_Root, Cdi_Child, Identification_Root, Identification_Child, Segment_Root, Segment_Child, Map_Child, Relation_Child: TLccXmlNode;
  MemOffset: DWord;
  ControlOffset: Integer;
  ItemStr: string;
  ParserBkGnd, FooterBkGnd, ScrollBoxBkGnd: TLccPanel;
  TabControl: TLccTabControl;
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

  ItemStr := '';

  FLccNode := AnLccNode;
  FPallet := ParentControl;
  Serializer.OnNotification := {$IFNDEF DELPHI}@{$ENDIF}OnSerializerNotification;
  Clear_CDI_Interface(False);

  // Background that holds everything and is passed back as the child of the ParentControl
  ParserBkGnd := TLccPanel.Create(ParentControl);
  ParserBkGnd.Parent := ParentControl;
  ParserBkGnd.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  ParserBkGnd.OnResize := {$IFNDEF DELPHI}@{$ENDIF}OnBkGndResize;
  Result := ParserBkGnd;

  // Bottom aligned Panel to hold the Read/Write all Buttons
  FooterBkGnd := TLccPanel.Create(ParserBkGnd);
  FooterBkGnd.Align := {$IFDEF DELPHI}TAlignLayout.Bottom{$ELSE}alBottom{$ENDIF};
  {$IFNDEF DELPHI}FooterBkGnd.BevelOuter := bvNone;{$ENDIF}
  FooterBkGnd.Parent := ParserBkGnd;
  FooterBkGnd.Height := BUTTON_HEIGHT;
  CreateButton(FooterBkGnd, 'Read All', @DoButtonReadPageClick, False, (FooterBkGnd.ClientWidth/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  CreateButton(FooterBkGnd, 'Write All', @DoButtonWritePageClick, False, (FooterBkGnd.ClientWidth/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});
  CreateButton(FooterBkGnd, 'Abort', @DoButtonStopClick, False, (FooterBkGnd.ClientWidth/3)-2, {$IFDEF DELPHI}TAlignLayout.Right{$ELSE}alRight{$ENDIF});

  // TabControl that is client aligned with the FooterBkGnd
  TabControl := TLccTabControl.Create(ParentControl);
  TabControl.Align := {$IFDEF DELPHI}TAlignLayout.Client{$ELSE}alClient{$ENDIF};
  TabControl.Parent := ParserBkGnd;

  CDI_Root := XmlFindRootNode(CDI, 'cdi');
  if Assigned(CDI_Root) then
  begin

    // Handle the Identification block
    Identification_Root := XmlFindChildNode(CDI_Root, 'identification');
    if Assigned(Identification_Root) then
    begin
      ControlOffset := 0;

      // Add a tab to place the Identification information on
      ScrollBoxBkGnd := CreateTab(TabControl, 'Identification');

      // Space on the Top
      CreateSpacer(ScrollBoxBkGnd);

      // Handle the manufacturer
      CreateLabel(ScrollBoxBkGnd, 'Manufacturer', IDENTIFICATION_INDENT, True);
      Identification_Child := XmlFindChildNode(Identification_Root, 'manufacturer');
      if Assigned(Identification_Child) then
        CreateLabel(ScrollBoxBkGnd, ItemStr + string(XmlNodeTextContent(Identification_Child)), IDENTIFICATION_INDENT + 4, False)
      else
        CreateSpacer(ScrollBoxBkGnd);


      // Handle the model number
      CreateLabel(ScrollBoxBkGnd, 'Model: ', IDENTIFICATION_INDENT, True);
      Identification_Child := XmlFindChildNode(Identification_Root, 'model');
      if Assigned(Identification_Child) then
        CreateLabel(ScrollBoxBkGnd, ItemStr + string(XmlNodeTextContent(Identification_Child)), IDENTIFICATION_INDENT + 4, False)
      else
        CreateSpacer(ScrollBoxBkGnd);

      // Handle the Hardware Version
      CreateLabel(ScrollBoxBkGnd, 'Hardware Version: ', IDENTIFICATION_INDENT, True);
      Identification_Child := XmlFindChildNode(Identification_Root, 'hardwareVersion');
      if Assigned(Identification_Child) then
        CreateLabel(ScrollBoxBkGnd, ItemStr + string(XmlNodeTextContent(Identification_Child)), IDENTIFICATION_INDENT + 4, False)
      else
        CreateSpacer(ScrollBoxBkGnd);

      // Handle the Software Version
      CreateLabel(ScrollBoxBkGnd, 'Software Version: ', IDENTIFICATION_INDENT, True);
      Identification_Child := XmlFindChildNode(Identification_Root, 'softwareVersion');
      if Assigned(Identification_Child) then
        CreateLabel(ScrollBoxBkGnd, ItemStr + string(XmlNodeTextContent(Identification_Child)), IDENTIFICATION_INDENT + 4, False)
      else
        CreateSpacer(ScrollBoxBkGnd);

      // Handle any map blocks that contain descriptive information
      Inc(ControlOffset, 8);

      Identification_Child := XmlFirstChild(Identification_Root);
      while Assigned(Identification_Child) do
      begin
        if LowerCase( Identification_Child.NodeName) = 'map' then
        begin
          Map_Child := XmlFirstChild(Identification_Child);
          while Assigned(Map_Child) do
          begin
            if LowerCase( Map_Child.NodeName) = 'relation' then
            begin
              Relation_Child := XmlFirstChild(Map_Child);
              while Assigned(Relation_Child) do
              begin
                if (LowerCase( Relation_Child.NodeName) = 'value') then
                  CreateLabel(ScrollBoxBkGnd, string(XmlNodeTextContent(Relation_Child)), IDENTIFICATION_INDENT + 16, False)
                else
                if (LowerCase( Relation_Child.NodeName) = 'property') then
                   CreateLabel(ScrollBoxBkGnd, string(XmlNodeTextContent(Relation_Child)), IDENTIFICATION_INDENT + 8, False);
                Relation_Child := Relation_Child.NextSibling;
              end;
              Map_Child := Map_Child.NextSibling;
            end
          end;
        end;
        Identification_Child := Identification_Child.NextSibling;
      end;
      CreateSpacer(ScrollBoxBkGnd);
    end;

    // Handled the Segment blocks
    ControlOffset := 0;
    Cdi_Child := XmlFirstChild(CDI_Root);
    while Assigned(Cdi_Child) do
    begin
      if LowerCase(Cdi_Child.NodeName) = 'segment' then
      begin
        Segment_Root := Cdi_Child;
        // First Find the Config Memory Segment
     //   if IsMemorySpace(Segment_Root, 253) then
        begin
          ControlOffset := 0;
          MemOffset := 0;
          CreateSpacer(ScrollBoxBkGnd);     // NOT SURE OF THIS... THIS WILL BE AT THE BOTTOM

          // Add a new Tabsheet for this Segment using it Name Element as the tab title
          if ExtractElementItem(Segment_Root, 'name', ItemStr) then
            ScrollBoxBkGnd := CreateTab(TabControl, ItemStr)
          else
            ScrollBoxBkGnd := CreateTab(TabControl, '[Unnamed]');

          // Select it to create the window so the size of the Scrollbox is correct
          // Set it back to a simple tab so it builds faster
          {$IFDEF DELPHI}
          TabControl.TabIndex := TabControl.TabCount - 1;
          TabControl.TabIndex := 0;
          {$ELSE}
          TabControl.ActivePageIndex := TabControl.PageCount - 1;
          TabControl.ActivePageIndex := 0;
          {$ENDIF}

          // Add the description of this segment as the first line of the Tab Page
          if ExtractElementItem(Segment_Root, 'description', ItemStr) then
            CreateLabel(ScrollBoxBkGnd, ItemStr, 4, False);

          // Time to build the UI for this segment
          UpdateMemOffsetJump(Segment_Root, MemOffset);      // Segment may override the Offset

          // Run all children of the Segment
          Segment_Child := XmlFirstChild(Segment_Root);
          while Segment_Child <> nil do
          begin
     //       if (LowerCase(Segment_Child.NodeName) <> 'name') and (LowerCase(Segment_Child.NodeName) <> 'description') then
      //        ProcessElementForUI(ScrollBox, Segment_Child, MemOffset, ControlOffset, 4, SuppressNameAndDescription, PrintMemOffset, ShowReadBtn, ShowWriteBtn);
            Segment_Child := Segment_Child.NextSibling;
          end;

          // Space on the bottom
          CreateSpacer(ScrollBoxBkGnd);
        end;
      end;
      Cdi_Child := Cdi_Child.NextSibling;
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
  if Assigned(Pallet) then
  begin
    {$IFDEF DELPHI}
    for i := Pallet.ControlsCount - 1 downto 0 do
    {$ELSE}
    for i := Pallet.ControlCount - 1 downto 0 do
    {$ENDIF}
      Pallet.Controls[i].Free;
  end;
//  StatusPanel := nil;;
  ButtonStop := nil;
  ButtonWritePage := nil;
  ButtonReadPage := nil;
  FPallet := nil;
  if ClearLccNode then
    FLccNode := nil;
end;

procedure TLccCdiParser.NotifyLccNodeDestroy(LccNode: TObject);
begin
  if LccNode = FLccNode then
    Clear_CDI_Interface(True);
end;


{ TConfigInfo }

procedure TConfigInfo.SetMemState(AValue: TConfigMemState);
begin
  if AValue <> FMemState then
  begin
    FMemState:=AValue;
    if Assigned(OnMemChangeState) then
      OnMemChangeState(Self);
  end;
end;

constructor TConfigInfo.Create(MemOffset, MemSize: DWord;
  ADataType: TLccConfigDataType);
begin
  inherited Create;
  FMemState := ocs_Unknown;
  FMemAddress := MemOffset;
  FMemSize := MemSize;
  FDataType := ADataType;
  MapList := TMap.Create;
  FConfigData := TConfigData.Create;
end;

destructor TConfigInfo.Destroy;
begin
   FreeAndNil( FMapList);
   FreeAndNil(FConfigData);
  inherited Destroy;
end;


end.


