unit dualcontroller_unit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, ComCtrls,
  StdCtrls, Buttons, lcc_node_manager, lcc_ethernet_client, lcc_node,
  lcc_node_controller, lcc_node_messages, lcc_defines, lcc_node_train, lcc_math_float16,
  throttle_takeover_request_form, lcc_ethernet_common,
  Contnrs, form_logging, form_visual_server, lcc_alias_server
,
  lcc_utilities;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    ButtonConsistSave1: TButton;
    ButtonConsistAdd1: TButton;
    ButtonConsistRemove1: TButton;
    ButtonCreateConstist2: TButton;
    ButtonConsistRelease2: TButton;
    ButtonConsistRefresh2: TButton;
    ButtonReleaseConsist1: TButton;
    ButtonReleaseConsist2: TButton;
    ButtonConnect1: TButton;
    ButtonConnect2: TButton;
    CheckBox1: TCheckBox;
    CheckBox2: TCheckBox;
    CheckBox3: TCheckBox;
    CheckBox4: TCheckBox;
    CheckBox5: TCheckBox;
    CheckBox6: TCheckBox;
    CheckBox7: TCheckBox;
    CheckBox8: TCheckBox;
    CheckBox9: TCheckBox;
    CheckBoxConsistReverseDir2: TCheckBox;
    CheckBoxConsistAddress2: TCheckBox;
    CheckBoxForwardF0_1: TCheckBox;
    CheckBoxForwardF0_2: TCheckBox;
    CheckBoxForwardFn_1: TCheckBox;
    CheckBoxForwardFn_2: TCheckBox;
    CheckBoxThrottleLongAddress1: TCheckBox;
    CheckBoxThrottleLongAddress2: TCheckBox;
    CheckBoxThrottleTakeover1: TCheckBox;
    CheckBoxThrottleTakeover2: TCheckBox;
    Edit1: TEdit;
    Edit2: TEdit;
    Edit3: TEdit;
    Edit4: TEdit;
    Edit5: TEdit;
    Edit6: TEdit;
    Edit7: TEdit;
    Edit8: TEdit;
    EditCommandStationIPAddress: TEdit;
    EditConsistAddress2: TEdit;
    EditThrottleAddress1: TEdit;
    EditThrottleAddress2: TEdit;
    ImageList: TImageList;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label8: TLabel;
    LabelConsistExpander1: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label2: TLabel;
    Label21: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label103: TLabel;
    Label101: TLabel;
    Label100: TLabel;
    Label7: TLabel;
    Label9: TLabel;
    LabelAlias2: TLabel;
    LabelNodeID2: TLabel;
    LabelNodeID1: TLabel;
    LabelAlias1: TLabel;
    Label104: TLabel;
    LabelThrottleSpeed1: TLabel;
    LabelThrottleSpeed2: TLabel;
    ListViewConsistTable1: TListView;
    PageControlConsists2: TPageControl;
    PageControlThrottle2: TPageControl;
    PageControlThrottle1: TPageControl;
    Panel1: TPanel;
    Panel2: TPanel;
    Panel3: TPanel;
    PanelConsitEditor1: TPanel;
    PanelConsistEditHeader1: TPanel;
    PanelThrottle1ServiceMode: TPanel;
    PanelConsistCreate2: TPanel;
    PanelConsistRelease2: TPanel;
    PanelConsistWizard2: TPanel;
    PanelThrottleAssign1: TPanel;
    PanelThrottleConsistEdit1: TPanel;
    PanelThrottleAssign2: TPanel;
    PanelThrottleFace1: TPanel;
    PanelThrottleFace2: TPanel;
    PanelThrottleKeypad1: TPanel;
    PanelThrottleKeypad2: TPanel;
    PanelTop: TPanel;
    PanelThrottle2: TPanel;
    PanelThrottleEthernet: TPanel;
    PanelThrottleEthernet1: TPanel;
    RadioGroupConstistSpeedStep2: TRadioGroup;
    RadioGroupThrottleSpeedSteps1: TRadioGroup;
    RadioGroupThrottleSpeedSteps2: TRadioGroup;
    ScrollBox1: TScrollBox;
    SpeedButtonConsistSubtract2: TSpeedButton;
    SpeedButtonConstistTrainAdd2: TSpeedButton;
    SpeedButtonForward1: TSpeedButton;
    SpeedButtonForward2: TSpeedButton;
    SpeedButtonFunction0: TSpeedButton;
    SpeedButtonFunction1: TSpeedButton;
    SpeedButtonFunction10: TSpeedButton;
    SpeedButtonFunction11: TSpeedButton;
    SpeedButtonFunction12: TSpeedButton;
    SpeedButtonFunction13: TSpeedButton;
    SpeedButtonFunction14: TSpeedButton;
    SpeedButtonFunction15: TSpeedButton;
    SpeedButtonFunction16: TSpeedButton;
    SpeedButtonFunction17: TSpeedButton;
    SpeedButtonFunction18: TSpeedButton;
    SpeedButtonFunction19: TSpeedButton;
    SpeedButtonFunction2: TSpeedButton;
    SpeedButtonFunction20: TSpeedButton;
    SpeedButtonFunction21: TSpeedButton;
    SpeedButtonFunction22: TSpeedButton;
    SpeedButtonFunction23: TSpeedButton;
    SpeedButtonFunction3: TSpeedButton;
    SpeedButtonFunction4: TSpeedButton;
    SpeedButtonFunction5: TSpeedButton;
    SpeedButtonFunction6: TSpeedButton;
    SpeedButtonFunction7: TSpeedButton;
    SpeedButtonFunction8: TSpeedButton;
    SpeedButtonFunction9: TSpeedButton;
    SpeedButtonQuerySpeedThrottle1: TSpeedButton;
    SpeedButtonReverse1: TSpeedButton;
    SpeedButtonReverse2: TSpeedButton;
    SpeedButtonThrottleAssign1: TSpeedButton;
    SpeedButtonThrottleAssign2: TSpeedButton;
    StatusBarThrottle1: TStatusBar;
    StatusBarThrottle2: TStatusBar;
    TabSheetThrottle1ServiceMode: TTabSheet;
    TabSheetThrottle2ServiceMode: TTabSheet;
    TabSheetThrottle2Consists: TTabSheet;
    TabSheetConsistNew2: TTabSheet;
    TabSheetConsists2: TTabSheet;
    TabSheetThrottle2Throttle: TTabSheet;
    TabSheetThrottle1Throttle: TTabSheet;
    ToggleBoxLogging: TToggleBox;
    ToggleBoxServerForm: TToggleBox;
    TrackBarThrottle1: TTrackBar;
    TrackBarThrottle2: TTrackBar;
    TreeViewConsists2: TTreeView;
    TreeViewConsistWizard2: TTreeView;
    procedure Button1Click(Sender: TObject);
    procedure ButtonCreateConstist1Click(Sender: TObject);
    procedure ButtonCreateConstist2Click(Sender: TObject);
    procedure ButtonConnect1Click(Sender: TObject);
    procedure ButtonConnect2Click(Sender: TObject);
    procedure ButtonShowLoggingClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure LabelConsistExpander1Click(Sender: TObject);
    procedure ScrollBox1Click(Sender: TObject);
    procedure SpeedButtonConsistSubtract1Click(Sender: TObject);
    procedure SpeedButtonConsistSubtract2Click(Sender: TObject);
    procedure SpeedButtonConsistTrainAdd1Click(Sender: TObject);
    procedure SpeedButtonConstistTrainAdd2Click(Sender: TObject);
    procedure SpeedButtonForward1Click(Sender: TObject);
    procedure SpeedButtonForward2Click(Sender: TObject);
    procedure SpeedButtonReverse1Click(Sender: TObject);
    procedure SpeedButtonReverse2Click(Sender: TObject);
    procedure SpeedButtonThrottle2FunctionClick(Sender: TObject);
    procedure SpeedButtonThrottle1FunctionClick(Sender: TObject);
    procedure SpeedButtonThrottleAssign1Click(Sender: TObject);
    procedure SpeedButtonThrottleAssign2Click(Sender: TObject);
    procedure ToggleBoxLoggingChange(Sender: TObject);
    procedure ToggleBoxServerFormChange(Sender: TObject);
    procedure TrackBarThrottle1Change(Sender: TObject);
    procedure TrackBarThrottle2Change(Sender: TObject);
    procedure TreeViewConsistWizard1Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewConsistWizard1MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewConsistWizard1SelectionChanged(Sender: TObject);
    procedure TreeViewConsistWizard2Deletion(Sender: TObject; Node: TTreeNode);
    procedure TreeViewConsistWizard2MouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure TreeViewConsistWizard2SelectionChanged(Sender: TObject);
  private
    FWorkerMessage: TLccMessage;

  protected
    property WorkerMessage: TLccMessage read FWorkerMessage write FWorkerMessage;

    procedure OnNodeManagerSendMessage1(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage1(Sender: TObject; LccMessage: TLccMessage);

     procedure OnNodeManagerSendMessage2(Sender: TObject; LccMessage: TLccMessage);
    procedure OnNodeManagerReceiveMessage2(Sender: TObject; LccMessage: TLccMessage);

    procedure OnNodeManager1IDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManager1AliasChange(Sender: TObject; LccSourceNode: TLccNode);

    procedure OnNodeManager2IDChange(Sender: TObject; LccSourceNode: TLccNode);
    procedure OnNodeManager2AliasChange(Sender: TObject; LccSourceNode: TLccNode);

    procedure OnClientServer1ConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnClientServer1ErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);

    procedure OnClientServer2ConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
    procedure OnClientServer2ErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);

    procedure OnQueryConfigurationReply1(Sender: TLccTrainController; ConfigMemAddress: LongWord; Value: LongWord; ErrorCode: Byte; ErrorMsg: string);
    procedure OnQueryConfigurationReply2(Sender: TLccTrainController; ConfigMemAddress: LongWord; Value: LongWord; ErrorCode: Byte; ErrorMsg: string);

    procedure OnMemoryConfigWrite1(Sender: TLccTrainController; ConfigMemAddress: LongWord; Value: LongWord; ErrorCode: Byte; ErrorMsg: string);
    procedure OnMemoryConfigWrite2(Sender: TLccTrainController; ConfigMemAddress: LongWord; Value: LongWord; ErrorCode: Byte; ErrorMsg: string);

    // The Controller is the Controller Node created in the NodeManager
    procedure ControllerTrainAssigned1(Sender: TLccTrainController);
    procedure ControllerTrainAssigned2(Sender: TLccTrainController);

    procedure ControllerTrainReleased1(Sender: TLccTrainController);
    procedure ControllerTrainReleased2(Sender: TLccTrainController);

    procedure OnTrainServerSpeedChange1(TractionObject: TLccTractionObject);
    procedure OnTrainServerFunctionChange1(TractionObject: TLccTractionObject);

    procedure OnTrainServerSpeedChange2(TractionObject: TLccTractionObject);
    procedure OnTrainServerFunctionChange2(TractionObject: TLccTractionObject);

    procedure OnControllerReqestTakeover1(Sender: TLccTrainController; var Allow: Boolean);
    procedure OnControllerReqestTakeover2(Sender: TLccTrainController; var Allow: Boolean);

    procedure OnControllerQueryListenerGetCount1(Sender: TLccTrainController; ListenerCount: Byte);
    procedure OnControllerQueryListenerGetCount2(Sender: TLccTrainController; ListenerCount: Byte);

    procedure OnControllerQueryListenerIndex1(Sender: TLccTrainController; ListenerCount, ListenerIndex: Byte; ListenerFlags: Byte; ListenerNodeID: TNodeID);
    procedure OnControllerQueryListenerIndex2(Sender: TLccTrainController; ListenerCount, ListenerIndex: Byte; ListenerFlags: Byte; ListenerNodeID: TNodeID);

    procedure ReleaseTrain1;
    procedure ReleaseTrain2;

    procedure OnAliasMappingChange(Sender: TObject; LccSourceNode: TLccNode; AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
    procedure OnTractionRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);

    procedure OnLccTractionUpdateSNIP1(Sender: TObject; LccSourceNode: TLccNode; Index: Integer);
    procedure OnLccTractionUpdateTrainSNIP1(Sender: TObject; LccSourceNode: TLccNode; Index: Integer);
    procedure OnLccTractionUpdateListenerCount1(Sender: TObject; LccSourceNode: TLccNode; Index: Integer);

  public
    NodeManager1: TLccNodeManager;
    ClientServer1: TLccEthernetClient;
    NodeManager2: TLccNodeManager;
    ClientServer2: TLccEthernetClient;

    ControllerNode1: TLccTrainController; // First Node created by the NodeManager, it is assigned when the Ethenetlink is established
    ControllerNode2: TLccTrainController; // First Node created by the NodeManager, it is assigned when the Ethenetlink is established

  end;

var
  Form1: TForm1;

  Verbose: Boolean = False;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.ButtonCreateConstist1Click(Sender: TObject);
{var
  TreeNode: TTreeNode;
  DccSearchCriteria: TObjectList;
  ConsistItem: TDccSearchCriteria;  }
begin
 { DccSearchCriteria := TObjectList.Create;
  DccSearchCriteria.OwnsObjects := False;
  try
    TreeNode := TreeViewConsistWizard1.Items.GetFirstNode;
    while Assigned(TreeNode) do
    begin
      ConsistItem := (TDccSearchCriteria( TreeNode.Data));
      DccSearchCriteria.Add( ConsistItem);
      TreeNode := TreeNode.GetNext;
    end;
  finally
    ControllerNode1.ConsistTrainsByDccAddress(DccSearchCriteria, 100);
    FreeAndNil(DccSearchCriteria);
  end;   }
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
 // ControllerNode1.QueryConfigurationVariables(0, 8);

  // This needs to be though out... can't read directly take too long... Functions store some assumed value it then fire off a read of the node to update...
  // Do that here?  I don't know if that makes sense... It would if not for the Service Track part of this... The train node would just have this info to return
  // immediately.

end;

procedure TForm1.ButtonCreateConstist2Click(Sender: TObject);
var
  TreeNode: TTreeNode;
  DccSearchCriteria: TObjectList;
 // ConsistItem: TLccDCCSearchCriteriaEvent;

begin
  DccSearchCriteria := TObjectList.Create;
  DccSearchCriteria.OwnsObjects := False;
  try
    TreeNode := TreeViewConsistWizard2.Items.GetFirstNode;
    while Assigned(TreeNode) do
    begin
  //    ConsistItem := (TDccSearchCriteria( TreeNode.Data));
 //     DccSearchCriteria.Add( ConsistItem);
      TreeNode := TreeNode.GetNext;
    end;
  finally
    // we give the ObjectList to the callee....
 //   ControllerNode2.SearchTrainsByDccAddress(DccSearchCriteria);
  end;
end;

procedure TForm1.ButtonConnect1Click(Sender: TObject);
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  if ClientServer1.Connected then
  begin
    if ControllerNode1.IsTrainAssigned then
      ReleaseTrain1;
    ClientServer1.CloseConnection
  end else
  begin
    LocalInfo := TLccEthernetConnectionInfo.Create;
    try
    LocalInfo.ListenerIP := EditCommandStationIPAddress.Text;
    LocalInfo.AutoResolveIP := True;
    LocalInfo.ListenerPort := 12021;
    LocalInfo.Gridconnect := True;
    ClientServer1.OpenConnection(LocalInfo);
    finally
      LocalInfo.Free;
    end
  end;
end;

procedure TForm1.ButtonConnect2Click(Sender: TObject);
var
  LocalInfo: TLccEthernetConnectionInfo;
begin
  if ClientServer2.Connected then
  begin
 //   if ControllerNode2.IsTrainAssigned then
  //    ReleaseTrain2;
    ClientServer2.CloseConnection
  end else
  begin
    LocalInfo := TLccEthernetConnectionInfo.Create;
    try
    LocalInfo.ListenerIP := EditCommandStationIPAddress.Text;
    LocalInfo.AutoResolveIP := True;
    LocalInfo.ListenerPort := 12021;
    LocalInfo.Gridconnect := True;
    ClientServer2.OpenConnection(LocalInfo);
    finally
      LocalInfo.Free;
    end;
  end;
end;

procedure TForm1.ButtonShowLoggingClick(Sender: TObject);
begin

end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
 { NodeManager1.Clear;
  ClientServer1.CloseConnection(nil);
  FreeAndNil(NodeManager1);

  NodeManager2.Clear;
  ClientServer2.CloseConnection(nil);
  FreeAndNil(NodeManager2);

  FreeAndNil(FWorkerMessage);     }
end;

procedure TForm1.FormCreate(Sender: TObject);
begin

  NodeManager1 := TLccNodeManager.Create(nil, True);
  ClientServer1 := TLccEthernetClient.Create(nil, NodeManager1);

  NodeManager1.OnNodeAliasIDChanged := @OnNodeManager1AliasChange;
  NodeManager1.OnNodeIDChanged := @OnNodeManager1IDChange;

  ClientServer1.OnConnectionStateChange := @OnClientServer1ConnectionChange;
  ClientServer1.OnErrorMessage := @OnClientServer1ErrorMessage;

  NodeManager2 := TLccNodeManager.Create(nil, True);
  ClientServer2 := TLccEthernetClient.Create(nil, NodeManager2);

  NodeManager2.OnNodeAliasIDChanged := @OnNodeManager2AliasChange;
  NodeManager2.OnNodeIDChanged := @OnNodeManager2IDChange;

  ClientServer1.OnLccMessageReceive := @OnNodeManagerReceiveMessage1;
  ClientServer1.OnLccMessageSend := @OnNodeManagerSendMessage1;

  ClientServer2.OnLccMessageReceive := @OnNodeManagerReceiveMessage2;
  ClientServer2.OnLccMessageSend := @OnNodeManagerSendMessage2;

  ClientServer2.OnConnectionStateChange := @OnClientServer2ConnectionChange;
  ClientServer2.OnErrorMessage := @OnClientServer2ErrorMessage;

  NodeManager1.OnAliasMappingChange := @OnAliasMappingChange;

  PageControlThrottle1.Enabled := False;
  PageControlThrottle2.Enabled := False;
  PanelThrottleKeypad1.Enabled := False;
  PanelThrottleKeypad2.Enabled := False;

  PanelConsitEditor1.Height := PanelConsistEditHeader1.Height;

  WorkerMessage := TLccMessage.Create;
end;

procedure TForm1.LabelConsistExpander1Click(Sender: TObject);
begin
  if PanelConsistEditHeader1.Height = PanelConsitEditor1.Height then
  begin
    PanelConsitEditor1.Height := 162;
    LabelConsistExpander1.Caption := '>';
  end else
  begin
    PanelConsitEditor1.Height := PanelConsistEditHeader1.Height;
    LabelConsistExpander1.Caption := 'v';
  end;
end;

procedure TForm1.ScrollBox1Click(Sender: TObject);
begin

end;

procedure TForm1.SpeedButtonForward1Click(Sender: TObject);
begin
 { if Assigned(ControllerNode1) then
  begin
    ControllerNode1.Direction := tdForward;
    SpeedButtonForward1.ImageIndex := 2;
    SpeedButtonReverse1.ImageIndex := -1;
  end;    }
end;

procedure TForm1.SpeedButtonForward2Click(Sender: TObject);
begin
 { if Assigned(ControllerNode2) then
  begin
    ControllerNode2.Direction := tdForward;
    SpeedButtonForward2.ImageIndex := 2;
    SpeedButtonReverse2.ImageIndex := -1;
  end;  }
end;

procedure TForm1.SpeedButtonReverse1Click(Sender: TObject);
begin
 { if Assigned(ControllerNode1) then
  begin
    ControllerNode1.Direction := tdReverse;
    SpeedButtonForward1.ImageIndex := -1;
    SpeedButtonReverse1.ImageIndex := 2;
  end;     }
end;

procedure TForm1.SpeedButtonReverse2Click(Sender: TObject);
begin
 { if Assigned(ControllerNode2) then
  begin
    ControllerNode2.Direction := tdReverse;
    SpeedButtonForward2.ImageIndex := -1;
    SpeedButtonReverse2.ImageIndex := 2;
  end;  }
end;

procedure TForm1.SpeedButtonThrottle2FunctionClick(Sender: TObject);
var
  Value: Word;
begin
{ if Assigned(ControllerNode2) then
  begin
    ControllerNode2.Functions[(Sender as TSpeedButton).Tag] := not ControllerNode2.Functions[(Sender as TSpeedButton).Tag];
    Value := ControllerNode2.Functions[(Sender as TSpeedButton).Tag];
    case (Sender as TSpeedButton).Tag of
       0 : begin if Value = 0 then SpeedButtonFunction12.ImageIndex := 0 else SpeedButtonFunction12.ImageIndex := 1; end;
       1 : begin if Value = 0 then SpeedButtonFunction13.ImageIndex := 0 else SpeedButtonFunction13.ImageIndex := 1; end;
       2 : begin if Value = 0 then SpeedButtonFunction14.ImageIndex := 0 else SpeedButtonFunction14.ImageIndex := 1; end;
       3 : begin if Value = 0 then SpeedButtonFunction15.ImageIndex := 0 else SpeedButtonFunction15.ImageIndex := 1; end;
       4 : begin if Value = 0 then SpeedButtonFunction16.ImageIndex := 0 else SpeedButtonFunction16.ImageIndex := 1; end;
       5 : begin if Value = 0 then SpeedButtonFunction17.ImageIndex := 0 else SpeedButtonFunction17.ImageIndex := 1; end;
       6 : begin if Value = 0 then SpeedButtonFunction18.ImageIndex := 0 else SpeedButtonFunction18.ImageIndex := 1; end;
       7 : begin if Value = 0 then SpeedButtonFunction19.ImageIndex := 0 else SpeedButtonFunction19.ImageIndex := 1; end;
       8 : begin if Value = 0 then SpeedButtonFunction20.ImageIndex := 0 else SpeedButtonFunction20.ImageIndex := 1; end;
       9 : begin if Value = 0 then SpeedButtonFunction21.ImageIndex := 0 else SpeedButtonFunction21.ImageIndex := 1; end;
       10 : begin if Value = 0 then SpeedButtonFunction22.ImageIndex := 0 else SpeedButtonFunction22.ImageIndex := 1; end;
       11 : begin if Value = 0 then SpeedButtonFunction23.ImageIndex := 0 else SpeedButtonFunction23.ImageIndex := 1; end;
    end;
  end; }
end;

procedure TForm1.SpeedButtonThrottle1FunctionClick(Sender: TObject);
var
  Value: Word;
begin
 { if Assigned(ControllerNode1) then
  begin
    ControllerNode1.Functions[(Sender as TSpeedButton).Tag] := not ControllerNode1.Functions[(Sender as TSpeedButton).Tag];
    Value := ControllerNode1.Functions[(Sender as TSpeedButton).Tag];
     case (Sender as TSpeedButton).Tag of
       0 : begin if Value = 0 then SpeedButtonFunction0.ImageIndex := 0 else SpeedButtonFunction0.ImageIndex := 1; end;
       1 : begin if Value = 0 then SpeedButtonFunction1.ImageIndex := 0 else SpeedButtonFunction1.ImageIndex := 1; end;
       2 : begin if Value = 0 then SpeedButtonFunction2.ImageIndex := 0 else SpeedButtonFunction2.ImageIndex := 1; end;
       3 : begin if Value = 0 then SpeedButtonFunction3.ImageIndex := 0 else SpeedButtonFunction3.ImageIndex := 1; end;
       4 : begin if Value = 0 then SpeedButtonFunction4.ImageIndex := 0 else SpeedButtonFunction4.ImageIndex := 1; end;
       5 : begin if Value = 0 then SpeedButtonFunction5.ImageIndex := 0 else SpeedButtonFunction5.ImageIndex := 1; end;
       6 : begin if Value = 0 then SpeedButtonFunction6.ImageIndex := 0 else SpeedButtonFunction6.ImageIndex := 1; end;
       7 : begin if Value = 0 then SpeedButtonFunction7.ImageIndex := 0 else SpeedButtonFunction7.ImageIndex := 1; end;
       8 : begin if Value = 0 then SpeedButtonFunction8.ImageIndex := 0 else SpeedButtonFunction8.ImageIndex := 1; end;
       9 : begin if Value = 0 then SpeedButtonFunction9.ImageIndex := 0 else SpeedButtonFunction9.ImageIndex := 1; end;
       10 : begin if Value = 0 then SpeedButtonFunction10.ImageIndex := 0 else SpeedButtonFunction10.ImageIndex := 1; end;
       11 : begin if Value = 0 then SpeedButtonFunction11.ImageIndex := 0 else SpeedButtonFunction11.ImageIndex := 1; end;
     end;
  end;  }
end;

procedure TForm1.SpeedButtonThrottleAssign1Click(Sender: TObject);
var
  DccAddress: LongInt;
begin
  if Assigned(ControllerNode1) then
  begin
    if ControllerNode1.IsTrainAssigned then
      ReleaseTrain1;
    // We will get a notification callback when the controller is assigned (or refused)
    if TryStrToInt(EditThrottleAddress1.Text, DccAddress) then
      ControllerNode1.AssignTrainByDccAddress(DccAddress, CheckBoxThrottleLongAddress1.Checked, IndexToSpeedStep(RadioGroupThrottleSpeedSteps1.ItemIndex))
    else
      ShowMessage('Invalid Address');
  end;
end;

procedure TForm1.SpeedButtonThrottleAssign2Click(Sender: TObject);
var
  DccAddress: LongInt;
begin
  if Assigned(ControllerNode2) then
  begin
    if ControllerNode2.IsTrainAssigned then
      ReleaseTrain2
    else begin
      // We will get a notification callback when the controller is assigned (or refused)
      if TryStrToInt(EditThrottleAddress2.Text, DccAddress) then
        ControllerNode2.AssignTrainByDccAddress(DccAddress, CheckBoxThrottleLongAddress2.Checked, IndexToSpeedStep(RadioGroupThrottleSpeedSteps2.ItemIndex))
      else
        ShowMessage('Invalid Address');
    end;
  end;
end;

procedure TForm1.ToggleBoxLoggingChange(Sender: TObject);
begin
  if ToggleBoxLogging.Checked then
    FormNetworkLogging.Show
  else
    FormNetworkLogging.Hide;
end;

procedure TForm1.ToggleBoxServerFormChange(Sender: TObject);
begin
  if ToggleBoxServerForm.Checked then
    FormServerInfo.Show
  else
    FormServerInfo.Hide
end;

procedure TForm1.TrackBarThrottle1Change(Sender: TObject);
begin
 { if Assigned(ControllerNode1) then
  begin
    ControllerNode1.Speed := TrackBarThrottle1.Position;
    LabelThrottleSpeed1.Caption := IntToStr(TrackBarThrottle1.Position);
  end;  }
end;

procedure TForm1.TrackBarThrottle2Change(Sender: TObject);
begin
 { if Assigned(ControllerNode2) then
  begin
    ControllerNode2.Speed := TrackBarThrottle2.Position;
    LabelThrottleSpeed2.Caption := IntToStr(TrackBarThrottle2.Position);
  end;   }
end;

procedure TForm1.TreeViewConsistWizard1Deletion(Sender: TObject; Node: TTreeNode);
//var
 // ConsistItem: TDccSearchCriteria;     // JDK
begin
 // ConsistItem := TDccSearchCriteria( Node.Data);
 // FreeAndNil(ConsistItem);
end;

procedure TForm1.TreeViewConsistWizard1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin

end;

procedure TForm1.TreeViewConsistWizard1SelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
 // ConsistItem: TDccSearchCriteria;              // JDK
begin
//  Node := TreeViewConsistWizard1.Selected;
 { if Assigned(Node) then
  begin
  //  ConsistItem := TDccSearchCriteria(Node.Data);
//    if Assigned(ConsistItem) then
    begin
//      CheckBoxConsistAddress1.Checked := ConsistItem.LongAddress;
 //     RadioGroupConstistSpeedStep1.ItemIndex := SpeedStepToIndex(ConsistItem.SpeedStep) - 1;
 //     EditConsistAddress1.Text := IntToStr(ConsistItem.Address);
    end
  end     }
end;

procedure TForm1.TreeViewConsistWizard2Deletion(Sender: TObject; Node: TTreeNode);
//var
//  ConsistItem: TDccSearchCriteria;        // JDK
begin
 // ConsistItem := TDccSearchCriteria( Node.Data);
//  FreeAndNil(ConsistItem);
end;

procedure TForm1.TreeViewConsistWizard2MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if htNowhere in TreeViewConsistWizard2.GetHitTestInfoAt(X, Y) then
    TreeViewConsistWizard2.Selected := nil;
end;

procedure TForm1.TreeViewConsistWizard2SelectionChanged(Sender: TObject);
var
  Node: TTreeNode;
 // ConsistItem: TDccSearchCriteria;     // JDK
begin
  Node := TreeViewConsistWizard2.Selected;
  if Assigned(Node) then
  begin
 //   ConsistItem := TDccSearchCriteria(Node.Data);
 //   if Assigned(ConsistItem) then
    begin
 //     CheckBoxConsistAddress2.Checked := ConsistItem.LongAddress;
//      RadioGroupConstistSpeedStep2.ItemIndex := SpeedStepToIndex(ConsistItem.SpeedStep) - 1;
 //     EditConsistAddress2.Text := IntToStr(ConsistItem.Address);
    end
  end
end;

procedure TForm1.OnNodeManagerSendMessage1(Sender: TObject; LccMessage: TLccMessage);
begin
  if ToggleBoxLogging.Checked then
  begin
    FormNetworkLogging.MemoLogging1.Lines.BeginUpdate;
    try
      FormNetworkLogging.MemoLogging1.Lines.Add('S: ' + MessageToDetailedMessage(LccMessage));
      FormNetworkLogging.MemoLogging1.SelStart := Length(FormNetworkLogging.MemoLogging1.Lines.Text);
    finally
      FormNetworkLogging.MemoLogging1.Lines.EndUpdate;
    end;
  end;
end;

procedure TForm1.OnNodeManagerReceiveMessage1(Sender: TObject; LccMessage: TLccMessage);
begin
  if ToggleBoxLogging.Checked then
  begin
    FormNetworkLogging.MemoLogging1.Lines.BeginUpdate;
    try
      FormNetworkLogging.MemoLogging1.Lines.Add('R: ' + MessageToDetailedMessage(LccMessage));
      FormNetworkLogging.MemoLogging1.SelStart := Length(FormNetworkLogging.MemoLogging1.Lines.Text);
    finally
      FormNetworkLogging.MemoLogging1.Lines.EndUpdate;
    end;
  end;
end;

procedure TForm1.OnNodeManagerSendMessage2(Sender: TObject; LccMessage: TLccMessage);
begin
  if ToggleBoxLogging.Checked then
  begin
    FormNetworkLogging.MemoLogging2.Lines.BeginUpdate;
    try
      FormNetworkLogging.MemoLogging2.Lines.Add('S: ' + MessageToDetailedMessage(LccMessage));
      FormNetworkLogging.MemoLogging2.SelStart := Length(FormNetworkLogging.MemoLogging2.Lines.Text);
    finally
      FormNetworkLogging.MemoLogging2.Lines.EndUpdate;
    end;
  end;
end;

procedure TForm1.OnNodeManagerReceiveMessage2(Sender: TObject; LccMessage: TLccMessage);
begin
  if ToggleBoxLogging.Checked then
  begin
    FormNetworkLogging.MemoLogging1.Lines.BeginUpdate;
    try
      FormNetworkLogging.MemoLogging1.Lines.Add('R: ' + MessageToDetailedMessage(LccMessage));
      FormNetworkLogging.MemoLogging1.SelStart := Length(FormNetworkLogging.MemoLogging1.Lines.Text);
    finally
      FormNetworkLogging.MemoLogging1.Lines.EndUpdate;
    end;
  end;
end;

procedure TForm1.OnClientServer1ConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  case (Info as TLccEthernetConnectionInfo).ConnectionState of
    lcsConnecting : StatusBarThrottle1.Panels[0].Text    := 'Connecting';
    lcsConnected  :
      begin
        ButtonConnect1.Caption := 'Disconnect';
        StatusBarThrottle1.Panels[0].Text := 'IP Address: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
        if NodeManager1.Nodes.Count = 0 then
          ControllerNode1 := NodeManager1.AddNodeByClass('', TLccTrainController, True, NULL_NODE_ID) as TLccTrainController;
        if Assigned(ControllerNode1) then
        begin
          ControllerNode1.TractionServer.OnRegisterChange := @OnTractionRegisterChange

        end;

     //   ControllerNode1.TractionServer.OnControllerAssign := @ControllerTrainAssigned1;
     //   ControllerNode1.TractionServer.OnControllerRelease := @ControllerTrainReleased1;
     {   ControllerNode1.OnControllerRequestTakeover := @OnControllerReqestTakeover1;
        ControllerNode1.OnQuerySpeedReply := @OnControllerQuerySpeedReply1;
        ControllerNode1.OnQueryFunctionReply := @OnControllerQueryFunctionReply1;
        ControllerNode1.OnSearchResult := @OnControllerSearchResult1;
        ControllerNode1.OnSearchMultiResult := @OnControllerSearchMultiResult1;
        ControllerNode1.OnAttachListenerReply := @OnControllerAttachListenerReply1;
        ControllerNode1.OnDetachListenerReply := @OnControllerDetachListenerReply1;
        ControllerNode1.OnQueryListenerGetCount := @OnControllerQueryListenerGetCount1;
        ControllerNode1.OnQueryListenerIndex := @OnControllerQueryListenerIndex1;
        ControllerNode1.OnQueryConfigurationReply := @OnQueryConfigurationReply1;
        PageControlThrottle1.Enabled := True;    }
      end;
    lcsDisconnecting :
      begin
        StatusBarThrottle1.Panels[0].Text := 'Disconnecting';
        FormServerInfo.TreeViewAliasMaps.Items.Clear;
        FormServerInfo.TreeViewTrains.Items.Clear;
      end;
    lcsDisconnected :
      begin
        StatusBarThrottle1.Panels[0].Text := 'Disconnected';
        ButtonConnect1.Caption := 'Connect';
        LabelAlias1.Caption := 'None';
        LabelNodeID1.Caption := 'None';
        PageControlThrottle1.Enabled := False;
      end;
  end;
end;

procedure TForm1.OnClientServer1ErrorMessage(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  ShowMessage(Info.MessageStr);
  NodeManager1.Clear;
  StatusBarThrottle1.Panels[0].Text := 'Disconnected';
  ButtonConnect1.Caption := 'Connect';
end;

procedure TForm1.OnClientServer2ConnectionChange(Sender: TObject; Info: TLccHardwareConnectionInfo);
begin
  if Sender is TLccConnectionThread then
  begin
    case (Info as TLccEthernetConnectionInfo).ConnectionState of
      lcsConnecting : StatusBarThrottle2.Panels[0].Text    := 'Connecting';
      lcsConnected  :
        begin
          ButtonConnect2.Caption := 'Disconnect';
          StatusBarThrottle2.Panels[0].Text := 'IP Address: ' + (Info as TLccEthernetConnectionInfo).ClientIP + ':' + IntToStr((Info as TLccEthernetConnectionInfo).ClientPort);
          if NodeManager2.Nodes.Count = 0 then
            ControllerNode2 := NodeManager2.AddNodeByClass('', TLccTrainController, True, NULL_NODE_ID) as TLccTrainController;
          if Assigned(NodeManager1) then
          begin
            ControllerNode2.TractionServer.OnRegisterChange := @OnTractionRegisterChange
          end;
          PageControlThrottle2.Enabled := True;
        end;
      lcsDisconnecting :
        begin
          StatusBarThrottle2.Panels[0].Text := 'Disconnecting';
        end;
      lcsDisconnected :
        begin
          StatusBarThrottle2.Panels[0].Text := 'Disconnected';
          ButtonConnect2.Caption := 'Connect';
          LabelAlias2.Caption := 'None';
          LabelNodeID2.Caption := 'None';
          PageControlThrottle2.Enabled := False;
        end;
    end;
  end;
end;

procedure TForm1.OnClientServer2ErrorMessage(Sender: TObject;
  Info: TLccHardwareConnectionInfo);
begin
  ShowMessage(Info.MessageStr);
  NodeManager2.Clear;
  StatusBarThrottle2.Panels[0].Text := 'Disconnected';
  ButtonConnect2.Caption := 'Connect';
end;

procedure TForm1.OnQueryConfigurationReply1(Sender: TLccTrainController;
  ConfigMemAddress: LongWord; Value: LongWord; ErrorCode: Byte; ErrorMsg: string
  );
begin
  if ErrorCode = S_OK then
  begin
    case ConfigMemAddress of
      0 : Edit1.Caption:=IntToStr(Value);
      1 : Edit2.Caption:=IntToStr(Value);
      2 : Edit3.Caption:=IntToStr(Value);
      3 : Edit4.Caption:=IntToStr(Value);
      4 : Edit5.Caption:=IntToStr(Value);
      5 : Edit6.Caption:=IntToStr(Value);
      6 : Edit7.Caption:=IntToStr(Value);
      7 : Edit8.Caption:=IntToStr(Value);
    end;
  end else
    ShowMessage('Configuration Memoery Read Failure: Error Code: ' + IntToStr(ErrorCode) + ' Message: ' + ErrorMsg);
end;

procedure TForm1.OnQueryConfigurationReply2(Sender: TLccTrainController;
  ConfigMemAddress: LongWord; Value: LongWord; ErrorCode: Byte; ErrorMsg: string
  );
begin

end;

procedure TForm1.OnMemoryConfigWrite1(Sender: TLccTrainController;
  ConfigMemAddress: LongWord; Value: LongWord; ErrorCode: Byte; ErrorMsg: string);
begin

end;

procedure TForm1.OnMemoryConfigWrite2(Sender: TLccTrainController;
  ConfigMemAddress: LongWord; Value: LongWord; ErrorCode: Byte; ErrorMsg: string);
begin

end;

procedure TForm1.ControllerTrainAssigned1(Sender: TLccTrainController);
begin
  if Sender.AssignedTrain.IsAssigned then
  begin
  //  ControllerNode1.QuerySpeed;
 //   ControllerNode1.QueryFunctions;
    PanelThrottleKeypad1.Enabled := True;
    SpeedButtonThrottleAssign1.Caption := 'Release Train';
  end;
end;

procedure TForm1.ControllerTrainAssigned2(Sender: TLccTrainController);
begin
{  case Reason of
    tarAssigned :
      begin
        ControllerNode2.QuerySpeed;
        ControllerNode2.QueryFunctions;
        PanelThrottleKeypad2.Enabled := True;
        SpeedButtonThrottleAssign2.Caption := 'Release Train';
      end;
    tarFailTrainRefused      : ShowMessage('Train refused assignment to controller');
    tarFailControllerRefused : ShowMessage('Current controller refused to release train');
  else
    ShowMessage('Unknown ControllerTrainAssigned2 result');
  end;    }
end;

procedure TForm1.ControllerTrainReleased1(Sender: TLccTrainController);
begin
  PanelThrottleKeypad1.Enabled := False;
  SpeedButtonThrottleAssign1.Caption := 'Allocate Train';
end;

procedure TForm1.ControllerTrainReleased2(Sender: TLccTrainController);
begin
  PanelThrottleKeypad2.Enabled := False;
  SpeedButtonThrottleAssign2.Caption := 'Allocate Train';
end;

procedure TForm1.OnTrainServerFunctionChange1(TractionObject: TLccTractionObject);
begin
  if EqualNodeID(ControllerNode1.NodeID, TractionObject.Controller.Node, false) then
  begin
    if TractionObject.Functions[0] = 0 then SpeedButtonFunction0.ImageIndex := 0 else SpeedButtonFunction0.ImageIndex := 1;
    if TractionObject.Functions[1] = 0 then SpeedButtonFunction1.ImageIndex := 0 else SpeedButtonFunction1.ImageIndex := 1;
    if TractionObject.Functions[2] = 0 then SpeedButtonFunction2.ImageIndex := 0 else SpeedButtonFunction2.ImageIndex := 1;
    if TractionObject.Functions[3] = 0 then SpeedButtonFunction3.ImageIndex := 0 else SpeedButtonFunction3.ImageIndex := 1;
    if TractionObject.Functions[4] = 0 then SpeedButtonFunction4.ImageIndex := 0 else SpeedButtonFunction4.ImageIndex := 1;
    if TractionObject.Functions[5] = 0 then SpeedButtonFunction5.ImageIndex := 0 else SpeedButtonFunction5.ImageIndex := 1;
    if TractionObject.Functions[6] = 0 then SpeedButtonFunction6.ImageIndex := 0 else SpeedButtonFunction6.ImageIndex := 1;
    if TractionObject.Functions[7] = 0 then SpeedButtonFunction7.ImageIndex := 0 else SpeedButtonFunction7.ImageIndex := 1;
    if TractionObject.Functions[8] = 0 then SpeedButtonFunction8.ImageIndex := 0 else SpeedButtonFunction8.ImageIndex := 1;
    if TractionObject.Functions[9] = 0 then SpeedButtonFunction9.ImageIndex := 0 else SpeedButtonFunction9.ImageIndex := 1;
    if TractionObject.Functions[10] = 0 then SpeedButtonFunction10.ImageIndex := 0 else SpeedButtonFunction10.ImageIndex := 1;
    if TractionObject.Functions[11] = 0 then SpeedButtonFunction11.ImageIndex := 0 else SpeedButtonFunction11.ImageIndex := 1;
  end;
end;

procedure TForm1.OnTrainServerFunctionChange2(TractionObject: TLccTractionObject);
begin
  if EqualNodeID(ControllerNode2.NodeID, TractionObject.Controller.Node, false) then
  begin
    if TractionObject.Functions[0] = 0 then SpeedButtonFunction12.ImageIndex := 0 else SpeedButtonFunction0.ImageIndex := 1;
    if TractionObject.Functions[1] = 0 then SpeedButtonFunction13.ImageIndex := 0 else SpeedButtonFunction1.ImageIndex := 1;
    if TractionObject.Functions[2] = 0 then SpeedButtonFunction14.ImageIndex := 0 else SpeedButtonFunction2.ImageIndex := 1;
    if TractionObject.Functions[3] = 0 then SpeedButtonFunction15.ImageIndex := 0 else SpeedButtonFunction3.ImageIndex := 1;
    if TractionObject.Functions[4] = 0 then SpeedButtonFunction16.ImageIndex := 0 else SpeedButtonFunction4.ImageIndex := 1;
    if TractionObject.Functions[5] = 0 then SpeedButtonFunction17.ImageIndex := 0 else SpeedButtonFunction5.ImageIndex := 1;
    if TractionObject.Functions[6] = 0 then SpeedButtonFunction18.ImageIndex := 0 else SpeedButtonFunction6.ImageIndex := 1;
    if TractionObject.Functions[7] = 0 then SpeedButtonFunction19.ImageIndex := 0 else SpeedButtonFunction7.ImageIndex := 1;
    if TractionObject.Functions[8] = 0 then SpeedButtonFunction20.ImageIndex := 0 else SpeedButtonFunction8.ImageIndex := 1;
    if TractionObject.Functions[9] = 0 then SpeedButtonFunction21.ImageIndex := 0 else SpeedButtonFunction9.ImageIndex := 1;
    if TractionObject.Functions[10] = 0 then SpeedButtonFunction22.ImageIndex := 0 else SpeedButtonFunction10.ImageIndex := 1;
    if TractionObject.Functions[11] = 0 then SpeedButtonFunction23.ImageIndex := 0 else SpeedButtonFunction11.ImageIndex := 1;
  end;
end;

procedure TForm1.OnTrainServerSpeedChange1(TractionObject: TLccTractionObject);
begin
  if EqualNodeID(ControllerNode1.NodeID, TractionObject.Controller.Node, false) then
  begin
    TrackBarThrottle1.Position := Abs( Round(HalfToFloat(TractionObject.SpeedSet)));

    if HalfIsNegative(TractionObject.SpeedSet) then
    begin
      SpeedButtonForward1.ImageIndex := -1;
      SpeedButtonReverse1.ImageIndex := 2;
    end else
    begin
      SpeedButtonForward1.ImageIndex := 2;
      SpeedButtonReverse1.ImageIndex := -1;
    end;
  end;
end;

procedure TForm1.OnTrainServerSpeedChange2(TractionObject: TLccTractionObject);
begin
  if EqualNodeID(ControllerNode2.NodeID, TractionObject.Controller.Node, false) then
  begin
    TrackBarThrottle1.Position := Abs( Round(HalfToFloat(TractionObject.SpeedSet)));

    if HalfIsNegative(TractionObject.SpeedSet) then
    begin
      SpeedButtonForward1.ImageIndex := -1;
      SpeedButtonReverse1.ImageIndex := 2;
    end else
    begin
      SpeedButtonForward1.ImageIndex := 2;
      SpeedButtonReverse1.ImageIndex := -1;
    end;
  end;
end;

procedure TForm1.OnControllerReqestTakeover1(Sender: TLccTrainController;
  var Allow: Boolean);
begin
{  if CheckBoxThrottleTakeover1.Checked then
    Allow :=  FormThrottleTakeover.ShowModal = mrYes;
  if Allow then
    ReleaseTrain1;  }
end;

procedure TForm1.OnControllerReqestTakeover2(Sender: TLccTrainController;
  var Allow: Boolean);
begin
{  if CheckBoxThrottleTakeover2.Checked then
    Allow :=  FormThrottleTakeover.ShowModal = mrYes;
  if Allow then
    ReleaseTrain2;  }
end;
{
procedure TForm1.OnControllerSearchMultiResult1(Sender: TLccTrainController;
  Trains: TLccActionTrainInfoList);
begin
  if Trains.Count > 1 then
  begin
  end;
  ShowMessage('Found Trains: ' + IntToStr(Trains.Count));
end;

procedure TForm1.OnControllerSearchMultiResult2(Sender: TLccTrainController;
  Trains: TLccActionTrainInfoList);
begin
  ShowMessage('Found Trains: ' + IntToStr(Trains.Count));
end;

procedure TForm1.OnControllerSearchResult1(Sender: TLccTrainController;
  TrainList: TLccActionTrainInfoList; var SelectedResultIndex: Integer);
begin
  SelectedResultIndex := 0;
  case TrainList.Count of
   0: ShowMessage('No Search Results');
   1: SelectedResultIndex := 0;
  else
    ShowMessage('Multiple Search Results: Please Select');
  end;
end;

procedure TForm1.OnControllerSearchResult2(Sender: TLccTrainController;
  TrainList: TLccActionTrainInfoList; var SelectedResultIndex: Integer);
begin
  SelectedResultIndex := 0;
  case TrainList.Count of
   0: ShowMessage('No Search Results');
   1: SelectedResultIndex := 0;
  else
    ShowMessage('Multiple Search Results: Please Select');
  end;
end;

procedure TForm1.OnControllerAttachListenerReply1(Sender: TLccTrainController;
  Listener: TLccActionTrainInfo; ReplyCode: Word);
begin

end;

procedure TForm1.OnControllerAttachListenerReply2(Sender: TLccTrainController;
  Listener: TLccActionTrainInfo; ReplyCode: Word);
begin

end;

procedure TForm1.OnControllerDetachListenerReply1(Sender: TLccTrainController;
  Listener: TLccActionTrainInfo; ReplyCode: Word);
begin

end;

procedure TForm1.OnControllerDetachListenerReply2(Sender: TLccTrainController;
  Listener: TLccActionTrainInfo; ReplyCode: Word);
begin

end;
       }
procedure TForm1.OnControllerQueryListenerGetCount1(
  Sender: TLccTrainController; ListenerCount: Byte);
begin

end;

procedure TForm1.OnControllerQueryListenerGetCount2(
  Sender: TLccTrainController; ListenerCount: Byte);
begin

end;

procedure TForm1.OnControllerQueryListenerIndex1(Sender: TLccTrainController;
  ListenerCount, ListenerIndex: Byte; ListenerFlags: Byte;
  ListenerNodeID: TNodeID);
begin

end;

procedure TForm1.OnControllerQueryListenerIndex2(Sender: TLccTrainController;
  ListenerCount, ListenerIndex: Byte; ListenerFlags: Byte;
  ListenerNodeID: TNodeID);
begin

end;

procedure TForm1.ReleaseTrain1;
begin
  if Assigned(ControllerNode1) then
    ControllerNode1.ReleaseTrain;
end;

procedure TForm1.ReleaseTrain2;
begin
  if Assigned(ControllerNode2) then
    ControllerNode2.ReleaseTrain;
end;

procedure TForm1.OnAliasMappingChange(Sender: TObject; LccSourceNode: TLccNode;
  AnAliasMapping: TLccAliasMapping; IsMapped: Boolean);
begin
  if IsMapped then
    FormServerInfo.AddAliasMap(AnAliasMapping)
  else
    FormServerInfo.RemoveAliasMap(AnAliasMapping);
end;

procedure TForm1.OnTractionRegisterChange(TractionObject: TLccTractionObject; IsRegistered: Boolean);
begin
  if IsRegistered then
    FormServerInfo.AddTrainObject(TractionObject)
  else
    FormServerInfo.RemoveTrainObject(TractionObject);
end;

procedure TForm1.OnLccTractionUpdateSNIP1(Sender: TObject;
  LccSourceNode: TLccNode; Index: Integer);
begin

end;

procedure TForm1.OnLccTractionUpdateTrainSNIP1(Sender: TObject;
  LccSourceNode: TLccNode; Index: Integer);
begin

end;

procedure TForm1.OnLccTractionUpdateListenerCount1(Sender: TObject;
  LccSourceNode: TLccNode; Index: Integer);
begin

end;

procedure TForm1.SpeedButtonConsistSubtract1Click(Sender: TObject);
begin

end;

procedure TForm1.SpeedButtonConsistSubtract2Click(Sender: TObject);
var
  TreeNode: TTreeNode;
begin
  TreeNode := TreeViewConsistWizard2.Selected;
  if Assigned(TreeNode) then
    TreeViewConsistWizard2.Items.Delete(TreeNode); // Object in Data will be free in the deletion callback event
end;

procedure TForm1.SpeedButtonConsistTrainAdd1Click(Sender: TObject);
var
  SelectedNode: TTreeNode;
//  ConsistItem: TDccSearchCriteria;      // JDK
  NodeCaption: string;
  DccAddress: Integer;
begin
 { if TryStrToInt(EditConsistAddress1.Text, DccAddress) then
  begin
    if Assigned(TreeViewConsistWizard1.Selected) then
    begin
      SelectedNode := TreeViewConsistWizard1.Selected;
  //    ConsistItem := TDccSearchCriteria(SelectedNode.Data);

      NodeCaption := EditConsistAddress1.Text + ': ' + AddressBooleanToText(ConsistItem.LongAddress, Verbose) + ',' + SpeedStepToString(IndexToSpeedStep(RadioGroupConstistSpeedStep1.ItemIndex + 1), Verbose);

      if TreeViewConsistWizard1.Items.FindNodeWithText(NodeCaption) = nil then // No duplicates
      begin
        SelectedNode.Text := NodeCaption;
        ConsistItem.Address := DccAddress;
        ConsistItem.SpeedStep := IndexToSpeedStep(RadioGroupConstistSpeedStep1.ItemIndex + 1);
        ConsistItem.LongAddress := CheckBoxConsistAddress1.Checked;

        SelectedNode.Update;
        TreeViewConsistWizard1.Selected := nil;
        EditConsistAddress1.SetFocus;
        EditConsistAddress1.SelectAll;
      end else
        ShowMessage('Duplicate Train');
    end else
    begin
      ConsistItem := TDccSearchCriteria.Create('', DccAddress, IndexToSpeedStep(RadioGroupConstistSpeedStep1.ItemIndex + 1), CheckBoxConsistAddress1.Checked);
      NodeCaption := EditConsistAddress1.Text + ': ' + AddressBooleanToText(ConsistItem.LongAddress, Verbose) + ',' + SpeedStepToString(IndexToSpeedStep(RadioGroupConstistSpeedStep1.ItemIndex + 1), Verbose);

      if TreeViewConsistWizard1.Items.FindNodeWithText(NodeCaption) = nil then // No duplicates
      begin
        SelectedNode := TreeViewConsistWizard1.Items.AddChild(nil, NodeCaption);
        TreeViewConsistWizard1.Selected := SelectedNode;
        SelectedNode.Data := ConsistItem;
        TreeViewConsistWizard1.Selected := nil;
        EditConsistAddress1.SetFocus;
        EditConsistAddress1.SelectAll;
      end else
        ShowMessage('Duplicate Train');
    end
  end else
    ShowMessage('Enter a valid DCC Address');    }
end;

procedure TForm1.SpeedButtonConstistTrainAdd2Click(Sender: TObject);
var
  SelectedNode: TTreeNode;
//  ConsistItem: TDccSearchCriteria;    // JDK
  NodeCaption: string;
  DccAddress: Integer;
begin
 { if TryStrToInt(EditConsistAddress2.Text, DccAddress) then
  begin
    if Assigned(TreeViewConsistWizard2.Selected) then
    begin
      SelectedNode := TreeViewConsistWizard2.Selected;
      ConsistItem := TDccSearchCriteria(SelectedNode.Data);

      NodeCaption := EditConsistAddress2.Text + ': ' + AddressBooleanToText(ConsistItem.LongAddress, Verbose) + ',' + SpeedStepToString(IndexToSpeedStep(RadioGroupConstistSpeedStep2.ItemIndex + 1), Verbose);

      if TreeViewConsistWizard2.Items.FindNodeWithText(NodeCaption) = nil then // No duplicates
      begin
        SelectedNode.Text := NodeCaption;
        ConsistItem.Address := DccAddress;
        ConsistItem.SpeedStep := IndexToSpeedStep(RadioGroupConstistSpeedStep2.ItemIndex + 1);
        ConsistItem.LongAddress := CheckBoxConsistAddress2.Checked;

        SelectedNode.Update;
        TreeViewConsistWizard2.Selected := nil;
        EditConsistAddress2.SetFocus;
        EditConsistAddress2.SelectAll;
      end else
        ShowMessage('Duplicate Train');
    end else
    begin
      ConsistItem := TDccSearchCriteria.Create('', DccAddress, IndexToSpeedStep(RadioGroupConstistSpeedStep2.ItemIndex + 1), CheckBoxConsistAddress2.Checked);
      NodeCaption := EditConsistAddress2.Text + ': ' + AddressBooleanToText(ConsistItem.LongAddress, Verbose) + ',' + SpeedStepToString(IndexToSpeedStep(RadioGroupConstistSpeedStep2.ItemIndex + 1), Verbose);

      if TreeViewConsistWizard2.Items.FindNodeWithText(NodeCaption) = nil then // No duplicates
      begin
        SelectedNode := TreeViewConsistWizard2.Items.AddChild(nil, NodeCaption);
        TreeViewConsistWizard2.Selected := SelectedNode;
        SelectedNode.Data := ConsistItem;
        TreeViewConsistWizard2.Selected := nil;
        EditConsistAddress2.SetFocus;
        EditConsistAddress2.SelectAll;
      end else
        ShowMessage('Duplicate Train');
    end
  end else
    ShowMessage('Enter a valid DCC Address');    }
end;

procedure TForm1.OnNodeManager1AliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelAlias1.Caption := 'NodeID: ' + LccSourceNode.AliasIDStr;
end;

procedure TForm1.OnNodeManager1IDChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelNodeID1.Caption := 'NodeID: ' + LccSourceNode.NodeIDStr[True];
end;


procedure TForm1.OnNodeManager2AliasChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelAlias2.Caption := 'NodeID: ' + LccSourceNode.AliasIDStr;
end;

procedure TForm1.OnNodeManager2IDChange(Sender: TObject; LccSourceNode: TLccNode);
begin
  LabelNodeID2.Caption := 'NodeID: ' + LccSourceNode.NodeIDStr[True];
end;

end.

