unit unitconsistselectorpanel;

{$mode ObjFPC}{$H+}

interface

uses
  Classes,
  SysUtils,
  ExtCtrls,
  contnrs,
  stdctrls,
  controls,
  lcc_defines,
  lcc_utilities,
  lcc_node,
  lcc_node_controller;


type
  TConsistSelectorPanel = class;

  { TConsistSelectorItem }

  TConsistSelectorItem = class(TPanel)
  private
    FButtonRemove: TButton;
    FCheckBoxF0Forward: TCheckBox;
    FCheckBoxFnForward: TCheckBox;
    FCheckBoxReverseDir: TCheckBox;
    FComboBoxEntry: TComboBox;
    FController: TLccTrainController;
    FNodeID: TNodeID;
    FOwnerPanel: TConsistSelectorPanel;
  protected
    property ComboBoxEntry: TComboBox read FComboBoxEntry write FComboBoxEntry;
    property CheckBoxReverseDir: TCheckBox read FCheckBoxReverseDir write FCheckBoxReverseDir;
    property CheckBoxF0Forward: TCheckBox read FCheckBoxF0Forward write FCheckBoxF0Forward;
    property CheckBoxFnForward: TCheckBox read FCheckBoxFnForward write FCheckBoxFnForward;
    property ButtonRemove: TButton read FButtonRemove write FButtonRemove;
    property NodeID: TNodeID read FNodeID write FNodeID;
    property Controller: TLccTrainController read FController write FController;
    property OwnerPanel: TConsistSelectorPanel read FOwnerPanel write FOwnerPanel;

    procedure ButtonRemoveClick(Sender: TObject);
    procedure CheckBoxClick(Sender: TObject);

    procedure CallbackListenerAttach(ATask: TLccTaskBase);
    procedure CallbackListenerDetach(ATask: TLccTaskBase);
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  { TConsistSelectorPanel }

  TConsistSelectorPanel = class(TPanel)
  private
    FController: TLccTrainController;
    FiListener: Byte;
    FListenerCount: Byte;
    FTractionNodeID: TNodeID;
    FSelectorItems: TObjectList;
  protected
    property SelectorItems: TObjectList read FSelectorItems write FSelectorItems;

    property ListenerCount: Byte read FListenerCount write FListenerCount;
    property iListener: Byte read FiListener write FiListener;
    property TractionNodeID: TNodeID read FTractionNodeID write FTractionNodeID;

    procedure CallbackListenerEnumerate(ATask: TLccTaskBase);
  public
    property Controller: TLccTrainController read FController write FController;


    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;

    function Add: TConsistSelectorItem;
    procedure Clear;
    procedure Initialize(ANodeID: TNodeID);
  end;

implementation

{ TConsistSelectorPanel }

procedure TConsistSelectorPanel.CallbackListenerEnumerate(ATask: TLccTaskBase);

  procedure AddListenerItem(Task: TLccTaskListenersEnumerate);
  var
    ListenerItem: TConsistSelectorItem;
    i: Integer;
  begin
    if Task.TrainListener.Hidden then Exit;

    ListenerItem := Add;

    ListenerItem.CheckBoxReverseDir.Checked := Task.TrainListener.ReverseDir;
    ListenerItem.CheckBoxF0Forward.Checked := Task.TrainListener.F0Forward;
    ListenerItem.CheckBoxFnForward.Checked := Task.TrainListener.FnForward;

    ListenerItem.ComboBoxEntry.Items.BeginUpdate;
    try
      ListenerItem.ComboBoxEntry.Clear;
      for i := 0 to Controller.TrainRoster.Count - 1 do
      begin
        if not EqualNodeID(Controller.TrainRoster[i].NodeID, TractionNodeID, False) then     // Do not show the node that is the parent
        begin
          ListenerItem.ComboBoxEntry.AddItem(Controller.TrainRoster[i].UserName, Controller.TrainRoster[i]);
          if EqualNodeID(Controller.TrainRoster[i].NodeID, Task.TrainListener.NodeID, False) then
            ListenerItem.ComboBoxEntry.ItemIndex  := ListenerItem.ComboBoxEntry.Items.Count - 1;
        end;
      end;

    finally
      ListenerItem.ComboBoxEntry.Items.EndUpdate;
    end;

    ListenerItem.NodeID := Task.TrainListener.NodeID;      // If the item is carrying the TrainInfo from above likely don't need this...
    ListenerItem.OwnerPanel := Self;
    ListenerItem.Controller := Controller;  // Important to do last so we don't fire off ListenerAssign as the checkboxes are set above
  end;

var
  TaskListenerEnumerate: TLccTaskListenersEnumerate;
begin
  TaskListenerEnumerate := ATask as TLccTaskListenersEnumerate;

  case ATask.TaskState of
    lesComplete :
      begin
        AddListenerItem(TaskListenerEnumerate)
      end;
    lesRunning :
      begin
         AddListenerItem(TaskListenerEnumerate)
      end;
    lesAbort   : begin end;
    lesTimeout : begin end;
    lesError   : begin end;
  end;
end;

constructor TConsistSelectorPanel.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  SelectorItems := TObjectList.Create(False);
  Align := alClient
end;

destructor TConsistSelectorPanel.Destroy;
begin
  FreeAndNil(FSelectorItems);
  inherited Destroy;
end;

function TConsistSelectorPanel.Add: TConsistSelectorItem;
var
  i: Integer;
  OldItem: TObject;
begin
  for i := 0 to SelectorItems.Count - 1 do
  begin
    if not (SelectorItems[i] as TConsistSelectorItem).Visible then
    begin
      OldItem :=SelectorItems[i];
      SelectorItems.Delete(i);
      OldItem.Free;
    end;
  end;

  Result := TConsistSelectorItem.Create(Self);
  Result.Top := $FFFF;
  Result.Parent := Self;
  SelectorItems.Add(Result);
  Height := Height + Result.Height;
end;

procedure TConsistSelectorPanel.Clear;
var
  i: Integer;
begin
  try
    for i := 0 to SelectorItems.Count - 1 do
      SelectorItems[i].Free;
  finally
    SelectorItems.Clear;
    Height := 0;
  end;
end;

procedure TConsistSelectorPanel.Initialize(ANodeID: TNodeID);
begin
  if not Assigned(Controller) then Exit;

  TractionNodeID := ANodeID;
  Controller.ListenerEnumerate(ANodeID, @CallbackListenerEnumerate);

end;


{ TConsistSelectorItem }

procedure TConsistSelectorItem.ButtonRemoveClick(Sender: TObject);
begin
  if not Assigned(Controller) then Exit;

  Controller.ListenerDetach(OwnerPanel.TractionNodeID, NodeID, @CallbackListenerDetach);
//  Controller.ListenerDetach(NodeIDOwnerPanel.TractionNodeID, @CallbackListenerDetach);
end;

procedure TConsistSelectorItem.CheckBoxClick(Sender: TObject);
begin
  if not Assigned(Controller) then Exit;

  Controller.ListenerAttach(OwnerPanel.TractionNodeID, NodeID, CheckBoxReverseDir.Checked, CheckBoxF0Forward.Checked, CheckBoxFnForward.Checked, False, @CallbackListenerAttach);
end;

procedure TConsistSelectorItem.CallbackListenerAttach(ATask: TLccTaskBase);
var
  TaskListenerAttach: TLccTaskListenerAttach;
begin
  TaskListenerAttach := ATask as TLccTaskListenerAttach;

  case ATask.TaskState of
    lesComplete :
      begin

      end;
    lesAbort   : begin end;
    lesTimeout : begin end;
    lesError   : begin end;
  end;
end;

procedure TConsistSelectorItem.CallbackListenerDetach(ATask: TLccTaskBase);
var
  TaskListenerAttach: TLccTaskListenerAttach;
begin
  TaskListenerAttach := ATask as TLccTaskListenerAttach;

  case ATask.TaskState of
    lesComplete :
      begin
        Visible := False;
      end;
    lesAbort   : begin end;
    lesTimeout : begin end;
    lesError   : begin end;
  end;
end;

constructor TConsistSelectorItem.Create(TheOwner: TComponent);
const
  CHECKBOX_INDENT = 16;
begin
  inherited Create(TheOwner);

  ComboBoxEntry := TComboBox.Create(Self);
  ComboBoxEntry.Parent := Self;
  ComboBoxEntry.Top := 4;
  ComboBoxEntry.Left := 4;
  ComboBoxEntry.Width := 160;

  CheckBoxReverseDir := TCheckBox.Create(Self);
  CheckBoxReverseDir.Parent := Self;
  CheckBoxReverseDir.Caption := 'Reverse Direction';
  CheckBoxReverseDir.Top := ComboBoxEntry.Top + ComboBoxEntry.Height + 4;
  CheckBoxReverseDir.Left := CHECKBOX_INDENT;
  CheckBoxReverseDir.Tag := 0;
  CheckBoxReverseDir.OnClick := @CheckBoxClick;

  CheckBoxF0Forward := TCheckBox.Create(Self);
  CheckBoxF0Forward.Parent := Self;
  CheckBoxF0Forward.Caption := 'Forward F0 (lights)';
  CheckBoxF0Forward.Top := CheckBoxReverseDir.Top + CheckBoxReverseDir.Height + 4;
  CheckBoxF0Forward.Left := CHECKBOX_INDENT;
  CheckBoxF0Forward.Tag := 1;
  CheckBoxF0Forward.OnClick := @CheckBoxClick;

  CheckBoxFnForward := TCheckBox.Create(Self);
  CheckBoxFnForward.Parent := Self;
  CheckBoxFnForward.Caption := 'Foward F1 and up';
  CheckBoxFnForward.Top := CheckBoxF0Forward.Top + CheckBoxF0Forward.Height + 4;
  CheckBoxFnForward.Left := CHECKBOX_INDENT;
  CheckBoxFnForward.Tag := 2;
  CheckBoxFnForward.OnClick := @CheckBoxClick;

  ButtonRemove := TButton.Create(Self);
  ButtonRemove.Parent := Self;
  ButtonRemove.Caption := 'Remove';
  ButtonRemove.Top := 2;
  ButtonRemove.Left := Self.Width - ButtonRemove.Width - 4;
  ButtonRemove.Anchors := [akTop, akRight];
  ButtonRemove.OnClick := @ButtonRemoveClick;

  Height := CheckBoxFnForward.Top + CheckBoxFnForward.Height + 4;

  Align := alTop;
end;

destructor TConsistSelectorItem.Destroy;
begin
  inherited Destroy;
end;

end.

