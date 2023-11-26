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

  { TConsistSelectorItem }

  TConsistSelectorItem = class(TPanel)
  private
    FButtonRemove: TButton;
    FCheckBoxF0Forward: TCheckBox;
    FCheckBoxFnForward: TCheckBox;
    FCheckBoxReverseDir: TCheckBox;
    FComboBoxEntry: TComboBox;
    FNodeID: TNodeID;
  protected
    property ComboBoxEntry: TComboBox read FComboBoxEntry write FComboBoxEntry;
    property CheckBoxReverseDir: TCheckBox read FCheckBoxReverseDir write FCheckBoxReverseDir;
    property CheckBoxF0Forward: TCheckBox read FCheckBoxF0Forward write FCheckBoxF0Forward;
    property CheckBoxFnForward: TCheckBox read FCheckBoxFnForward write FCheckBoxFnForward;
    property ButtonRemove: TButton read FButtonRemove write FButtonRemove;
    property NodeID: TNodeID read FNodeID write FNodeID;

    procedure ButtonRemoveClick(Sender: TObject);
    procedure CheckBoxReverseDirClick(Sender: TObject);
    procedure CheckBoxF0ForwardClick(Sender: TObject);
    procedure CheckBoxFnForwardClick(Sender: TObject);
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
    FSelectorItems: TObjectList;
  protected
    property SelectorItems: TObjectList read FSelectorItems write FSelectorItems;

    property ListenerCount: Byte read FListenerCount write FListenerCount;
    property iListener: Byte read FiListener write FiListener;

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

  procedure AddConsistItem(Task: TLccTaskListenersEnumerate);
  var
    ConsistItem: TConsistSelectorItem;
  begin
    if Task.TrainListener.Hidden then Exit;

    ConsistItem := Add;
    ConsistItem.CheckBoxReverseDir.Checked := Task.TrainListener.ReverseDir;
    ConsistItem.CheckBoxF0Forward.Checked := Task.TrainListener.F0Forward;
    ConsistItem.CheckBoxFnForward.Checked := Task.TrainListener.FnForward;

    ConsistItem.ComboBoxEntry.Items.Add( NodeIDToString(Task.TrainListener.NodeID, True));
    ConsistItem.ComboBoxEntry.ItemIndex := 0;
    ConsistItem.NodeID := Task.Target;
  end;

var
  TaskListenerEnumerate: TLccTaskListenersEnumerate;
begin
  TaskListenerEnumerate := ATask as TLccTaskListenersEnumerate;

  case ATask.TaskState of
    lesComplete :
      begin
        AddConsistItem(TaskListenerEnumerate)
      end;
    lesRunning :
      begin
         AddConsistItem(TaskListenerEnumerate)
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
begin
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

  Controller.ListenerEnumerate(ANodeID, @CallbackListenerEnumerate);

end;


{ TConsistSelectorItem }

procedure TConsistSelectorItem.ButtonRemoveClick(Sender: TObject);
begin
  // Still need to keep this information so we can remove the consist from the train later
  Visible := False;
end;

procedure TConsistSelectorItem.CheckBoxReverseDirClick(Sender: TObject);
begin

end;

procedure TConsistSelectorItem.CheckBoxF0ForwardClick(Sender: TObject);
begin

end;

procedure TConsistSelectorItem.CheckBoxFnForwardClick(Sender: TObject);
begin

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
  CheckBoxReverseDir.OnClick := @CheckBoxReverseDirClick;

  CheckBoxF0Forward := TCheckBox.Create(Self);
  CheckBoxF0Forward.Parent := Self;
  CheckBoxF0Forward.Caption := 'Forward F0 (lights)';
  CheckBoxF0Forward.Top := CheckBoxReverseDir.Top + CheckBoxReverseDir.Height + 4;
  CheckBoxF0Forward.Left := CHECKBOX_INDENT;
  CheckBoxF0Forward.Tag := 1;
  CheckBoxF0Forward.OnClick := @CheckBoxF0ForwardClick;

  CheckBoxFnForward := TCheckBox.Create(Self);
  CheckBoxFnForward.Parent := Self;
  CheckBoxFnForward.Caption := 'Foward F1 and up';
  CheckBoxFnForward.Top := CheckBoxF0Forward.Top + CheckBoxF0Forward.Height + 4;
  CheckBoxFnForward.Left := CHECKBOX_INDENT;
  CheckBoxFnForward.Tag := 2;
  CheckBoxFnForward.OnClick := @CheckBoxFnForwardClick;

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

