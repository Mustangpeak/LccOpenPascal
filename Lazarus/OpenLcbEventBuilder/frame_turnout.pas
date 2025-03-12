unit frame_turnout;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, Buttons,
  StdCtrls,
  {$warnings off}
   {$hints off}
  fgl,
  {$warnings on}
  {$hints on}
  ComCtrls;

type

  TTurnoutPositions = (tp_TwoWay, tpThreeway);

    { TTurnout }

    TTurnout = class(TInterfacedObject)

    private
      FHasFeedback: Boolean;
      FIdentifier: string;
      FLocation: string;
      FPostions: TTurnoutPositions;
    public
      property Location: string read FLocation write FLocation;
      property Identifier: string read FIdentifier write FIdentifier;
      property Positions: TTurnoutPositions read FPostions write FPostions;
      property HasFeedback: Boolean read FHasFeedback write FHasFeedback;
    end;

    TTurnoutList = specialize TFPGInterfacedObjectList<TTurnout>;

  { TFrameTurnout }

  TFrameTurnout = class(TFrame)
    BitBtnAdd: TBitBtn;
    BitBtnRemove: TBitBtn;
    ComboBoxIdentifier: TComboBox;
    ComboBoxLocation: TComboBox;
    Label1: TLabel;
    Panel_header_button_location: TPanel;
    Panel_header_button_identifier: TPanel;
    Panel_header_button_routes: TPanel;
    Panel_header_button_feedback: TPanel;
    Panel_header_buttons: TPanel;
    Panel_treeview_header: TPanel;
    Panel_treeview: TPanel;
    PanelTurnout: TPanel;
    RadioGroupRoutes: TRadioGroup;
    RadioGroupFeedback: TRadioGroup;
    SpeedButtonIdentifierNamingConvention: TSpeedButton;
    StaticTextIdentifier: TStaticText;
    StaticTextLocation: TStaticText;
    TreeView_turnouts: TTreeView;
    procedure BitBtnAddClick(Sender: TObject);
  private
    FTurnouts: TTurnoutList;

  public
    property Turnouts: TTurnoutList read FTurnouts write FTurnouts;

    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TFrameTuroutClass = class of TFrameTurnout;

implementation

{$R *.lfm}

{ TFrameTurnout }

procedure TFrameTurnout.BitBtnAddClick(Sender: TObject);
var
  Turnout: TTurnout;
  TreeNode: TTreeNode;
begin
  Turnout := TTurnout.Create;
  Turnout.Location := ComboBoxLocation.Text;
  Turnout.Identifier := ComboBoxIdentifier.Text;
  Turnout.Positions := TTurnoutPositions( RadioGroupRoutes.ItemIndex);
  Turnout.HasFeedback := RadioGroupFeedback.ItemIndex = 1;

  Turnouts.Add(Turnout);

  TreeNode := TreeView_turnouts.Items.Add(nil, Turnout.Location);
  TreeView_turnouts.Items.AddChild(TreeNode, 'Identifier: ' + Turnout.Location);
  case Turnout.Positions of
    tp_TwoWay: TreeView_turnouts.Items.AddChild(TreeNode, 'Postions: 2');
    tpThreeway: TreeView_turnouts.Items.AddChild(TreeNode, 'Postions: 3');
  end;
  case Turnout.HasFeedback of
    True:  TreeView_turnouts.Items.AddChild(TreeNode, 'Has Feedback:  Yes');
    False: TreeView_turnouts.Items.AddChild(TreeNode, 'Has Feedback:  Yes');
  end;

end;

constructor TFrameTurnout.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  Turnouts := TTurnoutList.Create();
end;

destructor TFrameTurnout.Destroy;
begin
  Turnouts.Free;
  inherited Destroy;
end;

end.

