unit unit_custom_fmx_listview;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes,
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.StdCtrls,
  FMX.Controls.Presentation, FMX.ListView, FMX.Listview.Types, FMX.TextLayout,
  FMX.ListView.Appearances;


type

  TCustomLayoutListview = class(TListview)

  protected
    procedure DoUpdateItemView(const AListItem: TListItem); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function GetTextHeight(const D: TListItemText; const Width: Single; const Text: string): Integer;

  end;

implementation

{ TCustomLayoutListview }

constructor TCustomLayoutListview.Create(AOwner: TComponent);
begin
  inherited;
  ItemAppearance.ItemAppearance := 'Custom';
  ItemAppearance.ItemEditAppearance := 'Custom';

  //----------------------------------------------------------------------------
  // Accessory Box
  //----------------------------------------------------------------------------
  ItemAppearanceObjects.ItemObjects.Accessory.AccessoryType := TAccessoryType.More;

      // Accessory box alignments
  ItemAppearanceObjects.ItemObjects.Accessory.Align := TListItemAlign.Trailing;
  ItemAppearanceObjects.ItemObjects.Accessory.VertAlign := TListItemAlign.Center;

  ItemAppearanceObjects.ItemObjects.Accessory.Visible := True;


  //----------------------------------------------------------------------------
  // Details TextBox
  //----------------------------------------------------------------------------
      // Text alignments
  ItemAppearanceObjects.ItemObjects.Detail.TextAlign := TTextAlign.Leading;
  ItemAppearanceObjects.ItemObjects.Detail.TextVertAlign := TTextAlign.Center;

      // Text attributes
  ItemAppearanceObjects.ItemObjects.Detail.TextColor := TAlphaColorRec.Steelblue;
  ItemAppearanceObjects.ItemObjects.Detail.Font.Size := 14;
  ItemAppearanceObjects.ItemObjects.Detail.Font.Style := [TFontSTyle.fsItalic];

      // Textbox alignments
  ItemAppearanceObjects.ItemObjects.Detail.VertAlign := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.Detail.Align := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.Detail.PlaceOffset.X := 18; // indent
  ItemAppearanceObjects.ItemObjects.Detail.PlaceOffset.Y := 0; // handled dynamically

  ItemAppearanceObjects.ItemObjects.Detail.Trimming := TTextTrimming.Character;  // if WordWrap=True this is unused
  ItemAppearanceObjects.ItemObjects.Detail.WordWrap := True;
  ItemAppearanceObjects.ItemObjects.Detail.Opacity := 1.0;
  ItemAppearanceObjects.ItemObjects.Detail.Visible := True;


  //----------------------------------------------------------------------------
  // Main Text TextBox
  //----------------------------------------------------------------------------
      // Text alignments
  ItemAppearanceObjects.ItemObjects.Text.TextAlign := TTextAlign.Leading;
  ItemAppearanceObjects.ItemObjects.Text.TextVertAlign := TTextAlign.Center;

      // Text attributes
  ItemAppearanceObjects.ItemObjects.Text.TextColor := TAlphaColorRec.Black;
  ItemAppearanceObjects.ItemObjects.Text.Font.Size := 16;
  ItemAppearanceObjects.ItemObjects.Text.Font.Style := [TFontSTyle.fsBold];

      // Textbox alignments
  ItemAppearanceObjects.ItemObjects.Text.VertAlign := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.Text.Align := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.Text.PlaceOffset.X := 0; // indent
  ItemAppearanceObjects.ItemObjects.Text.PlaceOffset.Y := 0; // handled dynamically

  ItemAppearanceObjects.ItemObjects.Text.Trimming := TTextTrimming.Character;  // if WordWrap=True this is unused
  ItemAppearanceObjects.ItemObjects.Text.WordWrap := True;
  ItemAppearanceObjects.ItemObjects.Text.Opacity := 1.0;
  ItemAppearanceObjects.ItemObjects.Text.Visible := True;

  //----------------------------------------------------------------------------
  // Image Box
  //----------------------------------------------------------------------------
      // Image alignments
  ItemAppearanceObjects.ItemObjects.Image.VertAlign := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.Image.Align := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.Image.PlaceOffset.X := 0; // indent
  ItemAppearanceObjects.ItemObjects.Image.PlaceOffset.Y := 0; // handled dynamically

  ItemAppearanceObjects.ItemObjects.Image.Width := 44;
  ItemAppearanceObjects.ItemObjects.Image.Height := 44;  // handled dynamically
  ItemAppearanceObjects.ItemObjects.Image.ScalingMode := TImageScalingMode.StretchWithAspect;
  ItemAppearanceObjects.ItemObjects.Image.Opacity := 1.0;
  ItemAppearanceObjects.ItemObjects.Image.Visible := True;

  //----------------------------------------------------------------------------
  // Glyph Button Box
  //----------------------------------------------------------------------------
      // Glyph alignments
  ItemAppearanceObjects.ItemObjects.GlyphButton.VertAlign := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.GlyphButton.Align := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.GlyphButton.PlaceOffset.X := 0; // indent
  ItemAppearanceObjects.ItemObjects.GlyphButton.PlaceOffset.Y := 0; // handled dynamically

  ItemAppearanceObjects.ItemObjects.GlyphButton.Enabled := True;
  ItemAppearanceObjects.ItemObjects.GlyphButton.ClickOnSelect := True;
  ItemAppearanceObjects.ItemObjects.GlyphButton.ButtonType := TGlyphButtonType.Checkbox;
  ItemAppearanceObjects.ItemObjects.GlyphButton.Visible := False;
   //----------------------------------------------------------------------------
  // Text Button Box
  //----------------------------------------------------------------------------

       // Text attributes
//  ItemAppearanceObjects.ItemObjects.TextButton.TextColor := TAlphaColorRec.Black;
//  ItemAppearanceObjects.ItemObjects.TextButton.Font.Size := 20;
//  ItemAppearanceObjects.ItemObjects.TextButton.Font.Style := [];

      // Glyph alignments
  ItemAppearanceObjects.ItemObjects.TextButton.VertAlign := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.TextButton.Align := TListItemAlign.Leading;
  ItemAppearanceObjects.ItemObjects.TextButton.PlaceOffset.X := 0; // indent
  ItemAppearanceObjects.ItemObjects.TextButton.PlaceOffset.Y := 0; // handled dynamically

  ItemAppearanceObjects.ItemObjects.TextButton.Enabled := True;
  ItemAppearanceObjects.ItemObjects.TextButton.ButtonType := TTextButtonType.Normal;
  ItemAppearanceObjects.ItemObjects.TextButton.Visible := False;



   //----------------------------------------------------------------------------
  // Header Objects
  //----------------------------------------------------------------------------
       // Text attributes
  ItemAppearanceObjects.HeaderObjects.Text.TextColor := TAlphaColorRec.Darkseagreen;
  ItemAppearanceObjects.HeaderObjects.Text.Font.Size := 18;
  ItemAppearanceObjects.HeaderObjects.Text.Font.Style := [TFontSTyle.fsBold, TFontStyle.fsUnderline];


  // Not sure if the ItemEditObject automatically picks these up or not... will see
end;

destructor TCustomLayoutListview.Destroy;
begin

  inherited;
end;

procedure TCustomLayoutListview.DoUpdateItemView(const AListItem: TListItem);
var
  NameTextBox, DescriptionTextBox: TListItemText;
  AvailableWidth, TotalHeight: Single;
  AccessoryMore: TListItemAccessory;
  TrainImg: TListItemImage;
  ImageW, AccessoryW: single;
  AListViewItem: TListViewItem;
begin
   // Default for Header/Footers
   if AListItem.Purpose <> TListItemPurpose.None then
    Exit;

  AListViewItem := AListItem as TListViewItem;


  TrainImg := AListViewItem.Objects.ImageObject;
  NameTextBox := AListViewItem.Objects.TextObject;
  DescriptionTextBox := AListViewItem.Objects.DetailObject;
  AccessoryMore := AListViewItem.Objects.AccessoryObject;

//   TrainImg := AItem.View.FindDrawable('Image') as TListItemImage;    // For ItemAppearance = Dynamic and done in the editor
//   NameTextBox := AItem.View.FindDrawable('Text') as TListItemText;
//   DescriptionTextBox := AItem.View.FindDrawable('Detail') as TListItemText;
//   AccessoryMore := AItem.View.FindDrawable('Accessory') as TListItemAccessory;
  TrainImg.Width := 44;

  if TrainImg.Visible then
    ImageW := TrainImg.Width
  else
    ImageW := 0;

  if AccessoryMore.Visible then
    AccessoryW := AccessoryMore.Width
  else
    AccessoryW := 0;

  AvailableWidth := Width - ItemSpaces.Left - ItemSpaces.Right - AccessoryW - ImageW - NameTextBox.PlaceOffset.X;
  NameTextBox.PlaceOffset.Y := 0;
  NameTextBox.PlaceOffset.X := NameTextBox.PlaceOffset.X + ImageW;
  NameTextBox.Height := GetTextHeight(NameTextBox, AvailableWidth, NameTextBox.Text);
  NameTextBox.Width := AvailableWidth;
  TotalHeight := NameTextBox.Height;

  AvailableWidth := Width - ItemSpaces.Left - ItemSpaces.Right - AccessoryW - ImageW - DescriptionTextBox.PlaceOffset.X;
  DescriptionTextBox.PlaceOffset.Y := NameTextBox.Height;
  DescriptionTextBox.PlaceOffset.X := DescriptionTextBox.PlaceOffset.X + ImageW;
  DescriptionTextBox.Height := GetTextHeight(DescriptionTextBox, AvailableWidth, DescriptionTextBox.Text);
  DescriptionTextBox.Width := AvailableWidth;

  TotalHeight := TotalHeight + DescriptionTextBox.Height;
  if TotalHeight < 44 then
    TotalHeight := 44;
  TrainImg.Height := TotalHeight;
  AListViewItem.Height := Round( TotalHeight);

  inherited;
end;

function TCustomLayoutListview.GetTextHeight(const D: TListItemText; const Width: Single; const Text: string): Integer;
var
  Layout: TTextLayout;
begin
  // Create a TTextLayout to measure text dimensions
  Layout := TTextLayoutManager.DefaultTextLayout.Create;
  try
    Layout.BeginUpdate;
    try
      // Initialize layout parameters with those of the drawable
      Layout.Font.Assign(D.Font);
      Layout.VerticalAlign := D.TextVertAlign;
      Layout.HorizontalAlign := D.TextAlign;
      Layout.WordWrap := D.WordWrap;
      Layout.Trimming := D.Trimming;
      Layout.MaxSize := TPointF.Create(Width, TTextLayout.MaxLayoutSize.Y);
      Layout.Text := Text;
    finally
      Layout.EndUpdate;
    end;
    Result := Round(Layout.Height * 1.1);   // 10% margin
  finally
    Layout.Free;
  end;

end;

end.
