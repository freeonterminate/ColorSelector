unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs,
  FMX.Effects, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Colors
  ,PK.Graphic.HSVSelectors, PK.Graphic.CellSelectors, PK.Graphic.ColorSelectors;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Image1: TImage;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    FCircleSelector: TCircleSelector;
    FRectSelector: TRectSelector;
    F16Selector: T16CellSelector;
    F128Selector: T128CellSelector;
  public
    procedure SelectorChange(Sender: TObject; const AColor: TAlphaColor);
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

uses
  PK.Utils.Log;

procedure TForm1.Button1Click(Sender: TObject);
begin
  var C := $ff000000 or UInt32(Random($ffffff));

  FCircleSelector.Color := C;
  Image1.Bitmap.Clear(C);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCircleSelector := TCircleSelector.Create(Self);
  FCircleSelector.OnChange := SelectorChange;
  FCircleSelector.Parent := Self;

  Image1.Bitmap.SetSize(Trunc(Image1.Width), Trunc(Image1.Height));

  FRectSelector := TRectSelector.Create(Self);
  FRectSelector.Position.X := 500;
  FRectSelector.OnChange := SelectorChange;
  FRectSelector.Parent := Self;

  F16Selector := T16CellSelector.Create(Self);
  F16Selector.Position.Y := 330;
  F16Selector.OnChange := SelectorChange;
  F16Selector.Parent := Self;

  F128Selector := T128CellSelector.Create(Self);
  F128Selector.Position.X := 500;
  F128Selector.Position.Y := 330;
  F128Selector.OnChange := SelectorChange;
  F128Selector.Parent := Self;

  //Log.d('');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCircleSelector.Free;
end;

procedure TForm1.SelectorChange(Sender: TObject; const AColor: TAlphaColor);
begin
  //Log.d(IntToHex(AColor, 8));
  Image1.Bitmap.Clear(AColor);

  for
    var S: TCustomSelector in
    [FCircleSelector, FRectSelector, F16Selector, F128Selector]
  do
    if S <> Sender then
      S.SetColorWihtoutEvent(AColor);
end;

end.
