unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, PK.Graphic.ColorSelectors,
  FMX.Effects, FMX.Objects, FMX.Controls.Presentation, FMX.StdCtrls, FMX.Colors;

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
  public
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
  //Log.d(IntToHex(C, 8));

  FCircleSelector.Color := C;
  Image1.Bitmap.Clear(C);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FCircleSelector := TCircleSelector.Create(Self);
  FCircleSelector.Parent := Self;

  Image1.Bitmap.SetSize(Trunc(Image1.Width), Trunc(Image1.Height));

  FRectSelector := TRectSelector.Create(Self);
  FRectSelector.Position.X := 500;

  FRectSelector.Parent := Self;

  //Log.d('');
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FCircleSelector.Free;
end;

end.
