unit PK.Graphic.ColorSelectors;

interface

uses
  System.Classes
  , System.SysUtils
  , System.UITypes
  , System.Types
  , FMX.Controls
  , FMX.Graphics
  , FMX.Objects
  , FMX.Types
  ;

type
  TColorChangeEvent =
    procedure(Sender: TObject; const AColor: TAlphaColor) of object;

  TCustomSelector = class(TControl)
  private var
    FBase: TBitmap;
    FBaseColor: TAlphaColor;
    FNoEvent: Boolean;
    FOnChange: TColorChangeEvent;
  private
    procedure SetBaseColor(const AColor: TAlphaColor);
  protected var
    FColor: TAlphaColor;
  protected
    procedure Resize; override;
    procedure Paint; override;

    procedure StartDraw; virtual;
    procedure Draw(const ACanvas: TCanvas); virtual;
    procedure Invalidate;

    procedure DoChange;

    procedure SetColor(const AColor: TAlphaColor); virtual;

    property Base: TBitmap read FBase;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;

    procedure SetColorWihtoutEvent(const AColor: TAlphaColor);

    property BaseColor: TAlphaColor
      read FBaseColor
      write SetBaseColor
      default TAlphaColors.White;
  published
    property Color: TAlphaColor read FColor write SetColor;
    property OnChange: TColorChangeEvent read FOnChange write FOnChange;
  end;

  // カーソル
  TSelectorCursor = class(TCircle)
  private const
    CUSOR_THICKNESS_SIZE = 3;
  private var
    FSelector: TCustomSelector;
  protected
    procedure MoveTo(const AX, AY: Single); virtual;
    procedure Update(const ASize: Single); virtual;
    property Selector: TCustomSelector read FSelector;
  public
    constructor Create(AOwner: TComponent); override;
  end;

implementation

uses
  System.Math
  , System.UIConsts
  , FMX.Effects
  , PK.Graphic.ColorConverter
  , PK.Math.AdjustUtils
  , PK.Utils.Log
  ;

{ TCustomSelector }

procedure TCustomSelector.AfterConstruction;
begin
  inherited;
  SetSize(320, 320);
end;

constructor TCustomSelector.Create(AOwner: TComponent);
begin
  inherited;

  FBaseColor := TAlphaColors.White;

  FBase := TBitmap.Create;
end;

destructor TCustomSelector.Destroy;
begin
  FBase.Free;

  inherited;
end;

procedure TCustomSelector.DoChange;
begin
  if (not FNoEvent) and Assigned(FOnChange) then
    FOnChange(Self, FColor);
end;

procedure TCustomSelector.Draw(const ACanvas: TCanvas);
begin
  // BaseColor で全体をクリア
  FBase.Clear(FBaseColor);
end;

procedure TCustomSelector.Invalidate;
begin
  InvalidateRect(LocalRect);
end;

procedure TCustomSelector.Paint;
begin
  inherited;

  if (Parent = nil) or (Canvas = nil) or (Scene = nil) then
    Exit;

  var W := FBase.Width;
  var H := FBase.Height;

  Canvas.DrawBitmap(
    FBase,
    RectF(0, 0, W, H),
    RectF(0, 0, W, H),
    1,
    False);
end;

procedure TCustomSelector.Resize;
begin
  inherited;

  var W := Trunc(Width);
  var H := Trunc(Height);
  FBase.SetSize(W, H);

  StartDraw;
end;

procedure TCustomSelector.SetBaseColor(const AColor: TAlphaColor);
begin
  FBaseColor := AColor;
  StartDraw;
end;


procedure TCustomSelector.SetColor(const AColor: TAlphaColor);
begin
  if FColor = AColor then
    Exit;

  FColor := AColor;

  DoChange;
end;

procedure TCustomSelector.SetColorWihtoutEvent(const AColor: TAlphaColor);
begin
  FNoEvent := True;
  SetColor(AColor);
  FNoEvent := False;
end;


procedure TCustomSelector.StartDraw;
begin
  if(FBase.Canvas = nil) then
    Exit;

  FBase.Canvas.BeginScene;
  try
    Draw(FBase.Canvas);
  finally
    FBase.Canvas.EndScene;
  end;
end;

{ TSelectorCursor }

constructor TSelectorCursor.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture:= True;
  HitTest := False;

  FSelector := AOwner as TCustomSelector;

  Fill.Color := $00ffffff;
  Stroke.Color := TAlphaColors.Black;

  var F := TGlowEffect.Create(Self);
  F.GlowColor := TAlphaColors.White;
  F.Softness := 0.2;
  F.Parent := Self;
end;

procedure TSelectorCursor.MoveTo(const AX, AY: Single);
begin
  // 継承先で上書き
end;

procedure TSelectorCursor.Update(const ASize: Single);
begin
  Stroke.Thickness := CUSOR_THICKNESS_SIZE;

  Width := ASize;
  Height := ASize;

  // Color を強制的に設定する
  var C := FSelector.FColor;
  FSelector.FColor := C xor $ffff_ffff;
  FSelector.SetColor(C);
end;

end.
