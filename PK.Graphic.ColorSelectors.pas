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

  THueCursor = class;
  TSVCursor = class;
  TRectCursor = class;

  TCustomSelector = class(TControl)
  private var
    FBase: TBitmap;
    FBaseColor: TAlphaColor;
    FColor: TAlphaColor;
    FOnChange: TColorChangeEvent;
  private
    procedure SetBaseColor(const AValue: TAlphaColor);
  protected
    procedure Resize; override;
    procedure Paint; override;

    procedure PreDraw; virtual;
    procedure Draw(const ACanvas: TCanvas); virtual;
    procedure Invalidate;

    procedure DoChange;

    procedure SetColor(const AValue: TAlphaColor); virtual;

    property Base: TBitmap read FBase;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BaseColor: TAlphaColor
      read FBaseColor
      write SetBaseColor
      default TAlphaColors.White;
  published
    property Color: TAlphaColor read FColor write SetColor;
    property OnChange: TColorChangeEvent read FOnChange write FOnChange;
  end;

  TCircleSelector = class(TCustomSelector)
  private const
    MARGIN = 8;
    DOUBLE_MARGIN = MARGIN * 2;
    OUTER_CIRCLE_RATIO = 7;
    INNER_CIRCLE_RATIO = OUTER_CIRCLE_RATIO - 2;
    // ピクセル単位のアンチエイリアス幅
    AA_WIDTH = 1;
  private var
    FDiameter: Integer;
    FRadius: Integer;
    FInnerDiameter: Integer;
    FInnerRadius: Integer;
    FInnerDelta: Integer;
    FHueRadius: Integer;
    FCX: Integer;
    FCY: Integer;
    FTriP0: TPoint;
    FTriP1: TPoint;
    FTriP2: TPoint;
    FTriRect: TRect;
    FTriDenom: Integer;
    FHue: Single;
    FSat: Single;
    FVal: Single;
    FHueCursor: THueCursor;
    FSVCursor: TSVCursor;
    FCursorMargin: Single;
    FInHueCircle: Boolean;
    FInSVTriangle: Boolean;
  private
    procedure CalcColor;
    function CalcBarycentric(
      const AX, AY: Single;
      out AW0, AW1, AW2: Single): Boolean;
    function IsInHueCircle(const AX, AY: Single): Boolean;
    function IsInSVTriangle(const AX, AY: Single): Boolean;
    function CalcHue(const AX, AY: Integer): Single;
    function GetColorFromCircle(const AX, AY: Integer): TAlphaColor;
    procedure DrawCircle(const ACanvas: TCanvas; const AData: TBitmapData);
    procedure DrawTriangle(const ACanvas: TCanvas; const AData: TBitmapData);
    procedure RedrawTriangle;
  protected
    procedure Resize; override;
    procedure Draw(const ACanvas: TCanvas); override;
    procedure SetColor(const AValue: TAlphaColor); override;
    procedure MouseDown(
      AButton: TMouseButton;
      AShift: TShiftState;
      AX, AY: Single); override;
    procedure MouseMove(
      AShift: TShiftState;
      AX, AY: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TRectSelector = class(TCustomSelector)
  private const
    CURSOR_RATIO = 0.07;
  private var
    FHue: Single;
    FSat: Single;
    FVal: Single;
    FCursor: TRectCursor;
  private
    procedure CalcColor;
  protected
    procedure Resize; override;
    procedure Draw(const ACanvas: TCanvas); override;
    procedure SetColor(const Value: TAlphaColor); override;

    procedure MouseDown(
      AButton: TMouseButton;
      AShift: TShiftState;
      AX, AY: Single); override;
    procedure MouseMove(
      AShift: TShiftState;
      AX, AY: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color: TAlphaColor read FColor write SetColor;
  end;

  // カーソル
  TSelectorCursor = class(TCircle)
  private const
    CURSOR_THICKNESS_RATIO = 1 / 10;
  private var
    FSelector: TCustomSelector;
  protected
    procedure MoveTo(const AX, AY: Single); virtual;
    procedure Update(const ASize: Single); virtual;
    property Selector: TCustomSelector read FSelector;
  public
    constructor Create(AOwner: TComponent); override;
  end;

  THueCursor = class(TSelectorCursor)
  private const
    CURSOR_MARGIN = 2;
  protected
    procedure MoveTo(const AX, AY: Single); override;
  end;

  TSVCursor = class(TSelectorCursor)
  protected
    procedure MoveTo(const AX, AY: Single); override;
  end;

  TRectCursor = class(TSelectorCursor)
  protected
    procedure MoveTo(const AX, AY: Single); override;
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

constructor TCustomSelector.Create(AOwner: TComponent);
begin
  inherited;

  FBaseColor := TAlphaColors.White;

  FBase := TBitmap.Create;
  SetSize(320, 320);
end;

destructor TCustomSelector.Destroy;
begin
  FBase.Free;

  inherited;
end;

procedure TCustomSelector.DoChange;
begin
  if Assigned(FOnChange) then
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

procedure TCustomSelector.PreDraw;
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

procedure TCustomSelector.Resize;
begin
  inherited;

  var W := Trunc(Width);
  var H := Trunc(Height);
  FBase.SetSize(W, H);

  PreDraw;
end;

procedure TCustomSelector.SetBaseColor(const AValue: TAlphaColor);
begin
  FBaseColor := AValue;
  PreDraw;
end;


procedure TCustomSelector.SetColor(const AValue: TAlphaColor);
begin
  if FColor = AValue then
    Exit;

  FColor := AValue;
  DoChange;
end;

{ TCircleSelector }

function TCircleSelector.CalcBarycentric(
  const AX, AY: Single;
  out AW0, AW1, AW2: Single): Boolean;
begin
  // 三角形が成立していない場合
  if FTriDenom = 0 then
  begin
    AW0 := -1;
    AW1 := -1;
    AW2 := -1;
    Exit(False);
  end;

  // バリセントリック計算 (P2を基準点とした式)
  AW0 :=
    (
      (FTriP1.Y - FTriP2.Y) * (AX - FTriP2.X) +
      (FTriP2.X - FTriP1.X) * (AY - FTriP2.Y)
    ) / FTriDenom;

  AW1 :=
    (
      (FTriP2.Y - FTriP0.Y) * (AX - FTriP2.X) +
      (FTriP0.X - FTriP2.X) * (AY - FTriP2.Y)
    ) / FTriDenom;

  AW2 := 1 - AW0 - AW1;

  Result := True;
end;

procedure TCircleSelector.CalcColor;
begin
  FColor := HSV2RGB(FHue, FSat, FVal);
  DoChange;
end;

function TCircleSelector.CalcHue(const AX, AY: Integer): Single;
begin
  // ラジアンを度数に変換
  Result := if (AX = 0) and (AY = 0) then 0.0 else RadToDeg(ArcTan2(AY, AX));
end;

constructor TCircleSelector.Create(AOwner: TComponent);
begin
  FHueCursor := THueCursor.Create(Self);
  FSVCursor := TSVCursor.Create(Self);

  FHueCursor.Parent := Self;
  FSVCursor.Parent := Self;

  AutoCapture := True;

  FHue := 0;
  FSat := 1;
  FVal := 1;

  inherited;
end;

destructor TCircleSelector.Destroy;
begin
  FSVCursor.Free;
  FHueCursor.Free;

  inherited;
end;

procedure TCircleSelector.Draw(const ACanvas: TCanvas);
begin
  inherited;

  var Data :=
    TBitmapData.Create(FBase.Width, FBase.Height, TPixelFormat.RGBA);

  FBase.Map(TMapAccess.ReadWrite, Data);
  try
    DrawCircle(ACanvas, Data);
    DrawTriangle(ACanvas, Data);
  finally
    FBase.Unmap(Data);
  end;
end;

procedure TCircleSelector.DrawCircle(
  const ACanvas: TCanvas;
  const AData: TBitmapData);
begin
  var W := AData.Width;
  var H := AData.Height;

  // BaseColor の TAlphaColorRec 表現 (ブレンドの背景色として使用)
  var BaseColorRec := TAlphaColorRec(FBaseColor);

  // 走査範囲を円環の外接四角形に限定
  var X1 := Max(0, FCX - FRadius - AA_WIDTH);
  var X2 := Min(W - 1, FCX + FRadius + AA_WIDTH);
  var Y1 := Max(0, FCY - FRadius - AA_WIDTH);
  var Y2 := Min(H - 1, FCY + FRadius + AA_WIDTH);

  for var Y := Y1 to Y2 do
  begin
    for var X := X1 to X2 do
    begin
      var DX := X - FCX;
      var DY := Y - FCY;
      var D := Sqrt(DX * DX + DY * DY); // 中心からの距離

      // 1. 色相環の本体領域外のピクセルはスキップ
      if (D > FRadius + AA_WIDTH) or (D < FInnerRadius - AA_WIDTH) then
        Continue;

      // 2. 色相環の色と背景色を取得
      var FRC := TAlphaColorRec(GetColorFromCircle(DX, DY));

      var OuterBlend := 1.0; // 0.0 (外側) から 1.0 (内側)
      var InnerBlend := 1.0; // 0.0 (穴の内側) から 1.0 (色相環の内側)

      // 3. 外周のアンチエイリアス処理
      if D > FRadius - AA_WIDTH then // 外周のブレンド領域内
      begin
        // D = FRadius + AA_WIDTH で 0.0 (完全に外側)
        // D = FRadius - AA_WIDTH で 1.0 (完全に内側)
        OuterBlend :=
          EnsureRange((FRadius + AA_WIDTH - D) / (AA_WIDTH * 2), 0, 1);
      end;

      // 4. 内周のアンチエイリアス処理
      if D < FInnerRadius + AA_WIDTH then // 内周のブレンド領域内
      begin
        // D = R_inner - AA_WIDTH で 0.0 (穴の内側、色が塗られない)
        // D = R_inner + AA_WIDTH で 1.0 (色相環の内側、完全に色が塗られる)
        InnerBlend :=
          EnsureRange((D - (FInnerRadius - AA_WIDTH)) / (AA_WIDTH * 2), 0, 1);
      end;

      // 5. アルファ値の計算 (両方の境界の影響を受ける)
      var Alpha := OuterBlend * InnerBlend;

      if Alpha > 0.001 then // ほぼ 0 でなければ描画
      begin
        var A := Trunc(Alpha * 255);

        // 背景色 (FBaseColor) と HueColor をブレンド
        var BackR := BaseColorRec.R;
        var BackG := BaseColorRec.G;
        var BackB := BaseColorRec.B;

        var ForeR := FRC.R;
        var ForeG := FRC.G;
        var ForeB := FRC.B;

        // アルファブレンド計算: C_final = C_fore * alpha + C_back * (1 - alpha)
        var R := (ForeR * A + BackR * (255 - A)) div 255;
        var G := (ForeG * A + BackG * (255 - A)) div 255;
        var B := (ForeB * A + BackB * (255 - A)) div 255;

        var FinalColor: TAlphaColorRec;
        FinalColor.R := R;
        FinalColor.G := G;
        FinalColor.B := B;
        FinalColor.A := 255; // ブレンド後の色は不透明として書き込む

        AData.SetPixel(X, Y, TAlphaColor(FinalColor));
      end;
    end;
  end;
end;

procedure TCircleSelector.DrawTriangle(
  const ACanvas: TCanvas;
  const AData: TBitmapData);
begin
  // SV三角形の枠を描画
  ACanvas.Stroke.Thickness := 1;
  ACanvas.Stroke.Color := FBaseColor xor $00_ff_ff_ff;
  ACanvas.DrawLine(FTriP0, FTriP1, 1);
  ACanvas.DrawLine(FTriP1, FTriP2, 1);
  ACanvas.DrawLine(FTriP2, FTriP0, 1);

  // SV三角形の内部をピクセル単位で描画
  for var Y := FTriRect.Top to FTriRect.Bottom do
  begin
    for var X := FTriRect.Left to FTriRect.Right do
    begin
      var W0, W1, W2: Single;
      CalcBarycentric(X, Y, W0, W1, W2);

      // 三角形の内側であるかを判定 (境界のアンチエイリアスは省略)
      if (W0 >= 0) and (W1 >= 0) and (W2 >= 0) then
      begin
        // P0: 純色(H,1,1), P1: 白(H,0,1), P2: 黒(H,0,0)

        // 輝度 V = W0 + W1
        var V := EnsureRange(W0 + W1, 0, 1);

        var S: Single;
        if V > 0 then
          // 彩度 S = W0 / V
          S := EnsureRange(W0 / V, 0, 1)
        else
          S := 0; // V=0 (黒) の場合は S=0

        var C := HSV2RGB(FHue, S, V);

        // 範囲チェック（念のため）
        if
          (X >= 0) and (X < AData.Width) and
          (Y >= 0) and (Y < AData.Height)
        then
          AData.SetPixel(X, Y, C);
      end;
    end;
  end;
end;

function TCircleSelector.GetColorFromCircle(const AX, AY: Integer): TAlphaColor;
begin
  Result := HSV2RGB(CalcHue(AX, AY), 1, 1);
end;

function TCircleSelector.IsInHueCircle(const AX, AY: Single): Boolean;
begin
  // 中心からの相対座標
  var DX := AX - FCX;
  var DY := AY - FCY;

  var Dist2 := DX * DX + DY * DY;

  // 半径の2乗
  var Inner2 := FInnerRadius * FInnerRadius;
  var Outer2 := FRadius * FRadius;

  // 内側の円の外、かつ外側の円の内側 → 色相環上
  Result := (Dist2 >= Inner2) and (Dist2 <= Outer2);
end;

function TCircleSelector.IsInSVTriangle(const AX, AY: Single): Boolean;
begin
  var W0, W1, W2: Single;
  if CalcBarycentric(AX, AY, W0, W1, W2) then
    Result := (W0 >= 0) and (W1 >= 0) and (W2 >= 0)
  else
    Result := False;
end;

procedure TCircleSelector.MouseDown(
  AButton: TMouseButton;
  AShift: TShiftState;
  AX, AY: Single);
begin
  inherited;

  FInHueCircle := IsInHueCircle(AX, AY);
  if FInHueCircle then
    FHueCursor.MoveTo(AX, AY);

  FInSVTriangle := IsInSVTriangle(AX, AY);
  if FInSVTriangle then
    FSVCursor.MoveTo(AX, AY);
end;

procedure TCircleSelector.MouseMove(
  AShift: TShiftState;
  AX, AY: Single);
begin
  inherited;

  if not Pressed then
    Exit;

  if FInHueCircle then
    FHueCursor.MoveTo(AX, AY);

  if FInSVTriangle then
    FSVCursor.MoveTo(AX, AY);
end;

procedure TCircleSelector.RedrawTriangle;
begin
  // Triangle のみを再描画
  FBase.Canvas.BeginScene;
  try
    var Data :=
      TBitmapData.Create(FBase.Width, FBase.Height, TPixelFormat.RGBA);

    FBase.Map(TMapAccess.Write, Data);
    try
      DrawTriangle(FBase.Canvas, Data);
    finally
      FBase.Unmap(Data);
    end;
  finally
    FBase.Canvas.EndScene;
  end;

  Invalidate;
end;

procedure TCircleSelector.Resize;

  function CalcTriPos(const AAngle: Single): TPoint;
  begin
    var S, C: Single;
    SinCos(DegToRad(AAngle), S, C);

    Result.X := FCX + Trunc(FInnerRadius * C);
    Result.Y := FCY + Trunc(FInnerRadius * S);
  end;

begin
  inherited;

  var W := FBase.Width;
  var H := FBase.Height;

  FDiameter := Min(W, H) - DOUBLE_MARGIN;
  FRadius := FDiameter div 2;

  FCX := MARGIN + FRadius;
  FCY := MARGIN + FRadius;

  // 円と三角形のパラメータ
  FInnerDelta := FDiameter div OUTER_CIRCLE_RATIO;
  FInnerDiameter := FInnerDelta * INNER_CIRCLE_RATIO;
  FInnerRadius := FInnerDiameter div 2;

  // 頂点定義: P0=純色(右), P1=白(左下), P2=黒(左上)
  FTriP0 := CalcTriPos(0);
  FTriP1 := CalcTriPos(-120);
  FTriP2 := CalcTriPos(120);

  // 三角形に外接する四角形 (描画範囲)
  FTriRect.Left := Min(FTriP1.X, FTriP2.X);
  FTriRect.Right := FTriP0.X;
  FTriRect.Top := Min(FTriP1.Y, FTriP2.Y);
  FTriRect.Bottom := Max(FTriP1.Y, FTriP2.Y);

  // 分母の計算
  FTriDenom :=
    (FTriP1.Y - FTriP2.Y) * (FTriP0.X - FTriP2.X) +
    (FTriP2.X - FTriP1.X) * (FTriP0.Y - FTriP2.Y);

  // カーソル
  FHueRadius := FInnerRadius + FInnerDelta div 2;

  var Size := FInnerDelta / 2;
  FCursorMargin := Size / 2;

  FHueCursor.Update(Size);
  FSVCursor.Update(Size);

  PreDraw;
end;

procedure TCircleSelector.SetColor(const AValue: TAlphaColor);
begin
  if FColor = AValue then
    Exit;

  FColor := AValue;
  RGB2HSV(FColor, FHue, FSat, FVal);
  RedrawTriangle;

  var Theta := DegToRad(FHue);

  var S, C: Single;
  SinCos(Theta, S, C);

  // 色相環カーソル位置
  var X := FCX + FHueRadius * C;
  var Y := FCY + FHueRadius * S;

  // MoveTo 内で CalcColor が呼ばれるため、ここでは呼ばない
  FHueCursor.MoveTo(X, Y);

  // SV Triangle カーソル位置 (S, V から重心座標 W0, W1, W2 を逆算)
  var Sat := EnsureRange(FSat, 0, 1);
  var Val := EnsureRange(FVal, 0, 1);

  var W0: Single;
  var W1: Single;
  var W2: Single;

  if Val = 0 then
  begin
    // V=0 の場合は常に黒 (P2)
    W0 := 0;
    W1 := 0;
    W2 := 1;
  end
  else
  begin
    // 逆算ロジック: P0=純色, P1=白, P2=黒
    W0 := Sat * Val;
    W1 := Val - W0; // W1 = V * (1 - S)
    W2 := 1 - Val;
  end;

  var SX :=
    FTriP0.X * W0 +
    FTriP1.X * W1 +
    FTriP2.X * W2;

  var SY :=
    FTriP0.Y * W0 +
    FTriP1.Y * W1 +
    FTriP2.Y * W2;

  // MoveTo 内で CalcColor が呼ばれるため、ここでは呼ばない
  FSVCursor.MoveTo(SX, SY);
end;

{ TRectSelector }

constructor TRectSelector.Create(AOwner: TComponent);
begin
  // カーソルを先に作る（Parent は後で）
  FCursor := TRectCursor.Create(Self);

  FHue := 0;
  FSat := 1;
  FVal := 1;
  CalcColor;

  AutoCapture := True;

  inherited;

  FCursor.Parent := Self;
end;

destructor TRectSelector.Destroy;
begin
  FCursor.Free;
  inherited;
end;

procedure TRectSelector.CalcColor;
begin
  FColor := HSV2RGB(FHue, FSat, FVal);
  DoChange;
end;

procedure TRectSelector.Draw(const ACanvas: TCanvas);
begin
  inherited;

  var Data :=
    TBitmapData.Create(FBase.Width, FBase.Height, TPixelFormat.RGBA);

  if not FBase.Map(TMapAccess.Write, Data) then
    Exit;
  try
    // グラデーションを描く矩形（マージンを除く）
    var R := LocalRect.Round;

    var W := Max(1.0, R.Width  - 1);
    var H := Max(1.0, R.Height - 1);

    // 内側だけ HSV グラデーション
    for var Y := R.Top to R.Bottom - 1 do
    begin
      var NY := (Y - R.Top) / H;
      var S, V: Single;

      if NY <= 0.5 then
      begin
        V := 1;
        S := EnsureRange(NY * 2, 0, 1);
      end
      else
      begin
        S := 1;
        V := EnsureRange(1 - (NY - 0.5) * 2, 0, 1);
      end;

      for var X := R.Left to R.Bottom - 1 do
      begin
        var Hdeg := EnsureRange((X - R.Left) / W, 0, 1) * 360;
        Data.SetPixel(X, Y, HSV2RGB(Hdeg, S, V));
      end;
    end;
  finally
    FBase.Unmap(Data);
  end;
end;

procedure TRectSelector.MouseDown(
  AButton: TMouseButton;
  AShift: TShiftState;
  AX, AY: Single);
begin
  inherited;

  if Pressed then
    FCursor.MoveTo(AX, AY);
end;

procedure TRectSelector.MouseMove(
  AShift: TShiftState;
  AX, AY: Single);
begin
  inherited;

  if Pressed then
    FCursor.MoveTo(AX, AY);
end;

procedure TRectSelector.Resize;
begin
  inherited;

  // カーソルサイズは短辺に対してちょっとだけ
  var Size := Min(Width, Height) * CURSOR_RATIO;
  if Size < 10 then
    Size := 10;

  FCursor.Update(Size);

  PreDraw;
end;

procedure TRectSelector.SetColor(const Value: TAlphaColor);
begin
  if FColor = Value then
    Exit;

  FColor := Value;
  RGB2HSV(FColor, FHue, FSat, FVal);

  // Hue は 0..360 に正規化
  Adjust360(FHue);

  FSat := EnsureRange(FSat, 0, 1);
  FVal := EnsureRange(FVal, 0, 1);

  CalcColor;
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
  Stroke.Thickness := Width * CURSOR_THICKNESS_RATIO;

  Width := ASize;
  Height := ASize;

  var C := FSelector.FColor;
  FSelector.FColor := C xor $ffff_ffff;
  FSelector.SetColor(C);
end;

{ THueCursor }

procedure THueCursor.MoveTo(const AX, AY: Single);
begin
  var X, Y: Single;

  with TCircleSelector(FSelector) do
  begin
    var Theta := ArcTan2(AY - FCY, AX - FCX);
    var S, C: Single;
    SinCos(Theta, S, C);

    X := FCX + FHueRadius * C;
    Y := FCY + FHueRadius * S;

    FHue := RadToDeg(Theta);
    Adjust360(FHue);

    CalcColor;
    RedrawTriangle;
  end;

  var S := if Scene = nil then 1.0 else Scene.GetSceneScale;
  SetBounds(
    X - Width / 2 + CURSOR_MARGIN * S,
    Y - Height / 2,
    Width,
    Height);
end;

{ TSVCursor }

procedure TSVCursor.MoveTo(const AX, AY: Single);
begin
  var P: TPointF;

  with TCircleSelector(FSelector) do
  begin
    var W0, W1, W2: Single;

    // 1) バリセントリックを計算
    if not CalcBarycentric(AX, AY, W0, W1, W2) then
      Exit;

    // 2) 三角形の外側なら、0 以上にクランプして正規化
    if W0 < 0 then
      W0 := 0;

    if W1 < 0 then
      W1 := 0;

    if W2 < 0 then
      W2 := 0;

    var Sum := W0 + W1 + W2;
    if Sum <= 0 then
      Exit;

    W0 := W0 / Sum;
    W1 := W1 / Sum;
    W2 := W2 / Sum;

    // 3) 三角形上の実際の座標を再構成
    P :=
      PointF(
        FTriP0.X * W0 + FTriP1.X * W1 + FTriP2.X * W2,
        FTriP0.Y * W0 + FTriP1.Y * W1 + FTriP2.Y * W2
      );

    // 4) HSV の S,V を算出
    // 頂点割り当て: P0=純色(S=1,V=1), P1=白(S=0,V=1), P2=黒(S=0,V=0)
    // FVal = W0 + W1 (P0とP1でV=1、P2でV=0)
    FVal := EnsureRange(W0 + W1, 0, 1);

    if FVal > 0 then
      FSat := EnsureRange(W0 / FVal, 0, 1)
    else
      FSat := 0; // FVal = 0 の場合は FSat は 0

    CalcColor;
  end;

  // 5) カーソル（TCircle）の中心を P に合わせる
  SetBounds(P.X - Width / 2, P.Y - Height / 2, Width, Height);
end;

{ TRectCursor }

procedure TRectCursor.MoveTo(const AX, AY: Single);
begin
  var X, Y: Single;

  with TRectSelector(FSelector) do
  begin
    // 有効範囲（グラデーション矩形）にクランプ
    var R := LocalRect.Round;

    X := AX;//EnsureRange(AX, R.Left, R.Right);
    Y := AY;//EnsureRange(AY, R.Top, R.Bottom);

    var W := Max(1.0, R.Width  - 1);
    var H := Max(1.0, R.Height - 1);

    // 0..1 に正規化
    var NX := (X - R.Left) / W;
    var NY := (Y - R.Top)  / H;

    // 横軸 : Hue
    FHue := EnsureRange(NX, 0, 1) * 360;

    // 縦軸 : 中央で S=1,V=1
    if NY <= 0.5 then
    begin
      // 上半分 : V=1, S=0→1
      FVal := 1;
      FSat := EnsureRange(NY / 0.5, 0, 1);
    end
    else
    begin
      // 下半分 : S=1, V=1→0
      FSat := 1;
      FVal := EnsureRange(1 - (NY - 0.5) / 0.5, 0, 1);
    end;

    Log.d([FHue]);

    CalcColor;
  end;

  SetBounds(
    X - Width / 2,
    Y - Height / 2,
    Width,
    Height);
end;

end.
