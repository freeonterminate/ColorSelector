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
  TColorSelectorBase = class(TControl)
  private var
    FBase: TBitmap;
    FBaseColor: TAlphaColor;
    FOnChange: TNotifyEvent;
  private
    procedure SetBaseColor(const Value: TAlphaColor);
  protected
    procedure Resize; override;
    procedure Paint; override;

    function GetBaseWidth: Integer;
    function GetBaseHeight: Integer;
    procedure PreDraw; virtual;
    procedure Draw(const ACanvas: TCanvas); virtual;

    procedure DoChange;

    property Base: TBitmap read FBase;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BaseColor: TAlphaColor
      read FBaseColor
      write SetBaseColor
      default TAlphaColors.White;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCircleSelector = class(TColorSelectorBase)
  private type
    TCircleCursor = class(TCircle)
    private var
      FSelector: TCircleSelector;
    protected
      procedure DoResized; override;
      procedure MoveTo(const AX, AY: Single); virtual;
      procedure Update; virtual;
    public
      constructor Create(AOwner: TComponent); override;
    end;

    THueCursor = class(TCircleCursor)
    private const
      CURSOR_MARGIN = 2;
    protected
      procedure MoveTo(const AX, AY: Single); override;
    end;

    TSVCursor = class(TCircleCursor)
    protected
      procedure MoveTo(const AX, AY: Single); override;
    end;
  private const
    MARGIN = 8;
    DOUBLE_MARGIN = MARGIN * 2;
    OUTER_CIRCLE_RATIO = 7;
    INNER_CIRCLE_RATIO = OUTER_CIRCLE_RATIO - 2;
    CURSOR_THICKNESS_RATIO = 3 / 20;
    // ピクセル単位のアンチエイリアス幅
    AA_WIDTH = 1.0;
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
    FColor: TAlphaColor;
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
    procedure SetColor(const Value: TAlphaColor);
  protected
    procedure Resize; override;
    procedure Draw(const ACanvas: TCanvas); override;
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

type
  TRectSelector = class(TColorSelectorBase)
  private type
    TRectCursor = class(TCircle)
    private var
      FSelector: TRectSelector;
    protected
      procedure DoResized; override;
      procedure MoveTo(const AX, AY: Single); virtual;
      procedure Update; virtual;
    public
      constructor Create(AOwner: TComponent); override;
    end;
  private const
    MARGIN = 4;
    CURSOR_THICKNESS_RATIO = 3 / 20;
  private var
    FHue: Single;
    FSat: Single;
    FVal: Single;
    FColor: TAlphaColor;
    FCursor: TRectCursor;
    FInnerRect: TRectF;
  private
    procedure CalcColor;
    procedure SetColor(const Value: TAlphaColor);
    procedure UpdateCursorFromHSV;
  protected
    procedure Resize; override;
    procedure Draw(const ACanvas: TCanvas); override;
    procedure MouseDown(
      AButton: TMouseButton;
      AShift: TShiftState;
      AX, AY: Single); override;
    procedure MouseMove(
      AShift: TShiftState;
      AX, AY: Single); override;
    procedure MouseUp(
      AButton: TMouseButton;
      AShift: TShiftState;
      AX, AY: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Color: TAlphaColor read FColor write SetColor;
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

{ TColorSelectorBase }
// ... (TColorSelectorBase の実装は変更なし) ...
constructor TColorSelectorBase.Create(AOwner: TComponent);
begin
  inherited;

  FBaseColor := TAlphaColors.White;

  FBase := TBitmap.Create;
  SetSize(320, 320);
end;

destructor TColorSelectorBase.Destroy;
begin
  FBase.Free;

  inherited;
end;

procedure TColorSelectorBase.DoChange;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TColorSelectorBase.Draw(const ACanvas: TCanvas);
begin
  // BaseColor で全体をクリア
  FBase.Clear(FBaseColor);
end;

function TColorSelectorBase.GetBaseHeight: Integer;
begin
  Result := FBase.Height;
end;

function TColorSelectorBase.GetBaseWidth: Integer;
begin
  Result := FBase.Width;
end;

procedure TColorSelectorBase.Paint;
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

procedure TColorSelectorBase.PreDraw;
begin
  if(FBase.Canvas = nil) then
    Exit;

  FBase.Canvas.BeginScene;
  try
    Draw(FBase.Canvas);
  finally
    FBase.Canvas.EndScene;
  end;

  //FBase.SaveToFile('D:\Temp\test.png');
end;

procedure TColorSelectorBase.Resize;
begin
  inherited;

  var W := Trunc(Width);
  var H := Trunc(Height);
  FBase.SetSize(W, H);

  PreDraw;
end;

procedure TColorSelectorBase.SetBaseColor(const Value: TAlphaColor);
begin
  FBaseColor := Value;
  PreDraw;
end;


{ TCircleSelector.TCircleCursor }
// ... (TCircleCursor, THueCursor, TSVCursor の実装は変更なし) ...
constructor TCircleSelector.TCircleCursor.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture:= True;
  HitTest := False;

  FSelector := AOwner as TCircleSelector;

  Fill.Color := $00ffffff;
  Stroke.Color := TAlphaColors.Black;
end;

procedure TCircleSelector.TCircleCursor.DoResized;
begin
  inherited;
  Update;
end;

procedure TCircleSelector.TCircleCursor.MoveTo(const AX, AY: Single);
begin
  // 継承先で上書き
end;

procedure TCircleSelector.TCircleCursor.Update;
begin
  Stroke.Thickness := Width * CURSOR_THICKNESS_RATIO;
end;

{ TCircleSelector.THueCursor }

procedure TCircleSelector.THueCursor.MoveTo(const AX, AY: Single);
begin
  var X, Y: Single;

  with FSelector do
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

  var S := Scene.GetSceneScale;
  SetBounds(
    X - Width / 2 + CURSOR_MARGIN * S,
    Y - Height / 2,
    Width,
    Height);
end;

{ TCircleSelector.TSVCursor }

procedure TCircleSelector.TSVCursor.MoveTo(const AX, AY: Single);
begin
  var P: TPointF;

  with FSelector do
  begin
    var W0, W1, W2: Single;

    // 1) バリセントリックを計算
    if not CalcBarycentric(AX, AY, W0, W1, W2) then
      Exit;

    // 2) 三角形の外側なら、0 以上にクランプして正規化
    if (W0 < 0) or (W1 < 0) or (W2 < 0) then
    begin
      if W0 < 0 then W0 := 0;
      if W1 < 0 then W1 := 0;
      if W2 < 0 then W2 := 0;

      var Sum := W0 + W1 + W2;
      if Sum <= 0 then
        Exit;

      W0 := W0 / Sum;
      W1 := W1 / Sum;
      W2 := W2 / Sum;
    end;

    // 3) 三角形上の実際の座標を再構成
    P :=
      PointF(
        FTriP0.X * W0 + FTriP1.X * W1 + FTriP2.X * W2,
        FTriP0.Y * W0 + FTriP1.Y * W1 + FTriP2.Y * W2
      );

    // 4) HSV の S,V を算出
    //    頂点割り当て: P0=純色(S=1,V=1), P1=白(S=0,V=1), P2=黒(S=0,V=0)
    var V := EnsureRange(W0 + W1, 0, 1); // V = W0 + W1 (P0とP1でV=1、P2でV=0)

    var S: Single;
    if V > 0 then
      S := EnsureRange(W0 / V, 0, 1) // S = W0 / V で計算
    else
      S := 0; // V=0 の場合は S は 0

    // ここで S,V をセレクタ側に反映
    FSat := S;
    FVal := V;
    CalcColor;
  end;

  // 5) カーソル（TCircle）の中心を P に合わせる
  SetBounds(P.X - Width / 2, P.Y - Height / 2, Width, Height);
end;

{ TCircleSelector }

function TCircleSelector.CalcBarycentric(
  const AX, AY: Single;
  out AW0, AW1, AW2: Single): Boolean;
begin
  // 三角形が成立していない場合は即座に false
  if FTriDenom = 0 then
  begin
    AW0 := -1;
    AW1 := -1;
    AW2 := -1;
    Exit(False);
  end;

  // 判定したい座標
  var PX := AX;
  var PY := AY;

  // バリセントリック計算 (P2を基準点とした式)
  AW0 :=
    (
      (FTriP1.Y - FTriP2.Y) * (PX - FTriP2.X) +
      (FTriP2.X - FTriP1.X) * (PY - FTriP2.Y)
    ) / FTriDenom;

  AW1 :=
    (
      (FTriP2.Y - FTriP0.Y) * (PX - FTriP2.X) +
      (FTriP0.X - FTriP2.X) * (PY - FTriP2.Y)
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
  // ラジアンを度数に変換。ArcTan2 は Y, X の順で渡す
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

  // FBase のビットマップデータにアクセス
  var Data :=
    TBitmapData.Create(GetBaseWidth, GetBaseHeight, TPixelFormat.RGBA);

  FBase.Map(TMapAccess.ReadWrite, Data);
  try
    // 色相環とSV三角形の描画を分離
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
  var ROuter := FRadius;
  var RInner := FInnerRadius;

  // BaseColor の TAlphaColorRec 表現 (ブレンドの背景色として使用)
  var BaseColorRec := TAlphaColorRec(FBaseColor);

  // 走査範囲を円環の外接四角形に限定
  var StartX := Max(0, Trunc(FCX - ROuter - AA_WIDTH));
  var EndX := Min(W - 1, Trunc(FCX + ROuter + AA_WIDTH));
  var StartY := Max(0, Trunc(FCY - ROuter - AA_WIDTH));
  var EndY := Min(H - 1, Trunc(FCY + ROuter + AA_WIDTH));

  for var Y := StartY to EndY do
  begin
    for var X := StartX to EndX do
    begin
      var DX := X - FCX;
      var DY := Y - FCY;
      var D := Sqrt(DX * DX + DY * DY); // 中心からの距離

      // 1. 色相環の本体領域外のピクセルはスキップ
      if (D > ROuter + AA_WIDTH) or (D < RInner - AA_WIDTH) then
        Continue;

      // 2. 色相環の色と背景色を取得
      var HueColor := GetColorFromCircle(DX, DY);
      var FRC := TAlphaColorRec(HueColor);

      var FinalAlpha := 1.0;
      var OuterBlend := 1.0; // 0.0 (外側) から 1.0 (内側)
      var InnerBlend := 1.0; // 0.0 (穴の内側) から 1.0 (色相環の内側)

      // 3. 外周のアンチエイリアス処理
      if D > ROuter - AA_WIDTH then // 外周のブレンド領域内
      begin
        // D = ROuter + AA_WIDTH で 0.0 (完全に外側)
        // D = ROuter - AA_WIDTH で 1.0 (完全に内側)
        OuterBlend :=
          EnsureRange((ROuter + AA_WIDTH - D) / (AA_WIDTH * 2), 0.0, 1.0);
      end;

      // 4. 内周のアンチエイリアス処理
      if D < RInner + AA_WIDTH then // 内周のブレンド領域内
      begin
        // D = R_inner - AA_WIDTH で 0.0 (穴の内側、色が塗られない)
        // D = R_inner + AA_WIDTH で 1.0 (色相環の内側、完全に色が塗られる)
        InnerBlend :=
          EnsureRange((D - (RInner - AA_WIDTH)) / (AA_WIDTH * 2), 0.0, 1.0);
      end;

      // 5. 最終アルファ値の計算 (両方の境界の影響を受ける)
      FinalAlpha := OuterBlend * InnerBlend;

      if FinalAlpha > 0.001 then // ほぼ 0 でなければ描画
      begin
        var A := Trunc(FinalAlpha * 255);

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
  // SV三角形の枠を描画 (この枠はCanvasに直接描画し、アンチエイリアスが適用される)
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
// ... (他のメソッドは変更なし) ...
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
      TBitmapData.Create(GetBaseWidth, GetBaseHeight, TPixelFormat.RGBA);

    FBase.Map(TMapAccess.Write, Data);
    try
      DrawTriangle(FBase.Canvas, Data);
    finally
      FBase.Unmap(Data);
    end;
  finally
    FBase.Canvas.EndScene;
  end;

  InvalidateRect(BoundsRect);
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

  FHueCursor.SetBounds(
    FTriP0.X + FCursorMargin,
    FTriP0.Y - FCursorMargin,
    Size,
    Size);

  // SVカーソルの初期位置を FTriP2 (黒, 左上) に合わせておく
  FSVCursor.SetBounds(
    FTriP2.X - FCursorMargin,
    FTriP2.Y - FCursorMargin,
    Size,
    Size);

  FHueCursor.Update;
  FSVCursor.Update;

  PreDraw;
end;

procedure TCircleSelector.SetColor(const Value: TAlphaColor);
begin
  FColor := Value;
  RGB2HSV(Value, FHue, FSat, FVal);
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

{ TRectSelector.TRectCursor }

constructor TRectSelector.TRectCursor.Create(AOwner: TComponent);
begin
  inherited;

  AutoCapture := True;
  HitTest := False;

  FSelector := AOwner as TRectSelector;

  Fill.Color   := $00FFFFFF;          // 中身は透過
  Stroke.Color := TAlphaColors.Black; // 枠線だけ見せる
end;

procedure TRectSelector.TRectCursor.DoResized;
begin
  inherited;
  Update;
end;

procedure TRectSelector.TRectCursor.Update;
begin
  Stroke.Thickness := Width * CURSOR_THICKNESS_RATIO;
end;

procedure TRectSelector.TRectCursor.MoveTo(const AX, AY: Single);
begin
  var X := AX;
  var Y := AY;

  with FSelector do
  begin
    // 有効範囲（グラデーション矩形）にクランプ
    if X < FInnerRect.Left   then X := FInnerRect.Left;
    if X > FInnerRect.Right  then X := FInnerRect.Right;
    if Y < FInnerRect.Top    then Y := FInnerRect.Top;
    if Y > FInnerRect.Bottom then Y := FInnerRect.Bottom;

    var InnerW := Max(1.0, FInnerRect.Width  - 1);
    var InnerH := Max(1.0, FInnerRect.Height - 1);

    // 0..1 に正規化
    var NX := (X - FInnerRect.Left) / InnerW;
    var NY := (Y - FInnerRect.Top)  / InnerH;

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

    CalcColor;
  end;

  SetBounds(
    X - Width  / 2,
    Y - Height / 2,
    Width,
    Height);
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
  FCursor.Update;
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
    TBitmapData.Create(GetBaseWidth, GetBaseHeight, TPixelFormat.RGBA);

  if not FBase.Map(TMapAccess.Write, Data) then
    Exit;
  try
    var W := Data.Width;
    var H := Data.Height;

    // グラデーションを描く矩形（マージンを除く）
    var R := FInnerRect;

    var LeftI   := EnsureRange(Trunc(R.Left),  0, W - 1);
    var RightI  := EnsureRange(Trunc(R.Right) - 1, 0, W - 1);
    var TopI    := EnsureRange(Trunc(R.Top),   0, H - 1);
    var BottomI := EnsureRange(Trunc(R.Bottom) - 1, 0, H - 1);

    var InnerW := Max(1.0, R.Width  - 1);
    var InnerH := Max(1.0, R.Height - 1);

    // 内側だけ HSV グラデーション
    for var y := TopI to BottomI do
    begin
      var NY := (y - R.Top) / InnerH;          // 0..1
      var S, V: Single;

      if NY <= 0.5 then
      begin
        V := 1;
        S := EnsureRange(NY / 0.5, 0, 1);
      end
      else
      begin
        S := 1;
        V := EnsureRange(1 - (NY - 0.5) / 0.5, 0, 1);
      end;

      for var x := LeftI to RightI do
      begin
        var NX := (x - R.Left) / InnerW;       // 0..1
        var Hdeg := EnsureRange(NX, 0, 1) * 360;

        var C := HSV2RGB(Hdeg, S, V);
        Data.SetPixel(x, y, C);
      end;
    end;
  finally
    FBase.Unmap(Data);
  end;
end;

procedure TRectSelector.MouseDown(
  AButton: TMouseButton; AShift: TShiftState; AX, AY: Single);
begin
  inherited;
  if AButton = TMouseButton.mbLeft then
  begin
    FCursor.MoveTo(AX, AY);
    InvalidateRect(BoundsRect);
  end;
end;

procedure TRectSelector.MouseMove(
  AShift: TShiftState; AX, AY: Single);
begin
  inherited;
  if Pressed then
  begin
    FCursor.MoveTo(AX, AY);
    InvalidateRect(BoundsRect);
  end;
end;

procedure TRectSelector.MouseUp(
  AButton: TMouseButton; AShift: TShiftState; AX, AY: Single);
begin
  inherited;
  // 特に何もしないが、将来用に残しておく
end;

procedure TRectSelector.Resize;
begin
  inherited;

  var W := GetBaseWidth;
  var H := GetBaseHeight;

  // マージンを除いた内側の矩形
  FInnerRect :=
    RectF(
      MARGIN,
      MARGIN,
      W - MARGIN,
      H - MARGIN
    );

  // カーソルサイズは短辺に対してちょっとだけ
  var Size := Min(FInnerRect.Width, FInnerRect.Height) * 0.06;
  if Size < 10 then
    Size := 10;

  FCursor.SetBounds(
    FInnerRect.Left,
    FInnerRect.Bottom,
    Size,
    Size);
  FCursor.Update;

  PreDraw;
  UpdateCursorFromHSV;
end;

procedure TRectSelector.SetColor(const Value: TAlphaColor);
begin
  if FColor = Value then
    Exit;

  FColor := Value;
  RGB2HSV(Value, FHue, FSat, FVal);

  // Hue は 0..360 に正規化
  if FHue < 0 then
    FHue := FHue + 360 * Ceil(-FHue / 360);
  FHue := EnsureRange(FHue, 0, 360);

  // S, V は 0..1 にクランプ
  FSat := EnsureRange(FSat, 0, 1);
  FVal := EnsureRange(FVal, 0, 1);

  CalcColor;          // FColor を HSV から再計算 & DoChange
  UpdateCursorFromHSV;
end;

procedure TRectSelector.UpdateCursorFromHSV;
begin
  if FInnerRect.IsEmpty then
    Exit;

  var InnerW := Max(1.0, FInnerRect.Width  - 1);
  var InnerH := Max(1.0, FInnerRect.Height - 1);

  // X : Hue
  var NX := EnsureRange(FHue / 360, 0, 1);
  var X  := FInnerRect.Left + NX * InnerW;

  // Y : どちらの半分に置くかを決める
  // V=1 からのズレと S=1 からのズレを比較して、近いほうを採用
  var UseTopHalf := Abs(FVal - 1) <= Abs(FSat - 1);

  var Y: Single;
  if UseTopHalf then
  begin
    // 上半分: V=1, S=0→1
    var S := EnsureRange(FSat, 0, 1);
    Y := FInnerRect.Top + S * (InnerH * 0.5);
  end
  else
  begin
    // 下半分: S=1, V=1→0
    var V := EnsureRange(FVal, 0, 1);
    Y := FInnerRect.Top + (InnerH * 0.5) + (1 - V) * (InnerH * 0.5);
  end;

  FCursor.SetBounds(
    X - FCursor.Width  / 2,
    Y - FCursor.Height / 2,
    FCursor.Width,
    FCursor.Height);
end;

end.
