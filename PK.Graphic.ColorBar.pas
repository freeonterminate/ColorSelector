unit PK.Graphic.ColorBar;

interface

uses
  System.SysUtils
  , System.Classes
  , System.UITypes
  , System.Types
  , FMX.Controls
  , FMX.Graphics
  , FMX.StdCtrls
  , FMX.Objects
  , FMX.Types
  ;

type
  TColorChangeEvent =
    procedure(Sender: TObject; const AColor: TAlphaColor) of object;

  TCustomColorBar = class(TControl)
  private const
    VALUE_MIN = 0;
    VALUE_MAX = 255;
    VALUE_RANGE = VALUE_MAX - VALUE_MIN;

    MARGIN_NAME = 2;
    MARGIN_VALUE = 4;

    DELTA_KEY_UPDOW = 8;
  private var
    FValue: Integer;
    FColorName: TLabel;
    FValueText: TLabel;
    FBarBase: TPanel;
    FBack: TRectangle;
    FBar: TRectangle;
    FOnChange: TNotifyEvent;
  private
    procedure SetValue(const AValue: Integer);
    procedure CalcPos(const AX: Single);
    function CalcBarMaxWidth: Single;
    procedure CalcBarCorner;
    procedure UpdateSize;
    procedure BarBaseApplyStyleLookupHandler(Sender: TObject);
  protected
    procedure Resize; override;
    function GetColor: TAlphaColor; virtual; abstract;
    function GetColorName: String; virtual; abstract;
    procedure KeyDown(
      var AKey: Word;
      var AKeyChar: WideChar;
      AShift: TShiftState); override;
    procedure MouseDown(
      AButton: TMouseButton;
      AShift: TShiftState;
      AX, AY: Single); override;
    procedure MouseMove(AShift: TShiftState; AX, AY: Single); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetNewScene(AScene: IScene); override;
    procedure SetValueWithoutEvent(const AValue: Integer);
    property Value: Integer read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TRBar = class(TCustomColorBar)
  protected
    function GetColor: TAlphaColor; override;
    function GetColorName: String; override;
  end;

  TGBar = class(TCustomColorBar)
  protected
    function GetColor: TAlphaColor; override;
    function GetColorName: String; override;
  end;

  TBBar = class(TCustomColorBar)
  protected
    function GetColor: TAlphaColor; override;
    function GetColorName: String; override;
  end;

  TRGBBars = class(TControl)
  private var
    FRBar: TRBar;
    FGBar: TGBar;
    FBBar: TBBar;
    FColor: TAlphaColor;
    FOnChange: TColorChangeEvent;
  private
    procedure SetColor(const AColor: TAlphaColor);
    procedure BarChangeHandler(Sender: TObject);
  protected
    function CreateBar<T: TCustomColorBar>(const AAlign: TAlignLayout): T;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetColorWithoutEvent(const AColor: TAlphaColor);
  published
    property Color: TAlphaColor read FColor write SetColor;
    property OnChange: TColorChangeEvent read FOnChange write FOnChange;
  end;

implementation

uses
  System.Math;

{ TCustomColorBar }

procedure TCustomColorBar.BarBaseApplyStyleLookupHandler(Sender: TObject);
begin
  UpdateSize;
end;

procedure TCustomColorBar.CalcBarCorner;
begin
  var PL := FBarBase.Padding.Left;
  var PR := FBarBase.Padding.Right;

  var D := FBarBase.Width - FBar.Width - PL - PR;
  var C: TCorners := [TCorner.TopLeft, TCorner.BottomLeft];

  if D < (FBar.XRadius / 2) then
    C := C + [TCorner.TopRight, TCorner.BottomRight];

  FBar.Corners := C;
end;

function TCustomColorBar.CalcBarMaxWidth: Single;
begin
  Result := FBarBase.Width - FBarBase.Padding.Right - FBarBase.Padding.Left;
end;

procedure TCustomColorBar.CalcPos(const AX: Single);
begin
  var W := CalcBarMaxWidth;
  if W = 0 then
    Exit;

  var V := EnsureRange(AX - FBarBase.Position.X + FBarBase.Padding.Left, 0, W);
  SetValue(Trunc(V * VALUE_RANGE / W));
end;

constructor TCustomColorBar.Create(AOwner: TComponent);
begin
  inherited;

  SetSize(160, 16);

  AutoCapture := True;
  CanFocus := True;

  FColorName := TLabel.Create(Self);
  FColorName.Align := TAlignLayout.Left;
  FColorName.HitTest := False;
  FColorName.Text := GetColorName;

  FValueText := TLabel.Create(Self);
  FValueText.Align := TAlignLayout.Right;
  FValueText.HitTest := False;
  FValueText.TextSettings.HorzAlign := TTextAlign.Trailing;

  FBarBase := TPanel.Create(Self);
  FBarBase.Align := TAlignLayout.Client;
  FBarBase.HitTest := False;
  FBarBase.ClipChildren := True;
  FBarBase.OnApplyStyleLookup := BarBaseApplyStyleLookupHandler;

  FBack := TRectangle.Create(Self);
  FBack.Align := TAlignLayout.Contents;
  FBack.HitTest := False;
  FBack.Stroke.Kind := TBrushKind.None;
  FBack.Fill.Color := TAlphaColors.White;
  FBack.Parent := FBarBase;

  FBar := TRectangle.Create(Self);
  FBar.Align := TAlignLayout.Left;
  FBar.HitTest := False;
  FBar.Stroke.Kind := TBrushKind.None;
  FBar.Fill.Color := GetColor;
  FBar.Parent := FBarBase;

  FColorName.Parent := Self;
  FValueText.Parent := Self;
  FBarBase.Parent := Self;
end;

procedure TCustomColorBar.KeyDown(
  var AKey: Word;
  var AKeyChar: WideChar;
  AShift: TShiftState);
begin
  inherited;

  case AKey of
    vkLeft:
      SetValue(FValue - 1);
    vkUp:
      SetValue(FValue + DELTA_KEY_UPDOW);
    vkRight:
      SetValue(FValue + 1);
    vkDown:
      SetValue(FValue - DELTA_KEY_UPDOW);
  end;
end;

procedure TCustomColorBar.MouseDown(
  AButton: TMouseButton;
  AShift: TShiftState;
  AX, AY: Single);
begin
  inherited;

  CalcPos(AX);
  SetFocus;
end;

procedure TCustomColorBar.MouseMove(AShift: TShiftState; AX, AY: Single);
begin
  inherited;

  if Pressed then
    CalcPos(AX);
end;

procedure TCustomColorBar.Resize;
begin
  inherited;
  UpdateSize;
end;

procedure TCustomColorBar.SetNewScene(AScene: IScene);
begin
  inherited;
  UpdateSize;
end;

procedure TCustomColorBar.SetValue(const AValue: Integer);
begin
  if FValue = AValue then
    Exit;

  SetValueWithoutEvent(AValue);

  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCustomColorBar.SetValueWithoutEvent(const AValue: Integer);
begin
  FValue := EnsureRange(AValue, VALUE_MIN, VALUE_MAX);

  var W := CalcBarMaxWidth;
  FBar.Width:= W * FValue / VALUE_RANGE;
  CalcBarCorner;

  FValueText.Text := FValue.ToString;
end;

procedure TCustomColorBar.UpdateSize;

  function FindBack(const AObject: TFmxObject): TRectangle;
  begin
    Result := nil;
    if AObject = nil then
      Exit;

    if AObject is TRectangle then
      Result := TRectangle(AObject)
    else
    begin
      for var C in AObject.Children do
      begin
        var Res := FindBack(C);
        if Res <> nil then
        begin
          Result := Res;
          Break;
        end;
      end;
    end;
  end;

begin
  if (Canvas = nil) or (Scene = nil) then
    Exit;

  FColorName.Width := Canvas.TextWidth('W') + MARGIN_NAME;
  FValueText.Width := Canvas.TextWidth('000') + MARGIN_VALUE;

  var MTop := (Height - Canvas.TextHeight('H')) / 2;
  FColorName.Margins.Top := MTop;
  FValueText.Margins.Top := MTop;

  var S := Scene.GetSceneScale;
  var R := RectF(S, S, S, S);
  FBarBase.Padding.Rect := R;
  FBack.Margins.Rect := R;

  var BackR := FindBack(FBarBase);

  if BackR <> nil then
  begin
    FBack.XRadius := BackR.XRadius;
    FBack.YRadius := BackR.YRadius;

    FBar.XRadius := BackR.XRadius;
    FBar.YRadius := BackR.YRadius;
  end;

  SetValue(FValue);
end;

{ TRBar }

function TRBar.GetColorName: String;
begin
  Result := 'R';
end;

function TRBar.GetColor: TAlphaColor;
begin
  Result := TAlphaColors.Red;
end;

{ TGBar }

function TGBar.GetColorName: String;
begin
  Result := 'G';
end;

function TGBar.GetColor: TAlphaColor;
begin
  Result := TAlphaColors.Green;
end;

{ TBBar }

function TBBar.GetColorName: String;
begin
  Result := 'B';
end;

function TBBar.GetColor: TAlphaColor;
begin
  Result := TAlphaColors.Blue;
end;

{ TRGBBars }

procedure TRGBBars.BarChangeHandler(Sender: TObject);
begin
  var C: TAlphaColorRec;
  C.A := $ff;
  C.R := FRBar.Value;
  C.G := FGBar.Value;
  C.B := FBBar.Value;

  SetColor(TAlphaColor(C));
end;

constructor TRGBBars.Create(AOwner: TComponent);
begin
  inherited;

  SetSize(180, 80);

  FRBar := CreateBar<TRBar>(TAlignLayout.Top);
  FGBar := CreateBar<TGBar>(TAlignLayout.VertCenter);
  FBBar := CreateBar<TBBar>(TAlignLayout.Bottom);
end;

function TRGBBars.CreateBar<T>(const AAlign: TAlignLayout): T;
begin
  Result := T.Create(Self);
  Result.Align := AAlign;
  Result.OnChange := BarChangeHandler;
  Result.Parent := Self;
end;

destructor TRGBBars.Destroy;
begin
  FBBar.Free;
  FGBar.Free;
  FRBar.Free;

  inherited;
end;

procedure TRGBBars.SetColor(const AColor: TAlphaColor);
begin
  if FColor = AColor then
    Exit;

  SetColorWithoutEvent(AColor);

  if Assigned(FOnChange) then
    FOnChange(Self, FColor);
end;

procedure TRGBBars.SetColorWithoutEvent(const AColor: TAlphaColor);
begin
  FColor := AColor;

  FRBar.SetValueWithoutEvent(TAlphaColorRec(FColor).R);
  FGBar.SetValueWithoutEvent(TAlphaColorRec(FColor).G);
  FBBar.SetValueWithoutEvent(TAlphaColorRec(FColor).B);
end;

end.
