program ColorSelector;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {Form1},
  PK.Graphic.ColorSelectors in 'PK.Graphic.ColorSelectors.pas',
  PK.Graphic.DDA in 'Lib\PK.Graphic.DDA.pas',
  PK.Graphic.ColorConverter in 'Lib\PK.Graphic.ColorConverter.pas',
  PK.Math.AdjustUtils in 'Lib\PK.Math.AdjustUtils.pas',
  PK.Graphic.HSVSelectors in 'PK.Graphic.HSVSelectors.pas',
  PK.Graphic.CellSelectors in 'PK.Graphic.CellSelectors.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
