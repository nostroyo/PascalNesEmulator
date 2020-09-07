program PNES;

uses
  System.StartUpCopy,
  FMX.Forms,
  Main in 'Main.pas' {Form4},
  UBus in 'UBus.pas',
  UInt6502 in 'UInt6502.pas',
  UImpl6502 in 'UImpl6502.pas',
  UIntBus in 'UIntBus.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
