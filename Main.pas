unit Main;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.ScrollBox,
  FMX.Memo, FMX.Controls.Presentation, FMX.StdCtrls
  , UIntBus
  ;

type
  TForm4 = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    LblStatus: TLabel;
    Label4: TLabel;
    LblRegister: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    Button1: TButton;
    LblCurrentOP: TLabel;
    DumpRAM: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure DumpRAMClick(Sender: TObject);
  private
    FBus: IBus;
    procedure RefreshView;
  public
    { Public declarations }
  end;

var
  Form4: TForm4;



implementation
uses
  StrUtils
  , UImpl6502
  , UBus
  ;
const
  TEST_CODE = '$A2 $0A $8E $00 $00 $A2 $03 $8E $01 $00 $AC $00 $00 $A9 $00 $18 $6D $01 $00 $88 $D0 $FA $8D $02 $00 $EA $EA $EA';
  BASE_CODE = $8000;


{$R *.fmx}

procedure TForm4.Button1Click(Sender: TObject);
begin
  (FBus as TBus).CPU.Clock;
  RefreshView;
end;

procedure TForm4.DumpRAMClick(Sender: TObject);
var
  LRamFile: File;
  i: Integer;
begin
  AssignFile(LRamFile, 'RAMDump.bin');
  Rewrite(LRamFile, 64);
  try
    BlockWrite(LRamFile, (FBus as TBus).FRAM[0], 1024);
  finally
    CloseFile(LRamFile);
  end;

end;

procedure TForm4.FormCreate(Sender: TObject);
var
  i: Integer;
  LHexaNumber: string;
  LCodeIndex: Integer;
begin
  FBus := TBus.Create;

  (FBus as TBus).FRAM[$FFFC] := $00;
  (FBus as TBus).FRAM[$FFFD] := $80;

  i := 1;
  LCodeIndex := BASE_CODE;
  while i < Length(TEST_CODE) do
  begin
    LHexaNumber := Copy(TEST_CODE, i, 3);
    (FBus as TBus).FRAM[LCodeIndex] := StrToInt(LHexaNumber);
    Inc(LCodeIndex);
    Inc(i, 4);
  end;


  (FBus as TBus).CPU.Reset;


end;

procedure TForm4.FormDestroy(Sender: TObject);
begin
  FBus := nil;
end;

procedure TForm4.RefreshView;
begin
  LblStatus.Text := '';
  if ((FBus as TBus).CPU.GetStatus and Ord(cfNegative)) <> 0 then
    LblStatus.Text := LblStatus.Text + '1 '
  else
    LblStatus.Text := LblStatus.Text + '0 ';

  if ((FBus as TBus).CPU.GetStatus and Ord(cfOverflow)) <> 0 then
    LblStatus.Text := LblStatus.Text + '1 '
  else
    LblStatus.Text := LblStatus.Text + '0 ';

  if ((FBus as TBus).CPU.GetStatus and Ord(cfUnused)) <> 0 then
    LblStatus.Text := LblStatus.Text + '1 '
  else
    LblStatus.Text := LblStatus.Text + '0 ';

  if ((FBus as TBus).CPU.GetStatus and Ord(cfBreak)) <> 0 then
    LblStatus.Text := LblStatus.Text + '1 '
  else
    LblStatus.Text := LblStatus.Text + '0 ';

  if ((FBus as TBus).CPU.GetStatus and Ord(cfDecimalMode)) <> 0 then
    LblStatus.Text := LblStatus.Text + '1 '
  else
    LblStatus.Text := LblStatus.Text + '0 ';

  if ((FBus as TBus).CPU.GetStatus and Ord(cfDisableInterrupts)) <> 0 then
    LblStatus.Text := LblStatus.Text + '1 '
  else
    LblStatus.Text := LblStatus.Text + '0 ';

  if ((FBus as TBus).CPU.GetStatus and Ord(cfZero)) <> 0 then
    LblStatus.Text := LblStatus.Text + '1 '
  else
    LblStatus.Text := LblStatus.Text + '0 ';

  if ((FBus as TBus).CPU.GetStatus and Ord(cfCarry)) <> 0 then
    LblStatus.Text := LblStatus.Text + '1'
  else
    LblStatus.Text := LblStatus.Text + '0';

  LblRegister.Text := '';
  LblRegister.Text := LblRegister.Text + Format('%.2x ', [(FBus as TBus).CPU.GetA]);
  LblRegister.Text := LblRegister.Text + Format('%.2x ', [(FBus as TBus).CPU.GetX]);
  LblRegister.Text := LblRegister.Text + Format('%.2x ', [(FBus as TBus).CPU.GetY]);
  LblRegister.Text := LblRegister.Text + Format('%.2x  ', [(FBus as TBus).CPU.GetStackP]);
  LblRegister.Text := LblRegister.Text + Format('%.4x ', [(FBus as TBus).CPU.GetPC]);

end;

end.
