unit UBus;

interface
uses
  SysUtils
  , UInt6502
  , UImpl6502
  , UIntBus;

type

  TBus = class(TInterfacedObject, IBus)
  private
    FCPU: I6502;


  public
    FRAM: TBytes;

    constructor Create;
    destructor Destroy; override;

    procedure Write(AAdress: UInt16; AData: UInt8);
    function Read(AAdress: UInt16; AIsReadOnly: Boolean = False): UInt8;

    property CPU: I6502 read FCPU;
  end;

implementation

{ TBus }

constructor TBus.Create;
begin
  SetLength(FRAM, 64 * 1024);
  for var i := 0 to Length(FRAM) - 1 do
    FRam[i] := $00;

  FCPU := T6502.Create(Self as IBus);
end;

destructor TBus.Destroy;
begin

  FCPU := nil;
  inherited;
end;

function TBus.Read(AAdress: UInt16; AIsReadOnly: Boolean): UInt8;
begin
  if (AAdress >= $0000) and (AAdress <= $FFFF) then
    Result := FRAM[AAdress]
  else
    Result := $00;
end;

procedure TBus.Write(AAdress: UInt16; AData: UInt8);
begin
  if (AAdress >= $0000) and (AAdress <= $FFFF) then
    FRAM[AAdress] := AData;
end;

end.
