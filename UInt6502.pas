unit UInt6502;

interface
type

  I6502 = Interface

    procedure Write(AAdress: UInt16; AData: UInt8);
    function Read(AAdress: UInt16): UInt8;
    procedure Clock;
    procedure Reset;
    procedure Irq;
    procedure Nmi;
    function GetStatus: UInt8;
    function GetA: UInt8;
    function GetX: UInt8;
    function GetY: UInt8;
    function GetStackP: UInt8;
    function GetPC: UInt16;
    function GetCurrentOpcode: string;

  End;

implementation

end.
