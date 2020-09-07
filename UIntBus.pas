unit UIntBus;

interface
type

  IBus = interface
    procedure Write(AAdress: UInt16; AData: UInt8);
    function Read(AAdress: UInt16; AIsReadOnly: Boolean = False): UInt8;
  end;


implementation

end.
