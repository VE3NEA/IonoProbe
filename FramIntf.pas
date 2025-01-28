unit FramIntf;

interface

uses
  SysUtils, Forms, SolArray, SolArrs;

type
  ILivePlotFrame = interface
    function Frame: TFrame;
    function GetNearestHeight(H: integer): integer;
    procedure LoadPanelOrder(const IniFile: TFileName);
    procedure SavePanelOrder(const IniFile: TFileName);
    function Get_TabIndex: integer;
    procedure Set_TabIndex(Value: integer);
    procedure SetArrays(Arrays: TSolarArrays);
    procedure DataChanged;
    property TabIndex: integer read Get_TabIndex write Set_TabIndex;
  end;


implementation

end.

