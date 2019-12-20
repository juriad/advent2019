with Ada.Text_IO; use Ada.Text_IO;
with Ada.Integer_Text_IO; use Ada.Integer_Text_IO;
with Ada.Command_line; use Ada.Command_line;

procedure Prg is
    type Dig is mod 10;
    type Pattern is mod 4;

    type Digs is array(Natural range <>) of Dig;
    type Digs_Acc is access Digs;

    type Ints is array(Natural range <>) of Integer;
    type Ints_Acc is access Ints;

    function Read_Input(FileName: String) return Digs_Acc is
        Input: File_Type;
        C: Character;
        Count: Integer := 0;
    begin
        Open(File => Input, Mode => In_File, Name => FileName);
        while (not End_Of_File(Input)) loop
            Get(File => Input, Item => C);
            if C in '0'..'9' then
                Count := Count + 1;
            end if;
        end loop;
        Close(Input);

        declare
            NN: Digs_Acc := new Digs(0 .. Count - 1);
        begin
            Open(File => Input, Mode => In_File, Name => FileName);
            for I in 0 .. Count - 1 loop
                Get(File => Input, Item => C);
                NN(I) := Dig(Character'Pos(C) - 48);
            end loop;
            Close(Input);
            return NN;
        end;
    end;

    function Calc_Prefixes(Ds: Digs_Acc) return Ints_Acc is
        Sums: Ints_Acc := new Ints(Ds'Range);
    begin
        for I in Ds'Range loop
            Sums(I) := (if I > 0 then Sums(I - 1) else 0) + Integer(Ds(I));
        end loop;
        return Sums;
    end;

    function Get_Sum(Prefixes: Ints_Acc; From: Integer; To: Integer) return Integer is
        T: Integer := (if To > Prefixes'Last then Prefixes'Last else To);
    begin
        if To < 0 then
            return 0;
        else
--            Put_Line("From "& Integer'Image(From) & " to " & Integer'Image(T));
            return Prefixes(T) - (if From <= 0 then 0 else Prefixes(From - 1));
        end if;
    end;

    function Get_Pattern(P: Pattern) return Integer is
    begin
        case P is
            when 0 => return 0;
            when 1 => return 1;
            when 2 => return 0;
            when 3 => return -1;
        end case;
    end;

    function Get_Last(I: Integer) return Dig is
    begin
        if I < 0 then
            return Dig((-I) mod 10);
        else
            return Dig(I mod 10);
        end if;
    end;

    function Calc_Phase(Ds: Digs_Acc) return Digs_Acc is
        Prefixes: Ints_Acc := Calc_Prefixes(Ds);
        Ds2: Digs_Acc := new Digs(Ds'Range);

        II: Integer;
        P: Pattern;
        Sum: Integer;
    begin
        for I in Ds'Range loop
            II := -1;
            P := 0;
            Sum := 0;

            while II <= Ds'Last loop
                Sum := Sum + Get_Pattern(P) * Get_Sum(Prefixes, II, II + I);
                II := II + I + 1;
                P := P + 1;
            end loop;

            Ds2(I) := Get_Last(Sum);
--            Put_Line(Dig'Image(Ds(I)) & " = " & Dig'Image(Ds2(I)));
        end loop;
--        New_Line;

        return Ds2;
    end;

    function Multiply(Ds: Digs_Acc; Count: Integer) return Digs_Acc is
        Ds2: Digs_Acc := new Digs(0..Ds'Length * Count);
    begin
        for I in Ds2'Range loop
            Ds2(I) := Ds(I mod Ds'Length);
        end loop;
        return Ds2;
    end;

    function As_Integer(Ds: Digs_Acc; From: Integer; To: Integer) return Integer is
        Int: Integer := 0;
    begin
        for I in reverse 0..6 loop
            Int := Int * 10 + Integer(Ds(6 - I));
        end loop;
        return Int;
    end;

    Ds : Digs_Acc := Read_Input(Argument(1));
    Offset: Integer := As_Integer(Ds, 0, 6);
    Ds2: Digs_Acc := Multiply(Ds, 10000);
begin
    for I in 1..100 loop
        Ds := Calc_Phase(Ds);
    end loop;

    for I in Ds'Range loop
        Put(Integer(Ds(I)), Width => 0);
    end loop;
    New_Line;

    for I in 1..100 loop
        Ds2 := Calc_Phase(Ds2);
        Put_Line("Iteration " & Integer'Image(I));
    end loop;

    Put_Line("Offset = " & Integer'Image(Offset));
    for I in Offset..Offset+7 loop
        Put(Integer(Ds2(I)), Width => 0);
    end loop;
    New_Line;

end Prg;
