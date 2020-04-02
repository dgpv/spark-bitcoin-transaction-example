-- Copyright (C) 2019-2020 Dmitry Petukhov https://github.com/dgpv
--
-- This file is part of spark-bitcoin-transaction-example
--
-- It is subject to the license terms in the LICENSE file found in the top-level
-- directory of this distribution.
--
-- No part of spark-bitcoin-transaction-example, including this file, may be copied, modified,
-- propagated, or distributed except according to the terms contained in the
-- LICENSE file.

pragma SPARK_Mode(On);

package body Utils is

package body Array_Summation is
    function Cumulative_Sums(State: State_Type) return Cumulative_Sums_Type
    is
        Cumulative_Sizes: Cumulative_Sums_Type := (others => 0);
    begin
        for Index in Array_Index_Type'Range loop

            Cumulative_Sizes(Index) :=
                Element_Size_Function(Index, State)
                    + (
                        if Index = Array_Index_Type'First
                        then 0
                        else Cumulative_Sizes(Array_Index_Type'Pred(Index))
                      );

            pragma Loop_Invariant(
                Cumulative_Sizes(Index) <=
                    Long_Natural(
                        Element_Size_Max
                            * (Array_Index_Type'Pos(Index) - Array_Index_Type'Pos(Array_Index_Type'First) + 1)
                    )
            );
            pragma Loop_Invariant(
                for all I in Array_Index_Type'First..Index =>
                    Cumulative_Sizes(I) <=
                        Long_Natural(
                            Element_Size_Max
                                * (Array_Index_Type'Pos(I) - Array_Index_Type'Pos(Array_Index_Type'First) + 1)
                        )
            );
            pragma Loop_Invariant(
                for all I in Array_Index_Type'First..Index =>
                    Cumulative_Sizes(I) =
                        Element_Size_Function(I, State)
                               + (
                                    if I = Array_Index_Type'First
                                    then 0
                                    else Cumulative_Sizes(Array_Index_Type'Pred(I))
                               )
            );
        end loop;
--        pragma Assert (
--            Cumulative_Sizes(Array_Index_Type'Last)
--                <= Long_Natural(
--                      Element_Size_Max * (
--                                                Array_Index_Type'Pos(Array_Index_Type'Last)
--                                                - Array_Index_Type'Pos(Array_Index_Type'First) + 1
--                                         )
--                   )
--        );
        return Cumulative_Sizes;
    end;
end Array_Summation;

package body Serialized_Data_Summation is
    function Cumulative_Sums(Arr: Array_Type) return Cumulative_Sums_Type
    is
        Cumulative_Sizes: Cumulative_Sums_Type (Arr'Range) := (others => 0);
        Element_Size: Long_Natural;
    begin
        for Index in Arr'Range loop

            Element_Size := Element_Size_Function(Arr(Index));
            pragma Assert (Element_Size <= Long_Natural(Element_Size_Max));

            Cumulative_Sizes(Index) :=
                Element_Size
                    + (
                        if Index = Arr'First
                        then 0
                        else Cumulative_Sizes(Array_Index_Type'Pred(Index))
                      );

            pragma Loop_Invariant(
                Cumulative_Sizes(Index) <=
                    Long_Natural(Element_Size_Max)
                        * (Array_Index_Type'Pos(Index) - Array_Index_Type'Pos(Arr'First) + 1)
            );
            pragma Loop_Invariant(
                for all I in Arr'First..Index =>
                    Cumulative_Sizes(I) <=
                        Long_Natural(Element_Size_Max)
                            * (Array_Index_Type'Pos(I) - Array_Index_Type'Pos(Arr'First) + 1)
            );
            pragma Loop_Invariant(
                for all I in Arr'First..Index =>
                    Cumulative_Sizes(I) =
                        Element_Size_Function(Arr(I))
                               + (
                                    if I = Arr'First
                                    then 0
                                    else Cumulative_Sizes(Array_Index_Type'Pred(I))
                               )
            );
        end loop;
        pragma Assert (Cumulative_Sizes(Arr'Last) <= Long_Natural(Element_Size_Max) * Arr'Length);
        return Cumulative_Sizes;
    end;
end Serialized_Data_Summation;

procedure Byte_Array_To_Hex(Bytes: in Byte_Array_Type; Output_String: out String)
is
begin
    -- need to show to the prover that it is initialized, until we have appropriate
    -- tools to track 'Initialized properies of the values
    Output_String := (others => ' ');

    for Index in 0..Bytes'Length-1 loop
        Output_String(Output_String'First+Index*2..Output_String'First + Index*2+1) :=
            Byte_To_Hex(Bytes(Bytes'First + Index));
        pragma Loop_Invariant((Output_String'First + Index*2) <= Output_String'Last);
    end loop;
end Byte_Array_To_Hex;

procedure Byte_Array_To_Hex_Reverse(Bytes: in Byte_Array_Type; Output_String: out String)
is
    Reverse_Index: Natural;
begin
    -- need to show to the prover that it is initialized, until we have appropriate
    -- tools to track 'Initialized properies of the values
    Output_String := (others => ' ');

    for Index in 0..Bytes'Length-1 loop
        Reverse_Index := (Bytes'Length - 1) - Index;
        Output_String(Output_String'First+Index*2..Output_String'First + Index*2+1) :=
            Byte_To_Hex(Bytes(Bytes'First + Reverse_Index));
        pragma Loop_Invariant((Output_String'First + Index*2) <= Output_String'Last);
    end loop;
end Byte_Array_To_Hex_Reverse;

end Utils;
