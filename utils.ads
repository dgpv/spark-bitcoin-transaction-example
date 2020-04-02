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

package Utils is

type Long_Natural is range 0..2**63-1;

generic
    Max_Length: Natural := 128;
package Bounded_Strings is

    type Bounded_String is private;

    function Length(S: Bounded_String) return Natural;

    function To_String (S: Bounded_String) return String
    with Post => (
        To_String'Result'Length = Length(S)
        and then To_String'Result'Length <= Max_Length
    );

    function To_Bounded_String (S: String) return Bounded_String
    with Pre => S'Length <= Max_Length and then S'Last < Positive'Last - (Max_Length - S'Length),
         Post => To_String(To_Bounded_String'Result) = S;

    function "=" (SB: Bounded_String; S: String) return Boolean;
    function "=" (S: String; SB: Bounded_String) return Boolean;

private
    subtype Bounded_String_Length_Type is Natural range 0..Max_Length;
    subtype Fixed_Length_String_Type is String (1..Max_Length);

    type Bounded_String is
        record
            Length: Bounded_String_Length_Type;
            Data: Fixed_Length_String_Type;
        end record;

    function Length(S: Bounded_String) return Natural
    is (S.Length);

    function To_String(S: Bounded_String) return String
    is (S.Data(S.Data'First..S.Data'First + S.Length - 1));

    function To_Bounded_String(S: String) return Bounded_String
    is ((Length => S'Length, Data => S & (1..(Max_Length - S'Length) => ' ')));

    function "=" (SB: Bounded_String; S: String) return Boolean
    is (To_String(SB) = S);

    function "=" (S: String; SB: Bounded_String) return Boolean
    is (To_String(SB) = S);

end Bounded_Strings;

type Byte_Type is mod 2**8;
type Nybble_Type is mod 2**4;

type Byte_Array_Type is array (Positive range <>) of Byte_Type;

function Is_Hex_Digit(Char: Character) return Boolean is
    ((Char in '0'..'9') or else (Char in 'A'..'F') or else (Char in 'a'..'f'));

function Hex_Digit_Value(Char: Character) return Byte_Type is
    (
        if Char in '0'..'9' then Character'Pos(Char) - Character'Pos('0')
        elsif Char in 'A'..'F' then Character'Pos(Char) - Character'Pos('A') + 10
        else Character'Pos(Char) - Character'Pos('a') + 10
    )
    with Pre => Is_Hex_Digit(Char);

function Nybble_To_Hex(Nybble: Nybble_Type) return Character
is (
    case Nybble is
        when 0..9 => Character'Val(Character'Pos('0') + Natural(Nybble)),
        when 16#A#..16#F# => Character'Val(Character'Pos('A') + Natural(Nybble))
);

function Hex_To_Byte(Hex: String) return Byte_Type
is (Hex_Digit_Value(Hex(Hex'First)) * 16 + Hex_Digit_Value(Hex(Hex'First + 1)))
with Pre => Hex'Length = 2 and (for all Char of Hex => Is_Hex_Digit(Char));

function Byte_To_Hex(Byte: Byte_Type) return String
is (Nybble_To_Hex(Nybble_Type(Byte / 2**4)) & Nybble_To_Hex(Nybble_Type(Byte and 16#F#)))
with Post => Byte_To_Hex'Result'Length = 2;

procedure Byte_Array_To_Hex(Bytes: in Byte_Array_Type; Output_String: out String)
with Pre => Output_String'Length = Bytes'Length * 2;

procedure Byte_Array_To_Hex_Reverse(Bytes: in Byte_Array_Type; Output_String: out String)
with Pre => Output_String'Length = Bytes'Length * 2;


-- These two packages are unused - too heavy for the prover when they are used
-- to count various limits, and the limits are therefore specified explicitly.

generic
    type State_Type (<>) is private;
    type Array_Index_Type is (<>);
    with function Element_Size_Function(Index: Array_Index_Type; State: State_Type)
        return Long_Natural;
    Element_Size_Max: Positive;
package Array_Summation with Ghost is
    type Cumulative_Sums_Type is array (Array_Index_Type) of Long_Natural;

    function Cumulative_Sums(State: State_Type) return Cumulative_Sums_Type
    with
        Post => (
            (
                for all I in Array_Index_Type'Range =>
                    Cumulative_Sums'Result(I) <=
                        Long_Natural(
                            Element_Size_Max
                                * (Array_Index_Type'Pos(I) - Array_Index_Type'Pos(Array_Index_Type'First) + 1)
                        )
            )
            and then
            (
                for all I in Array_Index_Type'Range =>
                    Cumulative_Sums'Result(I)
                        = Element_Size_Function(I, State)
                           + (
                                if I = Array_Index_Type'First
                                then 0
                                else Cumulative_Sums'Result(Array_Index_Type'Pred(I))
                           )
            )
        );

    function Sum_of_Element_Sizes(State: State_Type) return Long_Natural
    is (Cumulative_Sums(State)(Array_Index_Type'Last))
    with Post => (
        Sum_of_Element_Sizes'Result
            <= Long_Natural(
                  Element_Size_Max * (
                                            Array_Index_Type'Pos(Array_Index_Type'Last)
                                            - Array_Index_Type'Pos(Array_Index_Type'First) + 1
                                     )
               )
    );
end Array_Summation;

generic
    type Element_Type is private;
    type Array_Index_Type is (<>);
    type Array_Type is array (Array_Index_Type range <>) of Element_Type;
    with function Element_Size_Function(Element: Element_Type) return Long_Natural;
    Element_Size_Max: Positive;
package Serialized_Data_Summation with Ghost is
    type Cumulative_Sums_Type is array (Array_Index_Type range <>) of Long_Natural;

    function Cumulative_Sums(Arr: Array_Type) return Cumulative_Sums_Type
    with
        Pre => Arr'Length > 0,
        Post => (
            Cumulative_Sums'Result'First = Arr'First
            and then
            Cumulative_Sums'Result'Last = Arr'Last
            and then
            (
                for all I in Arr'Range =>
                    Cumulative_Sums'Result(I) <=
                        Long_Natural(Element_Size_Max)
                            * (Array_Index_Type'Pos(I) - Array_Index_Type'Pos(Arr'First) + 1)
            )
            and then
            (
                for all I in Arr'Range =>
                    Cumulative_Sums'Result(I)
                        = Element_Size_Function(Arr(I))
                           + (
                                if I = Arr'First
                                then 0
                                else Cumulative_Sums'Result(Array_Index_Type'Pred(I))
                           )
            )
        );

    function Sum_of_Element_Sizes(Arr: Array_Type) return Long_Natural
    is (if Arr'Length = 0 then 0 else Cumulative_Sums(Arr)(Arr'Last))
    with Post => (Sum_of_Element_Sizes'Result <= Long_Natural(Element_Size_Max) * Arr'Length);
end Serialized_Data_Summation;

end Utils;
