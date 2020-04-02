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

with Utils; use Utils;

with Bitcoin_Like.Utils; use Bitcoin_Like.Utils;
with Bitcoin_Like.Data_Accessors;

generic
    with package Data_Readers is new Bitcoin_Like.Data_Accessors.Readers (<>);
    with package Data_Writers is new Bitcoin_Like.Data_Accessors.Writers (<>);

    Max_Data_Size       : in Positive;

package Bitcoin_Like.Scripts is

    subtype Script_Octet is Byte_Type;
    subtype Script_Data_Length_Type is Natural range 0..Max_Data_Size;
    type Script_Data_Type is array (Positive range <>) of Script_Octet;

    type Script_Type is private;

    function Script_Length(Script: Script_Type) return Script_Data_Length_Type;
    function Script_Data(Script: Script_Type) return Script_Data_Type;

    function Empty_Script return Script_Type;
    function Construct_Script(Data: Script_Data_Type) return Script_Type
    with
        Pre => Data'Length <= Max_Data_Size and Data'Last < Positive'Last - Max_Data_Size,
        Post => (
            Script_Length(Construct_Script'Result) = Data'Length
            and Script_Data(Construct_Script'Result)'Length = Data'Length
        );

    package Script_Body_Reader is new Data_Readers.Block_Reader(
        Octet_Type => Script_Octet);
    package Script_Body_Writer is new Data_Writers.Block_Writer(
        Octet_Type => Script_Octet);

    package Read renames Data_Readers.Status_Control;

    Max_Serialized_Data_Size: constant Positive := Size_of_Compact_Size(Max_Data_Size) + Max_Data_Size;

    function Script_Serialized_Data_Length(Script: Script_Type) return Natural
    is (Size_of_Compact_Size(Script_Length(Script)) + Script_Length(Script));

    function Script_Ghost_Tape_Match(Script: Script_Type; Offset: Natural) return Boolean
    is (
        Data_Readers.Compact_Data_Size_Ghost_Tape_Match(Script_Length(Script), Offset)
        and then
        Script_Body_Reader.Block_Ghost_Tape_Match(
            Script_Body_Reader.Block_Type(Script_Data(Script)),
            Offset + Size_of_Compact_Size(Script_Length(Script)))
    )
    with Ghost => True,
         Pre => Offset <= Natural'Last - Script_Serialized_Data_Length(Script),
         Global => (Input => Read.Ghost_Data_Tape);

    procedure Deserialize(Script: out Script_Type)
    with
        Pre => Read.Status_OK,
        Post => (
            if Read.Status_OK
            then (
                Read.Bytes_Processed_Since(Read.Raw_Data_Status'Old) = Script_Serialized_Data_Length(Script)
                and then
                Script_Ghost_Tape_Match(Script, Read.Bytes_Processed_With(Read.Raw_Data_Status'Old))
                and then
                (
                    for all I in Read.Ghost_Data_Tape'Range =>
                        (if (I < Read.Bytes_Processed_With(Read.Raw_Data_Status'Old)
                             or I >= (Read.Bytes_Processed_With(Read.Raw_Data_Status'Old)
                                      + Script_Serialized_Data_Length(Script)))
                         then Read.Ghost_Data_Tape(I) = Read.Ghost_Data_Tape'Old(I))
                )
            )
        ),
        Depends => (
            Script => (Read.Raw_Data_Status, Read.Structural_Status),
            Read.Structural_Status =>+ Read.Raw_Data_Status,
            Read.Raw_Data_Status =>+ Read.Structural_Status,
            Read.Ghost_Data_Tape =>+ (Read.Raw_Data_Status, Read.Structural_Status)
        ),
        Global => (In_Out => (Read.Raw_Data_Status, Read.Structural_Status, Read.Ghost_Data_Tape));

    package Write renames Data_Writers.Status_Control;

    -- Note: structural status is not used in serialize, but it is present Status_Control
    -- and therefore has to be in dependencies in Serialize.
    -- Removing structural status for writing means separate Status_Control packages for
    -- read and write, which is inconvenient (there's no 'interfaces' for packages).
    procedure Serialize(Script: in Script_Type)
    with
        Pre => Write.Status_OK,
        Post => (
            if Write.Status_OK
            then Write.Bytes_Processed_Since(Write.Raw_Data_Status'Old) =
                    (Size_of_Compact_Size(Script_Length(Script)) + Script_Length(Script))
            else Write.Bytes_Processed_Since(Write.Raw_Data_Status'Old) <
                    (Size_of_Compact_Size(Script_Length(Script)) + Script_Length(Script))
        ),
        Depends => (
            Write.Raw_Data_Status =>+ (Script, Write.Structural_Status),
            Write.Structural_Status =>+ (Script, Write.Raw_Data_Status)
        ),
        Global => (In_Out => (Write.Raw_Data_Status, Write.Structural_Status));

private
    subtype Script_Data_Index_Type is Positive range 1..Max_Data_Size;
    subtype Script_Data_Fixed_Length_Array_Type is Script_Data_Type (1..Max_Data_Size);
    type Script_Type is
        record
            Length: Script_Data_Length_Type;
            Data: Script_Data_Fixed_Length_Array_Type;
        end record;

    function Script_Length(Script: Script_Type) return Script_Data_Length_Type
    is (Script.Length);

    function Script_Data(Script: Script_Type) return Script_Data_Type
    is (Script_Data_Type(
        Script.Data(Script.Data'First..Script.Data'First + Script.Length - 1)));

    function Empty_Script return Script_Type
    is (Construct_Script((1..0 => 0)));

    function Construct_Script(Data: Script_Data_Type) return Script_Type
    is (
            (
                Length => Data'Length,
                Data => Data & (1..(Max_Data_Size - Data'Length) => 0)
            )
    );

end Bitcoin_Like.Scripts;
