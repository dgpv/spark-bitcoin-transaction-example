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

package body Bitcoin_Like.Scripts is

procedure Deserialize(Script: out Script_Type) is
    Script_Data_Length : Compact_Size_Type;

    use Read;

    Bytes_Read_at_Start      : constant Natural := Bytes_Processed with Ghost;
    Bytes_Read_before_Script : Natural with Ghost;
    Ghost_Data_Tape_Old      : Ghost_Data_Tape_Type := Ghost_Data_Tape with Ghost;
begin
    Data_Readers.Read_Compact_Size(Script_Data_Length);

    if Got_Error then
        Script := Empty_Script;
        return;
    end if;

    if Script_Data_Length = 0 then
        pragma Assert (Bytes_Processed - Bytes_Read_at_Start = Size_of_Compact_Size(0));
        Script := Empty_Script;
        return;
    end if;

    if Script_Data_Length >= Script.Data'Length then
        Register_Structural_Error(
            "Script length" & Natural'Image(Script_Data_Length)
            & " is larger than maximum allowed length of" & Natural'Image(Script.Data'Length)
        );
        Script := Empty_Script;
        return;
    end if;

    Bytes_Read_before_Script := Bytes_Processed;

    Script_Body_Reader.Read_Block_Zero_Fill (
        Script_Body_Reader.Block_Type(Script.Data),
        Script_Data_Length
    );

    if Got_Error then
        Script := Empty_Script;
        return;
    end if;

    Script.Length := Script_Data_Length;

    pragma Assert (
        Script_Body_Reader.Block_Ghost_Tape_Match(
            Script_Body_Reader.Block_Type(
                Script.Data(Script.Data'First..Script.Data'First + (Script_Data_Length - 1))),
            Bytes_Read_before_Script)
    );

    pragma Assert (Script_Length(Script) = Script_Data_Length);
    pragma Assert (Script_Data(Script)'Length = Script_Data_Length);

    pragma Assert (
        Data_Readers.Compact_Data_Size_Ghost_Tape_Match(Script_Length(Script), Bytes_Read_at_Start)
        and then
        Script_Body_Reader.Block_Ghost_Tape_Match(
            Script_Body_Reader.Block_Type(Script_Data(Script)),
            Bytes_Read_at_Start + Size_of_Compact_Size(Script_Length(Script)))
    );

    pragma Assert_And_Cut (
            Status_OK
            and then
            Bytes_Processed - Bytes_Read_at_Start = Script_Serialized_Data_Length(Script)
            and then
            Script_Ghost_Tape_Match(Script, Bytes_Read_at_Start)
            and then
            (
                for all I in Read.Ghost_Data_Tape'Range =>
                    (if (I < Bytes_Read_at_Start
                         or I >= (Bytes_Read_at_Start + Script_Serialized_Data_Length(Script)))
                     then Read.Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I))
            )
    );
end Deserialize;

procedure Serialize(Script: in Script_Type) is
    use Write;
    Bytes_Written_at_Start : constant Natural := Bytes_Processed with Ghost;
begin

    Data_Writers.Write_Compact_Size(Script_Length(Script));

    if Got_Error then
        pragma Assert (Bytes_Processed - Bytes_Written_at_Start
                           < Size_of_Compact_Size(Script_Length(Script)));
        return;
    end if;

    pragma Assert (Bytes_Processed - Bytes_Written_at_Start
                       = Size_of_Compact_Size(Script_Length(Script)));

    if Script_Length(Script) = 0 then
        return;
    end if;

    Script_Body_Writer.Write_Block(Script_Body_Writer.Block_Type(Script_Data(Script)));

end Serialize;

end Bitcoin_Like.Scripts;
