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

package body Bitcoin_Like.Transaction_Outputs is

procedure Deserialize(Output: out Output_Type) is
    use Data_Checkpoints;

    U64_Value: Unsigned_Modular_Int64;

    Raw_Data_Status_Old: Raw_Data.Status_Type := Raw_Data_Status with Ghost;
    Ghost_Data_Tape_Old: Ghost_Data_Tape_Type := Ghost_Data_Tape with Ghost;
begin
    Data_Checkpoint(Tag_Output_Value);

    Data_Readers.Read_Unsigned_64(U64_Value);

    if Got_Error then
        Output := Empty_Output;
        return;
    end if;

    if U64_Value > Unsigned_Modular_Int64(Satoshi_Type'Last) then
        Register_Structural_Error(
            "Encountered an output with a value " & Unsigned_Modular_Int64'Image(U64_Value)
            & " that is larger than allowed by consensus rules (" & Satoshi_Type'Image(Satoshi_Type'Last) & ")"
        );
        Output := Empty_Output;
        return;
    end if;

    Output.Value := Satoshi_Type(U64_Value);

    Data_Checkpoint(Tag_Pubkey_Script);

    Scripts.Deserialize(Output.Pubkey_Script);

    if Got_Error then
        Output := Empty_Output;
        return;
    end if;

    Data_Checkpoint_Final;

    pragma Assert (
        Data_Readers.Unsigned64_Ghost_Tape_Match(
            Unsigned_Modular_Int64(Output.Value),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Output_Value, Data_Checkpoints.State))
    );
    pragma Assert (
        Scripts.Script_Ghost_Tape_Match(
            Output.Pubkey_Script,
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Pubkey_Script, Data_Checkpoints.State))
    );
    pragma Assert (Output_Ghost_Tape_Match(Output, Bytes_Processed_With(Raw_Data_Status_Old)));
    pragma Assert (
        for all I in Ghost_Data_Tape'Range => (
            if (I < Bytes_Processed_With(Raw_Data_Status_Old)
                or I >= Bytes_Processed_With(Raw_Data_Status_Old)
                            + Natural(Output_Serialized_Data_Length(Output)))
            then Ghost_Data_Tape(I) = Ghost_Data_Tape_Old(I)
        )
    );
end Deserialize;

end Bitcoin_Like.Transaction_Outputs;
