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

with Bitcoin_Like.Utils; use Bitcoin_Like.Utils;

package body Bitcoin_Like.Transaction_Inputs is

procedure Deserialize(Input: out Input_Type) is
    use Data_Checkpoints;

    U32_Value: Unsigned_Ranged_Int32;

    Raw_Data_Status_Old: Raw_Data.Status_Type := Raw_Data_Status with Ghost;
begin
    Data_Checkpoint(Tag_Txid);

    Txid_Reader.Read_Block(Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid));

    if Got_Error then
        Input := Empty_Input;
        return;
    end if;

    Data_Checkpoint(Tag_Prevout_Index);

    pragma Assert_And_Cut (
        Status_OK
        and then
        Current_Data_Tag = Tag_Prevout_Index
        and then
        Data_Checkpoints.Partial_State_Consistent(
            Bytes_Processed_Since(Raw_Data_Status_Old),
            Tag_Txid
        )
        and then
        Txid_Reader.Block_Ghost_Tape_Match(
            Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Txid, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Txid, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old))
        and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length)
        and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
    );

    Data_Readers.Read_Unsigned_32(U32_Value);

    if Got_Error then
        Input := Empty_Input;
        return;
    end if;

    if U32_Value > Unsigned_Ranged_Int32(Previous_Output_Index_Type'Last) then
        Register_Structural_Error(
            "Encountered index for previous output" & Unsigned_Ranged_Int32'Image(U32_Value)
            & " is larger than maximum expected length of" & Previous_Output_Index_Type'Image(Previous_Output_Index_Type'Last)
        );
        Input := Empty_Input;
        return;
    end if;

    -- If we want to ensure that the fields are only set within appropriate contexts,
    -- we can use setter procedures that would check Current_Data_Tag in precondition.
    --
    -- Set_Previous_Output_Index(Input, Previous_Output_Index_Type(U32_Value));

    Input.Previous_Output_Point.Index := Previous_Output_Index_Type(U32_Value);

    Data_Checkpoint(Tag_Signature_Script);

    pragma Assert_And_Cut (
        Status_OK
        and then
        Current_Data_Tag = Tag_Signature_Script
        and then
        Data_Checkpoints.Partial_State_Consistent(
            Bytes_Processed_Since(Raw_Data_Status_Old),
            Tag_Prevout_Index
        )
        and then
        Txid_Reader.Block_Ghost_Tape_Match(
            Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Txid, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Txid, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old))
        and then
        Data_Readers.Unsigned32_Ghost_Tape_Match(
            Unsigned_Ranged_Int32(Input.Previous_Output_Point.Index),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Prevout_Index, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Prevout_Index, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length)
        and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length + 4)
        and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
    );

    Scripts.Deserialize(Input.Signature_Script);

    if Got_Error then
        Input := Empty_Input;
        return;
    end if;

    -- prover tends to lose the state to verify this condition,
    -- this Assert helps to retain it until the next Assert_And_Cut
    pragma Assert (
        Txid_Reader.Block_Ghost_Tape_Match(
            Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Txid, Data_Checkpoints.State))
    );

    Data_Checkpoint(Tag_Sequence_Number);

    pragma Assert_And_Cut (
        Status_OK
        and then
        Current_Data_Tag = Tag_Sequence_Number
        and then
        Data_Checkpoints.Partial_State_Consistent(
            Bytes_Processed_Since(Raw_Data_Status_Old),
            Tag_Signature_Script
        )
        and then
        Txid_Reader.Block_Ghost_Tape_Match(
            Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Txid, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Txid, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old))
        and then
        Data_Readers.Unsigned32_Ghost_Tape_Match(
            Unsigned_Ranged_Int32(Input.Previous_Output_Point.Index),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Prevout_Index, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Prevout_Index, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length)
        and then
        Scripts.Script_Ghost_Tape_Match(
            Input.Signature_Script,
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Signature_Script, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Signature_Script, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length + 4)
        and then
        Data_Checkpoints.Element_Size_Match(
            Tag_Signature_Script,
            Long_Natural(
                Size_of_Compact_Size(Scripts.Script_Length(Input.Signature_Script))
                    + Scripts.Script_Length(Input.Signature_Script)
            )
        )
        and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length + 4 + Size_of_Compact_Size(Scripts.Script_Length(Input.Signature_Script)) + Scripts.Script_Length(Input.Signature_Script))
        and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
    );

    Data_Readers.Read_Unsigned_32(U32_Value);

    if Got_Error then
        Input := Empty_Input;
        return;
    end if;

    Input.Sequence := Input_Sequence_Type(U32_Value);

    -- prover tends to lose the state to verify this conditions,
    -- this Assert helps to retain it until the next Assert_And_Cut
    pragma Assert (
        Txid_Reader.Block_Ghost_Tape_Match(
            Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Txid, Data_Checkpoints.State))
        and then
        Data_Readers.Unsigned32_Ghost_Tape_Match(
            Unsigned_Ranged_Int32(Input.Previous_Output_Point.Index),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Prevout_Index, Data_Checkpoints.State))
        and then
        Scripts.Script_Ghost_Tape_Match(
            Input.Signature_Script,
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Signature_Script, Data_Checkpoints.State))
    );

    Data_Checkpoint_Final;

    pragma Assert_And_Cut (
        Status_OK
        and then
        Data_Checkpoints.Final_State_Consistent(
            Bytes_Processed_Since(Raw_Data_Status_Old)
        )
        and then
        Txid_Reader.Block_Ghost_Tape_Match(
            Txid_Reader.Block_Type(Input.Previous_Output_Point.Txid),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Txid, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Txid, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old))
        and then
        Data_Readers.Unsigned32_Ghost_Tape_Match(
            Unsigned_Ranged_Int32(Input.Previous_Output_Point.Index),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Prevout_Index, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Prevout_Index, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length)
        and then
        Scripts.Script_Ghost_Tape_Match(
            Input.Signature_Script,
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Signature_Script, Data_Checkpoints.State))
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Signature_Script, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length + 4)
        and then
        Data_Checkpoints.Element_Size_Match(
            Tag_Signature_Script,
            Long_Natural(
                Size_of_Compact_Size(Scripts.Script_Length(Input.Signature_Script))
                    + Scripts.Script_Length(Input.Signature_Script)
            )
        )
        and then (
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(Tag_Sequence_Number, Data_Checkpoints.State)
                = Bytes_Processed_With(Raw_Data_Status_Old)
                    + Input.Previous_Output_Point.Txid'Length + 4
                    + Size_of_Compact_Size(Scripts.Script_Length(Input.Signature_Script))
                        + Scripts.Script_Length(Input.Signature_Script))
        and then
        Data_Readers.Unsigned32_Ghost_Tape_Match(
            Unsigned_Ranged_Int32(Input.Sequence),
            Data_Checkpoints.Checkpoint_States.Ghost_Tape_Position(
                Tag_Sequence_Number, Data_Checkpoints.State))
        and then
        Input_Serialized_Data_Length(Input)
            = Long_Natural(Bytes_Processed_Since(Raw_Data_Status_Old))
        and then
        Input_Ghost_Tape_Match(Input, Bytes_Processed_With(Raw_Data_Status_Old))
        -- and then (Bytes_Processed = Bytes_Processed_With(Raw_Data_Status_Old) + Input.Previous_Output_Point.Txid'Length + Size_of_Compact_Size(Scripts.Script_Length(Input.Signature_Script)) + Scripts.Script_Length(Input.Signature_Script) + 4)
        -- and then Data_Checkpoints.Checkpoint_States.Bytes_Processed_Prev(Data_Checkpoints.State) = Bytes_Processed
    );

end Deserialize;

end Bitcoin_Like.Transaction_Inputs;
